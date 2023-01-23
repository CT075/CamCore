use std::collections::{HashMap, HashSet, VecDeque};

use nonempty::{nonempty, NonEmpty};

use crate::{
    lang::{
        parse::{Argument, Event},
        syntax,
        syntax::{Expr, Operator, Span, Spanned},
    },
    plumbing::*,
};

pub trait ErrorHandler: 'static {
    fn symbol_already_defined(
        symbol: String,
        old_loc: Span,
        new_loc: Span,
    ) -> Self;

    fn label_used_in_outer_scope(
        symbol: String,
        outer_loc: Span,
        defn_loc: Span,
    ) -> Self;

    fn cant_redefine_builtin(what: &'static str, span: Span) -> Self;

    fn unmatched_close_scope(span: Span) -> Self;

    fn expected_atomic(
        actual: &'static str,
        why: &'static str,
        span: Span,
    ) -> Self;

    fn org_no_args(span: Span) -> Self;

    fn org_too_many_args(span: Span) -> Self;

    fn org_unknown_vars(unknowns: HashSet<String>, span: Span) -> Self;

    fn push_takes_no_args(span: Span) -> Self;

    fn nothing_to_pop(span: Span) -> Self;

    fn pop_takes_no_args(span: Span) -> Self;

    fn fill_no_args(span: Span) -> Self;
}

pub trait Backend {
    fn write_bytes(&mut self, address: usize, data: &[u8]);
}

pub struct Assembler<B> {
    write_head: usize,
    symbols: SymbolTable,
    org_stack: Vec<Spanned<usize>>,
    backend: B,
}

enum EvalResult {
    Done(i32),
    HasUnknowns(Expr, HashSet<String>),
}

fn eval_impl(
    exp: Expr,
    lookup: impl Fn(&String) -> Option<i32> + Copy,
) -> EvalResult {
    use EvalResult::*;
    use Expr::*;

    match exp {
        Literal(x) => Done(x),
        Var(s) => match lookup(&s) {
            Some(value) => EvalResult::Done(value),
            None => HasUnknowns(Var(s.clone()), vec![s].into_iter().collect()),
        },
        Binop(op, left, right) => {
            match (eval_impl(*left, lookup), eval_impl(*right, lookup)) {
                (Done(l), Done(r)) => Done(op.operate(l, r)),
                (HasUnknowns(el, els), Done(r)) => HasUnknowns(
                    Binop(op, Box::new(el), Box::new(Literal(r))),
                    els,
                ),
                (Done(l), HasUnknowns(er, ers)) => HasUnknowns(
                    Binop(op, Box::new(Literal(l)), Box::new(er)),
                    ers,
                ),
                (HasUnknowns(el, els), HasUnknowns(er, ers)) => HasUnknowns(
                    Binop(op, Box::new(el), Box::new(er)),
                    &els | &ers,
                ),
            }
        }
    }
}

fn plug(exp: Expr, var: &String, value: i32) -> EvalResult {
    eval_impl(exp, |s| if s == var { Some(value) } else { None })
}

impl<B> Assembler<B> {
    pub fn handle_events<E>(
        &mut self,
        evs: Vec<Spanned<Event>>,
    ) -> Result<(), Vec<E>>
    where
        E: ErrorHandler,
    {
        let mut errors: Vec<E> = Vec::new();

        for (ev, span) in evs {
            match self.handle_event(ev, span) {
                Ok(()) => (),
                Err(es) => errors.extend(es),
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    pub fn handle_event<E>(
        &mut self,
        ev: Event,
        span: Span,
    ) -> Result<(), Vec<E>>
    where
        E: ErrorHandler,
    {
        use syntax::Event::*;
        use syntax::Statement::*;
        match ev {
            Statement(Instruction { head, args }) => {
                self.handle_instruction(&head, args, span)
            }
            Statement(Label(s)) => {
                let backlinks = self
                    .symbols
                    .set_label_to_offset((&s).clone(), self.write_head, span)
                    .map_err(|v| vec![v])?;

                todo!()
            }
            OpenScope => {
                self.symbols.open_scope();
                Ok(())
            }
            CloseScope => match self.symbols.close_scope() {
                Some(()) => Ok(()),
                None => Err(vec![E::unmatched_close_scope(span)]),
            },
        }
    }

    pub fn handle_instruction<E>(
        &mut self,
        head: &String,
        args: Vec<(Argument, Span)>,
        span: Span,
    ) -> Result<(), Vec<E>>
    where
        E: ErrorHandler,
    {
        match head.as_str() {
            "ORG" => self.do_org(args, span).map_err(|e| vec![e]),
            "PUSH" => {
                self.do_push(span.clone());
                if args.len() > 0 {
                    Err(vec![E::push_takes_no_args(span)])
                } else {
                    Ok(())
                }
            }
            "POP" => {
                self.do_pop(span.clone()).map_err(|e| vec![e])?;
                if args.len() > 0 {
                    Err(vec![E::pop_takes_no_args(span)])
                } else {
                    Ok(())
                }
            }
            "FILL" => self.do_fill(args, span).map_err(|e| vec![e]),
            _ => todo!(),
        }
    }

    // We don't want to register backlinks in this function, as some opcodes
    // (namely, ORG) don't permit them, so we only take [&self] instead of
    // [&mut self].
    fn eval(&self, exp: Expr) -> EvalResult {
        eval_impl(exp, |s| self.symbols.lookup(s).map(fst))
    }

    fn do_org<E>(
        &mut self,
        args: Vec<(Argument, Span)>,
        span: Span,
    ) -> Result<(), E>
    where
        E: ErrorHandler,
    {
        // XXX: This would be clearer as something along the lines of
        // ```no_run
        //   match args[..] {
        //     [] => ...
        //     [(Argument::Single(exp), span)] => ...
        //     // ...
        //     [_, _, ..] => ,,,
        //   }
        // ```
        //
        // but it doesn't quite work out with the way moves work in patterns,
        // so we need to be a bit more roundabout.

        let mut args: VecDeque<_> = args.into_iter().collect();

        let (first_arg, span) = match args.pop_front() {
            None => return Err(E::org_no_args(span)),
            Some(spanned_arg) => spanned_arg,
        };

        match args.len() {
            // no more args after popping means there was exactly one arg in
            // the vec
            0 => (),
            _ => return Err(E::org_too_many_args(span.clone())),
        };

        match first_arg {
            Argument::Tuple(_) => {
                Err(E::expected_atomic("tuple", "ORG's argument", span.clone()))
            }
            Argument::List(_) => {
                Err(E::expected_atomic("list", "ORG's argument", span.clone()))
            }
            Argument::Single(exp) => match self.eval(exp) {
                EvalResult::Done(i) => {
                    self.write_head = i as usize;
                    Ok(())
                }
                EvalResult::HasUnknowns(_, uks) => {
                    Err(E::org_unknown_vars(uks, span))
                }
            },
        }
    }

    fn do_push(&mut self, span: Span) {
        self.org_stack.push((self.write_head, span))
    }

    fn do_pop<E>(&mut self, span: Span) -> Result<(), E>
    where
        E: ErrorHandler,
    {
        match self.org_stack.pop() {
            Some((t, _)) => {
                self.write_head = t;
                Ok(())
            }
            None => Err(E::nothing_to_pop(span)),
        }
    }

    fn do_fill<E>(
        &mut self,
        args: Vec<(Argument, Span)>,
        span: Span,
    ) -> Result<(), E>
    where
        E: ErrorHandler,
    {
        let mut args: VecDeque<_> = args.into_iter().collect();

        let first_arg = match args.pop_front() {
            None => return Err(E::fill_no_args(span)),
            Some((Argument::Tuple(_), span)) => {
                return Err(E::expected_atomic(
                    "tuple",
                    "FILL's first argument",
                    span,
                ))
            }
            Some((Argument::List(_), span)) => {
                return Err(E::expected_atomic(
                    "list",
                    "FILL's first argument",
                    span,
                ))
            }
            Some((Argument::Single(expr), _)) => expr,
        };

        // TODO: size?
        let fill_value = match args.pop_front() {
            None => 0,
            Some((Argument::Tuple(_), span)) => {
                return Err(E::expected_atomic(
                    "tuple",
                    "FILL's second argument",
                    span,
                ))
            }
            Some((Argument::List(_), span)) => {
                return Err(E::expected_atomic(
                    "list",
                    "FILL's second argument",
                    span,
                ))
            }
            Some((Argument::Single(expr), _)) => match self.eval(expr) {
                _ => todo!(),
            },
        };

        todo!()
    }
}

#[derive(Clone)]
struct Backlink {}

// INVARIANT: The keysets of each hashmap are disjoint.
// INVARIANT: No hashmap contains any reserved key
struct SymbolTable {
    scopes: NonEmpty<HashMap<String, Entry>>,
}

enum Entry {
    Known(usize, Span),
    // The span refers to the location of the first forward reference
    // encountered
    Forwards(NonEmpty<Backlink>, Span),
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            scopes: nonempty![HashMap::new()],
        }
    }

    pub fn open_scope(&mut self) {
        self.scopes.push(HashMap::new())
    }

    pub fn close_scope(&mut self) -> Option<()> {
        self.scopes.pop().map(|_| ())
    }

    pub fn lookup(&self, s: &String) -> Option<Spanned<i32>> {
        // XXX: Currently, we only check whether a label is "good" (e.g., fits
        // in an [i32]) when evaluating expressions. In theory, we should be
        // guarding twrite head moves so we can immediately error when the
        // write head goes too far.
        for scope in self.scopes.iter().rev() {
            if let Some(Entry::Known(v, span)) = scope.get(s) {
                return Some((*v as i32, span.clone()));
            }
        }

        None
    }

    pub fn set_label_to_offset<E>(
        &mut self,
        symbol: String,
        offs: usize,
        span: Span,
    ) -> Result<Vec<Backlink>, E>
    where
        E: ErrorHandler,
    {
        if symbol.as_str() == "currentOffset" {
            return Err(E::cant_redefine_builtin("currentOffset", span));
        }

        let n = self.scopes.len();

        let result = match self.scopes.last_mut().remove(&symbol) {
            // If the innermost scope contains the symbol with backlinks to
            // propagate, do so.
            Some(Entry::Forwards(backlinks, _span)) => Ok(backlinks.into()),

            // If we already know this symbol, return an error
            Some(Entry::Known(_offs, old_span)) => {
                Err(E::symbol_already_defined(
                    symbol.clone(),
                    old_span,
                    span.clone(),
                ))
            }

            // Otherwise, check the outer scopes to ensure that no outer scope
            // has this symbol either.
            None => {
                let mut result = Ok(vec![]);
                for i in 2..=n {
                    let scope = &self.scopes[n - i];

                    match scope.get(&symbol) {
                        Some(Entry::Forwards(_backlinks, outer_span)) => {
                            result = Err(E::label_used_in_outer_scope(
                                symbol.clone(),
                                outer_span.clone(),
                                span.clone(),
                            ));
                            break;
                        }
                        Some(Entry::Known(_offs, old_span)) => {
                            result = Err(E::symbol_already_defined(
                                symbol.clone(),
                                old_span.clone(),
                                span.clone(),
                            ));
                            break;
                        }
                        None => (),
                    }
                }

                result
            }
        };

        // In all cases, we want to replace the entry anyway.
        self.scopes
            .last_mut()
            .insert(symbol, Entry::Known(offs, span));

        result
    }
}
