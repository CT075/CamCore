use std::collections::HashMap;

use nonempty::{nonempty, NonEmpty};

use crate::lang::{
    parse::Event,
    syntax,
    syntax::{span::SpannedW, Argument, Span, Spanned},
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

    fn unmatched_close_scope(span: Span) -> Self;
}

pub trait Backend {
    fn write_bytes(&mut self, address: usize, data: &[u8]);
}

pub struct Assembler<B> {
    write_head: usize,
    symbols: SymbolTable,
    backend: B,
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
                self.handle_instruction(&head, args)
            }
            Statement(Label(s)) => {
                let backlinks = self
                    .symbols
                    .register_label((&s).clone(), self.write_head, span)
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
        args: Vec<(Argument<SpannedW>, Span)>,
    ) -> Result<(), Vec<E>>
    where
        E: ErrorHandler,
    {
        todo!()
    }
}

#[derive(Clone)]
struct Backlink {}

// INVARIANT: The keysets of each hashmap are disjoint.
struct SymbolTable {
    scopes: NonEmpty<HashMap<String, Entry>>,
}

enum Entry {
    Known(usize, Span),
    // The span refers to the location of the first-encountered backlink
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

    pub fn register_label<E>(
        &mut self,
        symbol: String,
        offs: usize,
        span: Span,
    ) -> Result<Vec<Backlink>, E>
    where
        E: ErrorHandler,
    {
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
