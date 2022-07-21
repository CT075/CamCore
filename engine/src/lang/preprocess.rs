use std::collections::HashMap;

use indexmap::IndexSet;

use crate::plumbing::*;

use super::syntax::{
    Directive, GroupKind, MacroBody, Node, Span, Spanned, Token, TokenGroup,
    Tree,
};

pub mod parse;

pub trait PreprocessErrorHandler: 'static {
    fn defined_with_args_but_no_body(span: Span) -> Self;

    fn cant_redefine_builtin(span: Span) -> Self;

    fn already_defined(new_defn_loc: Span, old_defn_loc: Span) -> Self;

    fn undef_not_defined(ident: &String, span: Span) -> Self;

    fn pool_unimplemented(span: Span) -> Self;
}

enum Definition {
    Builtin(Box<dyn Fn(TokenGroup) -> TokenGroup>),
    Reserved,
    Empty(Span),
    Rename(Vec<TokenGroup>, Span),
    Macro(IndexSet<String>, Vec<TokenGroup>, Span),
}

struct Context<E> {
    defines: HashMap<String, Definition>,
    errors: Vec<E>,
}

impl<E: PreprocessErrorHandler> Context<E> {
    fn log_error(&mut self, err: E) {
        self.errors.push(err)
    }

    /*
    fn walk(&mut self, Tree(lines): &Tree) -> Vec<Spanned<FlatNode>> {
        lines
            .iter()
            .flat_map(|(node, span)| self.handle_node(node, span))
            .collect()
    }

    fn handle_node(
        &mut self,
        node: &Node,
        span: &Span,
    ) -> Vec<Spanned<FlatNode>> {
        match node {
            Node::Message(swv) => {
                vec![(FlatNode::Message(swv.clone()), span.clone())]
            }
            Node::Line(line) => {
                vec![(FlatNode::Line(self.expand_line(line)), span.clone())]
            }
            Node::Directive(d) => self.dispatch_directive(d, span),
        }
    }

    fn expand_group(
        &self,
        group: &Spanned<TokenGroup>,
        out: &mut Vec<Spanned<TokenGroup>>,
    ) {
        let (group, span) = group;
    }

    fn expand_line_impl(
        &self,
        line: &Vec<Spanned<TokenGroup>>,
        out: &mut Vec<Spanned<TokenGroup>>,
    ) {
        for (group, span) in line {
            match group {
                TokenGroup::Group { kind, members } => panic!("todo"),
                TokenGroup::Single(token) => panic!("todo"),
            };
        }
    }

    fn expand_line(
        &self,
        line: &Vec<Spanned<TokenGroup>>,
    ) -> Vec<Spanned<TokenGroup>> {
        let mut out = vec![];

        self.expand_line_impl(line, &mut out);

        out
    }

    fn walk_if(
        &mut self,
        f: impl Fn(bool) -> bool,
        ident: &String,
        then: &Tree,
        else_: &Tree,
    ) -> Vec<Spanned<FlatNode>> {
        if f(self.defines.contains_key(ident)) {
            self.walk(then)
        } else {
            self.walk(else_)
        }
    }

    fn dispatch_directive(
        &mut self,
        d: &Directive,
        span: &Span,
    ) -> Vec<Spanned<FlatNode>> {
        use Directive::*;
        match d {
            IfDef(ident, then, else_) => self.walk_if(id, ident, then, else_),
            IfNDef(ident, then, else_) => {
                self.walk_if(|b| !b, ident, then, else_)
            }
            Define(ident, args, body) => {
                let body = match (args, body) {
                    (None, MacroBody::Macro(body)) => Definition::Rename(
                        body.iter().map(|(group, _)| group.clone()).collect(),
                        span.clone(),
                    ),
                    (Some(args), MacroBody::Macro(body)) => Definition::Macro(
                        args.clone(),
                        body.iter().map(|(group, _)| group.clone()).collect(),
                        span.clone(),
                    ),
                    (None, MacroBody::Empty) => Definition::Empty(span.clone()),
                    (Some(args), MacroBody::Empty) => {
                        self.log_error(E::defined_with_args_but_no_body(
                            span.clone(),
                        ));
                        Definition::Macro(args.clone(), vec![], span.clone())
                    }
                };

                match self.defines.insert(ident.clone(), body) {
                    None => (),
                    Some(Definition::Empty(old_span))
                    | Some(Definition::Rename(_, old_span))
                    | Some(Definition::Macro(_, _, old_span)) => self
                        .log_error(E::already_defined(span.clone(), old_span)),
                    Some(Definition::Builtin(_))
                    | Some(Definition::Reserved) => {
                        self.log_error(E::cant_redefine_builtin(span.clone()))
                    }
                };

                vec![]
            }
            Include(path) => {
                vec![(
                    FlatNode::Directive((
                        IncludeKind::Event,
                        IncludeSource::File(path.clone()),
                    )),
                    span.clone(),
                )]
            }
            Incbin(path) => {
                vec![(
                    FlatNode::Directive((
                        IncludeKind::Binary,
                        IncludeSource::File(path.clone()),
                    )),
                    span.clone(),
                )]
            }
            Incext(exe, args) => vec![(
                FlatNode::Directive((
                    IncludeKind::Binary,
                    IncludeSource::Process(exe.clone(), args.clone()),
                )),
                span.clone(),
            )],
            Inctevent(exe, args) => vec![(
                FlatNode::Directive((
                    IncludeKind::Event,
                    IncludeSource::Process(exe.clone(), args.clone()),
                )),
                span.clone(),
            )],
            Pool => {
                self.log_error(E::pool_unimplemented(span.clone()));
                vec![]
            }
            Undef(ident) => {
                match self.defines.remove(ident) {
                    Some(_) => (),
                    None => self
                        .log_error(E::undef_not_defined(ident, span.clone())),
                };

                vec![]
            }
        }
    }
    */
}
