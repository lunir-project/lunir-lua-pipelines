mod backend;
pub mod frontend;

use std::borrow::Cow;

use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::{prelude::Input, span::SimpleSpan, Parser};
use frontend::{lexer::lex_spanned, parser::parsing::lua50};

pub fn compile<'a>(source: &str, filename: impl Into<Cow<'a, str>>) {
    let lexemes = lex_spanned(source);

    let lexemes = lexemes
        .into_iter()
        .map(|(token, span)| (token, SimpleSpan::from(span)))
        .collect::<Vec<_>>();

    let len = lexemes.len();
    let eoi = SimpleSpan::from(len..len);

    let spanned = (lexemes[..]).spanned(eoi);

    let output = lua50().parse(spanned).into_output_errors();

    let name = filename.into();

    output.1.iter().for_each(|e| {
        Report::build(ReportKind::Error, name.clone(), e.span().start)
            .with_code(1)
            .with_message(e.to_string())
            .with_label(
                Label::new((name.clone(), e.span().into_range()))
                    .with_message(format!("{:?}", e.reason()))
                    .with_color(Color::Red),
            )
            .finish()
            .eprint((name.clone(), Source::from(source)))
            .unwrap()
    });
}
