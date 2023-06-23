#![cfg(test)]
use crate::frontend::lexer::lex_spanned;

use super::*;
use chumsky::prelude::*;

macro_rules! lex_spanned {
    ($code:literal) => {
        crate::frontend::lexer::lex_spanned($code)
            .into_iter()
            .map(|(token, span)| (token, SimpleSpan::from(span)))
            .collect::<Vec<_>>()
    };
}

#[test]
fn parse_variable_decls() {
    let lexemes = lex_spanned!(
        r#"
        local w = 40
        local x, y = 50, 39
        z = "hi"
            "#
    );
    let len = lexemes.len();
    let eoi = SimpleSpan::new(len, len);
    let spanned_input = (lexemes[..]).spanned::<_, _>(eoi).clone();

    panic!("{:#?}", parsing::lua50().parse(spanned_input));
}

// TODO: remove
#[test]
fn parse_table_bracket_indexing() {
    let lexemes = lex_spanned!(r#"x = t[a[b[c[d[e]]]]]"#);
    let len = lexemes.len();
    let eoi = SimpleSpan::new(len, len);
    let spanned_input = (lexemes[..].spanned(eoi)).clone();

    panic!("{:#?}", parsing::lua50().parse(spanned_input));
}

#[test]
fn parse_string_literals() {
    let lexemes = lex_spanned!(
        r#"
        x = "
            hi
        "
            "#
    );
    let len = lexemes.len();
    let eoi = SimpleSpan::new(len, len);
    let spanned_input = (lexemes[..]).spanned::<_, _>(eoi).clone();

    panic!("{:#?}", parsing::lua50().parse(spanned_input));
}

#[test]
fn parse_binops() {
    let lexemes = lex_spanned!(
        r#"
            local x = 3 + 3
        "#
    );
    let len = lexemes.len();
    let eoi = SimpleSpan::new(len, len);
    let spanned_input = (lexemes[..]).spanned::<_, _>(eoi).clone();

    panic!("{:#?}", parsing::lua50().parse(spanned_input));
}
