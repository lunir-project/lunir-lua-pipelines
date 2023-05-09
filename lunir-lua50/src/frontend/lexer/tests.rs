#![cfg(test)]

use super::{Token::*, *};
use itertools::Itertools;
use lifering::lifering;

fn verify(tokens: &[Token], ground_truth: &[Token]) {
    for (index, either_tokens) in tokens.iter().zip_longest(ground_truth.iter()).enumerate() {
        let (ground_truth_token, token) = either_tokens.both().expect(&format!(
            "Slice length mismatch reached at {index}.\nDump:\n{tokens:#?}"
        ));

        if token != ground_truth_token {
            panic!(
                "{token:?} found instead of {ground_truth_token:?} at index {index}\nDump:\n{tokens:#?}"
            );
        }
    }
}

#[test]
fn integer_assignment() {
    let a = lex(r#"local x = 42"#);
    let b = [Local, Identifier("x"), Assign, Number(lifering!(42.0))];

    verify(&a, &b);
}

#[test]
fn arithmetic_expression() {
    let a = lex(r#"(5 / 8 * 2 ^ 32 - 1) + -42"#);
    let b = [
        OpenParen,
        Number(lifering!(5.0)),
        Divide,
        Number(lifering!(8.0)),
        Multiply,
        Number(lifering!(2.0)),
        Exponentiate,
        Number(lifering!(32.0)),
        Subtract,
        Number(lifering!(1.0)),
        CloseParen,
        Add,
        Number(lifering!(-42.0)),
    ];

    verify(&a, &b);
}

#[test]
fn forwards_numeric_for_loop() {
    let a = lex(r#"for i in 1, 10 do
            print(i)
        end"#);
    let b = [
        For,
        Identifier("i"),
        In,
        Number(lifering!(1.0)),
        Comma,
        Number(lifering!(10.0)),
        Do,
        Identifier("print"),
        OpenParen,
        Identifier("i"),
        CloseParen,
        End,
    ];

    verify(&a, &b);
}

#[test]
fn backwards_numeric_for_loop() {
    let a = lex(r#"for i in 10, 1, -1 do
           print(i) 
        end"#);
    let b = [
        For,
        Identifier("i"),
        In,
        Number(lifering!(10.0)),
        Comma,
        Number(lifering!(1.0)),
        Comma,
        Number(lifering!(-1.0)),
        Do,
        Identifier("print"),
        OpenParen,
        Identifier("i"),
        CloseParen,
        End,
    ];

    verify(&a, &b)
}

#[test]
fn numeric_while_loop() {
    let a = lex(r#"
        local counter = 0
        while counter < 10 do
            counter = counter + 1
        end"#);
    let b = [
        Local,
        Identifier("counter"),
        Assign,
        Number(lifering!(0.0)),
        While,
        Identifier("counter"),
        Lt,
        Number(lifering!(10.0)),
        Do,
        Identifier("counter"),
        Assign,
        Identifier("counter"),
        Add,
        Number(lifering!(1.0)),
        End,
    ];

    verify(&a, &b);
}

#[test]
fn commented_code() {
    let a = lex(r#"--local counter = 0
        --while counter < 10 do
        --  counter = counter + 1
        --end"#);
    let b = [];

    verify(&a, &b);
}

#[test]
fn code_with_line_comments() {
    let a = lex(r#"--this does something
        local x = 5;
        -- this does another thing
        local y = x + 5
        do -- whats going on here
            print(x + y)
        end
    "#);
    let b = [
        Local,
        Identifier("x"),
        Assign,
        Number(lifering!(5.0)),
        Semicolon,
        Local,
        Identifier("y"),
        Assign,
        Identifier("x"),
        Add,
        Number(lifering!(5.0)),
        Do,
        Identifier("print"),
        OpenParen,
        Identifier("x"),
        Add,
        Identifier("y"),
        CloseParen,
        End,
    ];

    verify(&a, &b);
}

#[test]
fn ternary() {
    let a = lex(r#"switch and a or b"#);
    let b = [
        Identifier("switch"),
        And,
        Identifier("a"),
        Or,
        Identifier("b"),
    ];

    verify(&a, &b);
}

#[test]
fn table_literal() {
    let a = lex(r#"local t = {1, 2, 3}"#);
    let b = [
        Local,
        Identifier("t"),
        Assign,
        OpenCurly,
        Number(lifering!(1.0)),
        Comma,
        Number(lifering!(2.0)),
        Comma,
        Number(lifering!(3.0)),
        CloseCurly,
    ];

    verify(&a, &b);
}

#[test]
fn variadic_function() {
    let a = lex(r#"function vararg(...)
            local args = {...};
            for item in args do
                print(item)
            end
        end"#);
    let b = [
        Function,
        Identifier("vararg"),
        OpenParen,
        Vararg,
        CloseParen,
        Local,
        Identifier("args"),
        Assign,
        OpenCurly,
        Vararg,
        CloseCurly,
        Semicolon,
        For,
        Identifier("item"),
        In,
        Identifier("args"),
        Do,
        Identifier("print"),
        OpenParen,
        Identifier("item"),
        CloseParen,
        End,
        End,
    ];

    verify(&a, &b);
}

#[test]
fn table_indexing() {
    let a = lex(r#"local t = { 
	    ["hello"] = function(p) print(p) end;
        [true] = 3;
        [false] = -5;
        [3] = "hello";
    }

    t[t[t[true]]]("hello")"#);
    let b = [
        Local,
        Identifier("t"),
        Assign,
        OpenCurly,
        OpenBracket,
        StringLiteral("hello"),
        CloseBracket,
        Assign,
        Function,
        OpenParen,
        Identifier("p"),
        CloseParen,
        Identifier("print"),
        OpenParen,
        Identifier("p"),
        CloseParen,
        End,
        Semicolon,
        OpenBracket,
        True,
        CloseBracket,
        Assign,
        Number(lifering!(3.0)),
        Semicolon,
        OpenBracket,
        False,
        CloseBracket,
        Assign,
        Number(lifering!(-5.0)),
        Semicolon,
        OpenBracket,
        Number(lifering!(3.0)),
        CloseBracket,
        Assign,
        StringLiteral("hello"),
        Semicolon,
        CloseCurly,
        Identifier("t"),
        OpenBracket,
        Identifier("t"),
        OpenBracket,
        Identifier("t"),
        OpenBracket,
        True,
        CloseBracket,
        CloseBracket,
        CloseBracket,
        OpenParen,
        StringLiteral("hello"),
        CloseParen,
    ];

    verify(&a, &b)
}

#[test]
fn boolean_literals() {
    let a = lex(r#"true false true falsee truee false"#);
    let b = [
        True,
        False,
        True,
        Identifier("falsee"),
        Identifier("truee"),
        False,
    ];

    verify(&a, &b);
}

#[test]
fn numeric_literals() {
    let a = lex(r#"0.3141592653589793E1 .5 .30 360 1e-1"#);
    let b = [
        Number(lifering!(3.141592653589793)),
        Number(lifering!(0.5)),
        Number(lifering!(0.30)),
        Number(lifering!(360.0)),
        Number(lifering!(0.1)),
    ];

    verify(&a, &b);
}

#[test]
fn string_literal() {
    let a = lex(r#""what's up?""#);
    let b = [StringLiteral("what's up?")];

    verify(&a, &b);
}
