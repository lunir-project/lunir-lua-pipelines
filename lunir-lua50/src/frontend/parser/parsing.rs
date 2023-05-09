use crate::frontend::lexer::Token::{self, *};
use crate::frontend::parser::parsing::NodeData::KeyValuePair;
use chumsky::{
    extra::Err, input::SpannedInput, prelude::IterParser, prelude::*, recursive::Recursive,
};
use tinyvec::TinyVec;

use super::ast::{Block, FunctionData, Locality, Node, NodeData, NodeKind, UnaryOperator};

type TokenSpanPair<'a> = (Token<'a>, SimpleSpan);
type SpannedTokens<'a> = SpannedInput<Token<'a>, SimpleSpan, &'a [TokenSpanPair<'a>]>;
type RichError<'a> = Err<Rich<'a, Token<'a>>>;

pub(crate) fn lua50<'a>() -> impl Parser<'a, SpannedTokens<'a>, Node, RichError<'a>> + Clone {
    let mut block = Recursive::declare();

    block.define(
        statement(block.clone())
            .padded_by(just(Semicolon).repeated())
            .repeated()
            .collect::<Block>(),
    );

    block.map(|statements| Node {
        children: statements,
        data: None,
        kind: NodeKind::Root,
    })
}

fn statement<'a>(
    block_parser: impl Parser<'a, SpannedTokens<'a>, Block, RichError<'a>> + Clone,
) -> impl Parser<'a, SpannedTokens<'a>, Node, RichError<'a>> + Clone {
    choice((variable_declaration(), function(block_parser)))
}

fn variable_declaration<'a>() -> impl Parser<'a, SpannedTokens<'a>, Node, RichError<'a>> + Clone {
    group((
        just(Local).ignored().or_not().labelled("local"),
        identifier().labelled("name"),
        group((just(Assign).ignored(), expression())).or_not(),
    ))
    .map(|(locality, name, initialised_to)| {
        let mut n = Node {
            children: vec![Node {
                children: vec![],
                data: Some(NodeData::String(Box::new(name.to_string()))),
                kind: NodeKind::String,
            }],
            data: Some(NodeData::Locality(match locality {
                Some(_) => Locality::Local,
                None => Locality::Global,
            })),
            kind: NodeKind::VariableDeclaration,
        };

        if let Some(t) = initialised_to {
            n.children.push(t.1);
        }

        n
    })
}

fn expression<'a>() -> impl Parser<'a, SpannedTokens<'a>, Node, RichError<'a>> + Clone {
    choice((variable_name(), literal(), unary_operation())).labelled("expression")
}

fn literal<'a>() -> impl Parser<'a, SpannedTokens<'a>, Node, RichError<'a>> + Clone {
    choice((string_literal(), number_literal(), boolean_literal()))
}

fn expression_list<'a>() -> impl Parser<'a, SpannedTokens<'a>, (), RichError<'a>> + Clone {
    expression().separated_by(just(Comma)).allow_trailing()
}

fn map<'a>() -> impl Parser<'a, SpannedTokens<'a>, Vec<(Node, Node)>, RichError<'a>> + Clone {
    group((
        choice((
            expression().delimited_by(just(OpenBracket), just(CloseBracket)),
            variable_name(),
        ))
        .labelled("key"),
        just(Assign).ignored(),
        expression().labelled("value"),
    ))
    .map(|(key, _, value)| (key, value))
    .repeated()
    .collect::<Vec<_>>()
}

// fn array<'a>() -> impl Parser<'a, SpannedTokens<'a>, Node, RichError<'a>> + Clone {
//     expression_list()
//         .labelled("array")
//         .delimited_by(just(OpenCurly), just(CloseCurly))
//         .map(|v| Node {
//             children: vec![],
//             data: Some(NodeData::KeyValuePair(Box::new(KeyValuePair()))),
//             kind: todo!(),
//         })
// }

fn boolean_literal<'a>() -> impl Parser<'a, SpannedTokens<'a>, Node, RichError<'a>> + Clone {
    choice((just(True), just(False))).map(|tok| Node {
        children: vec![],
        data: Some(NodeData::Boolean(matches!(tok, True))),
        kind: NodeKind::Boolean,
    })
}

fn boolean<'a>() -> impl Parser<'a, SpannedTokens<'a>, Node, RichError<'a>> + Clone {
    choice((boolean_literal(), variable_name())).labelled("boolean")
}

fn unary_operation<'a>() -> impl Parser<'a, SpannedTokens<'a>, Node, RichError<'a>> + Clone {
    let mut unary = Recursive::declare();

    unary.define(
        just(Not)
            .or(just(Subtract))
            .repeated()
            .foldr(expression(), |op, exp| Node {
                children: vec![exp],
                data: Some(NodeData::UnaryOperation(match op {
                    Not => UnaryOperator::Not,
                    Subtract => UnaryOperator::Negate,
                    _ => unreachable!(),
                })),
                kind: NodeKind::UnaryOperation,
            }),
    );

    unary
}

fn binary_operation<'a>() -> impl Parser<'a, SpannedTokens<'a>, Node, RichError<'a>> + Clone {
    expression().foldl(just(Add), |prev, e| Node {
        children: vec![prev],
        data: todo!(),
        kind: todo!(),
    });

    todo!()
}

fn function<'a>(
    block_parser: impl Parser<'a, SpannedTokens<'a>, Block, RichError<'a>> + Clone,
) -> impl Parser<'a, SpannedTokens<'a>, Node, RichError<'a>> + Clone {
    group((
        just(Function).ignored(),
        identifier().labelled("function name").or_not(),
        identifier_list()
            .delimited_by(just(OpenParen), just(CloseParen))
            .labelled("function arguments"),
        block_parser,
        just(End).ignored(),
    ))
    .map(|(_, name, args, body, _)| Node {
        children: body,
        data: Some(NodeData::Function(Box::new(FunctionData {
            name: name.map(|n| n.to_string()),
            args: args.iter().map(|a| a.to_string()).collect(),
        }))),
        kind: NodeKind::Function,
    })
}

fn identifier_list<'a>() -> impl Parser<'a, SpannedTokens<'a>, Vec<&'a str>, RichError<'a>> + Clone
{
    identifier()
        .separated_by(just(Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
}

fn number_literal<'a>() -> impl Parser<'a, SpannedTokens<'a>, Node, RichError<'a>> + Clone {
    number().map(|n| Node {
        children: vec![],
        data: Some(NodeData::Number(n)),
        kind: NodeKind::Number,
    })
}

fn number<'a>() -> impl Parser<'a, SpannedTokens<'a>, f64, RichError<'a>> + Clone {
    select! {
    Number(num) => num.into() }
}

fn variable_name<'a>() -> impl Parser<'a, SpannedTokens<'a>, Node, RichError<'a>> + Clone {
    identifier().labelled("variable name").map(|n| Node {
        children: vec![],
        kind: NodeKind::Identifier,
        data: Some(NodeData::Identifier(Box::new(n.to_string()))),
    })
}

fn string_literal<'a>() -> impl Parser<'a, SpannedTokens<'a>, Node, RichError<'a>> + Clone {
    select! {
    StringLiteral(string) => Node {
            children: vec![],
            kind: NodeKind::String,
            data: Some(NodeData::String(Box::new(string.to_string())))
    }}
    .labelled("string literal")
}

fn identifier<'a>() -> impl Parser<'a, SpannedTokens<'a>, &'a str, RichError<'a>> + Clone {
    select! {
    Identifier(ident) => ident.clone() }
}
