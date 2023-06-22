use std::cmp::max;

use crate::frontend::lexer::Token::{self, *};
use crate::frontend::parser::parsing::NodeData::KeyValuePair;
use chumsky::recursive::Indirect;
use chumsky::{
    extra::Err, input::SpannedInput, prelude::IterParser, prelude::*, recursive::Recursive,
};

use super::ast::{self, Block, FunctionData, Locality, Node, NodeData, NodeKind, UnaryOperator};

type TokenSpanPair<'a> = (Token<'a>, SimpleSpan);
pub(super) type SpannedTokens<'a> = SpannedInput<Token<'a>, SimpleSpan, &'a [TokenSpanPair<'a>]>;
type RichError<'a> = Err<Rich<'a, Token<'a>>>;

pub(crate) fn lua50<'a>() -> impl Parser<'a, SpannedTokens<'a>, Node, RichError<'a>> + Clone {
    let mut block = Recursive::declare();

    block.define(
        statement(block.clone())
            .or(expression())
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
    let function = group((
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
    });

    choice((variable_declaration(), function))
}

fn variable_declaration<'a>() -> impl Parser<'a, SpannedTokens<'a>, Node, RichError<'a>> + Clone {
    group((
        just(Local)
            .or_not()
            .labelled("local")
            .map(|local| local.is_some()),
        identifier()
            .labelled("name")
            .separated_by(just(Comma))
            .at_least(1)
            .collect::<Vec<_>>()
            .labelled("names"),
        just(Assign)
            .ignore_then(
                expression()
                    .labelled("initialiser")
                    .separated_by(just(Comma))
                    .at_least(1)
                    .collect::<Vec<_>>()
                    .labelled("initialisers"),
            )
            .or_not(),
    ))
    .map(|(is_local, names, initialised_to)| {
        if let Some(init) = &initialised_to {
            if init.len() > names.len() {
                panic!()
            }
        }

        let initialised_to = initialised_to.unwrap();

        let mut nodes = Vec::with_capacity(names.len());

        for (index, name) in names.iter().enumerate() {
            let mut node = Node {
                children: vec![Node {
                    children: vec![],
                    data: Some(NodeData::String(Box::new(name.to_string()))),
                    kind: NodeKind::String,
                }],
                data: Some(NodeData::Locality(if is_local {
                    Locality::Local
                } else {
                    Locality::Global
                })),
                kind: NodeKind::VariableDeclaration,
            };

            node.children[0]
                .children
                .push(initialised_to[index].clone());

            nodes.push(node);
        }

        Node {
            children: nodes,
            data: None,
            kind: NodeKind::VariableDeclarations,
        }
    })
}

fn expression<'a>() -> Recursive<Indirect<'a, 'a, SpannedTokens<'a>, Node, RichError<'a>>> {
    let mut expr = Recursive::declare();

    let variable_name = identifier().labelled("variable name").map(|n| Node {
        children: vec![],
        kind: NodeKind::Identifier,
        data: Some(NodeData::Identifier(Box::new(n.to_string()))),
    });

    let string_literal = select! {
    StringLiteral(string) => Node {
            children: vec![],
            kind: NodeKind::String,
            data: Some(NodeData::String(Box::new(string.to_string())))
    }}
    .labelled("string literal");

    let number_literal = number()
        .map(|n| Node {
            children: vec![],
            data: Some(NodeData::Number(n)),
            kind: NodeKind::Number,
        })
        .labelled("number");

    let boolean_literal = just(True)
        .or(just(False))
        .map(|tok| Node {
            children: vec![],
            data: Some(NodeData::Boolean(matches!(tok, True))),
            kind: NodeKind::Boolean,
        })
        .labelled("boolean");

    let literal = choice((string_literal, number_literal, boolean_literal));

    let parenthetic_expression = expr.clone().delimited_by(just(OpenParen), just(CloseParen));
    let atom = literal.or(parenthetic_expression);

    let unary_operation = just(Not)
        .or(just(Subtract))
        .repeated()
        .foldr(atom.clone(), |op, exp| Node {
            children: vec![exp],
            data: Some(NodeData::UnaryOperation(match op {
                Not => UnaryOperator::Not,
                Subtract => UnaryOperator::Negate,
                _ => unreachable!(),
            })),
            kind: NodeKind::UnaryOperation,
        });

    let product_operation = unary_operation.clone().foldl(
        product_operations()
            .then(unary_operation.clone())
            .repeated(),
        |lhs, (op, rhs)| Node {
            children: vec![lhs, rhs],
            data: Some(NodeData::BinaryOperation(match op {
                Multiply => ast::BinaryOperator::Multiply,
                Divide => ast::BinaryOperator::Divide,
                _ => unreachable!(),
            })),
            kind: NodeKind::BinaryOperation,
        },
    );

    let binary_operation = product_operation.clone().foldl(
        sum_operations().then(product_operation.clone()).repeated(),
        |lhs, (op, rhs)| Node {
            children: vec![lhs, rhs],
            data: Some(NodeData::BinaryOperation(match op {
                Add => ast::BinaryOperator::Add,
                Subtract => ast::BinaryOperator::Subtract,
                _ => unreachable!(),
            })),
            kind: NodeKind::BinaryOperation,
        },
    );

    expr.define(
        choice((atom, variable_name, unary_operation, binary_operation)).labelled("expression"),
    );

    expr
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

fn sum_operations<'a>() -> impl Parser<'a, SpannedTokens<'a>, Token<'a>, RichError<'a>> + Clone {
    just(Add).or(just(Subtract))
}

fn product_operations<'a>() -> impl Parser<'a, SpannedTokens<'a>, Token<'a>, RichError<'a>> + Clone
{
    just(Multiply).or(just(Divide))
}

// fn binary_operation<'a>() -> impl Parser<'a, SpannedTokens<'a>, Node, RichError<'a>> + Clone {
//     expression().foldl(
//         product_operations()
//             .or(sum_operations())
//             .ignore_then(expression())
//             .repeated(),
//         |prev, e| Node {
//             children: vec![prev, e],
//             data: Some(NodeData::BinaryOperation(super::ast::BinaryOperator::Add)),
//             kind: NodeKind::BinaryOperation,
//         },
//     )
// }

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
