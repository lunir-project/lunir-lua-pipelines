mod tests;
use std::{
    fmt::{self, Display, Formatter},
    ops::Range,
};

use lifering::FloatingPointComponents;
use logos::{Filter, Lexer, Logos};

#[derive(Clone, Debug, Logos, PartialEq)]
#[logos(skip r"\s")]
pub(super) enum Token<'a> {
    /// The "`+`" operator.
    #[token("+")]
    Add,

    /// The "`and`" operator.
    #[token("and")]
    And,

    /// The "`=`" operator.
    #[token("=")]
    Assign,

    /// The "`break`" keyword.
    #[token("break")]
    Break,

    /// The "`]`" token.
    #[token("]")]
    CloseBracket,

    /// The "`}`" token.
    #[token("}")]
    CloseCurly,

    /// The "`)`" token.
    #[token(")")]
    CloseParen,

    /// The "`:`" operator.
    #[token(":")]
    Colon,

    /// The "`,`" token.
    #[token(",")]
    Comma,

    /// Line and block comments, line comments are eaten by
    /// the lexer, but block comments are left for the parser
    /// to handle.
    #[token("--", comment)]
    CommentStart,

    /// The "`..`" operator.
    #[token("..")]
    Concat,

    /// The "`/`" operator.
    #[token("/")]
    Divide,

    /// The "`do`" keyword.
    #[token("do")]
    Do,

    /// The "`else`" keyword.
    #[token("else")]
    Else,

    /// The "`elseif`" keyword.
    #[token("elseif")]
    ElseIf,

    /// The "`end`" keyword.
    #[token("end")]
    End,

    /// The "`==`" operator.
    #[token("==")]
    Eq,

    /// The "`^`" operator.
    #[token("^")]
    Exponentiate,

    /// The "`false`" keyword.
    #[token("false")]
    False,

    /// The "`for`" keyword.
    #[token("for")]
    For,

    /// The "`function`" keyword.
    #[token("function")]
    Function,

    /// The "`>=`" operator.
    #[token(">=")]
    Ge,

    /// The "`>`" operator.
    #[token(">")]
    Gt,

    /// All identifiers.
    #[regex("[_a-zA-Z][_a-zA-Z0-9]*")]
    Identifier(&'a str),

    /// The "`if`" keyword.
    #[token("if")]
    If,

    /// The "`in`" keyword.
    #[token("in")]
    In,

    /// The "`<=`" operator.
    #[token("<=")]
    Le,

    /// The "`local`" keyword.
    #[token("local")]
    Local,

    /// The "`<`" operator.
    #[token("<")]
    Lt,

    /// The "`*`" operator.
    #[token("*")]
    Multiply,

    /// The "`~=`" operator.
    #[token("~=")]
    Ne,

    /// Newlines, this variant serves an *internal purpose only*
    /// and should never be used as it is filtered out.
    #[token("\n")]
    Newline,

    /// The "`nil`" keyword.
    #[token("nil")]
    Nil,

    /// The "`not`" operator.
    #[token("not")]
    Not,

    /// Lua numeric literals, stored as components to allow the type
    /// to be [`Hash`].
    #[regex(r#"(-?([0-9]*[.])?[0-9]+)((e|E)(-?)[0-9]+)?"#, |lex| FloatingPointComponents::new(lex.slice().parse::<f64>().expect("Invalid float literal.")).unwrap())]
    Number(FloatingPointComponents),

    /// The "`[`" token.
    #[token("[")]
    OpenBracket,

    /// The "`{`" token.
    #[token("{")]
    OpenCurly,

    /// The "`(`" token.
    #[token("(")]
    OpenParen,

    /// The "`or`" operator.
    #[token("or")]
    Or,

    /// The "`.`" operator.
    #[token(".")]
    Period,

    /// The "`repeat`" keyword.
    #[token("repeat")]
    Repeat,

    /// The "`return`" keyword.
    #[token("return")]
    Return,

    /// The "`;`" token.
    #[token(";")]
    Semicolon,

    /// Lua string literals.
    #[regex(r#""([^\\"]|\\.)*""#, string_literal)]
    StringLiteral(&'a str),

    /// The "`-`" operator.
    #[token("-")]
    Subtract,

    /// The "`then`" keyword.
    #[token("then")]
    Then,

    /// The "`true`" keyword.
    #[token("true")]
    True,

    /// The "`until`" keyword.
    #[token("until")]
    Until,

    /// The "`...`" token.
    #[token("...")]
    Vararg,

    /// The "`while`" keyword.
    #[token("while")]
    While,
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Token::Add => " + ",
                Token::And => " and ",
                Token::Assign => " = ",
                Token::Break => "break",
                Token::CloseBracket => "]",
                Token::CloseCurly => "} ",
                Token::CloseParen => ") ",
                Token::Colon => ": ",
                Token::Comma => ", ",
                Token::CommentStart => "--",
                Token::Concat => " .. ",
                Token::Divide => " / ",
                Token::Do => "do",
                Token::Else => "else",
                Token::ElseIf => "elseif ",
                Token::End => "end",
                Token::Eq => " == ",
                Token::Exponentiate => " ^ ",
                Token::False => "false",
                Token::For => "for ",
                Token::Function => "function",
                Token::Ge => " >= ",
                Token::Gt => " <= ",
                Token::Identifier(ident) => *ident,
                Token::If => "if ",
                Token::In => "in ",
                Token::Le => " <= ",
                Token::Local => "local ",
                Token::Lt => " < ",
                Token::Multiply => " * ",
                Token::Ne => " ~= ",
                Token::Newline => "\n",
                Token::Nil => "nil",
                Token::Not => "not ",
                Token::Number(num) => return write!(f, "{}", num.as_f64()),
                Token::OpenBracket => "[",
                Token::OpenCurly => "{",
                Token::OpenParen => "(",
                Token::Or => " or ",
                Token::Period => ".",
                Token::Repeat => "repeat ",
                Token::Return => "return",
                Token::Semicolon => "; ",
                Token::StringLiteral(lit) => return write!(f, "{lit:?}"),
                Token::Subtract => "- ",
                Token::Then => "then ",
                Token::True => "true",
                Token::Until => "until ",
                Token::Vararg => "...",
                Token::While => "while ",
            }
        )
    }
}

impl<'a> Token<'a> {
    /// Returns a vector of [`Token`]s.
    #[inline]
    pub fn lex_unspanned(source: &'a str) -> Vec<Self> {
        Self::lexer(source)
            .filter_map(|tok| {
                let t = tok.unwrap();
                (!matches!(t, Token::Newline)).then_some(t)
            })
            .collect::<Vec<_>>()
    }

    /// Returns a vector of `([Token], [Range<usize>])`
    #[inline]
    pub fn lex_spanned(source: &'a str) -> Vec<(Self, Range<usize>)> {
        Self::lexer(source)
            .spanned()
            .filter_map(|(tok, span)| {
                let t = tok.unwrap();
                (!matches!(t, Token::Newline)).then_some((t, span))
            })
            .collect::<Vec<_>>()
    }
}

fn comment<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Filter<()> {
    // Let parser handle block comments
    if matches!(lex.peekable().peek().unwrap().unwrap(), Token::OpenBracket) {
        return Filter::Emit(());
    }

    // Lexer can eat line comments
    for tok in lex {
        if matches!(tok.unwrap(), Token::Newline) {
            return Filter::Skip;
        }
    }

    Filter::Skip
}

fn string_literal<'a>(lex: &mut Lexer<'a, Token<'a>>) -> &'a str {
    let slice = lex.slice();
    let len = slice.len();

    &slice[1..len - 1]
}

/// Returns a vector of [`Token`]s.
#[inline]
pub fn lex(source: &str) -> Vec<Token> {
    Token::lex_unspanned(source)
}

/// Returns a vector of `(Token, Range<usize>)`.
#[inline]
pub fn lex_spanned<'a>(source: &'a str) -> Vec<(Token<'a>, Range<usize>)> {
    Token::lex_spanned(source)
}
