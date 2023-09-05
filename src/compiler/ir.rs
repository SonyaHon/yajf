use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct Location {
    pub row: usize,
    pub column: usize,
    pub path: Option<String>,
}

impl Location {
    pub fn new(row: usize, column: usize, path: Option<String>) -> Self {
        Self { row, column, path }
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(path) = &self.path {
            write!(f, "{}:{}:{}", path, self.row, self.column)
        } else {
            write!(f, "{}:{}", self.row, self.column)
        }
    }
}

//TODO: Strings, Proper numbers (floats, binary, hex),
//      Binary Operations,
#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    // Utility
    Illegal,
    EndOfFile,

    // Literals
    Identifier,
    Number,

    // Sepparators
    Comma,
    SemiColon,
    ParenOpen,
    ParenClose,
    BraceOpen,
    BraceClose,
    BracketOpen,
    BracketClose,
    Colon,
    ArrowRight,

    // Operators
    Plus,
    PlusEquals,

    Minus,
    MinusEquals,

    Asterisk,
    AsteriskEquals,

    Slash,
    SlashEquals,

    Bang,
    NotEquals,

    GreaterThen,
    GreaterEqualThen,

    LessThen,
    LessEqualThen,

    Assign,
    Equals,

    ColonColon,
    ColonEquals,

    // Keywords
    If,
    Else,
    Function,
    True,
    False,
    Return,
    Use,
    Import,
    For,
    From,
    While,
    Loop,
    Null,
    Try,
    Catch,
    Enum,
    Class,
    Match,
    Interface,
    Async,
    Await,
    Break,
    Continue,
    Where,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub location: Location,
    pub literal: String,
    pub typ: TokenType,
}

impl Token {
    pub fn new<T: Into<String>>(
        typ: TokenType,
        literal: T,
        location: Location,
    ) -> Self {
        Self {
            location,
            literal: literal.into(),
            typ,
        }
    }

    pub fn is_of_type(&self, typ: TokenType) -> bool {
        self.typ == typ
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{:?} {:?}]", self.typ, self.literal)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn should_format_location_correctly() {
        assert_eq!(format!("{}", Location::new(69, 69, None)), "69:69");
        assert_eq!(
            format!("{}", Location::new(69, 69, Some("/usr/lib".to_string()))),
            "/usr/lib:69:69"
        );
    }
}
