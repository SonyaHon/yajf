use super::ir::{Location, Token, TokenType};

pub struct Lexer {
    input: String,
    filepath: Option<String>,
    position: usize,
    read_position: usize,
    line_offsets: Vec<(usize, usize)>,
    character: char,
}

impl Lexer {
    pub fn new<T: Into<String>>(input: T) -> Self {
        let input = input.into();

        let mut line_offsets = vec![];
        let mut offset = 0;

        for line in input.lines() {
            let length = line.len();
            line_offsets.push((offset, length));
            offset += length + 1;
        }

        let mut lexer = Self {
            input,
            filepath: None,
            position: 0,
            read_position: 0,
            character: '\0',
            line_offsets,
        };
        lexer.read_char();
        lexer
    }

    pub fn file<T: Into<String>>(mut self, path: String) -> Self {
        self.filepath = Some(path.into());
        self
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespaces();

        let location = self.get_current_location();

        match self.character {
            '\0' => {
                self.read_char();
                Token::new(TokenType::EndOfFile, "\0", location)
            }
            ',' => {
                self.read_char();
                Token::new(TokenType::Comma, ",", location)
            }
            ';' => {
                self.read_char();
                Token::new(TokenType::SemiColon, ";", location)
            }
            '(' => {
                self.read_char();
                Token::new(TokenType::ParenOpen, "(", location)
            }
            ')' => {
                self.read_char();
                Token::new(TokenType::ParenClose, ")", location)
            }
            '{' => {
                self.read_char();
                Token::new(TokenType::BraceOpen, "{", location)
            }
            '}' => {
                self.read_char();
                Token::new(TokenType::BraceClose, "}", location)
            }
            '[' => {
                self.read_char();
                Token::new(TokenType::BracketOpen, "[", location)
            }
            ']' => {
                self.read_char();
                Token::new(TokenType::BracketClose, "]", location)
            }
            '+' => self.parse_plus_starting_token(location),
            '-' => self.parse_minus_starting_token(location),
            '*' => self.parse_asterisk_starting_token(location),
            '/' => self.parse_slash_starting_token(location),
            '!' => self.parse_bang_starting_token(location),
            '>' => self.parse_greater_starting_token(location),
            '<' => self.parse_less_starting_token(location),
            '=' => self.parse_equal_starting_token(location),
            ':' => self.parse_colon_starting_token(location),
            ch => {
                if ch.is_alphabetic() {
                    self.parse_keyword_or_identifier(location)
                } else if ch.is_numeric() {
                    self.parse_number(location)
                } else {
                    Token::new(TokenType::Illegal, ch, location)
                }
            }
        }
    }

    fn parse_colon_starting_token(&mut self, location: Location) -> Token {
        self.read_char();
        if self.character == '=' {
            self.read_char();
            Token::new(TokenType::ColonEquals, ":=", location)
        } else if self.character == ':' {
            self.read_char();
            Token::new(TokenType::ColonColon, "::", location)
        } else {
            Token::new(TokenType::Colon, ":", location)
        }
    }

    fn parse_equal_starting_token(&mut self, location: Location) -> Token {
        self.read_char();
        if self.character == '=' {
            self.read_char();
            Token::new(TokenType::Equals, "==", location)
        } else if self.character == '>' {
            self.read_char();
            Token::new(TokenType::ArrowRight, "=>", location)
        } else {
            Token::new(TokenType::Assign, "=", location)
        }
    }

    fn parse_less_starting_token(&mut self, location: Location) -> Token {
        self.read_char();
        if self.character == '=' {
            self.read_char();
            Token::new(TokenType::LessEqualThen, "<=", location)
        } else {
            Token::new(TokenType::LessThen, "<", location)
        }
    }

    fn parse_greater_starting_token(&mut self, location: Location) -> Token {
        self.read_char();
        if self.character == '=' {
            self.read_char();
            Token::new(TokenType::GreaterEqualThen, ">=", location)
        } else {
            Token::new(TokenType::GreaterThen, ">", location)
        }
    }

    fn parse_bang_starting_token(&mut self, location: Location) -> Token {
        self.read_char();
        if self.character == '=' {
            self.read_char();
            Token::new(TokenType::NotEquals, "!=", location)
        } else {
            Token::new(TokenType::Bang, "!", location)
        }
    }

    fn parse_slash_starting_token(&mut self, location: Location) -> Token {
        self.read_char();
        if self.character == '=' {
            self.read_char();
            Token::new(TokenType::SlashEquals, "/=", location)
        } else {
            Token::new(TokenType::Slash, "/", location)
        }
    }

    fn parse_asterisk_starting_token(&mut self, location: Location) -> Token {
        self.read_char();
        if self.character == '=' {
            self.read_char();
            Token::new(TokenType::AsteriskEquals, "*=", location)
        } else {
            Token::new(TokenType::Asterisk, "*", location)
        }
    }

    fn parse_minus_starting_token(&mut self, location: Location) -> Token {
        self.read_char();
        if self.character == '=' {
            self.read_char();
            Token::new(TokenType::MinusEquals, "-=", location)
        } else {
            Token::new(TokenType::Minus, "-", location)
        }
    }

    fn parse_plus_starting_token(&mut self, location: Location) -> Token {
        self.read_char();
        if self.character == '=' {
            self.read_char();
            Token::new(TokenType::PlusEquals, "+=", location)
        } else {
            Token::new(TokenType::Plus, "+", location)
        }
    }

    fn parse_alphabetic_literal(&mut self) -> String {
        let position = self.position;

        while self.character.is_alphanumeric() {
            self.read_char()
        }

        self.input.get(position..self.position).unwrap().to_string()
    }

    //TODO: Only dumb integer parser for now
    fn parse_number(&mut self, location: Location) -> Token {
        let position = self.position;
        while self.character.is_numeric() {
            self.read_char()
        }
        let literal =
            self.input.get(position..self.position).unwrap().to_string();
        Token::new(TokenType::Number, literal, location)
    }

    fn parse_keyword_or_identifier(&mut self, location: Location) -> Token {
        let literal = self.parse_alphabetic_literal();
        Token::new(
            match literal.as_str() {
                "if" => TokenType::If,
                "else" => TokenType::Else,
                "func" => TokenType::Function,
                "true" => TokenType::True,
                "false" => TokenType::False,
                "return" => TokenType::Return,
                "use" => TokenType::Use,
                "import" => TokenType::Import,
                "for" => TokenType::For,
                "from" => TokenType::From,
                "while" => TokenType::While,
                "loop" => TokenType::Loop,
                "null" => TokenType::Null,
                "try" => TokenType::Try,
                "catch" => TokenType::Catch,
                "enum" => TokenType::Enum,
                "class" => TokenType::Class,
                "match" => TokenType::Match,
                "interface" => TokenType::Interface,
                "async" => TokenType::Async,
                "await" => TokenType::Await,
                "break" => TokenType::Break,
                "continue" => TokenType::Continue,
                "where" => TokenType::Where,
                _ => TokenType::Identifier,
            },
            literal,
            location,
        )
    }

    fn get_current_location(&self) -> Location {
        let last_row_data = [*self.line_offsets.last().unwrap()];
        let (row, lines) = self
            .line_offsets
            .windows(2)
            .enumerate()
            .find(|(_, data)| {
                self.position >= data[0].0 && self.position < data[1].0
            })
            .unwrap_or((self.line_offsets.len() - 1, &last_row_data));

        Location::new(row, self.position - lines[0].0, self.filepath.clone())
    }

    fn skip_whitespaces(&mut self) {
        while self.character.is_whitespace() {
            self.read_char()
        }
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.character = '\0';
        } else {
            self.character =
                self.input.chars().nth(self.read_position).unwrap();
        }
        self.position = self.read_position;
        self.read_position += 1;
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn should_give_correct_location() {
        let input = "+++\nfalse++\n+true+";
        let test_data = vec![
            "0:0", "0:1", "0:2", "1:0", "1:5", "1:6", "2:0", "2:1", "2:5",
        ];

        let mut lexer = Lexer::new(input);
        for test_location in test_data {
            let pos = lexer.position;
            let token = lexer.next_token();
            assert_eq!(
                test_location,
                format!("{}", token.location),
                "Position: {}",
                pos,
            );
        }
    }

    #[test]
    fn should_parse_all_tokens() {
        let input = r#"x 10 , ; ( ) { } [ ] : => + += - -= * *= / /= ! != > >=
                       < <= = == :: := if else func true false return use
                       import for from while loop null try catch enum class
                       match interface async await break continue where"#;

        let test_data = vec![
            (TokenType::Identifier, "x"),
            (TokenType::Number, "10"),
            (TokenType::Comma, ","),
            (TokenType::SemiColon, ";"),
            (TokenType::ParenOpen, "("),
            (TokenType::ParenClose, ")"),
            (TokenType::BraceOpen, "{"),
            (TokenType::BraceClose, "}"),
            (TokenType::BracketOpen, "["),
            (TokenType::BracketClose, "]"),
            (TokenType::Colon, ":"),
            (TokenType::ArrowRight, "=>"),
            (TokenType::Plus, "+"),
            (TokenType::PlusEquals, "+="),
            (TokenType::Minus, "-"),
            (TokenType::MinusEquals, "-="),
            (TokenType::Asterisk, "*"),
            (TokenType::AsteriskEquals, "*="),
            (TokenType::Slash, "/"),
            (TokenType::SlashEquals, "/="),
            (TokenType::Bang, "!"),
            (TokenType::NotEquals, "!="),
            (TokenType::GreaterThen, ">"),
            (TokenType::GreaterEqualThen, ">="),
            (TokenType::LessThen, "<"),
            (TokenType::LessEqualThen, "<="),
            (TokenType::Assign, "="),
            (TokenType::Equals, "=="),
            (TokenType::ColonColon, "::"),
            (TokenType::ColonEquals, ":="),
            (TokenType::If, "if"),
            (TokenType::Else, "else"),
            (TokenType::Function, "func"),
            (TokenType::True, "true"),
            (TokenType::False, "false"),
            (TokenType::Return, "return"),
            (TokenType::Use, "use"),
            (TokenType::Import, "import"),
            (TokenType::For, "for"),
            (TokenType::From, "from"),
            (TokenType::While, "while"),
            (TokenType::Loop, "loop"),
            (TokenType::Null, "null"),
            (TokenType::Try, "try"),
            (TokenType::Catch, "catch"),
            (TokenType::Enum, "enum"),
            (TokenType::Class, "class"),
            (TokenType::Match, "match"),
            (TokenType::Interface, "interface"),
            (TokenType::Async, "async"),
            (TokenType::Await, "await"),
            (TokenType::Break, "break"),
            (TokenType::Continue, "continue"),
            (TokenType::Where, "where"),
        ];

        let mut lexer = Lexer::new(input);
        for (test_typ, test_literal) in test_data {
            let token = lexer.next_token();
            assert_eq!(
                test_typ, token.typ,
                "Expected: {:?} - {:?}\nReceived: {:?} - {:?}",
                test_typ, test_literal, token.typ, token.literal
            );
            assert_eq!(
                test_literal, token.literal,
                "Expected: {:?} - {:?}\nReceived: {:?} - {:?}",
                test_typ, test_literal, token.typ, token.literal
            );
        }
    }
}
