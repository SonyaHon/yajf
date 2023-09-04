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
            offset += length;
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
            ch => {
                if ch.is_alphabetic() {
                    self.parse_keyword_or_identifier(location)
                } else {
                    Token::new(TokenType::Illegal, ch, location)
                }
            }
        }
    }

    fn parse_alphabetic_literal(&mut self) -> String {
        let position = self.position;

        while self.character.is_alphanumeric() {
            self.read_char()
        }

        self.input.get(position..self.position).unwrap().to_string()
    }

    fn parse_keyword_or_identifier(&mut self, location: Location) -> Token {
        let literal = self.parse_alphabetic_literal();
        Token::new(
            match literal.as_str() {
                "if" => TokenType::IfKeyword,
                "else" => TokenType::ElseKeyword,
                "func" => TokenType::Function,
                _ => TokenType::Identifier,
            },
            literal,
            location,
        )
    }

    fn get_current_location(&self) -> Location {
        let (row, (line_start, _)) = self
            .line_offsets
            .iter()
            .enumerate()
            .find(|(_, (start_offset, _))| start_offset == &self.position)
            .unwrap();

        Location::new(row, self.position - line_start, self.filepath.clone())
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
