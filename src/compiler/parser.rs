use std::mem::swap;

use super::{
    ast::{
        DeclarationData, DeclarationVariant, Expression, IdentifierData,
        Module, NumberLiteralData, Statement, Type,
    },
    ir::{Token, TokenType},
    lexer::Lexer,
};

pub struct Parser {
    lexer: Lexer,

    errors: Vec<String>,

    current_token: Token,
    next_token: Token,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let current_token = lexer.next_token();
        let next_token = lexer.next_token();

        Self {
            lexer,
            errors: vec![],
            current_token,
            next_token,
        }
    }

    pub fn parse(mut self) -> Module {
        let mut statements = vec![];

        while !self.current_token.is_of_type(TokenType::EndOfFile) {
            let statement = self.parse_statement();
            if statement.is_some() {
                statements.push(statement.unwrap());
            }
        }

        Module {
            errors: self.errors,
            statements,
        }
    }

    fn advance(&mut self) {
        swap(&mut self.current_token, &mut self.next_token);
        self.next_token = self.lexer.next_token();
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        self.parse_declaration_statement()
    }

    fn parse_declaration_statement(&mut self) -> Option<Statement> {
        let token = self.current_token.clone();
        let identifier = self.parse_identifier();

        if identifier.is_none() {
            self.gen_error("Expected identifier");
            return None;
        }

        if !self.current_token.is_of_type(TokenType::ColonColon)
            && !self.current_token.is_of_type(TokenType::ColonEquals)
        {
            self.gen_error("Expected :: or :=");
            return None;
        }

        let variant = if self.current_token.is_of_type(TokenType::ColonColon) {
            DeclarationVariant::Constant
        } else {
            DeclarationVariant::Mutable
        };
        self.advance();

        let expression = self.parse_expression();
        if expression.is_none() {
            self.gen_error("Expected expression");
            return None;
        }

        Some(Statement::Declaration(DeclarationData {
            token,
            identifier: identifier.unwrap().into(),
            variant,
            expression: expression.unwrap(),
        }))
    }

    fn parse_expression(&mut self) -> Option<Expression> {
        self.parse_number_literal()
    }

    fn parse_number_literal(&mut self) -> Option<Expression> {
        let token = self.current_token.clone();
        if !self.current_token.is_of_type(TokenType::Number) {
            self.gen_error("Expected number");
            return None;
        }
        self.advance();
        let value = token.literal.parse::<f64>();
        if value.is_err() {
            self.gen_error(format!("{}", value.unwrap_err()));
            return None;
        }

        Some(Expression::NumberLiteral(NumberLiteralData {
            token,
            value: value.unwrap(),
        }))
    }

    fn parse_identifier(&mut self) -> Option<Expression> {
        let token = self.current_token.clone();
        if !self.current_token.is_of_type(TokenType::Identifier) {
            self.gen_error("Expected identifier");
            return None;
        }

        self.advance();
        let typ = self.parse_type();

        let name = token.literal.clone();
        Some(Expression::Identifier(IdentifierData { token, name, typ }))
    }

    fn parse_type(&mut self) -> Type {
        Type::Untyped
    }

    fn gen_error<T: Into<String>>(&mut self, err: T) {
        self.errors.push(err.into());
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn do_test(input: &str, expected: &str) {
        let module = Parser::new(Lexer::new(input)).parse();
        assert_eq!(module.test_string(), expected, "Module data: {:?}", module);
    }

    #[test]
    fn declaration_statement() {
        do_test("x :: 10", "x any :: 10");
        do_test("x := 10", "x any := 10");
    }
}
