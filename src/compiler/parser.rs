use std::mem::swap;

use super::{
    ast::{
        BlockExpressionData, DeclarationData, DeclarationVariant, Expression,
        ExpressionStatementData, FunctionExpressionData, IdentifierData,
        IfExpressionBranch, IfExpressionData, InfixOperationData,
        InvokationExpressionData, Module, NumberLiteralData,
        PrefixOperationData, ReturnData, Statement, Type,
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

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Priority {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
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
            } else {
                self.advance();
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
        match self.current_token.typ {
            TokenType::Return => self.parse_return_statement(),
            TokenType::Identifier => self.parse_declaration_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let token = self.current_token.clone();
        self.advance();
        let expression = self.parse_expression(Priority::Lowest);
        if expression.is_none() {
            self.gen_error("Expected expression");
            return None;
        }
        Some(Statement::Return(ReturnData {
            token,
            return_value: expression.unwrap(),
        }))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let token = self.current_token.clone();
        let expression = self.parse_expression(Priority::Lowest);
        if expression.is_none() {
            self.gen_error("Expected expression");
            return None;
        }

        Some(Statement::Expression(ExpressionStatementData {
            token,
            expression: expression.unwrap(),
        }))
    }

    fn parse_declaration_statement(&mut self) -> Option<Statement> {
        if !self.next_token.is_of_type(TokenType::ColonColon)
            && !self.next_token.is_of_type(TokenType::ColonEquals)
            && !self.next_token.is_of_type(TokenType::Identifier)
        {
            return self.parse_expression_statement();
        }

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

        let expression = self.parse_expression(Priority::Lowest);
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

    fn get_next_token_priority(&self) -> Priority {
        match self.next_token.typ {
            TokenType::Equals => Priority::Equals,
            TokenType::NotEquals => Priority::Equals,
            TokenType::LessThen => Priority::LessGreater,
            TokenType::GreaterThen => Priority::LessGreater,
            TokenType::LessEqualThen => Priority::LessGreater,
            TokenType::GreaterEqualThen => Priority::LessGreater,
            TokenType::Plus => Priority::Sum,
            TokenType::Minus => Priority::Sum,
            TokenType::Slash => Priority::Product,
            TokenType::Asterisk => Priority::Product,
            _ => Priority::Lowest,
        }
    }

    fn get_current_token_priority(&self) -> Priority {
        match self.current_token.typ {
            TokenType::Equals => Priority::Equals,
            TokenType::NotEquals => Priority::Equals,
            TokenType::LessThen => Priority::LessGreater,
            TokenType::GreaterThen => Priority::LessGreater,
            TokenType::LessEqualThen => Priority::LessGreater,
            TokenType::GreaterEqualThen => Priority::LessGreater,
            TokenType::Plus => Priority::Sum,
            TokenType::Minus => Priority::Sum,
            TokenType::Slash => Priority::Product,
            TokenType::Asterisk => Priority::Product,
            _ => Priority::Lowest,
        }
    }

    fn parse_expression(&mut self, priority: Priority) -> Option<Expression> {
        let mut expr = match &self.current_token.typ {
            TokenType::Number => self.parse_number_literal(),
            TokenType::Identifier => {
                self.parse_identifier_starting_expression()
            }
            TokenType::Bang | TokenType::Minus => {
                self.parse_prefix_expression()
            }
            TokenType::ParenOpen => self.parse_grouped_expression(),
            TokenType::If => self.parse_if_expression(),
            TokenType::BraceOpen => self.parse_block_expression(),
            TokenType::Function => self.parse_function_expression(),
            _ => None,
        };

        if expr.is_none() {
            self.gen_error("Expected expression");
            return None;
        }

        expr = match self.current_token.typ {
            TokenType::ParenOpen => {
                self.parse_invokation_expression(expr.unwrap())
            }
            _ => expr,
        };

        while !self.current_token.is_of_type(TokenType::EndOfFile)
            && priority < self.get_current_token_priority()
        {
            expr = match &self.current_token.typ {
                TokenType::Equals
                | TokenType::NotEquals
                | TokenType::LessThen
                | TokenType::GreaterThen
                | TokenType::LessEqualThen
                | TokenType::GreaterEqualThen
                | TokenType::Plus
                | TokenType::Minus
                | TokenType::Slash
                | TokenType::Asterisk => {
                    self.parse_infix_expression(expr.unwrap())
                }
                _ => {
                    break;
                }
            }
        }

        expr
    }

    fn parse_invokation_expression(
        &mut self,
        expr: Expression,
    ) -> Option<Expression> {
        let token = self.current_token.clone();
        self.advance();

        let mut arguments = vec![];

        while !self.current_token.is_of_type(TokenType::ParenClose) {
            let arg = self.parse_expression(Priority::Lowest);
            if arg.is_none() {
                self.gen_error("Expected argument");
                return None;
            }
            arguments.push(arg.unwrap().into());

            if !self.current_token.is_of_type(TokenType::Comma)
                && !self.current_token.is_of_type(TokenType::ParenClose)
            {
                self.gen_error("Expected )");
                return None;
            } else if self.current_token.is_of_type(TokenType::Comma) {
                self.advance();
            }
        }
        self.advance();

        Some(Expression::InvokationExpression(InvokationExpressionData {
            token,
            arguments,
            target: Box::from(expr),
        }))
    }

    fn parse_function_expression(&mut self) -> Option<Expression> {
        let token = self.current_token.clone();
        self.advance();

        if !self.current_token.is_of_type(TokenType::ParenOpen) {
            self.gen_error("Expected (");
            return None;
        }
        self.advance();

        let mut arguments = vec![];

        while !self.current_token.is_of_type(TokenType::ParenClose) {
            let arg = self.parse_identifier();
            if arg.is_none() {
                self.gen_error("Expected argument");
                return None;
            }
            arguments.push(arg.unwrap().into());

            if !self.current_token.is_of_type(TokenType::Comma)
                && !self.current_token.is_of_type(TokenType::ParenClose)
            {
                self.gen_error("Expected )");
                return None;
            } else if self.current_token.is_of_type(TokenType::Comma) {
                self.advance();
            }
        }
        self.advance();

        let body = self.parse_block_expression();
        if body.is_none() {
            self.gen_error("Expected block statement");
            return None;
        }

        Some(Expression::FunctionExpression(FunctionExpressionData {
            token,
            arguments,
            body: body.unwrap().into(),
        }))
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        let token = self.current_token.clone();
        self.advance();

        let condition = self.parse_expression(Priority::Lowest);
        if condition.is_none() {
            self.gen_error("Expected expression");
            return None;
        }

        let consequense = self.parse_block_expression();
        if consequense.is_none() {
            self.gen_error("Block statement");
            return None;
        }

        let mut branches = vec![IfExpressionBranch {
            token: token.clone(),
            condition,
            consequense: consequense.unwrap().into(),
        }];

        while self.current_token.is_of_type(TokenType::Else) {
            let token = self.current_token.clone();
            self.advance();
            let mut condition = None;
            if self.current_token.is_of_type(TokenType::If) {
                self.advance();
                condition = self.parse_expression(Priority::Lowest);
                if condition.is_none() {
                    self.gen_error("Expected expression");
                    return None;
                }
            }
            let consequense = self.parse_block_expression();
            if consequense.is_none() {
                self.gen_error("Block statement");
                return None;
            }

            branches.push(IfExpressionBranch {
                token,
                condition,
                consequense: consequense.unwrap().into(),
            });
        }

        Some(Expression::IfExpression(IfExpressionData {
            token,
            branches,
        }))
    }

    fn parse_block_expression(&mut self) -> Option<Expression> {
        let token = self.current_token.clone();
        self.advance();
        let mut statements = vec![];

        while !self.current_token.is_of_type(TokenType::BraceClose) {
            if self.current_token.is_of_type(TokenType::EndOfFile) {
                self.gen_error("Unfinished }");
                return None;
            }
            let statement = self.parse_statement();
            if statement.is_some() {
                statements.push(statement.unwrap());
            } else {
                self.advance();
            }
        }

        self.advance();

        Some(Expression::BlockExpression(BlockExpressionData {
            token,
            statements,
        }))
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.advance();
        let expr = self.parse_expression(Priority::Lowest);
        if expr.is_none() {
            self.gen_error("Expected expression");
            return None;
        }

        if !self.current_token.is_of_type(TokenType::ParenClose) {
            self.gen_error("Unclosed )");
            return None;
        }

        self.advance();
        expr
    }

    fn parse_infix_expression(
        &mut self,
        expr: Expression,
    ) -> Option<Expression> {
        let token = self.current_token.clone();
        let operator = self.current_token.literal.clone();
        let priority = self.get_current_token_priority();
        self.advance();

        let rhs = self.parse_expression(priority);
        if rhs.is_none() {
            self.gen_error("Expected expression");
            return None;
        }

        Some(Expression::InfixOperation(InfixOperationData {
            token,
            operation: operator,
            left: Box::from(expr),
            right: Box::from(rhs.unwrap()),
        }))
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let token = self.current_token.clone();
        let operator = self.current_token.literal.clone();
        self.advance();

        println!("Token: {}\nOperator: {}\n", token, operator);

        let expr = self.parse_expression(Priority::Prefix);
        if expr.is_none() {
            self.gen_error("Expected expression");
            return None;
        }

        Some(Expression::PrefixOperation(PrefixOperationData {
            token,
            operation: operator,
            expression: Box::from(expr.unwrap()),
        }))
    }

    fn parse_identifier_starting_expression(&mut self) -> Option<Expression> {
        self.parse_identifier()
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
        self.errors.push(format!(
            "Error: {}. Found {} {}",
            err.into(),
            self.current_token,
            self.next_token,
        ));
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

    #[test]
    fn prefix_expression() {
        do_test("!10", "(!10)");
        do_test("!some_value", "(!some_value any)");
    }

    #[test]
    fn infix_expression() {
        do_test("1 + 1", "(1 + 1)")
    }

    #[test]
    fn grouped_expression_and_operators_priority() {
        do_test("3 + 1 * 2", "(3 + (1 * 2))");
        do_test("3 * 1 + 2", "((3 * 1) + 2)");
        do_test("3 * (1 + 2)", "(3 * (1 + 2))");
    }

    #[test]
    fn if_expression() {
        do_test("if x == 10 { 10 }", "if (x any == 10) {10}");
        do_test(
            "if x == 10 { 10 } else { 20 }",
            "if (x any == 10) {10} else {20}",
        );
        do_test(
            "if x == 10 { 10 } else if y == 20 { 20 }",
            "if (x any == 10) {10} else if (y any == 20) {20}",
        );
        do_test(
            "if x == 10 { 10 } else if y == 10 { 30 } else { 20 }",
            "if (x any == 10) {10} else if (y any == 10) {30} else {20}",
        );
        do_test(
            "if x == 10 { 10 } else if y == 10 { 30 } else if y == 10 { 30 } else { 20 }",
            "if (x any == 10) {10} else if (y any == 10) {30} else if (y any == 10) {30} else {20}",
        );
    }

    #[test]
    fn block_expressions() {
        do_test("{10 20}", "{10; 20}");
    }

    #[test]
    fn function_expression() {
        do_test(
            "func(x, y) { x + y }",
            "func(x any, y any) {(x any + y any)}",
        )
    }

    #[test]
    fn invokation_expression() {
        do_test("foo(x, 3 + 1)", "foo any(x any, (3 + 1))");
    }

    #[test]
    fn return_statement() {
        do_test("return foo()", "return foo any()");
    }
}
