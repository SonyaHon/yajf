use std::fmt::Display;

use super::ir::Token;

#[derive(Debug)]
pub enum Type {
    Untyped,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "any")
    }
}

#[derive(Debug)]
pub enum Expression {
    NumberLiteral(NumberLiteralData),
    Identifier(IdentifierData),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Expression::NumberLiteral(data) => data.test_string(),
                Expression::Identifier(data) => data.test_string(),
            }
        )
    }
}

impl Into<NumberLiteralData> for Expression {
    fn into(self) -> NumberLiteralData {
        match self {
            Expression::NumberLiteral(data) => data,
            _ => unreachable!(),
        }
    }
}

impl Into<IdentifierData> for Expression {
    fn into(self) -> IdentifierData {
        match self {
            Expression::Identifier(data) => data,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub struct NumberLiteralData {
    pub token: Token,
    pub value: f64,
}

impl NumberLiteralData {
    pub fn test_string(&self) -> String {
        format!("{}", self.value)
    }
}

#[derive(Debug)]
pub enum Statement {
    Declaration(DeclarationData),
    Expression(ExpressionStatementData),
}

impl Statement {
    pub fn test_string(&self) -> String {
        match self {
            Statement::Declaration(data) => data.test_string(),
            Statement::Expression(data) => data.test_string(),
        }
    }
}

#[derive(Debug)]
pub struct ExpressionStatementData {
    pub token: Token,
    pub expression: Expression,
}

impl ExpressionStatementData {
    pub fn test_string(&self) -> String {
        format!("{}", self.expression)
    }
}

#[derive(Debug)]
pub struct IdentifierData {
    pub token: Token,
    pub typ: Type,
    pub name: String,
}

impl IdentifierData {
    pub fn test_string(&self) -> String {
        format!("{} {}", self.name, self.typ)
    }
}

#[derive(Debug)]
pub enum DeclarationVariant {
    Constant,
    Mutable,
}

impl Display for DeclarationVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DeclarationVariant::Constant => write!(f, "::"),
            DeclarationVariant::Mutable => write!(f, ":="),
        }
    }
}

#[derive(Debug)]
pub struct DeclarationData {
    pub token: Token,
    pub identifier: IdentifierData,
    pub variant: DeclarationVariant,
    pub expression: Expression,
}

impl DeclarationData {
    pub fn test_string(&self) -> String {
        format!(
            "{} {} {}",
            self.identifier.test_string(),
            self.variant,
            self.expression
        )
    }
}

#[derive(Debug)]
pub struct Module {
    pub errors: Vec<String>,
    pub statements: Vec<Statement>,
}

impl Module {
    pub fn test_string(&self) -> String {
        if !self.errors.is_empty() {
            panic!("Module has errors:\n{}", self.errors.join("\n"));
        }

        self.statements[0].test_string()
    }
}
