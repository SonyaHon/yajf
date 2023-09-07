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
    PrefixOperation(PrefixOperationData),
    InfixOperation(InfixOperationData),
    IfExpression(IfExpressionData),
    BlockExpression(BlockExpressionData),
    FunctionExpression(FunctionExpressionData),
    InvokationExpression(InvokationExpressionData),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Expression::NumberLiteral(data) => data.test_string(),
                Expression::Identifier(data) => data.test_string(),
                Expression::PrefixOperation(data) => data.test_string(),
                Expression::InfixOperation(data) => data.test_string(),
                Expression::IfExpression(data) => data.test_string(),
                Expression::BlockExpression(data) => data.test_string(),
                Expression::FunctionExpression(data) => data.test_string(),
                Expression::InvokationExpression(data) => data.test_string(),
            }
        )
    }
}

impl Into<InvokationExpressionData> for Expression {
    fn into(self) -> InvokationExpressionData {
        match self {
            Expression::InvokationExpression(data) => data,
            _ => unreachable!(),
        }
    }
}

impl Into<FunctionExpressionData> for Expression {
    fn into(self) -> FunctionExpressionData {
        match self {
            Expression::FunctionExpression(data) => data,
            _ => unreachable!(),
        }
    }
}

impl Into<BlockExpressionData> for Expression {
    fn into(self) -> BlockExpressionData {
        match self {
            Expression::BlockExpression(data) => data,
            _ => unreachable!(),
        }
    }
}

impl Into<IfExpressionData> for Expression {
    fn into(self) -> IfExpressionData {
        match self {
            Expression::IfExpression(data) => data,
            _ => unreachable!(),
        }
    }
}

impl Into<InfixOperationData> for Expression {
    fn into(self) -> InfixOperationData {
        match self {
            Expression::InfixOperation(data) => data,
            _ => unreachable!(),
        }
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

impl Into<PrefixOperationData> for Expression {
    fn into(self) -> PrefixOperationData {
        match self {
            Expression::PrefixOperation(data) => data,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub struct InvokationExpressionData {
    pub token: Token,
    pub target: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl InvokationExpressionData {
    pub fn test_string(&self) -> String {
        format!(
            "{}({})",
            self.target,
            self.arguments
                .iter()
                .map(|arg| format!("{}", arg))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[derive(Debug)]
pub struct FunctionExpressionData {
    pub token: Token,
    pub arguments: Vec<IdentifierData>,
    pub body: BlockExpressionData,
}

impl FunctionExpressionData {
    pub fn test_string(&self) -> String {
        format!(
            "func({}) {}",
            self.arguments
                .iter()
                .map(|arg| arg.test_string())
                .collect::<Vec<String>>()
                .join(", "),
            self.body.test_string()
        )
    }
}

#[derive(Debug)]
pub struct BlockExpressionData {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl BlockExpressionData {
    pub fn test_string(&self) -> String {
        format!(
            "{{{}}}",
            self.statements
                .iter()
                .map(|stmt| stmt.test_string())
                .collect::<Vec<String>>()
                .join("; ")
        )
    }
}

#[derive(Debug)]
pub struct IfExpressionBranch {
    pub token: Token,
    pub condition: Option<Expression>,
    pub consequense: BlockExpressionData,
}

#[derive(Debug)]
pub struct IfExpressionData {
    pub token: Token,
    pub branches: Vec<IfExpressionBranch>,
}

impl IfExpressionData {
    pub fn test_string(&self) -> String {
        format!(
            "{}",
            self.branches
                .iter()
                .enumerate()
                .map(|(idx, branch)| {
                    if idx == 0 {
                        format!(
                            "if {} {}",
                            branch.condition.as_ref().unwrap(),
                            branch.consequense.test_string(),
                        )
                    } else if branch.condition.is_some() {
                        format!(
                            "else if {} {}",
                            branch.condition.as_ref().unwrap(),
                            branch.consequense.test_string(),
                        )
                    } else {
                        format!("else {}", branch.consequense.test_string())
                    }
                })
                .collect::<Vec<String>>()
                .join(" ")
        )
    }
}

#[derive(Debug)]
pub struct InfixOperationData {
    pub token: Token,
    pub operation: String,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

impl InfixOperationData {
    pub fn test_string(&self) -> String {
        format!("({} {} {})", self.left, self.operation, self.right)
    }
}

#[derive(Debug)]
pub struct PrefixOperationData {
    pub token: Token,
    pub operation: String,
    pub expression: Box<Expression>,
}

impl PrefixOperationData {
    pub fn test_string(&self) -> String {
        format!("({}{})", self.operation, self.expression)
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
    Return(ReturnData),
    Expression(ExpressionStatementData),
}

impl Statement {
    pub fn test_string(&self) -> String {
        match self {
            Statement::Declaration(data) => data.test_string(),
            Statement::Expression(data) => data.test_string(),
            Statement::Return(data) => data.test_string(),
        }
    }
}

#[derive(Debug)]
pub struct ReturnData {
    pub token: Token,
    pub return_value: Expression,
}

impl ReturnData {
    pub fn test_string(&self) -> String {
        format!("return {}", self.return_value)
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
