use super::ast::{
    BlockExpressionData, DeclarationData, Expression, Module, Statement,
};

#[derive(Debug)]
pub struct Backend {
    pub module: Module,
    pub data: String,
}

impl Backend {
    pub fn new(module: Module) -> Self {
        Self {
            module,
            data: "".to_string(),
        }
    }

    pub fn transpile(&mut self) -> String {
        for statement in self.module.statements.iter() {
            let code = self.emit_statement(statement);
            self.data += &code;
        }

        self.data.clone()
    }

    fn emit_statement(&self, statement: &Statement) -> String {
        match statement {
            Statement::Declaration(data) => self.emit_declaration(data),
            Statement::Return(_) => todo!(),
            Statement::Expression(expr) => match &expr.expression {
                Expression::NumberLiteral(data) => format!("{}", data.value),
                Expression::IfExpression(if_expr) => format!(
                    "{}",
                    if_expr
                        .branches
                        .iter()
                        .map(|branch| {
                            match &branch.condition {
                                Some(cond) => format!(
                                    "if ({}) {}",
                                    self.emit_expression(cond),
                                    self.emit_block_statemnt(
                                        &branch.consequense
                                    )
                                ),
                                None => format!(
                                    "{}",
                                    self.emit_block_statemnt(
                                        &branch.consequense
                                    )
                                ),
                            }
                        })
                        .collect::<Vec<String>>()
                        .join("else ")
                ),
                Expression::Identifier(ident) => format!("{}", ident.name),
                Expression::PrefixOperation(_) => todo!(),
                Expression::InfixOperation(op) => format!(
                    "({} {} {})",
                    self.emit_expression(&op.left),
                    op.operation,
                    self.emit_expression(&op.right)
                ),
                Expression::BlockExpression(_) => todo!(),
                Expression::FunctionExpression(_) => todo!(),
                Expression::InvokationExpression(_) => todo!(),
            },
        }
    }

    fn emit_expression(&self, expression: &Expression) -> String {
        match expression {
            Expression::NumberLiteral(data) => format!("{}", data.value),
            Expression::IfExpression(if_expr) => format!(
                "(() => {{{}}})()",
                if_expr
                    .branches
                    .iter()
                    .map(|branch| {
                        match &branch.condition {
                            Some(cond) => format!(
                                "if ({}) {}",
                                self.emit_expression(cond),
                                self.emit_block_statemnt_with_last_return(
                                    &branch.consequense
                                )
                            ),
                            None => format!(
                                "{}",
                                self.emit_block_statemnt_with_last_return(
                                    &branch.consequense
                                )
                            ),
                        }
                    })
                    .collect::<Vec<String>>()
                    .join("else ")
            ),
            Expression::Identifier(ident) => format!("{}", ident.name),
            Expression::PrefixOperation(_) => todo!(),
            Expression::InfixOperation(op) => format!(
                "({} {} {})",
                self.emit_expression(&op.left),
                op.operation,
                self.emit_expression(&op.right)
            ),
            Expression::BlockExpression(_) => todo!(),
            Expression::FunctionExpression(_) => todo!(),
            Expression::InvokationExpression(_) => todo!(),
        }
    }

    fn emit_block_statemnt(&self, block: &BlockExpressionData) -> String {
        format!(
            "{{\n{}\n}}",
            block
                .statements
                .iter()
                .map(|statement| format!("{};", self.emit_statement(statement)))
                .collect::<Vec<String>>()
                .join("\n")
        )
    }

    fn emit_block_statemnt_with_last_return(
        &self,
        block: &BlockExpressionData,
    ) -> String {
        format!(
            "{{\n{}\n}}",
            block
                .statements
                .iter()
                .enumerate()
                .map(|(index, statement)| {
                    if index != block.statements.len() - 1 {
                        format!("{};", self.emit_statement(statement))
                    } else {
                        format!("return {};", self.emit_statement(statement))
                    }
                })
                .collect::<Vec<String>>()
                .join("\n")
        )
    }

    fn emit_declaration(&self, declration_data: &DeclarationData) -> String {
        match declration_data.variant {
            super::ast::DeclarationVariant::Constant => {
                format!(
                    "const {} = {};\n",
                    declration_data.identifier.name,
                    self.emit_expression(&declration_data.expression)
                )
            }
            super::ast::DeclarationVariant::Mutable => todo!(),
        }
    }
}
