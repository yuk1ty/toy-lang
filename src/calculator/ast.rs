use std::fmt::Debug;

pub fn add(lhs: Expression, rhs: Expression) -> Expression {
    Expression::BinaryExpression {
        operator: Operator::Add,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn subtract(lhs: Expression, rhs: Expression) -> Expression {
    Expression::BinaryExpression {
        operator: Operator::Subtract,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn multiply(lhs: Expression, rhs: Expression) -> Expression {
    Expression::BinaryExpression {
        operator: Operator::Multiply,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn divide(lhs: Expression, rhs: Expression) -> Expression {
    Expression::BinaryExpression {
        operator: Operator::Divide,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn integer(value: i32) -> Expression {
    Expression::IntegerLiteral(value)
}

#[derive(PartialEq, Debug)]
pub enum Expression {
    BinaryExpression {
        operator: Operator,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    IntegerLiteral(i32),
}

#[derive(PartialEq, Debug)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl Operator {
    pub fn name(&self) -> String {
        match *self {
            Operator::Add => "+".to_string(),
            Operator::Subtract => "-".to_string(),
            Operator::Multiply => "*".to_string(),
            Operator::Divide => "/".to_string(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_create_ast() {
        let ast = add(
            subtract(integer(1), multiply(integer(2), integer(3))),
            integer(4),
        );

        assert_eq!(
            Expression::BinaryExpression {
                operator: Operator::Add,
                lhs: Box::new(Expression::BinaryExpression {
                    operator: Operator::Subtract,
                    lhs: Box::new(Expression::IntegerLiteral(1)),
                    rhs: Box::new(Expression::BinaryExpression {
                        operator: Operator::Multiply,
                        lhs: Box::new(Expression::IntegerLiteral(2)),
                        rhs: Box::new(Expression::IntegerLiteral(3)),
                    }),
                }),
                rhs: Box::new(Expression::IntegerLiteral(4))
            },
            ast
        );
    }
}
