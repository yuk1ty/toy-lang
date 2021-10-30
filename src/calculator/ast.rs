use std::fmt::Debug;

pub fn add<'a>(lhs: Expression<'a>, rhs: Expression<'a>) -> Expression<'a> {
    Expression::BinaryExpression {
        operator: Operator::Add,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn subtract<'a>(lhs: Expression<'a>, rhs: Expression<'a>) -> Expression<'a> {
    Expression::BinaryExpression {
        operator: Operator::Subtract,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn multiply<'a>(lhs: Expression<'a>, rhs: Expression<'a>) -> Expression<'a> {
    Expression::BinaryExpression {
        operator: Operator::Multiply,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn divide<'a>(lhs: Expression<'a>, rhs: Expression<'a>) -> Expression<'a> {
    Expression::BinaryExpression {
        operator: Operator::Divide,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn integer<'a>(value: i32) -> Expression<'a> {
    Expression::IntegerLiteral(value)
}

pub fn assignment<'a>(name: &'a str, expression: Expression<'a>) -> Expression<'a> {
    Expression::Assignment {
        name,
        expression: Box::new(expression),
    }
}

pub fn identifier<'a>(name: &'a str) -> Expression<'a> {
    Expression::Identifier(name)
}

#[derive(PartialEq, Debug)]
pub enum Expression<'a> {
    BinaryExpression {
        operator: Operator,
        lhs: Box<Expression<'a>>,
        rhs: Box<Expression<'a>>,
    },
    IntegerLiteral(i32),
    Assignment {
        name: &'a str,
        expression: Box<Expression<'a>>,
    },
    Identifier(&'a str),
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
