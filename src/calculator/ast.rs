use std::{cell::RefCell, collections::HashMap, fmt::Debug, rc::Rc};

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

pub fn less_than<'a>(lhs: Expression<'a>, rhs: Expression<'a>) -> Expression<'a> {
    Expression::BinaryExpression {
        operator: Operator::LessThan,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn greater_than<'a>(lhs: Expression<'a>, rhs: Expression<'a>) -> Expression<'a> {
    Expression::BinaryExpression {
        operator: Operator::GreaterThan,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn less_than_equal<'a>(lhs: Expression<'a>, rhs: Expression<'a>) -> Expression<'a> {
    Expression::BinaryExpression {
        operator: Operator::LessThanEqual,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn greater_than_equal<'a>(lhs: Expression<'a>, rhs: Expression<'a>) -> Expression<'a> {
    Expression::BinaryExpression {
        operator: Operator::GreaterThanEqual,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn equal_equal<'a>(lhs: Expression<'a>, rhs: Expression<'a>) -> Expression<'a> {
    Expression::BinaryExpression {
        operator: Operator::EqualEqual,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn not_equal<'a>(lhs: Expression<'a>, rhs: Expression<'a>) -> Expression<'a> {
    Expression::BinaryExpression {
        operator: Operator::NotEqual,
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

pub fn identifier(name: &str) -> Expression {
    Expression::Identifier(name)
}

pub fn r#if<'a>(
    condition: Expression<'a>,
    then_clause: Expression<'a>,
    else_clause: Option<Expression<'a>>,
) -> Expression<'a> {
    Expression::IfExpression {
        condition: Box::new(condition),
        then_clause: Box::new(then_clause),
        else_clause: else_clause.map(Box::new),
    }
}

pub fn r#while<'a>(condition: Expression<'a>, body: Expression<'a>) -> Expression<'a> {
    Expression::WhileExpression {
        condition: Box::new(condition),
        body: Box::new(body),
    }
}

pub fn block(expression: Vec<Expression>) -> Expression {
    Expression::BlockExpression(expression)
}

#[derive(PartialEq, Clone, Debug)]
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
    IfExpression {
        condition: Box<Expression<'a>>,
        then_clause: Box<Expression<'a>>,
        else_clause: Option<Box<Expression<'a>>>,
    },
    WhileExpression {
        condition: Box<Expression<'a>>,
        body: Box<Expression<'a>>,
    },
    BlockExpression(Vec<Expression<'a>>),
    FunctionCall {
        name: &'a str,
        args: Vec<Expression<'a>>,
    },
}

#[derive(PartialEq, Clone, Debug)]
pub enum TopLevel<'t> {
    // To avoid using unstable feature (#65490)
    FunctionDefinition {
        name: &'t str,
        args: Vec<&'t str>,
        body: Box<Expression<'t>>,
    },
}

#[derive(PartialEq, Clone, Debug)]
pub struct Program<'p>(Vec<TopLevel<'p>>);

impl<'p> Program<'p> {
    pub fn definitions(self) -> Vec<TopLevel<'p>> {
        self.0
    }
}

pub type Binding<'e> = Rc<RefCell<HashMap<&'e str, i32>>>;

#[derive(PartialEq, Clone, Debug)]
pub struct Environment<'e> {
    pub bindings: Binding<'e>,
    next: Option<Box<Environment<'e>>>,
}

impl<'a> Environment<'a> {
    pub fn new() -> Self {
        Self {
            bindings: Rc::new(RefCell::new(HashMap::new())),
            next: None,
        }
    }

    pub fn with_next(next: Option<Box<Environment<'a>>>) -> Self {
        Self {
            bindings: Rc::new(RefCell::new(HashMap::new())),
            next,
        }
    }

    pub fn find_binding(&self, name: &str) -> Option<Binding<'a>> {
        if self.bindings.borrow_mut().get(name).is_some() {
            return Some(self.bindings.clone());
        }
        self.next.as_ref().and_then(|n| n.find_binding(name))
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    EqualEqual,
    NotEqual,
}

impl Operator {
    pub fn name(&self) -> String {
        match *self {
            Operator::Add => "+".to_string(),
            Operator::Subtract => "-".to_string(),
            Operator::Multiply => "*".to_string(),
            Operator::Divide => "/".to_string(),
            Operator::LessThan => "<".to_string(),
            Operator::GreaterThan => ">".to_string(),
            Operator::LessThanEqual => "<=".to_string(),
            Operator::GreaterThanEqual => ">=".to_string(),
            Operator::EqualEqual => "==".to_string(),
            Operator::NotEqual => "!=".to_string(),
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
