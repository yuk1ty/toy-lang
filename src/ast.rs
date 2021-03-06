#![allow(clippy::enum_variant_names)]

use std::{cell::RefCell, collections::HashMap, fmt::Debug, rc::Rc};

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

pub fn less_than(lhs: Expression, rhs: Expression) -> Expression {
    Expression::BinaryExpression {
        operator: Operator::LessThan,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn greater_than(lhs: Expression, rhs: Expression) -> Expression {
    Expression::BinaryExpression {
        operator: Operator::GreaterThan,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn less_than_equal(lhs: Expression, rhs: Expression) -> Expression {
    Expression::BinaryExpression {
        operator: Operator::LessThanEqual,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn greater_than_equal(lhs: Expression, rhs: Expression) -> Expression {
    Expression::BinaryExpression {
        operator: Operator::GreaterThanEqual,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn equal_equal(lhs: Expression, rhs: Expression) -> Expression {
    Expression::BinaryExpression {
        operator: Operator::EqualEqual,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn not_equal(lhs: Expression, rhs: Expression) -> Expression {
    Expression::BinaryExpression {
        operator: Operator::NotEqual,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn integer(value: i32) -> Expression {
    Expression::IntegerLiteral(value)
}

pub fn assignment(name: impl Into<String>, expression: Expression) -> Expression {
    Expression::Assignment {
        name: name.into(),
        expression: Box::new(expression),
    }
}

pub fn symbol(name: impl Into<String>) -> Expression {
    Expression::Identifier(name.into())
}

pub fn r#if(
    condition: Expression,
    then_clause: Expression,
    else_clause: Option<Expression>,
) -> Expression {
    Expression::IfExpression {
        condition: Box::new(condition),
        then_clause: Box::new(then_clause),
        else_clause: else_clause.map(Box::new),
    }
}

pub fn r#while(condition: Expression, body: Expression) -> Expression {
    Expression::WhileExpression {
        condition: Box::new(condition),
        body: Box::new(body),
    }
}

pub fn block(expression: Vec<Expression>) -> Expression {
    Expression::BlockExpression(expression)
}

pub fn call(name: impl Into<String>, elements: Vec<Expression>) -> Expression {
    Expression::FunctionCall {
        name: name.into(),
        args: elements,
    }
}

pub fn println(arg: Expression) -> Expression {
    Expression::Println(Box::new(arg))
}

pub fn define_function(name: impl Into<String>, args: Vec<String>, body: Expression) -> TopLevel {
    TopLevel::FunctionDefinition {
        name: name.into(),
        args,
        body: Box::new(body),
    }
}

pub fn global_definition(name: impl Into<String>, expression: Expression) -> TopLevel {
    TopLevel::GlobalVariableDefinition {
        name: name.into(),
        expression: Box::new(expression),
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Expression {
    BinaryExpression {
        operator: Operator,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    IntegerLiteral(i32),
    Assignment {
        name: String,
        expression: Box<Expression>,
    },
    Identifier(String),
    IfExpression {
        condition: Box<Expression>,
        then_clause: Box<Expression>,
        else_clause: Option<Box<Expression>>,
    },
    WhileExpression {
        condition: Box<Expression>,
        body: Box<Expression>,
    },
    BlockExpression(Vec<Expression>),
    FunctionCall {
        name: String,
        args: Vec<Expression>,
    },
    Println(Box<Expression>),
}

impl Expression {
    pub fn extract_params(self) -> Expressions {
        fn create_vec(expr: Expression, exprs: &mut Vec<Expression>) {
            match expr {
                e @ (Expression::IntegerLiteral(_) | Expression::Identifier(_)) => exprs.push(e),
                e
                @
                (Expression::BinaryExpression {
                    operator: _,
                    lhs: _,
                    rhs: _,
                }
                | Expression::FunctionCall { name: _, args: _ }) => exprs.push(e),
                // panic here is not recommended. It's inconsistent from the aspect of the entire program.
                // If I fully figure out the usage of `ParseResult` in combine,
                // I'll replace here to use anyhow's Result.
                Expression::WhileExpression {
                    condition: _,
                    body: _,
                } => {
                    panic!("while can't place here");
                }
                Expression::IfExpression {
                    condition: _,
                    then_clause: _,
                    else_clause: _,
                } => panic!("if can't place here"),
                Expression::Assignment {
                    name: _,
                    expression: _,
                } => panic!("assignment can't place here"),
                _ => create_vec(expr, exprs),
            }
        }
        let mut exprs = Vec::new();
        create_vec(self, &mut exprs);
        Expressions(exprs)
    }
}

pub struct Expressions(pub Vec<Expression>);

impl Default for Expressions {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl Extend<Expression> for Expressions {
    fn extend<T: IntoIterator<Item = Expression>>(&mut self, iter: T) {
        for elem in iter {
            self.0.push(elem);
        }
    }
}

impl Extend<Expressions> for Expressions {
    fn extend<T: IntoIterator<Item = Expressions>>(&mut self, iter: T) {
        for elem in iter {
            self.0.extend(elem.0)
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum TopLevel {
    FunctionDefinition {
        name: String,
        args: Vec<String>,
        body: Box<Expression>,
    },
    GlobalVariableDefinition {
        name: String,
        expression: Box<Expression>,
    },
}

#[derive(PartialEq, Clone, Debug)]
pub struct Program(Vec<TopLevel>);

impl Program {
    pub fn new(top_levels: Vec<TopLevel>) -> Self {
        Program(top_levels)
    }

    pub fn definitions(self) -> Vec<TopLevel> {
        self.0
    }
}

// TODO Rc & RefCell can be removed
pub type Binding = Rc<RefCell<HashMap<String, i32>>>;

#[derive(PartialEq, Clone, Debug)]
pub struct Environment {
    pub bindings: Binding,
    next: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            bindings: Rc::new(RefCell::new(HashMap::new())),
            next: None,
        }
    }

    pub fn with_next(next: Option<Box<Environment>>) -> Self {
        Self {
            bindings: Rc::new(RefCell::new(HashMap::new())),
            next,
        }
    }

    pub fn find_binding(&self, name: &str) -> Option<Binding> {
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
