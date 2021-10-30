use std::collections::HashMap;

use crate::calculator::ast::{Expression, Operator};

pub struct Interpreter<'a> {
    environment: HashMap<&'a str, i32>,
}

impl<'a> Interpreter<'a> {
    pub fn new() -> Self {
        Self {
            environment: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, expression: Expression<'a>) -> i32 {
        match expression {
            Expression::BinaryExpression { operator, lhs, rhs } => {
                let lhs = self.interpret(*lhs);
                let rhs = self.interpret(*rhs);
                match operator {
                    Operator::Add => lhs + rhs,
                    Operator::Subtract => lhs - rhs,
                    Operator::Multiply => lhs * rhs,
                    Operator::Divide => lhs / rhs,
                    Operator::GreaterThan => (lhs > rhs) as i32,
                    Operator::LessThan => (lhs < rhs) as i32,
                    Operator::GreaterThanEqual => (lhs >= rhs) as i32,
                    Operator::LessThanEqual => (lhs <= rhs) as i32,
                    Operator::EqualEqual => (lhs == rhs) as i32,
                    Operator::NotEqual => (lhs != rhs) as i32,
                }
            }
            Expression::IntegerLiteral(value) => value,
            Expression::Identifier(ident) => *self
                .environment
                .get(ident)
                .unwrap_or_else(|| panic!("Undefined variable: {}", ident)),
            Expression::Assignment { name, expression } => {
                let value = self.interpret(*expression);
                self.environment.insert(name, value);
                value
            }
            Expression::IfExpression {
                condition,
                then_clause,
                else_clause,
            } => {
                let condition = self.interpret(*condition);
                if condition != 0 {
                    self.interpret(*then_clause)
                } else {
                    else_clause.map(|expr| self.interpret(*expr)).unwrap_or(1)
                }
            }
            Expression::WhileExpression { condition, body } => {
                loop {
                    // TODO
                    let condition = self.interpret(*condition.clone());
                    if condition != 0 {
                        self.interpret(*body.clone());
                    } else {
                        break;
                    }
                }
                1
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::calculator::ast::*;

    fn interpreter<'a>() -> Interpreter<'a> {
        Interpreter::new()
    }

    #[test]
    fn test_10_plus_20_should_work() {
        let e = add(integer(10), integer(20));
        assert_eq!(30, interpreter().interpret(e));
    }

    #[test]
    fn test_10_plus_0_should_work() {
        let e = add(integer(10), integer(0));
        assert_eq!(10, interpreter().interpret(e));
    }

    #[test]
    fn test_0_plus_10_should_work() {
        let e = add(integer(0), integer(10));
        assert_eq!(10, interpreter().interpret(e));
    }

    #[test]
    fn test_10_minus_20_should_work() {
        let e = subtract(integer(10), integer(20));
        assert_eq!(-10, interpreter().interpret(e));
    }

    #[test]
    fn test_10_minus_0_should_work() {
        let e = subtract(integer(10), integer(0));
        assert_eq!(10, interpreter().interpret(e));
    }

    #[test]
    fn test_0_minus_10_should_work() {
        let e = subtract(integer(0), integer(10));
        assert_eq!(-10, interpreter().interpret(e));
    }

    #[test]
    fn test_10_multiply_20_should_work() {
        let e = multiply(integer(10), integer(20));
        assert_eq!(200, interpreter().interpret(e));
    }

    #[test]
    fn test_10_multiply_0_should_work() {
        let e = multiply(integer(10), integer(0));
        assert_eq!(0, interpreter().interpret(e));
    }

    #[test]
    fn test_0_multiply_10_should_work() {
        let e = multiply(integer(0), integer(10));
        assert_eq!(0, interpreter().interpret(e));
    }

    #[test]
    fn test_20_divide_10_should_work() {
        let e = divide(integer(20), integer(10));
        assert_eq!(2, interpreter().interpret(e));
    }

    #[test]
    #[should_panic]
    fn test_10_divide_0_should_work() {
        let e = divide(integer(10), integer(0));
        interpreter().interpret(e);
    }

    #[test]
    fn test_0_divide_10_should_work() {
        let e = divide(integer(0), integer(10));
        assert_eq!(0, interpreter().interpret(e));
    }

    #[test]
    fn test_assignment_should_work() {
        let e = assignment("a", add(integer(10), integer(10)));
        let mut interpreter = interpreter();
        assert_eq!(20, interpreter.interpret(e));
        assert_eq!(20, *interpreter.environment.get("a").unwrap());
    }

    #[test]
    fn test_identifier_after_assignment_should_work() {
        let mut interpreter = interpreter();
        let e1 = assignment("a", add(integer(10), integer(10)));
        assert_eq!(20, interpreter.interpret(e1));
        let e2 = add(identifier("a"), integer(10));
        assert_eq!(30, interpreter.interpret(e2));
    }

    #[test]
    fn test_increment_should_work() {
        let mut interpreter = interpreter();
        let e1 = assignment("a", add(integer(10), integer(10)));
        assert_eq!(20, interpreter.interpret(e1));
        let e2 = add(identifier("a"), integer(10));
        assert_eq!(30, interpreter.interpret(e2));
        assert_eq!(30, *interpreter.environment.get("a").unwrap());
    }

    #[test]
    #[should_panic]
    fn test_identifier_not_found() {
        let mut interpreter = interpreter();
        let e1 = assignment("a", add(integer(10), integer(10)));
        assert_eq!(20, interpreter.interpret(e1));
        let e2 = add(identifier("b"), integer(10));
        interpreter.interpret(e2);
    }

    #[test]
    fn test_if_expression_partially_should_work() {
        let e = r#if(
            less_than(integer(1), integer(2)),
            integer(33),
            Some(integer(42)),
        );
        assert_eq!(33, interpreter().interpret(e));
    }

    #[test]
    fn test_while_expression_partially_should_work() {
        let mut interpreter = interpreter();
        // val count = 0
        let e1 = assignment("count", integer(0));
        assert_eq!(0, interpreter.interpret(e1));
        // while (count < 3) {
        //    count = count + 1
        // }
        let e2 = r#while(
            less_than(identifier("count"), integer(3)),
            assignment("count", add(identifier("count"), integer(1))),
        );
        assert_eq!(1, interpreter.interpret(e2));
    }
}
