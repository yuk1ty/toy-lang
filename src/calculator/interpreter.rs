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
                }
            }
            Expression::IntegerLiteral(value) => value,
            Expression::Identifier(ident) => *self
                .environment
                .get(ident)
                .expect(&format!("Undefined variable: {}", ident)),
            Expression::Assignment { name, expression } => {
                let value = self.interpret(*expression);
                self.environment.insert(name, value);
                value
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
}
