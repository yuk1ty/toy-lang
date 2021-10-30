use crate::calculator::ast::{Expression, Operator};

pub fn interpret(expression: Expression) -> i32 {
    match expression {
        Expression::BinaryExpression { operator, lhs, rhs } => {
            let lhs = interpret(*lhs);
            let rhs = interpret(*rhs);
            match operator {
                Operator::Add => lhs + rhs,
                Operator::Subtract => lhs - rhs,
                Operator::Multiply => lhs * rhs,
                Operator::Divide => lhs / rhs,
            }
        }
        Expression::IntegerLiteral(value) => value,
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::calculator::ast::*;

    #[test]
    fn test_10_plus_20_should_work() {
        let e = add(integer(10), integer(20));
        assert_eq!(30, interpret(e));
    }

    #[test]
    fn test_10_plus_0_should_work() {
        let e = add(integer(10), integer(0));
        assert_eq!(10, interpret(e));
    }

    #[test]
    fn test_0_plus_10_should_work() {
        let e = add(integer(0), integer(10));
        assert_eq!(10, interpret(e));
    }

    #[test]
    fn test_10_minus_20_should_work() {
        let e = subtract(integer(10), integer(20));
        assert_eq!(-10, interpret(e));
    }

    #[test]
    fn test_10_minus_0_should_work() {
        let e = subtract(integer(10), integer(0));
        assert_eq!(10, interpret(e));
    }

    #[test]
    fn test_0_minus_10_should_work() {
        let e = subtract(integer(0), integer(10));
        assert_eq!(-10, interpret(e));
    }

    #[test]
    fn test_10_multiply_20_should_work() {
        let e = multiply(integer(10), integer(20));
        assert_eq!(200, interpret(e));
    }

    #[test]
    fn test_10_multiply_0_should_work() {
        let e = multiply(integer(10), integer(0));
        assert_eq!(0, interpret(e));
    }

    #[test]
    fn test_0_multiply_10_should_work() {
        let e = multiply(integer(0), integer(10));
        assert_eq!(0, interpret(e));
    }

    #[test]
    fn test_20_divide_10_should_work() {
        let e = divide(integer(20), integer(10));
        assert_eq!(2, interpret(e));
    }

    #[test]
    #[should_panic]
    fn test_10_divide_0_should_work() {
        let e = divide(integer(10), integer(0));
        interpret(e);
    }

    #[test]
    fn test_0_divide_10_should_work() {
        let e = divide(integer(0), integer(10));
        assert_eq!(0, interpret(e));
    }
}
