use std::collections::HashMap;

use crate::calculator::ast::{Expression, Operator};

use super::ast::{Environment, Program, TopLevel};

/// Bureaucrat type to hold FunctionDefinition values.
#[derive(PartialEq, Clone, Debug)]
struct FunctionDef<'a> {
    name: &'a str,
    args: Vec<&'a str>,
    body: Box<Expression<'a>>,
}

pub struct Interpreter<'a> {
    variable_environment: Environment<'a>,
    function_environment: HashMap<&'a str, FunctionDef<'a>>,
}

impl<'a> Interpreter<'a> {
    pub fn new() -> Self {
        Self {
            variable_environment: Environment::new(),
            function_environment: HashMap::new(),
        }
    }

    pub fn call_main(&mut self, program: Program<'a>) -> i32 {
        let top_levels = program.definitions();
        for top_level in top_levels {
            match top_level {
                TopLevel::FunctionDefinition { name, args, body } => {
                    self.function_environment
                        // To avoid using unstable feature (#65490)
                        .insert(name, FunctionDef { name, args, body });
                }
                TopLevel::GlobalVariableDefinition { name, expression } => {
                    let body = self.interpret(*expression);
                    self.variable_environment
                        .bindings
                        .borrow_mut()
                        .insert(name, body);
                }
            }
        }
        let main_function = self.function_environment.get("main").cloned();
        match main_function {
            Some(main_func) => {
                let FunctionDef {
                    name: _,
                    args: _,
                    body,
                } = main_func;
                self.interpret(*body)
            }
            None => panic!("This program doesn't have main() function."),
        }
    }

    fn interpret(&mut self, expression: Expression<'a>) -> i32 {
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
            Expression::Identifier(ident) => self
                .variable_environment
                .find_binding(&ident)
                // FIXME: Wildly unwrapping ;) Needs to be replaced to anyhow.
                .and_then(|elem| {
                    let elem = &*elem;
                    elem.borrow().get(ident.as_str()).cloned()
                })
                .unwrap_or_else(|| panic!("Undefined variable: {}", ident)),
            Expression::Assignment { name, expression } => {
                let value = self.interpret(*expression);
                let bindings_opt = self.variable_environment.find_binding(name);
                match bindings_opt {
                    Some(map) => map.borrow_mut().insert(name, value),
                    None => self
                        .variable_environment
                        .bindings
                        .borrow_mut()
                        .insert(name, value),
                };
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
            Expression::BlockExpression(elements) => {
                let mut value = 0;
                for elem in elements {
                    value = self.interpret(elem);
                }
                value
            }
            Expression::FunctionCall { name, args } => {
                let definition = self.function_environment.get(name).cloned();
                match definition {
                    Some(def) => {
                        let FunctionDef {
                            name: _,
                            args: fd_args,
                            body: fd_body,
                        } = def;
                        let values: Vec<i32> =
                            args.into_iter().map(|a| self.interpret(a)).collect();

                        let backup = self.variable_environment.clone();
                        // TODO
                        self.variable_environment = Interpreter::new_enviromnent(Some(Box::new(
                            self.variable_environment.clone(),
                        )));

                        for (i, formal_param_name) in fd_args.into_iter().enumerate() {
                            self.variable_environment.bindings.borrow_mut().insert(
                                formal_param_name,
                                *values
                                    .get(i)
                                    .unwrap_or_else(|| panic!("Value not found at index: {}", i)),
                            );
                        }
                        let result = self.interpret(*fd_body);
                        self.variable_environment = backup;
                        result
                    }
                    None => {
                        panic!("Function {} is not found", name);
                    }
                }
            }
        }
    }

    fn new_enviromnent(next: Option<Box<Environment<'a>>>) -> Environment<'a> {
        Environment::with_next(next)
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
        // assert_eq!(20, *interpreter.environment.get("a").unwrap());
    }

    #[test]
    fn test_identifier_after_assignment_should_work() {
        let mut interpreter = interpreter();
        let e1 = assignment("a", add(integer(10), integer(10)));
        assert_eq!(20, interpreter.interpret(e1));
        let e2 = add(symbol("a"), integer(10));
        assert_eq!(30, interpreter.interpret(e2));
    }

    #[test]
    fn test_increment_should_work() {
        let mut interpreter = interpreter();
        let e1 = assignment("a", add(integer(10), integer(10)));
        assert_eq!(20, interpreter.interpret(e1));
        let e2 = add(symbol("a"), integer(10));
        assert_eq!(30, interpreter.interpret(e2));
    }

    #[test]
    #[should_panic]
    fn test_identifier_not_found() {
        let mut interpreter = interpreter();
        let e1 = assignment("a", add(integer(10), integer(10)));
        assert_eq!(20, interpreter.interpret(e1));
        let e2 = add(symbol("b"), integer(10));
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
            less_than(symbol("count"), integer(3)),
            assignment("count", add(symbol("count"), integer(1))),
        );
        assert_eq!(1, interpreter.interpret(e2));
    }

    #[test]
    fn test_factorial() {
        let top_levels = vec![
            define_function("main", vec![], block(vec![call("fact", vec![integer(5)])])),
            define_function(
                "fact",
                vec!["n"],
                block(vec![r#if(
                    less_than(symbol("n"), integer(2)),
                    integer(1),
                    Some(multiply(
                        symbol("n"),
                        call("fact", vec![subtract(symbol("n"), integer(1))]),
                    )),
                )]),
            ),
        ];

        let result = interpreter().call_main(Program::new(top_levels));
        assert_eq!(120, result);
    }
}
