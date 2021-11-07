#![allow(clippy::new_without_default)]

use std::collections::HashMap;

use super::ast::{Environment, Expression, Operator, Program, TopLevel};

use anyhow::{anyhow, Result};

/// Bureaucrat type to hold FunctionDefinition values.
#[derive(PartialEq, Clone, Debug)]
struct FunctionDef {
    name: String,
    args: Vec<String>,
    body: Box<Expression>,
}

pub struct Interpreter {
    variable_environment: Environment,
    function_environment: HashMap<String, FunctionDef>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            variable_environment: Environment::new(),
            function_environment: HashMap::new(),
        }
    }

    pub fn call_main(&mut self, program: Program) -> Result<i32> {
        let top_levels = program.definitions();
        for top_level in top_levels {
            match top_level {
                TopLevel::FunctionDefinition { name, args, body } => {
                    self.function_environment
                        .insert(name.clone(), FunctionDef { name, args, body });
                }
                TopLevel::GlobalVariableDefinition { name, expression } => {
                    let body = self.interpret(*expression)?;
                    self.variable_environment
                        .bindings
                        .borrow_mut()
                        .insert(name.to_string(), body);
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
                Ok(self.interpret(*body)?)
            }
            None => Err(anyhow!("This program doesn't have main() function.")),
        }
    }

    fn interpret(&mut self, expression: Expression) -> Result<i32> {
        match expression {
            Expression::BinaryExpression { operator, lhs, rhs } => {
                let lhs = self.interpret(*lhs)?;
                let rhs = self.interpret(*rhs)?;
                Ok(match operator {
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
                })
            }
            Expression::IntegerLiteral(value) => Ok(value),
            Expression::Identifier(ident) => match self
                .variable_environment
                .find_binding(&ident)
                // FIXME: Wildly unwrapping ;) Needs to be replaced to anyhow.
                .and_then(|elem| {
                    let elem = &*elem;
                    elem.borrow().get(ident.as_str()).cloned()
                }) {
                Some(value) => Ok(value),
                None => Err(anyhow!("Undefined variable {}", ident)),
            },
            Expression::Assignment { name, expression } => {
                let value = self.interpret(*expression)?;
                let bindings_opt = self.variable_environment.find_binding(&name);
                match bindings_opt {
                    Some(map) => map.borrow_mut().insert(name, value),
                    None => self
                        .variable_environment
                        .bindings
                        .borrow_mut()
                        .insert(name, value),
                };
                Ok(value)
            }
            Expression::IfExpression {
                condition,
                then_clause,
                else_clause,
            } => {
                let condition = self.interpret(*condition)?;
                if condition != 0 {
                    Ok(self.interpret(*then_clause)?)
                } else {
                    else_clause.map_or_else(|| Ok(1), |expr| self.interpret(*expr))
                }
            }
            Expression::WhileExpression { condition, body } => {
                loop {
                    // TODO
                    let condition = self.interpret(*condition.clone())?;
                    if condition != 0 {
                        self.interpret(*body.clone())?;
                    } else {
                        break;
                    }
                }
                Ok(1)
            }
            Expression::BlockExpression(elements) => {
                let mut value = 0;
                for elem in elements {
                    value = self.interpret(elem)?;
                }
                Ok(value)
            }
            Expression::FunctionCall { name, args } => {
                let definition = self.function_environment.get(name.as_str()).cloned();
                match definition {
                    Some(def) => {
                        let FunctionDef {
                            name: _,
                            args: fd_args,
                            body: fd_body,
                        } = def;
                        let values = {
                            let mut ret = Vec::new();
                            for arg in args {
                                ret.push(self.interpret(arg)?);
                            }
                            ret
                        };

                        let backup = self.variable_environment.clone();
                        // TODO
                        self.variable_environment = Interpreter::new_enviromnent(Some(Box::new(
                            self.variable_environment.clone(),
                        )));

                        for (i, formal_param_name) in fd_args.into_iter().enumerate() {
                            self.variable_environment.bindings.borrow_mut().insert(
                                formal_param_name.to_string(),
                                values
                                    .get(i)
                                    .cloned()
                                    .ok_or_else(|| anyhow!("Value not found at index: {}", i))?,
                            );
                        }
                        let result = self.interpret(*fd_body);
                        self.variable_environment = backup;
                        result
                    }
                    None => Err(anyhow!("Function {} is not found", name)),
                }
            }
            Expression::Println(args) => {
                let value = self.interpret(*args)?;
                println!("{}", value);
                Ok(value)
            }
        }
    }

    fn new_enviromnent(next: Option<Box<Environment>>) -> Environment {
        Environment::with_next(next)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::*;

    use test_case::test_case;

    fn interpreter() -> Interpreter {
        Interpreter::new()
    }

    #[test_case(10, 20 => 30; "10_plus_20")]
    #[test_case(10, 0 => 10; "10_plus_0")]
    #[test_case(0, 10 => 10; "0_plus_10")]
    fn test_plus_should_work(lhs: i32, rhs: i32) -> i32 {
        let e = add(integer(lhs), integer(rhs));
        interpreter().interpret(e).unwrap()
    }

    #[test_case(10, 20 => -10; "10_minus_20")]
    #[test_case(10, 0 => 10; "10_minus_0")]
    #[test_case(0, 10 => -10; "0_minus_10")]
    fn test_minus_should_work(lhs: i32, rhs: i32) -> i32 {
        let e = subtract(integer(lhs), integer(rhs));
        interpreter().interpret(e).unwrap()
    }

    #[test_case(10, 20 => 200; "10_multiply_20")]
    #[test_case(10, 0 => 0; "10_multiply_0")]
    #[test_case(0, 10 => 0; "0_multiply_10")]
    fn test_multiply_should_work(lhs: i32, rhs: i32) -> i32 {
        let e = multiply(integer(lhs), integer(rhs));
        interpreter().interpret(e).unwrap()
    }

    #[test_case(20, 10 => 2; "20_divide_10")]
    #[test_case(10, 0 => panics "attempt to divide by zero"; "10_divide_0")]
    #[test_case(0, 10 => 0; "0_divide_10")]
    fn test_divide_should_work(lhs: i32, rhs: i32) -> i32 {
        let e = divide(integer(lhs), integer(rhs));
        interpreter().interpret(e).unwrap()
    }

    #[test]
    fn test_assignment_should_work() {
        let e = assignment("a", add(integer(10), integer(10)));
        let mut interpreter = interpreter();
        assert_eq!(20, interpreter.interpret(e).unwrap());
    }

    #[test]
    fn test_identifier_after_assignment_should_work() {
        let mut interpreter = interpreter();
        let e1 = assignment("a", add(integer(10), integer(10)));
        assert_eq!(20, interpreter.interpret(e1).unwrap());
        let e2 = add(symbol("a"), integer(10));
        assert_eq!(30, interpreter.interpret(e2).unwrap());
    }

    #[test]
    fn test_increment_should_work() {
        let mut interpreter = interpreter();
        let e1 = assignment("a", add(integer(10), integer(10)));
        assert_eq!(20, interpreter.interpret(e1).unwrap());
        let e2 = add(symbol("a"), integer(10));
        assert_eq!(30, interpreter.interpret(e2).unwrap());
    }

    #[test]
    #[should_panic]
    fn test_identifier_not_found() {
        let mut interpreter = interpreter();
        let e1 = assignment("a", add(integer(10), integer(10)));
        assert_eq!(20, interpreter.interpret(e1).unwrap());
        let e2 = add(symbol("b"), integer(10));
        interpreter.interpret(e2).unwrap();
    }

    #[test]
    fn test_if_expression_partially_should_work() {
        let e = r#if(
            less_than(integer(1), integer(2)),
            integer(33),
            Some(integer(42)),
        );
        assert_eq!(33, interpreter().interpret(e).unwrap());
    }

    #[test]
    fn test_while_expression_partially_should_work() {
        let mut interpreter = interpreter();
        // val count = 0
        let e1 = assignment("count", integer(0));
        assert_eq!(0, interpreter.interpret(e1).unwrap());
        // while (count < 3) {
        //    count = count + 1
        // }
        let e2 = r#while(
            less_than(symbol("count"), integer(3)),
            assignment("count", add(symbol("count"), integer(1))),
        );
        assert_eq!(1, interpreter.interpret(e2).unwrap());
    }

    #[test]
    fn test_factorial() {
        let top_levels = vec![
            define_function("main", vec![], block(vec![call("fact", vec![integer(5)])])),
            define_function(
                "fact",
                vec!["n".to_string()],
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
        assert_eq!(120, result.unwrap());
    }
}
