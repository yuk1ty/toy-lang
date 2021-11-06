use anyhow::Result;
use combine::{
    attempt, between, chainl1, choice, many, many1, optional, parser,
    parser::char::{alpha_num, digit, spaces, string},
    sep_by, ParseError, Parser, Stream, StreamOnce,
};

use crate::ast::*;

use super::ast::Program;

fn r#if<'a, Input>() -> impl Parser<Input, Output = &'a str>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    spaces().with(string("if").skip(spaces()))
}

fn r#else<'a, Input>() -> impl Parser<Input, Output = &'a str>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    spaces().with(string("else").skip(spaces()))
}

fn r#while<'a, Input>() -> impl Parser<Input, Output = &'a str>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    spaces().with(string("while").skip(spaces()))
}

fn plus<'a, Input>() -> impl Parser<Input, Output = &'a str>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    spaces().with(string("+").skip(spaces()))
}

fn minus<'a, Input>() -> impl Parser<Input, Output = &'a str>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    spaces().with(string("-").skip(spaces()))
}

fn aster<'a, Input>() -> impl Parser<Input, Output = &'a str>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    spaces().with(string("*").skip(spaces()))
}

fn slash<'a, Input>() -> impl Parser<Input, Output = &'a str>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    spaces().with(string("/").skip(spaces()))
}

fn lt<'a, Input>() -> impl Parser<Input, Output = &'a str>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    spaces().with(string("<").skip(spaces()))
}

fn lt_eq<'a, Input>() -> impl Parser<Input, Output = &'a str>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    spaces().with(string("<=").skip(spaces()))
}

fn gt<'a, Input>() -> impl Parser<Input, Output = &'a str>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    spaces().with(string(">").skip(spaces()))
}

fn gt_eq<'a, Input>() -> impl Parser<Input, Output = &'a str>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    spaces().with(string(">=").skip(spaces()))
}

fn eq_eq<'a, Input>() -> impl Parser<Input, Output = &'a str>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    spaces().with(string("==").skip(spaces()))
}

fn eq<'a, Input>() -> impl Parser<Input, Output = &'a str>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    spaces().with(string("=").skip(spaces()))
}

fn not_eq<'a, Input>() -> impl Parser<Input, Output = &'a str>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    spaces().with(string("!=").skip(spaces()))
}

fn global<'a, Input>() -> impl Parser<Input, Output = &'a str>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    spaces().with(string("global").skip(spaces()))
}

fn define<'a, Input>() -> impl Parser<Input, Output = &'a str>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    spaces().with(string("define").skip(spaces()))
}

fn comma<'a, Input>() -> impl Parser<Input, Output = &'a str>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    spaces().with(string(",").skip(spaces()))
}

fn lparen<'a, Input>() -> impl Parser<Input, Output = &'a str>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    spaces().with(string("(").skip(spaces()))
}

fn rparen<'a, Input>() -> impl Parser<Input, Output = &'a str>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    spaces().with(string(")").skip(spaces()))
}

fn lbrace<'a, Input>() -> impl Parser<Input, Output = &'a str>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    spaces().with(string("{").skip(spaces()))
}

fn rbrace<'a, Input>() -> impl Parser<Input, Output = &'a str>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    spaces().with(string("}").skip(spaces()))
}

fn semi_colon<'a, Input>() -> impl Parser<Input, Output = &'a str>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    spaces().with(string(";").skip(spaces()))
}

fn ident<Input>() -> impl Parser<Input, Output = String>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    many(alpha_num()).skip(spaces())
}

fn identifier<Input>() -> impl Parser<Input, Output = Expression>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    ident().map(symbol)
}

fn integer<Input>() -> impl Parser<Input, Output = Expression>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    many1(digit())
        .map(|d: String| crate::ast::integer(d.parse().unwrap()))
        .skip(spaces())
}

fn primary<Input>() -> impl Parser<Input, Output = Expression>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    choice! {
        attempt(lparen()).with(expression()).skip(rparen()),
        integer(),
        function_call(),
        identifier()
    }
}

fn multitive<Input>() -> impl Parser<Input, Output = Expression>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    let op = aster().or(slash()).map(|s| match s {
        "*" => |l: Expression, r: Expression| multiply(l, r),
        "/" => |l: Expression, r: Expression| divide(l, r),
        _ => unreachable!(),
    });
    chainl1(primary(), op)
}

fn additive<Input>() -> impl Parser<Input, Output = Expression>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    let op = plus().or(minus()).map(|s| match s {
        "+" => |l: Expression, r: Expression| add(l, r),
        "-" => |l: Expression, r: Expression| subtract(l, r),
        _ => unreachable!(),
    });
    chainl1(multitive(), op)
}

fn comparative<Input>() -> impl Parser<Input, Output = Expression>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    let tokens = choice! {
        attempt(lt_eq()),
        attempt(gt_eq()),
        attempt(eq_eq()),
        attempt(not_eq()),
        attempt(lt()),
        attempt(gt())
    };

    let op = attempt(tokens.map(|s| match s {
        "<" => |l: Expression, r: Expression| less_than(l, r),
        ">" => |l: Expression, r: Expression| greater_than(l, r),
        "<=" => |l: Expression, r: Expression| less_than_equal(l, r),
        ">=" => |l: Expression, r: Expression| greater_than_equal(l, r),
        "==" => |l: Expression, r: Expression| equal_equal(l, r),
        "!=" => |l: Expression, r: Expression| not_equal(l, r),
        _ => unreachable!(),
    }));

    chainl1(additive(), op)
}

parser! {
    fn expression[Input]()(Input) -> Expression where [ Input: Stream<Token = char> ] {
        comparative()
    }
}

parser! {
    fn line[Input]()(Input) -> Expression where [ Input: Stream<Token = char> ] {
        choice! {
            while_expression(),
            if_expression(),
            assignment(),
            expression_line(),
            block_expression()
        }
    }
}

fn expression_line<Input>() -> impl Parser<Input, Output = Expression>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    attempt(expression()).skip(semi_colon())
}

fn block_expression<Input>() -> impl Parser<Input, Output = Expression>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    lbrace().with(many(line())).skip(rbrace()).map(block)
}

fn assignment<Input>() -> impl Parser<Input, Output = Expression>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    attempt(ident().then(|name| {
        eq().with(expression()).then(move |expr| {
            // bit technically. :thinking_face:
            let name = name.to_string();
            semi_colon().map(move |_| crate::ast::assignment(name.to_string(), expr.clone()))
        })
    }))
}

fn if_expression<Input>() -> impl Parser<Input, Output = Expression>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    let condition = r#if().with(between(lparen(), rparen(), expression()));
    attempt(condition.then(|c| {
        line().then(move |then_clause| {
            let c = c.clone();
            optional(r#else().with(line())).map(move |else_clause| {
                crate::ast::r#if(c.clone(), then_clause.clone(), else_clause)
            })
        })
    }))
}

fn while_expression<Input>() -> impl Parser<Input, Output = Expression>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    let condition = r#while().with(between(lparen(), rparen(), expression()));
    attempt(condition.then(|c| line().map(move |body| crate::ast::r#while(c.clone(), body))))
}

fn function_call<Input>() -> impl Parser<Input, Output = Expression>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    attempt(ident().then(|name| {
        between(
            lparen(),
            rparen(),
            sep_by(expression().map(|expr| expr.extract_params()), comma()),
        )
        .map(move |exprs: Expressions| call(name.clone(), exprs.0))
    }))
}

fn program<Input>() -> impl Parser<Input, Output = Program>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    spaces()
        .with(many(top_level_definition()))
        .map(Program::new)
}

fn top_level_definition<Input>() -> impl Parser<Input, Output = TopLevel>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    choice! {
        global_variable_definition(),
        function_definition()
    }
}

fn function_definition<Input>() -> impl Parser<Input, Output = TopLevel>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    let def_name = define().with(ident());
    def_name.then(|name| {
        between(lparen(), rparen(), sep_by(ident(), comma())).then(move |args: Vec<String>| {
            let name = name.clone();
            block_expression().map(move |body| define_function(name.clone(), args.clone(), body))
        })
    })
}

fn global_variable_definition<Input>() -> impl Parser<Input, Output = TopLevel>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    global().with(ident()).then(|name| {
        eq().with(expression())
            .skip(semi_colon())
            .map(move |expression| global_definition(&name, expression))
    })
}

pub fn parse(source: &str) -> Result<Program> {
    let mut parser = program();
    let parsed = parser.parse(source)?;

    if !parsed.1.is_empty() {
        dbg!(
            "Parsing result is still remaning. Please check your input.",
            &parsed
        );
    }

    Ok(parsed.0)
}

#[cfg(test)]
mod test {
    use combine::Parser;
    use test_case::test_case;

    use crate::{
        ast::{
            add, divide, equal_equal, greater_than, greater_than_equal, integer, less_than,
            less_than_equal, multiply, not_equal, subtract, symbol, Expression, TopLevel,
        },
        parser::{comparative, multitive},
    };

    use super::{additive, assignment, block_expression, if_expression, while_expression};

    #[test]
    fn test_add() {
        let mut parser = additive();
        let actual = parser.parse("1 + 2");
        assert_eq!((add(integer(1), integer(2)), ""), actual.unwrap());
    }

    #[test]
    fn test_sub() {
        let mut parser = additive();
        let actual = parser.parse("2 - 1");
        assert_eq!((subtract(integer(2), integer(1)), ""), actual.unwrap());
    }

    #[test]
    fn test_mul() {
        let mut parser = multitive();
        let actual = parser.parse("2 * 1");
        assert_eq!((multiply(integer(2), integer(1)), ""), actual.unwrap());
    }

    #[test]
    fn test_div() {
        let mut parser = multitive();
        let actual = parser.parse("2 / 1");
        assert_eq!((divide(integer(2), integer(1)), ""), actual.unwrap());
    }

    #[test]
    fn test_add_many_times() {
        let mut parser = additive();
        let actual = parser.parse("1 + 2 + 1 + 2");
        assert_eq!("", actual.unwrap().1);
    }

    #[test]
    fn test_sub_many_times() {
        let mut parser = additive();
        let actual = parser.parse("3 - 1 - 1 - 1");
        assert_eq!("", actual.unwrap().1);
    }

    #[test]
    fn test_mul_many_times() {
        let mut parser = multitive();
        let actual = parser.parse("2 * 1 * 2 * 1");
        assert_eq!("", actual.unwrap().1);
    }

    #[test]
    fn test_div_many_times() {
        let mut parser = multitive();
        let actual = parser.parse("2 / 1 / 1 / 1");
        assert_eq!("", actual.unwrap().1);
    }

    #[test]
    fn test_less_than() {
        let mut parser = comparative();
        let actual = parser.parse("1 < 2");
        assert_eq!((less_than(integer(1), integer(2)), ""), actual.unwrap());
    }

    #[test]
    fn test_greater_than() {
        let mut parser = comparative();
        let actual = parser.parse("2 > 1");
        assert_eq!((greater_than(integer(2), integer(1)), ""), actual.unwrap());
    }

    #[test]
    fn test_less_than_equal() {
        let mut parser = comparative();
        let actual = parser.parse("1 <= 2");
        assert_eq!(
            (less_than_equal(integer(1), integer(2)), ""),
            actual.unwrap()
        );
    }

    #[test]
    fn test_greater_than_equal() {
        let mut parser = comparative();
        let actual = parser.parse("2 >= 1");
        assert_eq!(
            (greater_than_equal(integer(2), integer(1)), ""),
            actual.unwrap()
        );
    }

    #[test]
    fn test_equal_equal() {
        let mut parser = comparative();
        let actual = parser.parse("1 == 1");
        assert_eq!((equal_equal(integer(1), integer(1)), ""), actual.unwrap());
    }

    #[test]
    fn test_not_equal() {
        let mut parser = comparative();
        let actual = parser.parse("2 != 1");
        assert_eq!((not_equal(integer(2), integer(1)), ""), actual.unwrap());
    }

    #[test]
    fn test_assignment() {
        let mut parser = assignment();
        let actual = parser.parse("a = 1 + 2;");
        assert_eq!(
            (crate::ast::assignment("a", add(integer(1), integer(2))), ""),
            actual.unwrap()
        );
    }

    #[test]
    fn test_line() {
        let mut parser = super::line();
        let actual = parser.parse("i;");
        assert_eq!(actual.unwrap().0, symbol("i"));
    }

    #[test]
    fn test_block() {
        let mut parser = block_expression();
        let actual = parser.parse(
            r#"
        {
            i;
            i = 1;
            a = i * 2;
            b = a > i;
        }"#,
        );
        assert!(matches!(
            actual.unwrap(),
            (Expression::BlockExpression(_), "")
        ));
    }

    #[test]
    fn test_if_expression_without_else() {
        let mut parser = if_expression();
        let actual = parser.parse(
            r#"
            if (i == 0) {
                i = i + 1;
            }
        "#,
        );
        assert!(matches!(
            actual.unwrap(),
            (
                Expression::IfExpression {
                    condition: _,
                    then_clause: _,
                    else_clause: None
                },
                ""
            )
        ));
    }

    #[test]
    fn test_if_expression_with_else() {
        let mut parser = if_expression();
        let actual = parser.parse(
            r#"
            if (i == 0) {
                i = i + 1;
            } else {
                i = i + 2;
            }
        "#,
        );
        assert!(matches!(
            actual.unwrap(),
            (
                Expression::IfExpression {
                    condition: _,
                    then_clause: _,
                    else_clause: Some(_)
                },
                ""
            )
        ));
    }

    #[test]
    fn test_while() {
        let mut parser = while_expression();
        let actual = parser.parse(
            r#"
            while (i < 10) {
                i = i + 1;
            }
        "#,
        );
        assert!(matches!(
            actual.unwrap(),
            (
                Expression::WhileExpression {
                    condition: _,
                    body: _
                },
                ""
            )
        ));
    }

    #[test_case("add(1, 2, 3)"; "func_call_with_3_args")]
    #[test_case("add(a, b)"; "func_call_with_identifiers")]
    #[test_case("add(1 * 2, 3 - 2)"; "func_call_with_binary_operators")]
    #[test_case("add(add(1, 2), 3)"; "func_call_with_piling_function")]
    fn test_function_call<'a>(source: &'a str) {
        let mut parser = crate::parser::function_call();
        let actual = parser.parse(source);
        assert!(matches!(
            actual.unwrap(),
            (Expression::FunctionCall { name: _, args: _ }, "")
        ));
    }

    #[test_case("add(while (i > 0) { i; })" => panics "called `Result::unwrap()` on an `Err` value: UnexpectedParse")]
    #[test_case("add(if (n > 1) { 1; } else { 0; })" => panics "called `Result::unwrap()` on an `Err` value: UnexpectedParse")]
    #[test_case("add(n = 1)" => panics "called `Result::unwrap()` on an `Err` value: UnexpectedParse")]
    fn test_function_call_disability<'a>(source: &'a str) {
        let mut parser = crate::parser::function_call();
        let actual = parser.parse(source);
        actual.unwrap();
    }

    #[test]
    fn test_function_definition() {
        let mut parser = super::function_definition();
        let actual = parser.parse(
            r#"
        define hello(a, b) { 
            result = a + b; 
        }"#,
        );
        assert!(matches!(
            actual.unwrap(),
            (
                TopLevel::FunctionDefinition {
                    name: _,
                    args: _,
                    body: _
                },
                ""
            )
        ));
    }

    #[test]
    fn test_program() {
        let mut parser = super::program();
        let actual = parser.parse(
            r#"
            define factorial(n) {
                if (n < 2) {
                    1;
                } else {
                    n * factorial(n - 1);
                }
            }
            define main() {
                factorial(5);
            }
        "#,
        );
        let result = actual.unwrap();
        assert!(matches!(result.1, ""));
    }
}
