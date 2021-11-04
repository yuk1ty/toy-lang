use combine::{
    attempt, chainl1, many, many1, parser,
    parser::char::{alpha_num, digit, spaces, string},
    ParseError, Parser, Stream, StreamOnce,
};

use crate::calculator::ast::*;

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

fn r#while<'a>() -> impl Parser<&'a str, Output = &'a str> {
    spaces().with(string("else").skip(spaces()))
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

fn ident<'a, Input>() -> impl Parser<Input, Output = String>
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

fn identifier<'a, Input>() -> impl Parser<Input, Output = Expression<'a>>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    ident().map(|s| symbol(s))
}

fn integer<'a, Input>() -> impl Parser<Input, Output = Expression<'a>>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    many1(digit())
        .map(|d: String| crate::calculator::ast::integer(d.parse().unwrap()))
        .skip(spaces())
}

fn primary<'a, Input>() -> impl Parser<Input, Output = Expression<'a>>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    let s1 = attempt(lparen()).then(|_| expression().then(|v| rparen().map(move |_| v.clone())));
    let s2 = s1.or(integer());
    s2
}

fn multitive<'a, Input>() -> impl Parser<Input, Output = Expression<'a>>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    let mul = aster().map(|_| |l: Expression<'a>, r: Expression<'a>| multiply(l, r));
    let div = slash().map(|_| |l: Expression<'a>, r: Expression<'a>| divide(l, r));
    chainl1(primary(), mul).or(chainl1(primary(), div))
}

fn additive<'a, Input>() -> impl Parser<Input, Output = Expression<'a>>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    let add = plus().map(|_| |l: Expression<'a>, r: Expression<'a>| add(l, r));
    let sub = minus().map(|_| |l: Expression<'a>, r: Expression<'a>| subtract(l, r));
    chainl1(multitive(), add).or(chainl1(multitive(), sub))
}

fn comparative<'a, Input>() -> impl Parser<Input, Output = Expression<'a>>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    let lt = attempt(lt()).map(|_| |l: Expression<'a>, r: Expression<'a>| less_than(l, r));
    let gt = attempt(gt()).map(|_| |l: Expression<'a>, r: Expression<'a>| greater_than(l, r));
    let lte =
        attempt(lt_eq()).map(|_| |l: Expression<'a>, r: Expression<'a>| less_than_equal(l, r));
    let gte =
        attempt(gt_eq()).map(|_| |l: Expression<'a>, r: Expression<'a>| greater_than_equal(l, r));
    let eq = attempt(eq_eq()).map(|_| |l: Expression<'a>, r: Expression<'a>| equal_equal(l, r));
    let neq = attempt(not_eq()).map(|_| |l: Expression<'a>, r: Expression<'a>| not_equal(l, r));
    chainl1(additive(), lt)
        .or(chainl1(additive(), gt))
        .or(chainl1(additive(), lte))
        .or(chainl1(additive(), gte))
        .or(chainl1(additive(), eq))
        .or(chainl1(additive(), neq))
}

fn expression_<'a, Input>() -> impl Parser<Input, Output = Expression<'a>>
where
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    comparative()
}

parser! {
    fn expression['a, Input]()(Input) -> Expression<'a> where [ Input: Stream<Token = char> ] {
        expression_()
    }
}

fn parse<'a>(source: &'a str) -> Program<'a> {
    let mut parser = expression();
    let parsed = parser.parse(source).unwrap();
    Program::new(Vec::new())
}
