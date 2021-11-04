use combine::{
    attempt, chainl1, many,
    parser::char::{alpha_num, digit, spaces, string},
    Parser,
};

use crate::calculator::ast::*;

use super::ast::Program;

fn r#if<'a>() -> impl Parser<&'a str, Output = &'a str> {
    spaces().with(string("if").skip(spaces()))
}

fn r#else<'a>() -> impl Parser<&'a str, Output = &'a str> {
    spaces().with(string("else").skip(spaces()))
}

fn r#while<'a>() -> impl Parser<&'a str, Output = &'a str> {
    spaces().with(string("else").skip(spaces()))
}

fn plus<'a>() -> impl Parser<&'a str, Output = &'a str> {
    spaces().with(string("+").skip(spaces()))
}

fn minus<'a>() -> impl Parser<&'a str, Output = &'a str> {
    spaces().with(string("-").skip(spaces()))
}

fn aster<'a>() -> impl Parser<&'a str, Output = &'a str> {
    spaces().with(string("*").skip(spaces()))
}

fn slash<'a>() -> impl Parser<&'a str, Output = &'a str> {
    spaces().with(string("/").skip(spaces()))
}

fn lt<'a>() -> impl Parser<&'a str, Output = &'a str> {
    spaces().with(string("<").skip(spaces()))
}

fn lt_eq<'a>() -> impl Parser<&'a str, Output = &'a str> {
    spaces().with(string("<=").skip(spaces()))
}

fn gt<'a>() -> impl Parser<&'a str, Output = &'a str> {
    spaces().with(string(">").skip(spaces()))
}

fn gt_eq<'a>() -> impl Parser<&'a str, Output = &'a str> {
    spaces().with(string(">=").skip(spaces()))
}

fn eq_eq<'a>() -> impl Parser<&'a str, Output = &'a str> {
    spaces().with(string("==").skip(spaces()))
}

fn not_eq<'a>() -> impl Parser<&'a str, Output = &'a str> {
    spaces().with(string("!=").skip(spaces()))
}

fn global<'a>() -> impl Parser<&'a str, Output = &'a str> {
    spaces().with(string("global").skip(spaces()))
}

fn define<'a>() -> impl Parser<&'a str, Output = &'a str> {
    spaces().with(string("define").skip(spaces()))
}

fn comma<'a>() -> impl Parser<&'a str, Output = &'a str> {
    spaces().with(string(",").skip(spaces()))
}

fn lparen<'a>() -> impl Parser<&'a str, Output = &'a str> {
    spaces().with(string("(").skip(spaces()))
}

fn rparen<'a>() -> impl Parser<&'a str, Output = &'a str> {
    spaces().with(string(")").skip(spaces()))
}

fn lbrace<'a>() -> impl Parser<&'a str, Output = &'a str> {
    spaces().with(string("{").skip(spaces()))
}

fn rbrace<'a>() -> impl Parser<&'a str, Output = &'a str> {
    spaces().with(string("}").skip(spaces()))
}

fn semi_colon<'a>() -> impl Parser<&'a str, Output = &'a str> {
    spaces().with(string(";").skip(spaces()))
}

fn ident<'a>() -> impl Parser<&'a str, Output = String> {
    many(alpha_num()).skip(spaces())
}

fn identifier<'a>() -> impl Parser<&'a str, Output = Expression<'a>> {
    ident().map(|s| symbol(s))
}

fn integer<'a>() -> impl Parser<&'a str, Output = Expression<'a>> {
    many::<String, &'a str, _>(digit())
        .map(|d| crate::calculator::ast::integer(d.parse().unwrap()))
        .skip(spaces())
}

fn primary<'a>() -> impl Parser<&'a str, Output = Expression<'a>> {
    let s1 = attempt(lparen()).then(|_| expression().then(|v| rparen().map(move |_| v.clone())));
    let s2 = s1.or(integer());
    s2
}

fn multitive<'a>() -> impl Parser<&'a str, Output = Expression<'a>> {
    let mul = aster().map(|_| |l: Expression<'a>, r: Expression<'a>| multiply(l, r));
    let div = slash().map(|_| |l: Expression<'a>, r: Expression<'a>| divide(l, r));
    chainl1(primary(), mul).or(chainl1(primary(), div))
}

fn additive<'a>() -> impl Parser<&'a str, Output = Expression<'a>> {
    let add = plus().map(|_| |l: Expression<'a>, r: Expression<'a>| add(l, r));
    let sub = minus().map(|_| |l: Expression<'a>, r: Expression<'a>| subtract(l, r));
    chainl1(multitive(), add).or(chainl1(multitive(), sub))
}

fn comparative<'a>() -> impl Parser<&'a str, Output = Expression<'a>> {
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

fn expression<'a>() -> impl Parser<&'a str, Output = Expression<'a>> {
    comparative()
}

fn parse<'a>(source: &'a str) -> Program<'a> {
    let parser = expression();
    let parsed = parser.parse(source).unwrap();
    Program::new(Vec::new())
}
