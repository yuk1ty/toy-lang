use combine::{
    many,
    parser::char::{alpha_num, digit, spaces, string},
    Parser,
};

use crate::calculator::ast::*;

use super::ast::Program;

fn parse<'a>(source: &'a str) -> Program<'a> {
    let mut r#if = spaces().with(string::<'a, &str>("if").skip(spaces()));
    let mut r#else = spaces().with(string::<'a, &str>("else").skip(spaces()));
    let mut r#while = spaces().with(string::<'a, &str>("while").skip(spaces()));
    let mut plus = spaces().with(string::<'a, &str>("+").skip(spaces()));
    let mut minus = spaces().with(string::<'a, &str>("-").skip(spaces()));
    let mut aster = spaces().with(string::<'a, &str>("*").skip(spaces()));
    let mut slash = spaces().with(string::<'a, &str>("/").skip(spaces()));
    let mut lt = spaces().with(string::<'a, &str>("<").skip(spaces()));
    let mut lt_eq = spaces().with(string::<'a, &str>("<=").skip(spaces()));
    let mut gt = spaces().with(string::<'a, &str>(">").skip(spaces()));
    let mut gt_eq = spaces().with(string::<'a, &str>(">=").skip(spaces()));
    let mut eq_eq = spaces().with(string::<'a, &str>("==").skip(spaces()));
    let mut not_eq = spaces().with(string::<'a, &str>("!=").skip(spaces()));
    let mut eq = spaces().with(string::<'a, &str>("=").skip(spaces()));
    let mut global = spaces().with(string::<'a, &str>("global").skip(spaces()));
    let mut define = spaces().with(string::<'a, &str>("define").skip(spaces()));
    let mut comma = spaces().with(string::<'a, &str>(",").skip(spaces()));
    let mut lparen = spaces().with(string::<'a, &str>("(").skip(spaces()));
    let mut rparen = spaces().with(string::<'a, &str>(")").skip(spaces()));
    let mut lbrace = spaces().with(string::<'a, &str>("{").skip(spaces()));
    let mut rbrace = spaces().with(string::<'a, &str>("}").skip(spaces()));
    let mut semi_colon = spaces().with(string::<'a, &str>(";").skip(spaces()));
    let mut ident = many::<String, &'a str, _>(alpha_num()).skip(spaces());
    let mut integer = many::<String, &'a str, _>(digit())
        .map(|d| integer(d.parse().unwrap()))
        .skip(spaces());

    let mut identifier = || ident.map(|s| symbol(s));

    Program::new(Vec::new())
}
