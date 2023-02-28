use std::rc::Rc;

use nom::branch::alt;
use nom::bytes::complete::is_not;
use nom::bytes::complete::tag;
use nom::character::complete::char;
use nom::character::complete::multispace0;
use nom::character::complete::multispace1;
use nom::combinator::map_res;
use nom::error::ErrorKind;
use nom::error::ParseError;
use nom::error_position;
use nom::multi::separated_list0;
use nom::sequence::delimited;
use nom::Err;
use nom::IResult;
use nom::Parser;
use nom::sequence::preceded;

use crate::types::LispVal;

fn parse_atom(input: &str) -> IResult<&str, Rc<LispVal>> {
    let re =
        regex::Regex::new(r"^([\-!#$%&*+./:<=>?@^_~a-zA-Z][!#$%&*+./:<=>?@^_~a-zA-Z0-9]*)").unwrap();
    let captures = re.captures(input);
    match captures {
        Some(captures) => {
            let atom = captures.get(1).unwrap().as_str();
            let rest = &input[atom.len()..];
            Ok((rest, Rc::new(LispVal::Atom(atom.to_string()))))
        }
        None => Err(Err::Error(error_position!(input, ErrorKind::AlphaNumeric))),
    }
}

fn parse_number(input: &str) -> IResult<&str, Rc<LispVal>> {
    let re = regex::Regex::new(r"^(-?[0-9]+)").unwrap();
    let captures = re.captures(input);

    match captures {
        Some(captures) => {
            let number = captures.get(1).unwrap().as_str();
            let rest = &input[number.len()..];
            let number = number.parse::<i32>().unwrap();
            Ok((rest, Rc::new(LispVal::Number(number))))
        }
        None => Err(Err::Error(error_position!(input, ErrorKind::Digit))),
    }
}

fn parse_string(input: &str) -> IResult<&str, Rc<LispVal>> {
    map_res(
        delimited(char('\"'), is_not("\""), char('\"')),
        |s: &str| -> Result<Rc<LispVal>, Err<&str>> {
            Ok(Rc::new(LispVal::String(s.to_string())))
        },
    )
    .parse(input)
}

fn parse_lispVal(input: &str) -> IResult<&str, Rc<LispVal>> {
    preceded(multispace0, alt((parse_reserved, parse_quote, parse_number, parse_atom, parse_list, parse_reserved, parse_string)))(input)
}

fn parse_list(input: &str) -> IResult<&str, Rc<LispVal>> {
    map_res(
        delimited(
            preceded(multispace0,char('(')),
            parse_lisp,
            preceded(multispace0,char(')')),
        ),
        |v: Vec<Rc<LispVal>>| -> Result<Rc<LispVal>, Err<&str>> {
            Ok(Rc::new(LispVal::List(v)))
        },
    )
    .parse(input)
}

fn parse_reserved(input: &str) -> IResult<&str, Rc<LispVal>> {
    alt((
        map_res(tag("Nil"), |_| -> Result<Rc<LispVal>, Err<&str>> {
            Ok(Rc::new(LispVal::Nil))
        }),
        map_res(tag("#t"), |_| -> Result<Rc<LispVal>, Err<&str>> {
            Ok(Rc::new(LispVal::Bool(true)))
        }),
        map_res(tag("#f"), |_| -> Result<Rc<LispVal>, Err<&str>> {
            Ok(Rc::new(LispVal::Bool(false)))
        }),
    ))(input)
}

fn parse_quote(input: &str) -> IResult<&str, Rc<LispVal>> {
    map_res(
        preceded(
            char('\''),
            parse_lispVal,
        ),
        |v: Rc<LispVal>| -> Result<Rc<LispVal>, Err<&str>> {
            Ok(Rc::new(LispVal::List(vec![Rc::new(LispVal::Atom("quote".to_string())), v])))
        },
    )
    .parse(input)
}

fn parse_lisp(input: &str) -> IResult<&str, Vec<Rc<LispVal>>> {
    separated_list0(multispace1, parse_lispVal)(input)
}

pub fn parse(input: &str) -> Result<Vec<Rc<LispVal>>, String> {
    match parse_lisp(input){
        Ok((_, v)) => Ok(v),
        Err(e) => Err(format!("Parsing error: {:?}", e)),
    }
}

// Add testing
#[cfg(test)]
mod tests{
    use std::rc::Rc;

    #[test]
    fn parse_negative_number() {
        let input = "-123";
        let expected = -123;
        let result = super::parse_number(input);
        match result {
            Ok((rest, number)) => {
                assert_eq!(rest, "");
                assert_eq!(number, Rc::new(super::LispVal::Number(expected)));
            }
            Err(e) => {
                panic!("Error: {:?}", e);
            }
        }
    }

    #[test]
    fn test_parse_atom() {
        let input = "abc";
        let expected = "abc";
        let result = super::parse_atom(input);
        match result {
            Ok((rest, atom)) => {
                assert_eq!(rest, "");
                assert_eq!(atom, Rc::new(super::LispVal::Atom(expected.to_string())));
            }
            Err(e) => {
                panic!("Error: {:?}", e);
            }
        }
    }

    #[test]
    fn test_parse_number() {
        let input = "123";
        let expected = 123;
        let result = super::parse_number(input);
        match result {
            Ok((rest, number)) => {
                assert_eq!(rest, "");
                assert_eq!(number, Rc::new(super::LispVal::Number(expected)));
            }
            Err(e) => {
                panic!("Error: {:?}", e);
            }
        }
    }

    #[test]
    fn test_parse_string() {
        let input = "\"abc\"";
        let expected = "abc";
        let result = super::parse_string(input);
        match result {
            Ok((rest, string)) => {
                assert_eq!(rest, "");
                assert_eq!(string, Rc::new(super::LispVal::String(expected.to_string())));
            }
            Err(e) => {
                panic!("Error: {:?}", e);
            }
        }
    }

    #[test]
    fn test_parse_list() {
        let input = "(abc 123 \"abc\")";
        let expected = vec![
            Rc::new(super::LispVal::Atom("abc".to_string())),
            Rc::new(super::LispVal::Number(123)),
            Rc::new(super::LispVal::String("abc".to_string())),
        ];
        let result = super::parse_list(input);
        match result {
            Ok((rest, list)) => {
                assert_eq!(rest, "");
                assert_eq!(list, Rc::new(super::LispVal::List(expected)));
            }
            Err(e) => {
                panic!("Error: {:?}", e);
            }
        }
    }

    #[test]
    fn test_parse_reserved() {
        let input = "Nil";
        let expected = super::LispVal::Nil;
        let result = super::parse_reserved(input);
        match result {
            Ok((rest, reserved)) => {
                assert_eq!(rest, "");
                assert_eq!(reserved, Rc::new(expected));
            }
            Err(e) => {
                panic!("Error: {:?}", e);
            }
        }
    }

    #[test]
    fn test_parse_true() {
        let input = "#t";
        let expected = super::LispVal::Bool(true);
        let result = super::parse_reserved(input);
        match result {
            Ok((rest, reserved)) => {
                assert_eq!(rest, "");
                assert_eq!(reserved, Rc::new(expected));
            }
            Err(e) => {
                panic!("Error: {:?}", e);
            }
        }
    }

    #[test]
    fn test_parse_false() {
        let input = "#f";
        let expected = super::LispVal::Bool(false);
        let result = super::parse_reserved(input);
        match result {
            Ok((rest, reserved)) => {
                assert_eq!(rest, "");
                assert_eq!(reserved, Rc::new(expected));
            }
            Err(e) => {
                panic!("Error: {:?}", e);
            }
        }
    }

    #[test]
    fn test_parse_quote() {
        let input = "'abc";
        let expected = vec![
            Rc::new(super::LispVal::Atom("quote".to_string())),
            Rc::new(super::LispVal::Atom("abc".to_string())),
        ];
        let result = super::parse_quote(input);
        match result {
            Ok((rest, quote)) => {
                assert_eq!(rest, "");
                assert_eq!(quote, Rc::new(super::LispVal::List(expected)));
            }
            Err(e) => {
                panic!("Error: {:?}", e);
            }
        }
    }

    #[test]
    fn test_parse() {
        let input = "abc 123 \"abc\" (abc 123 \"abc\") Nil #t #f 'abc";
        let expected = vec![
            Rc::new(super::LispVal::Atom("abc".to_string())),
            Rc::new(super::LispVal::Number(123)),
            Rc::new(super::LispVal::String("abc".to_string())),
            Rc::new(super::LispVal::List(vec![
                Rc::new(super::LispVal::Atom("abc".to_string())),
                Rc::new(super::LispVal::Number(123)),
                Rc::new(super::LispVal::String("abc".to_string())),
            ])),
            Rc::new(super::LispVal::Nil),
            Rc::new(super::LispVal::Bool(true)),
            Rc::new(super::LispVal::Bool(false)),
            Rc::new(super::LispVal::List(vec![
                Rc::new(super::LispVal::Atom("quote".to_string())),
                Rc::new(super::LispVal::Atom("abc".to_string())),
            ])),
        ];
        let result = super::parse_lisp(input);
        match result {
            Ok((rest, parsed)) => {
                assert_eq!(parsed, expected);
                assert_eq!(rest, "");
            }
            Err(e) => {
                panic!("Error: {:?}", e);
            }
        }
    }

    #[test]
    fn test_parse_math() {
        let input = "(+ 1 2)";
        let expected = vec![
            Rc::new(super::LispVal::Atom("+".to_string())),
            Rc::new(super::LispVal::Number(1)),
            Rc::new(super::LispVal::Number(2)),
        ];
        let result = super::parse_list(input);
        match result {
            Ok((rest, list)) => {
                assert_eq!(rest, "");
                assert_eq!(list, Rc::new(super::LispVal::List(expected)));
            }
            Err(e) => {
                panic!("Error: {:?}", e);
            }
        }
    }

    #[test]
    fn test_parse_more_math(){
        let input = "(+ (* 2 3) (- 5 -1))";
        let expected = vec![
            Rc::new(super::LispVal::Atom("+".to_string())),
            Rc::new(super::LispVal::List(vec![
                Rc::new(super::LispVal::Atom("*".to_string())),
                Rc::new(super::LispVal::Number(2)),
                Rc::new(super::LispVal::Number(3)),
            ])),
            Rc::new(super::LispVal::List(vec![
                Rc::new(super::LispVal::Atom("-".to_string())),
                Rc::new(super::LispVal::Number(5)),
                Rc::new(super::LispVal::Number(-1)),
            ])),
        ];
        let result = super::parse_list(input);
        match result {
            Ok((rest, list)) => {
                assert_eq!(rest, "");
                assert_eq!(list, Rc::new(super::LispVal::List(expected)));
            }
            Err(e) => {
                panic!("Error: {:?}", e);
            }
        }
    }

    #[test]
    fn test_nested_let_lambda(){
        let input = "(let (f (lambda x (add x x ))) (f 1))";
        let expected = vec![
            Rc::new(super::LispVal::Atom("let".to_string())),
            Rc::new(super::LispVal::List(vec![
                Rc::new(super::LispVal::Atom("f".to_string())),
                Rc::new(super::LispVal::List(vec![
                    Rc::new(super::LispVal::Atom("lambda".to_string())),
                    Rc::new(super::LispVal::Atom("x".to_string())),
                    Rc::new(super::LispVal::List(vec![
                        Rc::new(super::LispVal::Atom("add".to_string())),
                        Rc::new(super::LispVal::Atom("x".to_string())),
                        Rc::new(super::LispVal::Atom("x".to_string())),
                    ])),
                ])),
            ])),
            Rc::new(super::LispVal::List(vec![
                Rc::new(super::LispVal::Atom("f".to_string())),
                Rc::new(super::LispVal::Number(1)),
            ])),
        ];
        let result = super::parse_list(input);
        match result {
            Ok((rest, list)) => {
                assert_eq!(rest, "");
                assert_eq!(list, Rc::new(super::LispVal::List(expected)));
            }
            Err(e) => {
                panic!("Error: {:?}", e);
            }
        }
    }

    #[test]
    fn parse_expressions(){
        let exprs = vec![
            "(+ 1 2)",
            "1",
            "(if (#f) 1 2)",
            "((lambda x (add x 1)) 1)",
            "(define f (x) (add x x))",
            "(f 2)",
            "(define x (a) (add a a)) (add (x 1) 1)",
            "(let (x 1) (add x 1))",
            "(let (f (lambda x (add x x))) (f 1))",
            "(let (f (lambda x (add x x ))) (f 1))",
            "(  let ( f (   lambda        x   (    add   x  x     )))      ( f   1   )  )",
            "(let (f (lambda x (add x x))) (f 1))",
        ];
        for expr in exprs {
            let result = super::parse_lisp(expr);
            match result {
                Ok((rest, parsed)) => {
                    assert_eq!(rest, "");
                }
                Err(e) => {
                    panic!("Error: {:?}", e);
                }
            }
        }
    }
}
