use std::rc::Rc;

use crate::{
    evaluator::{default_env, eval},
    parser::parse,
    types::{Env, LispVal},
};

fn repl_evaluator<'a>(input: &'a str, old_env: Env) -> Result<(Vec<Rc<LispVal>>, Env), String> {
    let ast = parse(input)?;
    println!("{:?}", ast);
    let mut result = Vec::new();
    let mut env = old_env;
    for expr in ast {
        let (evaluated_expr, new_env) = eval(env, expr)?;
        result.push(evaluated_expr);
        env = new_env;
    }

    Ok((result, env))
}

pub fn repl() {
    print!("Welcome to the Rusty Lisp REPL!");
    let mut env = default_env();
    loop {
        let mut input = String::new();
        print!("> ");
        match std::io::stdin().read_line(&mut input) {
            Ok(_) => match repl_evaluator(&input, env.clone()) {
                Ok((result, new_env)) => {
                    let output = result
                        .iter()
                        .map(|v| format!("{}", v))
                        .collect::<Vec<String>>()
                        .join("\n");
                    println!("{}", output);
                    env = new_env;
                }
                Err(err) => println!("Error: {}", err),
            },
            Err(err) => println!("Error: {}", err),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{ LispVal};
    use std::rc::Rc;

    #[test]
    fn test_repl_evaluator() {
        let input = "(+ 1 2)";
        let expected = vec![Rc::new(LispVal::Number(3))];
        let env = default_env();
        let (result, _) = repl_evaluator(input, env).unwrap();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_number() {
        let input = "1";
        let expected = vec![Rc::new(LispVal::Number(1))];
        let env = default_env();
        let (result, _) = repl_evaluator(input, env).unwrap();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_if() {
        let input = "(if (#f) 1 2)";
        let expected = vec![Rc::new(LispVal::Number(2))];
        let env = default_env();
        let (result, _) = repl_evaluator(input, env).unwrap();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_let(){
        let input = "(let (x 1) (add x 1))";
        let expected = vec![Rc::new(LispVal::Number(2))];
        let env = default_env();
        let (result, _) = repl_evaluator(input, env).unwrap();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_lambda1(){
        let input = "(let (f (lambda x (add x x ))) (f 1))";
        let expected = vec![Rc::new(LispVal::Number(2))];
        let env = default_env();
        let (result, _) = repl_evaluator(input, env).unwrap();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_lambda2(){
        let input = "((lambda x (add x 1)) 1)";
        let expected = vec![Rc::new(LispVal::Number(2))];
        let env = default_env();
        let (result, _) = repl_evaluator(input, env).unwrap();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_define(){
        let input = "(define f (x) (add x x))";
        let expected = vec![Rc::new(LispVal::Number(4))];
        let env = default_env();
        let (_, env) = repl_evaluator(input, env).unwrap();
        let input2 = "(f 2)";
        let (result, _) = repl_evaluator(input2, env).unwrap();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_multiple_statements(){
        let input = "(define x (a) (add a a)) (add (x 1) 1)";
        let expected = Rc::new(LispVal::Number(3));
        let env = default_env();
        let (res, _) = repl_evaluator(input, env).unwrap();
        let v = res.last().unwrap();
        assert_eq!(v, &expected);
    }
}
