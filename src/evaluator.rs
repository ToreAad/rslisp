use std::{rc::Rc};

use crate::types::{LispVal, Env, LambdaDef};

pub fn default_env() -> Env {
    let mut env = Env::new();
    
    let add = LispVal::Prim(|args| {
        let mut sum = 0;
        for arg in args{
            match arg.as_ref(){
                LispVal::Number(i) => sum += i,
                _ => return Err(format!("Expression is not a Number: {:?}", arg))
            }
        }
        Ok(Rc::new(LispVal::Number(sum)))
    });

    env.insert("+".to_string(), Rc::new(add));

    let add_lambda = LispVal::Lambda(LambdaDef{
        params: vec!("a".to_string(), "b".to_string()),
        body: Rc::new(LispVal::List(vec![Rc::new(LispVal::Atom("+".to_string())), Rc::new(LispVal::Atom("a".to_string())), Rc::new(LispVal::Atom("b".to_string()))])),
        closure: env.clone(),
    });
    env.insert("add".to_string(), Rc::new(add_lambda));
    env
}

pub fn eval(env: Env, exp: Rc<LispVal>) -> Result<(Rc<LispVal>, Env), String>{
    match exp.as_ref() {
        LispVal::Atom(atom) => {
            match env.get(atom) {
                Some(val) => Ok((Rc::clone(val), env)),
                None => Err(format!("Unbound variable: {}", atom)),
            }
        },
        LispVal::Number(_) => Ok((exp, env)),
        LispVal::String(_) => Ok((exp, env)),
        LispVal::Bool(_) => Ok((exp, env)),
        LispVal::Nil => Ok((exp, env)),
        LispVal::List(vals) =>{
            match &vals[..]{
                [_if, pred, consequence, alternative] if possible_atom_is(&_if, "if") =>{
                    let (b, env) = get_bool(env, Rc::clone(pred))?;
                    if b {
                        eval(env, Rc::clone(consequence))
                    } else{
                        eval(env, Rc::clone(alternative))
                    }

                },
                [_let, pairs, expr] if  possible_atom_is(_let, "let") => {
                    match pairs.as_ref(){
                        LispVal::List(ls) =>{
                            let (symbols, vals) = symbol_expr_pairs(ls)?;
                            let new_pairs = vals.iter().map(|v| eval(env.clone(), Rc::clone(v))).collect::<Result<Vec<_>, _>>()?;
                            let new_vals = new_pairs.iter().map(|(v, _)| Rc::clone(v)).collect::<Vec<_>>();
                            let new_env = bind(&env, &symbols, &new_vals)?;
                            match eval(new_env, Rc::clone(expr)){
                                Ok((lval, _)) => Ok((lval, env)),
                                Err(e) => Err(e)
                            }
                        }
                        _ => Err(format!("Expression is not a list: {:?}", exp))
                    }
                },
                [_define, atom, params, expr] if possible_atom_is(_define, "define") => {
                    let params = match params.as_ref(){
                        LispVal::List(ls) => {
                            ls.iter().map(|p| get_atom(p).ok_or_else(|| format!("Unrecognized expression: {:?}", exp))).collect::<Result<Vec<_>, _>>()
                        },
                        LispVal::Atom(atom) => Ok(vec!(atom.clone())),
                        _ => Err(format!("Expression is not a list or an atom: {:?}", exp))
                    }?;

                    let func = get_lambda(Rc::clone(expr), params, &env)?;
                    let atom = get_atom(atom).unwrap();
                    let new_env = bind(&env, &vec![atom], &vec![Rc::clone(&func)])?;
                    Ok((func, new_env))
                },
                [_lambda, params, expr] if possible_atom_is(_lambda, "lambda") => {
                    
                    let params = match params.as_ref(){
                        LispVal::List(ls) => {
                            ls.iter().map(|p| get_atom(p).ok_or_else(|| format!("Unrecognized expression: {:?}", exp))).collect::<Result<Vec<_>, _>>()
                        },
                        LispVal::Atom(atom) => Ok(vec!(atom.clone())),
                        _ => Err(format!("Expression is not a list or an atom: {:?}", exp))
                    }?;

                    let func = get_lambda(Rc::clone(expr), params, &env)?;
                    Ok((func, env))
                    

                },
                [expr] => eval(env, Rc::clone(expr)),
                [f, args @ ..] => { 
                    let (func, new_env) = eval(env, f.clone())?;
                    let (args, last_env) = eval_list(new_env, args.to_vec())?;
                    let res = apply(func, args)?;
                    Ok((res, last_env))
                }
                [] => Ok((exp, env)),
            }
        }
        _ => Err(format!("Unrecognized expression: {:?}", exp)),

    }
}

fn possible_atom_is(expr: &Rc<LispVal>, target: &str) -> bool {
    match expr.as_ref() {
        LispVal::Atom(atom) => atom == target,
        _ => false,
    }
}

fn get_lambda(expr: Rc<LispVal>, params: Vec<String>, env: &Env) -> Result<Rc<LispVal>, String> {
    Ok(Rc::new(LispVal::Lambda(LambdaDef{
        params: params,
        body: expr,
        closure: env.clone(),
    })))
}

fn apply(func: Rc<LispVal>, args: Vec<Rc<LispVal>>) -> Result<Rc<LispVal>, String> {
    match func.as_ref(){
        LispVal::Prim(prim) => prim(args),
        LispVal::Lambda(LambdaDef{params, body, closure}) => {
            let new_env = bind(closure, params, &args)?;
            let (exp, _) = eval(new_env, Rc::clone(body))?;
            Ok(exp)
        },
        _ => Err(format!("Expression is not applyable: {:?}", func)),
    }
}

fn eval_list(new_env: Env, to_vec: Vec<Rc<LispVal>>) -> Result<(Vec<Rc<LispVal>>, Env), String> {
    let mut evaluated = Vec::new();
    let mut current_env: Env = new_env;
    for expr in to_vec{
        let (evaluated_exp, temp_env ) = eval(current_env, expr)?;
        current_env = temp_env;
        evaluated.push(evaluated_exp);
    }
    Ok((evaluated, current_env))
}


fn get_atom(expr: &Rc<LispVal>) -> Option<String> {
    match expr.as_ref() {
        LispVal::Atom(atom) => Some(atom.clone()),
        _ => None,
    }
}
fn symbol_expr_pairs(ls: &Vec<Rc<LispVal>>) -> Result<(Vec<String>, Vec<Rc<LispVal>>),String> {

    // Iterate over pairs of elements from ls
    let mut symbols = Vec::new();
    let mut vals = Vec::new();
    for pair in ls.chunks(2) {
        let (symbol, val) = match pair {
            [symbol, val] => (symbol, val),
            _ => return Err(format!("Expression is not a symbol: {:?}", ls)),
        };

        let symbol = match symbol.as_ref() {
            LispVal::Atom(atom) => Ok(atom),
            _ => Err(format!("Expression is not an atom: {:?}", ls)),
        }?;
        symbols.push(symbol.clone());
        vals.push(Rc::clone(val));
    }
    Ok((symbols, vals))
}

fn bind(env: &Env, symbols: &Vec<String>, vals: &Vec<Rc<LispVal>>) -> Result<Env, String> {
    let mut new_env = env.clone();
    if symbols.len() != vals.len() {
        return Err(format!("Symbols and values do not match"));
    }
    for (symbol, val) in symbols.iter().zip(vals.iter()){
        new_env.insert(symbol.clone(), Rc::clone(val));
    }
    Ok(new_env)
}

fn get_bool(env: Env, pred: Rc<LispVal>) -> Result<(bool, Env), String> {
    let (expr, env) = eval(env, Rc::clone(&pred))?;

    match expr.as_ref() {
        LispVal::Bool(b) => Ok((*b, env)),
        _ => Err(format!("Expression is not a bool: {:?}", pred))
    }
}

pub fn evaluate(expr: Rc<LispVal>) ->Result<Rc<LispVal>, String>{
    let env = default_env();
    let (res, _) = eval(env, expr)?;
    Ok(res)
}

pub fn evaluate_exprs(exprs: Vec<Rc<LispVal>>) ->Result<Rc<LispVal>, String>{
    let mut env = default_env();
    let mut res = Rc::new(LispVal::Nil);
    for e in exprs{
        (res, env) = eval(env.clone(), e)?;
        // env = new_env;
    }
    Ok(res)
}


#[cfg(test)]
mod tests{
    use std::rc::Rc;

    use super::*;

    #[test]
    fn test_number(){
        let expr = Rc::new(LispVal::Number(1));
        let res = evaluate(expr).unwrap();
        assert_eq!(res, Rc::new(LispVal::Number(1)));
    }

    #[test]
    fn test_string(){
        let expr = Rc::new(LispVal::String("hello".to_string()));
        let res = evaluate(expr).unwrap();
        assert_eq!(res, Rc::new(LispVal::String("hello".to_string())));
    }

    #[test]
    fn test_nil(){
        let expr = Rc::new(LispVal::Nil);
        let res = evaluate(expr).unwrap();
        assert_eq!(res, Rc::new(LispVal::Nil));
    }

    #[test]
    fn test_add(){
        let expr = Rc::new(LispVal::List(vec![Rc::new(LispVal::Atom("+".to_string())), Rc::new(LispVal::Number(1)), Rc::new(LispVal::Number(2))]));
        let res = evaluate(expr).unwrap();
        assert_eq!(res, Rc::new(LispVal::Number(3)));
    }

    #[test]
    fn test_atom(){
        let expr = Rc::new(LispVal::Atom("a".to_string()));
        let mut env = Env::new();
        env.insert("a".to_string(), Rc::new(LispVal::Number(1)));
        let (expr, _) = eval(env, expr).unwrap();
        assert_eq!(expr, Rc::new(LispVal::Number(1)));
    }

    #[test]
    fn test_if(){
        let expr = Rc::new(LispVal::List(vec![Rc::new(LispVal::Atom("if".to_string())), Rc::new(LispVal::Bool(true)), Rc::new(LispVal::Number(1)), Rc::new(LispVal::Number(2))]));
        let res = evaluate(expr).unwrap();
        assert_eq!(res, Rc::new(LispVal::Number(1)));
    }

    #[test]
    fn test_nif(){
        let expr = Rc::new(LispVal::List(vec![Rc::new(LispVal::Atom("if".to_string())), Rc::new(LispVal::Bool(false)), Rc::new(LispVal::Number(1)), Rc::new(LispVal::Number(2))]));
        let res = evaluate(expr).unwrap();
        assert_eq!(res, Rc::new(LispVal::Number(2)));
    }

    #[test]
    fn test_let(){
        let expr = Rc::new(LispVal::List(vec![Rc::new(LispVal::Atom("let".to_string())), Rc::new(LispVal::List(vec![Rc::new(LispVal::Atom("a".to_string())), Rc::new(LispVal::Number(1))])), Rc::new(LispVal::Atom("a".to_string()))]));
        let res = evaluate(expr).unwrap();
        assert_eq!(res, Rc::new(LispVal::Number(1)));
    }

    #[test]
    fn test_define(){
        let expr1 = Rc::new(LispVal::List(vec![Rc::new(LispVal::Atom("define".to_string())), Rc::new(LispVal::Atom("a".to_string())), Rc::new(LispVal::List(vec![Rc::new(LispVal::Atom("a".to_string())), Rc::new(LispVal::Atom("b".to_string()))])), Rc::new(LispVal::List(vec![Rc::new(LispVal::Atom("+".to_string())), Rc::new(LispVal::Atom("a".to_string())), Rc::new(LispVal::Atom("b".to_string()))]))]));
        let expr2 = Rc::new(LispVal::List(vec![Rc::new(LispVal::Atom("a".to_string())), Rc::new(LispVal::Number(1)), Rc::new(LispVal::Number(1))]));
        let res = evaluate_exprs(vec![expr1, expr2]).unwrap();
        assert_eq!(res, Rc::new(LispVal::Number(2)));
    }

    #[test]
    fn test_lambda(){
        let expr = Rc::new(LispVal::List(vec![Rc::new(LispVal::Atom("lambda".to_string())), Rc::new(LispVal::List(vec![Rc::new(LispVal::Atom("a".to_string())), Rc::new(LispVal::Atom("b".to_string()))])), Rc::new(LispVal::List(vec![Rc::new(LispVal::Atom("+".to_string())), Rc::new(LispVal::Atom("a".to_string())), Rc::new(LispVal::Atom("b".to_string()))]))]));
        let res = evaluate(expr).unwrap();
        // Atleast it didnt crash!
    }

    #[test]
    fn test_lambda_call(){
        let expr = Rc::new(LispVal::List(vec![Rc::new(LispVal::Atom("add".to_string())), Rc::new(LispVal::Number(1)), Rc::new(LispVal::Number(2))]));
        let res = evaluate(expr).unwrap();
        assert_eq!(res, Rc::new(LispVal::Number(3)));
    }
}