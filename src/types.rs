use std::{collections::HashMap, rc::Rc, fmt};

#[derive(Debug, Clone, PartialEq)]
pub enum LispVal {
    Atom(String),
    List(Vec<Rc<LispVal>>),
    Number(i32),
    String(String),
    Bool(bool),
    Lambda(LambdaDef),
    Prim(Prim),
    Nil
}

impl fmt::Display for LispVal{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self{
            LispVal::Atom(s) => write!(f, "Atom {}", s),
            LispVal::List(l) => {
                let mut s = String::new();
                for item in l{
                    s.push_str(&format!("{} ", item));
                }
                write!(f, "({})", s)
            },
            LispVal::Number(i) => write!(f, "Number {}", i),
            LispVal::String(s) => write!(f, "String \"{}\"", s),
            LispVal::Bool(b) => write!(f, "Bool {}", b),
            LispVal::Lambda(l) => write!(f, "Lambda({:?})", l),
            LispVal::Prim(_) => write!(f, "Prim"),
            LispVal::Nil => write!(f, "Nil")
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LambdaDef {
    pub params: Vec<String>,
    pub body: Rc<LispVal>,
    pub closure: Env,
}

impl fmt::Display for LambdaDef{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "LambdaDef({:?})", self)
    }
}

pub type Env = HashMap<String, Rc<LispVal>>;

pub type Prim = fn(Vec<Rc<LispVal>>) -> Result<Rc<LispVal>, String>;