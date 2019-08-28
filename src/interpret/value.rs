use super::{EvalError, Function, TypeError};
use std::rc::Rc;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Type {
    List,
    Int,
    Str,
    Type,
    Func,
    Unit,
    Bool,
    Error,
}

#[derive(Debug, Clone)]
pub struct ListData {
    pub hd: Rc<Value>,
    pub tl: Option<Rc<ListData>>,
}

impl PartialEq for ListData {
    fn eq(&self, other: &Self) -> bool {
        let (mut s, mut o) = (self, other);
        while let (Some(ns), Some(no)) = (&s.tl, &o.tl) {
            if s.hd != o.hd {
                return false;
            }
            s = &ns;
            o = &no;
        }
        s.tl.is_none() && o.tl.is_none()
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Str(String),
    Type(Type),
    Func(Box<Function>),
    Bool(bool),
    List {
        list_type: Option<Type>,
        data: Option<Rc<ListData>>,
    },
    Unit,
    Error(EvalError),
}

impl std::cmp::PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        if self.kind() != other.kind() {
            false
        } else {
            match self {
                Value::Error(_) => unimplemented!(),
                Value::List { data: l, list_type } => (l, *list_type) == other.as_list(),
                Value::Unit => true,
                Value::Int(i) => *i == other.as_int(),
                Value::Str(s) => s == other.as_str(),
                Value::Type(t) => *t == other.as_type(),
                Value::Func(_) => false,
                Value::Bool(b) => *b == other.as_bool(),
            }
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.convert(Type::Str).as_str())
    }
}

fn list_to_string(l: &Option<Rc<ListData>>) -> String {
    let mut ret = "[".to_owned();
    if let Some(l) = l {
        let mut to_add = &l.hd;
        let mut list = l;
        while let Some(next) = &list.tl {
            ret.push_str(&format!("{}, ", to_add));
            to_add = &next.hd;
            list = &next;
        }
        ret.push_str(&format!("{}", to_add));
    }
    ret.push(']');
    ret
}

#[macro_export]
macro_rules! err {
    ($e:expr) => {
        match $e.kind() {
            Type::Error => return $e,
            _ => $e,
        }
    };
    ($e:expr, $w:expr) => {
        match $e.kind() {
            Type::Error => return $w(Rc::new($e)),
            _ => $e,
        }
    };
}

#[macro_export]
macro_rules! eval_res_to_val {
    ($e:expr) => {
        match $e {
            Err(e) => return Rc::new(Value::Error(e)),
            Ok(x) => x,
        }
    };
}

impl Value {
    pub fn convert(&self, target: Type) -> Self {
        match target {
            Type::Str => Value::Str(match &self {
                Value::Int(i) => format!("{}", i),
                Value::Str(s) => s.clone(),
                Value::Type(t) => format!("{:?}", t),
                Value::Func(_) => "< FUNCTION >".to_string(),
                Value::Unit => "()".to_string(),
                Value::Bool(b) => format!("{}", b),
                Value::List { data: l, .. } => list_to_string(&l),
                Value::Error(e) => format!("Err: {:?}", e),
            }),
            Type::Type => Value::Type(self.kind()),
            _ => panic!("Cannot convert Value to {:?}", target),
        }
    }
    pub fn as_int(&self) -> i64 {
        match self {
            Value::Int(i) => *i,
            _ => panic!("Value is not int: {:?}", self),
        }
    }
    pub fn as_str(&self) -> &str {
        match self {
            Value::Str(s) => s,
            _ => panic!("Value is not String: {:?}", self),
        }
    }
    pub fn as_string(self) -> String {
        match self {
            Value::Str(s) => s,
            _ => panic!("Value is not String: {:?}", self),
        }
    }
    pub fn as_type(&self) -> Type {
        match self {
            Value::Type(t) => *t,
            _ => panic!("Value is not Type: {:?}", self),
        }
    }
    pub fn as_function(&self) -> &Function {
        match self {
            Value::Func(f) => f,
            _ => panic!("Value is not Function: {:?}", self),
        }
    }
    pub fn as_bool(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            _ => panic!("Value is not bool: {:?}", self),
        }
    }
    pub fn as_list(&self) -> (&Option<Rc<ListData>>, Option<Type>) {
        match self {
            Value::List { data: l, list_type } => (l, *list_type),
            _ => panic!("Value is not list: {:?}", self),
        }
    }
    pub fn kind(&self) -> Type {
        match self {
            Value::Int(_) => Type::Int,
            Value::Str(_) => Type::Str,
            Value::Type(_) => Type::Type,
            Value::Func(_) => Type::Func,
            Value::Unit => Type::Unit,
            Value::Bool(_) => Type::Bool,
            Value::List { .. } => Type::List,
            Value::Error(_) => Type::Error,
        }
    }
    pub fn assert_type(&self, kind: Type) -> Result<(), EvalError> {
        if self.kind() == kind {
            Ok(())
        } else {
            Err(EvalError::Type(TypeError::InvalidType {
                expected: kind,
                got: Rc::new(self.clone()),
            }))
        }
    }
}
