use super::ListData;
use super::{Context, EvalError, RefValue, Type, Value};
use crate::ASTNode;
use std::collections::HashMap;
use std::rc::Rc;

#[cfg(debug_assertions)]
const REC_LIMIT: usize = 600;
#[cfg(not(debug_assertions))]
const REC_LIMIT: usize = 2500;

#[derive(Debug, Clone)]
pub enum TypeError {
    InvalidArgCount { expected: usize, got: usize },
    InvalidType { expected: Type, got: RefValue },
}

#[derive(Clone, Debug)]
enum Argument {
    Value(RefValue),
    Type(Option<Type>),
}
#[derive(Debug, Clone)]
pub enum ArgumentError {
    Type(TypeError),
    SatisfyValue { was: RefValue, tried: RefValue },
}
impl From<TypeError> for ArgumentError {
    fn from(err: TypeError) -> ArgumentError {
        ArgumentError::Type(err)
    }
}

impl Argument {
    fn as_value(self) -> RefValue {
        match self {
            Argument::Value(v) => v,
            _ => panic!("Can't take argument as value: {:?}", self),
        }
    }
    fn needs_value(&self) -> bool {
        if let Argument::Type(_) = self {
            true
        } else {
            false
        }
    }
    fn satisfy(&mut self, val: RefValue) -> Result<(), ArgumentError> {
        match self {
            Argument::Value(was) => Err(ArgumentError::SatisfyValue {
                was: was.clone(),
                tried: val,
            }),
            Argument::Type(t) => {
                if t.is_none() || t.unwrap() == val.kind() {
                    *self = Argument::Value(val);
                    Ok(())
                } else {
                    Err(ArgumentError::Type(TypeError::InvalidType {
                        expected: t.unwrap(),
                        got: val,
                    }))
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    func: FunctionType,
    args: Vec<Argument>,
    remain: usize,
    rec: Option<Box<Function>>,
    rec_depth: usize,
}

#[derive(Debug, Clone)]
pub enum FunctionError {
    InvalidArgument(ArgumentError),
    InvalidArgCount { expected: usize, got: usize },
    Eval(Box<EvalError>),
}
impl From<ArgumentError> for FunctionError {
    fn from(err: ArgumentError) -> FunctionError {
        FunctionError::InvalidArgument(err)
    }
}
impl From<EvalError> for FunctionError {
    fn from(err: EvalError) -> FunctionError {
        match err {
            EvalError::FunctionCall(e) => e,
            e => FunctionError::Eval(Box::new(e)),
        }
    }
}

impl Function {
    pub fn plug(
        &self,
        args: Vec<Option<RefValue>>,
        ctx: &Context,
    ) -> Result<RefValue, FunctionError> {
        if args.len() != self.remain {
            Err(FunctionError::InvalidArgCount {
                expected: self.remain,
                got: args.len(),
            })
        } else {
            let mut arg_to_insert: usize = 0;
            let mut new_args = self.args.clone();
            let mut remain = self.remain.clone();
            for arg in args {
                while arg_to_insert < new_args.len() && !new_args[arg_to_insert].needs_value() {
                    arg_to_insert += 1
                }
                if arg_to_insert >= new_args.len() {
                    panic!("Not enought args to satisfy, invariant broken")
                }
                if !arg.is_none() {
                    new_args[arg_to_insert].satisfy(arg.unwrap())?;
                    remain -= 1;
                }
                arg_to_insert += 1;
            }
            let func = Function {
                remain,
                args: new_args,
                func: self.func.clone(),
                rec: self.rec.clone(),
                rec_depth: self.rec_depth.clone(),
            };
            if func.remain == 0 {
                func.call(ctx).map_err(FunctionError::from)
            } else {
                Ok(Rc::new(Value::Func(Box::new(func))))
            }
        }
    }
    fn call(self, ctx: &Context) -> Result<RefValue, EvalError> {
        let (func, args, mut rec) = (self.func, self.args, self.rec);
        let args = args.into_iter().map(|v| v.as_value()).collect();
        match func {
            FunctionType::BuiltIn(s) => Ok(ctx.builtins.get(&s).unwrap().call(args)?),
            FunctionType::BuiltUp(BuiltUp {
                arg_names,
                expr,
                local_scope,
            }) => {
                let mut new_scope: HashMap<_, _> = arg_names.into_iter().zip(args).collect();
                new_scope.extend(local_scope);
                let mut new_context = ctx.with_new_scope(new_scope);
                if let Some(rec) = &mut rec {
                    rec.rec_depth += 1;
                    rec.rec = Some(rec.clone());
                }
                if self.rec_depth > REC_LIMIT {
                    return Err(EvalError::RecursionLimit(REC_LIMIT));
                }
                new_context.current_eval = rec.map(|f| Rc::new(Value::Func(f)));
                super::interpret_expr(expr, &mut new_context)
            }
        }
    }
    pub fn built_up(f: BuiltUp, rec: bool) -> Self {
        let arg_count = f.arg_names.len();
        let args = vec![Argument::Type(None); arg_count];
        let mut func = Function {
            func: FunctionType::BuiltUp(f),
            args,
            remain: arg_count,
            rec: None,
            rec_depth: 0,
        };
        if rec {
            func.rec = Some(Box::new(func.clone()));
        };
        func
    }
}

#[derive(Clone, Debug)]
enum FunctionType {
    BuiltIn(String),
    BuiltUp(BuiltUp),
}

#[derive(Clone, Debug)]
pub struct BuiltUp {
    pub expr: ASTNode,
    pub local_scope: HashMap<String, RefValue>,
    pub arg_names: Vec<String>,
}

pub struct BuiltIn {
    f: Box<dyn Fn(Vec<RefValue>) -> Result<RefValue, EvalError>>,
    expected: Vec<Option<Type>>,
}

impl BuiltIn {
    fn call(&self, args: Vec<RefValue>) -> Result<RefValue, EvalError> {
        if args.len() != self.expected.len() {
            return Err(EvalError::Type(TypeError::InvalidArgCount {
                expected: self.expected.len(),
                got: args.len(),
            }));
        }
        for (ind, arg) in args.iter().enumerate() {
            if let Some(t) = self.expected[ind] {
                if arg.kind() != t {
                    return Err(EvalError::Type(TypeError::InvalidType {
                        expected: t,
                        got: arg.clone(),
                    }));
                }
            }
        }
        (*self.f)(args)
    }
    pub fn as_function(&self, name: String) -> Function {
        Function {
            func: FunctionType::BuiltIn(name),
            args: self
                .expected
                .clone()
                .into_iter()
                .map(Argument::Type)
                .collect(),
            remain: self.expected.len(),
            rec: None,
            rec_depth: 0,
        }
    }
}
pub fn get_builtins() -> HashMap<String, BuiltIn> {
    let mut m = HashMap::new();
    m.insert(
        "empty".to_owned(),
        BuiltIn {
            f: Box::new(|args| Ok(Rc::new(Value::Bool(args[0].as_list().0.is_none())))),
            expected: vec![Some(Type::List)],
        },
    );
    m.insert(
        "cons".to_owned(),
        BuiltIn {
            f: Box::new(|args| {
                let list_type = if let Some(t) = args[1].as_list().1 {
                    args[0].assert_type(t)?;
                    Some(t)
                } else {
                    Some(args[0].kind())
                };
                Ok(Rc::new(Value::List {
                    data: Some(Rc::new(ListData {
                        hd: args[0].clone(),
                        tl: args[1].as_list().0.clone(),
                    })),
                    list_type,
                }))
            }),
            expected: vec![None, Some(Type::List)],
        },
    );
    m.insert(
        "head".to_owned(),
        BuiltIn {
            f: Box::new(|args| {
                args[0]
                    .as_list()
                    .0
                    .as_ref()
                    .ok_or(EvalError::EmptyList)
                    .map(|l| l.hd.clone())
            }),
            expected: vec![Some(Type::List)],
        },
    );
    m.insert(
        "tail".to_owned(),
        BuiltIn {
            f: Box::new(|args| match args[0].as_list().0 {
                Some(e) => Ok(Rc::new(Value::List {
                    data: e.tl.clone(),
                    list_type: args[0].as_list().1,
                })),
                None => Err(EvalError::EmptyList),
            }),
            expected: vec![Some(Type::List)],
        },
    );
    m.insert(
        "eq".to_owned(),
        BuiltIn {
            f: Box::new(|args| Ok(Rc::new(Value::Bool(args[0] == args[1])))),
            expected: vec![None, None],
        },
    );
    m.insert(
        "print".to_owned(),
        BuiltIn {
            f: Box::new(|args| {
                println!("{}", args[0]);
                Ok(Rc::new(Value::Unit))
            }),
            expected: vec![None],
        },
    );
    m.insert(
        "type".to_string(),
        BuiltIn {
            f: Box::new(|args| Ok(Rc::new(args[0].convert(Type::Type)))),
            expected: vec![None],
        },
    );
    m.insert(
        "str".to_string(),
        BuiltIn {
            f: Box::new(|args| Ok(Rc::new(args[0].convert(Type::Str)))),
            expected: vec![None],
        },
    );
    m.insert(
        "gt".to_string(),
        BuiltIn {
            f: Box::new(|args| Ok(Rc::new(Value::Bool(args[0].as_int() > args[1].as_int())))),
            expected: vec![Some(Type::Int), Some(Type::Int)],
        },
    );
    m.insert(
        "lt".to_string(),
        BuiltIn {
            f: Box::new(|args| Ok(Rc::new(Value::Bool(args[0].as_int() < args[1].as_int())))),
            expected: vec![Some(Type::Int),Some(Type::Int)],
        },
    );
    m.insert(
        "and".to_string(),
        BuiltIn {
            f: Box::new(|args| Ok(Rc::new(Value::Bool(args[0].as_bool() == true && args[1].as_bool() == true)))),
            expected: vec![Some(Type::Bool),Some(Type::Bool)],
        }
    );

    m.insert(
        "or".to_string(),
        BuiltIn {
            f: Box::new(|args| Ok(Rc::new(Value::Bool(args[0].as_bool() == true || args[1].as_bool() == true)))),
            expected: vec![Some(Type::Bool),Some(Type::Bool)],
        }
    );
    m.insert(
        "add".to_string(),
        BuiltIn {
            f: Box::new(|args| Ok(Rc::new(Value::Int(args[0].as_int() + args[1].as_int())))),
            expected: vec![Some(Type::Int), Some(Type::Int)],
        },
    );
    m.insert(
        "mult".to_string(),
        BuiltIn {
            f: Box::new(|args| Ok(Rc::new(Value::Int(args[0].as_int() * args[1].as_int())))),
            expected: vec![Some(Type::Int), Some(Type::Int)]
        },
    );
    m.insert(
        "div".to_string(),
        BuiltIn {
            f: Box::new(|args| Ok(Rc::new(Value::Int(args[0].as_int() / args[1].as_int())))),
            expected: vec![Some(Type::Int),Some(Type::Int)],
        },
    );
    m.insert(
        "mod".to_string(),
        BuiltIn {
            f: Box::new(|args| Ok(Rc::new(Value::Int(args[0].as_int() % args[1].as_int())))),
            expected: vec![Some(Type::Int),Some(Type::Int)],
        },
    );
    m.insert(
        "sub".to_string(),
        BuiltIn {
            f: Box::new(|args| Ok(Rc::new(Value::Int(args[0].as_int() - args[1].as_int())))),
            expected: vec![Some(Type::Int), Some(Type::Int)],
        },
    );
    m.insert(
        "concat".to_string(),
        BuiltIn {
            f: Box::new(|args| {
                Ok(Rc::new(Value::Str({
                    let mut s = String::from(args[0].as_str());
                    s.push_str(args[1].as_str());
                    s
                })))
            }),
            expected: vec![Some(Type::Str), Some(Type::Str)],
        },
    );
    m
}
