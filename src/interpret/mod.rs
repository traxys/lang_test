use crate::ASTNode;
use crate::CallType;
use crate::Litteral;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

type RefValue = Rc<Value>;
mod func;
mod semi_resolve;
mod value;

use func::{get_builtins, BuiltIn, BuiltUp, Function, FunctionError, TypeError};
pub use semi_resolve::{semi_expr, ResolveResult};
pub use value::{ListData, Type, Value};

#[derive(Debug, Clone)]
pub enum EvalError {
    UnknownLitteral(String),
    Type(TypeError),
    ShadowsBuiltin(String),
    FunctionCall(FunctionError),
    InvalidRecCall,
    RecursionLimit(usize),
    EmptyList,
}

impl From<FunctionError> for EvalError {
    fn from(err: FunctionError) -> EvalError {
        EvalError::FunctionCall(err)
    }
}

impl From<TypeError> for EvalError {
    fn from(err: TypeError) -> EvalError {
        EvalError::Type(err)
    }
}

pub struct Context {
    variables: HashMap<String, RefValue>,
    builtins: Rc<HashMap<String, BuiltIn>>,
    current_eval: Option<RefValue>,
}

impl Context {
    pub fn new() -> Context {
        let mut ctx = Context {
            variables: HashMap::new(),
            builtins: Rc::new(get_builtins()),
            current_eval: None,
        };
        for (name, builtin) in ctx.builtins.iter() {
            let fn_builtin = builtin.as_function(name.clone());
            ctx.variables
                .insert(name.clone(), Rc::new(Value::Func(Box::new(fn_builtin))));
        }
        ctx
    }

    fn get(&self, var: String) -> Result<RefValue, EvalError> {
        if let Some(v) = self.variables.get(&var) {
            Ok(v.clone())
        } else {
            Err(EvalError::UnknownLitteral(var))
        }
    }

    pub fn with_new_scope(&self, scope: HashMap<String, RefValue>) -> Context {
        Context {
            builtins: self.builtins.clone(),
            variables: scope,
            current_eval: None,
        }
    }
    fn with_masked(&self, to_mask: &HashSet<String>) -> Context {
        Context {
            builtins: self.builtins.clone(),
            current_eval: self.current_eval.clone(),
            variables: self
                .variables
                .iter()
                .filter(|(k, _)| !to_mask.contains(*k))
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect(),
        }
    }
}

pub fn interpret_ast(ast: ASTNode, ctx: &mut Context) -> Result<Option<RefValue>, EvalError> {
    match ast {
        ASTNode::Body(nodes) => {
            let node_count = nodes.len();
            for (ind, node) in nodes.into_iter().enumerate() {
                if ind + 1 == node_count && node.is_expr() {
                    return Ok(Some(interpret_expr(node, ctx)?));
                } else {
                    interpret_statement(node, ctx)?;
                }
            }
            Ok(None)
        }
        invalid_node => panic!("Invalid node in interpret_ast: {:?}", invalid_node),
    }
}

fn interpret_statement(ast: ASTNode, ctx: &mut Context) -> Result<(), EvalError> {
    match ast {
        ASTNode::Assign {
            lhs: var,
            expr: rhs,
        } => {
            let expr = interpret_expr(*rhs, ctx)?;
            if !ctx.builtins.contains_key(&var) {
                ctx.variables.insert(var, expr);
            } else {
                return Err(EvalError::ShadowsBuiltin(var));
            }
        }
        invalid_node => panic!("Invalid node in interpret_statement: {:?}", invalid_node),
    }
    Ok(())
}

fn interpret_list(ast: Vec<ASTNode>, ctx: &mut Context) -> Result<RefValue, EvalError> {
    let mut values = vec![];
    for node in ast {
        values.push(interpret_expr(node, ctx)?);
    }
    let list_type = values.first().map(|t| t.kind());
    let mut data = None;
    for elem in values.into_iter().rev() {
        elem.assert_type(*list_type.as_ref().unwrap())?;
        data = Some(Rc::new(ListData { hd: elem, tl: data }));
    }
    Ok(Rc::new(Value::List { data, list_type }))
}

fn interpret_expr(ast: ASTNode, ctx: &mut Context) -> Result<RefValue, EvalError> {
    match ast {
        ASTNode::List(l) => interpret_list(l, ctx),
        ASTNode::Litteral(lit) => interpret_litteral(lit, ctx),
        ASTNode::Value(v) => Ok(v),
        ASTNode::Expr {
            body,
            end_expr: expr,
        } => {
            for statement in body {
                interpret_statement(statement, ctx)?;
            }
            interpret_expr(*expr, ctx)
        }
        ASTNode::Call { kind, args } => {
            let mut v_args = vec![];
            for arg in args {
                if let ASTNode::Placeholder = arg {
                    v_args.push(None)
                } else {
                    v_args.push(Some(interpret_expr(arg, ctx)?));
                }
            }
            call_function(kind, v_args, ctx)
        }
        ASTNode::FuncDef { arg_names, body } => {
            let expected = arg_names.iter().cloned().collect();
            let mut resolve_scope = ctx.with_masked(&expected);
            let resolved_body =
                semi_resolve::semi_expr(*body, &expected, &mut HashSet::new(), &mut resolve_scope)?;
            let ResolveResult { ast: expr, is_rec } = resolved_body;
            let bu = BuiltUp {
                local_scope: ctx.variables.clone(),
                arg_names,
                expr,
            };
            Ok(Rc::new(Value::Func(Box::new(Function::built_up(
                bu, is_rec,
            )))))
        }
        ASTNode::Cond {
            cond,
            true_branch,
            false_branch,
        } => {
            let cond = interpret_expr(*cond, ctx)?;
            cond.assert_type(Type::Bool)?;
            if cond.as_bool() {
                interpret_expr(*true_branch, ctx)
            } else {
                interpret_expr(*false_branch, ctx)
            }
        }
        invalid_node => panic!("Invalid node in interpret_expr: {:?}", invalid_node),
    }
}

fn call_function(
    name: CallType,
    args: Vec<Option<RefValue>>,
    ctx: &Context,
) -> Result<RefValue, EvalError> {
    let var = match name {
        CallType::Recursive => {
            if let Some(f) = &ctx.current_eval {
                f.clone()
            } else {
                return Err(EvalError::InvalidRecCall);
            }
        }
        CallType::Name(name) => ctx.get(name)?,
    };
    var.assert_type(Type::Func)?;
    let f = var.as_function();
    f.plug(args, ctx).map_err(EvalError::from)
}

fn interpret_litteral(litteral: Litteral, ctx: &Context) -> Result<RefValue, EvalError> {
    match litteral {
        Litteral::Int(n) => Ok(Rc::new(Value::Int(n))),
        Litteral::Str(s) => Ok(Rc::new(Value::Str(s))),
        Litteral::Ident(id) => match ctx.variables.get(&id) {
            None => Err(EvalError::UnknownLitteral(id)),
            Some(val) => Ok(val.clone()),
        },
    }
}
