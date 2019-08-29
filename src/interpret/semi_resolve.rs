use super::{Context, EvalError, Type, Value};
use crate::{ASTNode, Litteral};
use std::collections::HashSet;
use std::rc::Rc;

pub struct ResolveResult {
    pub ast: ASTNode,
    pub is_rec: bool,
}

fn semi_statement(
    ast: ASTNode,
    exclude: &HashSet<String>,
    known: &mut HashSet<String>,
    ctx: &mut Context,
) -> Result<ResolveResult, EvalError> {
    match ast {
        ASTNode::Assign {
            lhs: var,
            expr: rhs,
        } => {
            let ResolveResult { ast: expr, is_rec } = semi_expr(*rhs, exclude, known, ctx)?;
            if let ASTNode::Value(v) = &expr {
                ctx.variables.insert(var.clone(), v.clone());
            } else {
                known.insert(var.clone());
            }
            return Ok(ResolveResult {
                ast: ASTNode::Assign {
                    lhs: var,
                    expr: Box::new(expr),
                },
                is_rec,
            });
        }
        _ => unreachable!(),
    }
}

fn semi_list(
    ast: Vec<ASTNode>,
    exclude: &HashSet<String>,
    known: &mut HashSet<String>,
    ctx: &mut Context,
) -> Result<ResolveResult, EvalError> {
    let mut values = vec![];
    let mut rec_total = false;
    for node in ast {
        let ResolveResult { ast, is_rec } = semi_expr(node, exclude, known, ctx)?;
        rec_total |= is_rec;
        values.push(ast);
    }
    Ok(ResolveResult {
        ast: ASTNode::List(values),
        is_rec: rec_total,
    })
}

pub fn semi_expr(
    ast: ASTNode,
    exclude: &HashSet<String>,
    known: &mut HashSet<String>,
    ctx: &mut Context,
) -> Result<ResolveResult, EvalError> {
    match ast {
        ASTNode::Litteral(lit) => {
            semi_litteral(lit, exclude, known, ctx).map(|ast| ResolveResult { ast, is_rec: false })
        }
        ASTNode::List(v) => semi_list(v, exclude, known, ctx),
        ASTNode::Expr {
            body,
            end_expr: expr,
        } => {
            let mut is_any_rec = false;
            let mut new_body = vec![];
            for statement in body {
                new_body.push({
                    let ResolveResult { ast, is_rec } =
                        semi_statement(statement, exclude, known, ctx)?;
                    is_any_rec |= is_rec;
                    ast
                });
            }
            let end_expr = {
                let ResolveResult { ast, is_rec } = semi_expr(*expr, exclude, known, ctx)?;
                is_any_rec |= is_rec;
                ast
            };
            Ok(ResolveResult {
                ast: ASTNode::Expr {
                    body: new_body,
                    end_expr: Box::new(end_expr),
                },
                is_rec: is_any_rec,
            })
        }
        ASTNode::MultiCall { kind, applications } => {
            let mut is_any_rec = false;
            if let crate::CallType::Recursive = &kind {
                is_any_rec = true;
            };
            let mut applications = applications.into_iter();
            let mut v_args = vec![];
            for arg in applications.next().unwrap() {
                if let ASTNode::Placeholder = arg {
                    v_args.push(ASTNode::Placeholder)
                } else {
                    v_args.push({
                        let ResolveResult { ast, is_rec } = semi_expr(arg, exclude, known, ctx)?;
                        is_any_rec |= is_rec;
                        ast
                    });
                }
            }
            let mut partial = semi_function(kind.clone(), v_args, ctx)?;
            let mut can_eval = if let ASTNode::Value(_) = &partial {
                true
            } else {
                false
            };
            let mut to_add_applications = vec![];
            for args in applications {
                let mut v_args = vec![];
                for arg in args {
                    if let ASTNode::Placeholder = arg {
                        v_args.push(ASTNode::Placeholder)
                    } else {
                        v_args.push({
                            let ResolveResult { ast, is_rec } =
                                semi_expr(arg, exclude, known, ctx)?;
                            is_any_rec |= is_rec;
                            ast
                        });
                    }
                }
                if can_eval {
                    partial = semi_function(kind.clone(), v_args, ctx)?;
                    can_eval = if let ASTNode::Value(_) = &partial {
                        true
                    } else {
                        false
                    };
                } else {
                    to_add_applications.push(v_args)
                }
            }
            Ok(match partial {
                ASTNode::Value(f) => ResolveResult {
                    ast: ASTNode::Value(f),
                    is_rec: is_any_rec,
                },
                ASTNode::MultiCall {
                    kind,
                    mut applications,
                } => {
                    applications.append(&mut to_add_applications);
                    ResolveResult {
                        ast: ASTNode::MultiCall { kind, applications },
                        is_rec: is_any_rec,
                    }
                }
                _ => unreachable!(),
            })
        }
        ASTNode::FuncDef { arg_names, body } => {
            let expected = arg_names.iter().cloned().collect();
            let mut resolve_scope = ctx.with_masked(&expected);
            let ResolveResult {
                ast: resolved_body,
                is_rec,
            } = semi_expr(
                *body,
                &expected.union(exclude).cloned().collect(),
                &mut HashSet::new(),
                &mut resolve_scope,
            )?;
            Ok(ResolveResult {
                ast: ASTNode::FuncDef {
                    arg_names,
                    body: Box::new(resolved_body),
                },
                is_rec,
            })
        }
        ASTNode::Cond {
            cond,
            true_branch,
            false_branch,
        } => {
            let ResolveResult { ast: cond, is_rec } = semi_expr(*cond, exclude, known, ctx)?;
            match cond {
                ASTNode::Value(cond) => {
                    cond.assert_type(Type::Bool)?;
                    if cond.as_bool() {
                        let mut res = semi_expr(*true_branch, exclude, known, ctx)?;
                        res.is_rec |= is_rec;
                        Ok(res)
                    } else {
                        let mut res = semi_expr(*false_branch, exclude, known, ctx)?;
                        res.is_rec |= is_rec;
                        Ok(res)
                    }
                }
                ast => {
                    let ResolveResult {
                        ast: true_branch,
                        is_rec: rec_true,
                    } = semi_expr(*true_branch, exclude, known, ctx)?;
                    let ResolveResult {
                        ast: false_branch,
                        is_rec: rec_false,
                    } = semi_expr(*false_branch, exclude, known, ctx)?;
                    Ok(ResolveResult {
                        ast: ASTNode::Cond {
                            cond: Box::new(ast),
                            true_branch: Box::new(true_branch),
                            false_branch: Box::new(false_branch),
                        },
                        is_rec: is_rec | rec_false | rec_true,
                    })
                }
            }
        }
        invalid_node => panic!("Invalid node in semi_expr: {:?}", invalid_node),
    }
}

fn semi_function(
    kind: crate::CallType,
    args: Vec<ASTNode>,
    ctx: &Context,
) -> Result<ASTNode, EvalError> {
    let compute = |var: Rc<Value>, args: Vec<ASTNode>| {
        var.assert_type(Type::Func)?;
        if args.iter().all(|n| match n {
            ASTNode::Placeholder | ASTNode::Value(_) => true,
            _ => false,
        }) {
            let mut v_args = vec![];
            for arg in args {
                match arg {
                    ASTNode::Placeholder => v_args.push(None),
                    ASTNode::Value(v) => v_args.push(Some(v)),
                    _ => unreachable!(),
                }
            }
            let f = var.as_function();
            Ok(ASTNode::Value(
                f.plug(v_args, ctx).map_err(EvalError::from)?,
            ))
        } else {
            Ok(ASTNode::MultiCall {
                kind: kind.clone(),
                applications: vec![args],
            })
        }
    };
    match kind.clone() {
        crate::CallType::Name(name) => {
            if let Ok(var) = ctx.get(name) {
                compute(var, args)
            } else {
                Ok(ASTNode::MultiCall {
                    kind,
                    applications: vec![args],
                })
            }
        }
        crate::CallType::Resolved(var) => compute(var, args),
        crate::CallType::Recursive => Ok(ASTNode::MultiCall {
            kind,
            applications: vec![args],
        }),
    }
}

fn semi_litteral(
    litteral: Litteral,
    exclude: &HashSet<String>,
    known: &HashSet<String>,
    ctx: &Context,
) -> Result<ASTNode, EvalError> {
    match litteral {
        Litteral::Int(n) => Ok(ASTNode::Value(Rc::new(Value::Int(n)))),
        Litteral::Str(s) => Ok(ASTNode::Value(Rc::new(Value::Str(s)))),
        Litteral::Ident(id) => match ctx.variables.get(&id) {
            None if exclude.contains(&id) || known.contains(&id) => {
                Ok(ASTNode::Litteral(Litteral::Ident(id)))
            }
            None => Err(EvalError::UnknownLitteral(id)),
            Some(val) => Ok(ASTNode::Value(val.clone())),
        },
    }
}
