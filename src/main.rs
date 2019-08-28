mod completer;
mod interpret;
use std::io::prelude::*;

#[macro_use]
extern crate pest_derive;

use pest::Parser;

#[derive(Parser)]
#[grammar = "zelang.pest"]
struct ZeLangParser;

use pest::error::Error;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Litteral {
    Int(i64),
    Str(String),
    Ident(String),
}

#[derive(Debug, Clone)]
pub enum CallType {
    Name(String),
    Recursive,
}

#[derive(Debug, Clone)]
pub enum ASTNode {
    Litteral(Litteral),
    Call {
        kind: CallType,
        args: Vec<ASTNode>,
    },
    Assign {
        lhs: String,
        expr: Box<ASTNode>,
    },
    Body(Vec<ASTNode>),
    Expr {
        body: Vec<ASTNode>,
        end_expr: Box<ASTNode>,
    },
    Placeholder,
    FuncDef {
        arg_names: Vec<String>,
        body: Box<ASTNode>,
    },
    Value(Rc<interpret::Value>),
    Cond {
        cond: Box<ASTNode>,
        true_branch: Box<ASTNode>,
        false_branch: Box<ASTNode>,
    },
    List(Vec<ASTNode>),
}

impl ASTNode {
    pub fn is_expr(&self) -> bool {
        match self {
            ASTNode::Litteral(_)
            | ASTNode::Cond { .. }
            | ASTNode::Value(_)
            | ASTNode::Expr { .. }
            | ASTNode::List(_)
            | ASTNode::Call { .. }
            | ASTNode::Placeholder => true,
            _ => false,
        }
    }
}

fn parse(source: &str) -> Result<ASTNode, Error<Rule>> {
    let mut pairs = ZeLangParser::parse(Rule::program, source)?;
    let mut body = vec![];
    for pair in pairs.next().unwrap().into_inner() {
        match pair.as_rule() {
            Rule::statement => body.push(parse_statement(pair.into_inner().next().unwrap())),
            Rule::expr => body.push(parse_expr(pair.into_inner())),
            Rule::EOI => (),
            _ => unreachable!(),
        }
    }
    Ok(ASTNode::Body(body))
}

fn parse_statement(pair: pest::iterators::Pair<Rule>) -> ASTNode {
    match pair.as_rule() {
        Rule::assign => parse_assign(pair),
        r => panic!("Unexpected {:?} in parse_statement", r),
    }
}

fn parse_litteral(pair: pest::iterators::Pair<Rule>) -> ASTNode {
    match pair.as_rule() {
        Rule::int => ASTNode::Litteral(Litteral::Int(pair.as_str().parse().unwrap())),
        Rule::string => ASTNode::Litteral(Litteral::Str(
            pair.into_inner().next().unwrap().as_str().to_string(),
        )),
        Rule::name => ASTNode::Litteral(Litteral::Ident(pair.as_str().to_string())),
        Rule::list_decl => parse_list(pair.into_inner().next().unwrap()),
        _ => unreachable!(),
    }
}

fn parse_expr(mut pairs: pest::iterators::Pairs<Rule>) -> ASTNode {
    let first = pairs.next().unwrap();
    match first.as_rule() {
        Rule::placeholder => ASTNode::Placeholder,
        Rule::call => {
            let mut pair = first.into_inner();
            let call_id = pair.next().unwrap();
            let kind = match call_id.as_rule() {
                Rule::rec_call => CallType::Recursive,
                Rule::name => CallType::Name(call_id.as_str().to_string()),
                _ => unreachable!(),
            };
            let args = pair.next().unwrap();
            let args = parse_arguments(args);
            ASTNode::Call { kind, args }
        }
        Rule::value => parse_litteral(first.into_inner().next().unwrap()),
        Rule::func_definition => {
            let mut pair = first.into_inner();
            let arg_names = parse_name_list(pair.next().unwrap());
            let body = Box::new(parse_expr(pair.next().unwrap().into_inner()));
            ASTNode::FuncDef { arg_names, body }
        }
        Rule::cond => {
            let mut pairs = first.into_inner();
            let cond = Box::new(parse_expr(pairs.next().unwrap().into_inner()));
            let true_branch = Box::new(parse_expr(pairs.next().unwrap().into_inner()));
            let false_branch = Box::new(parse_expr(pairs.next().unwrap().into_inner()));
            ASTNode::Cond {
                cond,
                true_branch,
                false_branch,
            }
        }
        Rule::statement => {
            let mut body = vec![parse_statement(first.into_inner().next().unwrap())];
            for pair in pairs {
                match pair.as_rule() {
                    Rule::statement => {
                        body.push(parse_statement(pair.into_inner().next().unwrap()));
                    }
                    Rule::expr => {
                        return ASTNode::Expr {
                            body,
                            end_expr: Box::new(parse_expr(pair.into_inner())),
                        }
                    }
                    r => panic!("Unexpected in {:?} in parsing expr/{{statement;* expr}}", r),
                }
            }
            unimplemented!();
        }
        r => unreachable!("Got to {:?}", r),
    }
}

fn parse_assign(pair: pest::iterators::Pair<Rule>) -> ASTNode {
    let mut pair = pair.into_inner();
    let lhs = pair.next().unwrap().as_str().to_string();
    let expr = parse_expr(pair.next().unwrap().into_inner());
    ASTNode::Assign {
        lhs,
        expr: Box::new(expr),
    }
}

fn parse_list(pair: pest::iterators::Pair<Rule>) -> ASTNode {
    let mut args = vec![];
    for arg in pair.into_inner() {
        args.push(parse_expr(arg.into_inner()))
    }
    ASTNode::List(args)
}

fn parse_name_list(pair: pest::iterators::Pair<Rule>) -> Vec<String> {
    let mut names = vec![];
    for name in pair.into_inner() {
        names.push(name.as_str().to_owned())
    }
    names
}

fn parse_arguments(pair: pest::iterators::Pair<Rule>) -> Vec<ASTNode> {
    let mut args = vec![];
    for arg in pair.into_inner() {
        match arg.as_rule() {
            Rule::placeholder => args.push(ASTNode::Placeholder),
            _ => args.push(parse_expr(arg.into_inner())),
        }
    }
    args
}

fn parse_and_exec(code: &str, context: &mut interpret::Context) {
    let ast = match parse(code) {
        Ok(a) => a,
        Err(e) => {
            eprintln!("Err: {}", e);
            return;
        }
    };
    match interpret::interpret_ast(ast, context) {
        Ok(Some(v)) => println!("-> {}", v),
        Ok(None) => (),
        Err(e) => println!("Err: {:?}", e),
    }
}

fn main() {
    let mut rl = completer::get_editor();
    let mut context = interpret::Context::new();
    loop {
        use rustyline::error::ReadlineError;
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                if line == "quit" {
                    break;
                } else if line == "help" {
                    println!("\t Builtins:");
                    println!("\t\t-help: Shows this");
                    println!("\t\t-quit: Exit the interpreter");
                    println!("\t\t-load [file]: Executes each part of a file, separated by ;;");
                } else if line.starts_with("load ") {
                    let file_name = line.trim_start_matches("load").trim();
                    let mut file = match std::fs::File::open(file_name) {
                        Ok(f) => f,
                        Err(e) => {
                            println!("Error opening file: {}", e);
                            continue;
                        }
                    };
                    let mut code = String::new();
                    if let Err(e) = file.read_to_string(&mut code) {
                        println!("Error reading code: {}", e);
                        continue;
                    }
                    for code_body in code.trim().split_terminator(";;") {
                        let trimed = code_body.trim();
                        println!("{}", trimed);
                        parse_and_exec(trimed, &mut context)
                    }
                } else {
                    parse_and_exec(&line, &mut context)
                }
            }

            Err(ReadlineError::Interrupted) => (),
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                eprintln!("Error: {}", err);
                break;
            }
        }
    }
    println!("Bye !");
}
