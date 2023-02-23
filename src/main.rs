#![feature(iter_intersperse)]
use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::str::Chars;
use std::usize;

use druid::widget::Label;
use druid::{AppLauncher, Widget, WindowDesc};

#[derive(Copy, Clone, Default, Debug, Eq, PartialEq)]
pub struct Localization {
    start_line: usize,
    start_col: usize,
    end_line: usize,
    end_col: usize,
}

impl Localization {
    fn advance_by(&self, lines: usize, cols: usize) -> Self {
        Self {
            start_line: self.end_line,
            start_col: self.end_col,
            end_line: self.end_line + lines,
            end_col: self.end_col + cols,
        }
    }

    fn combine_first_last(&self, other: &Self) -> Self {
        Self {
            start_line: self.start_line,
            start_col: self.start_col,
            end_line: other.end_line,
            end_col: other.end_col,
        }
    }

    fn combine_last(&self, other: &Self) -> Self {
        Self {
            start_line: self.end_line,
            start_col: self.end_col,
            end_line: other.end_line,
            end_col: other.end_col,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Number {
        value: usize,
        local: Localization,
    },
    Call {
        name: String, // Later this should change to an enum Lambda | Variable
        args: Vec<Expression>,
        local: Localization,
    },
    LazyCall {
        name: String, // Later this should change to an enum Lambda | Variable
        args: Vec<Expression>,
        local: Localization,
    },
    StructCall {
        name: String, // Later this should change to an enum Lambda | Variable
        args: Vec<Expression>,
        local: Localization,
    },
    Variable {
        name: String,
        local: Localization,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Definition {
    Atom {
        head: Expression, //only variables
        body: Expression,
        local: Localization,
    },
    Call {
        head: Expression, // Only structcalls numbers and vars
        body: Expression,
        local: Localization,
    },
    LazyCall {
        head: Expression, // anything goes
        body: Expression,
        local: Localization,
    },
    StructCall {
        head: Expression, // Only Variables
        body: Expression,
        local: Localization,
    },
}
#[derive(Clone, Debug, Default)]
pub struct Env {
    const_map: HashMap<String, Expression>,
    call_map: HashMap<String, Vec<(Expression, Expression)>>,
    lazy_call_map: HashMap<String, Vec<(Expression, Expression)>>,
    struct_map: HashMap<String, Vec<(Expression, Expression)>>,
}

#[derive(Clone, Debug, Default)]
pub struct Program {
    defs: Vec<Definition>,
    exprs: Vec<Expression>,
}

impl Program {
    fn display_as_tree(&self, n: usize) {
        (0..n).for_each(|_| print!(".."));
        println!(" Program");
        (0..n).for_each(|_| print!(".."));
        println!(" Definitions : ");
        self.defs.iter().for_each(|x| x.display_as_tree(n + 1));
        println!(" Expressions : ");
        self.exprs.iter().for_each(|x| x.display_as_tree(n + 1));
    }

    fn from_defs_and_exprs(defs: Vec<Definition>, exprs: Vec<Expression>) -> Self {
        Self { defs, exprs }
    }
    fn eval(&self) -> Vec<Result<Expression, EvalError>> {
        let mut env = Env::default();
        self.defs.iter().for_each(|def| {
            env.insert(def.clone());
        });
        self.exprs.iter().map(|expr| expr.eval(&env)).collect()
    }
    fn eval_with_env(&self, env: &mut Env) -> Vec<Result<Expression, EvalError>> {
        //println!("    DEBUG : Env =  {:?}",&env);
        self.defs.iter().for_each(|def| {
            env.insert(def.clone());
        });
        self.exprs.iter().map(|expr| expr.eval(&env)).collect()
    }
}

impl Env {
    fn from_program(prog: &Program) -> Self {
        let mut ret = Self::default();
        prog.defs.iter().for_each(|x| {
            ret.insert(x.clone());
        });
        ret
    }

    fn insert(&mut self, def: Definition) -> bool {
        match def {
            Definition::Atom { head, body, .. } => {
                if self
                    .const_map
                    .contains_key(&head.get_variable_name().unwrap())
                {
                    return false;
                } else {
                    self.const_map
                        .insert(head.get_variable_name().unwrap().into(), body.clone());
                    return true;
                }
            }
            Definition::Call { head, body, .. } => {
                if self.call_map.contains_key(&head.get_call_name().unwrap()) {
                    self.call_map
                        .get_mut(&head.get_call_name().unwrap())
                        .unwrap()
                        .push((head.to_owned(), body.to_owned()));
                    return true;
                } else {
                    self.call_map.insert(
                        head.get_call_name().unwrap().into(),
                        vec![(head.to_owned(), body.to_owned())],
                    );
                    return true;
                }
            }
            Definition::LazyCall { head, body, .. } => {
                if self
                    .lazy_call_map
                    .contains_key(&head.get_lazy_call_name().unwrap())
                {
                    self.lazy_call_map
                        .get_mut(&head.get_lazy_call_name().unwrap())
                        .unwrap()
                        .push((head.to_owned(), body.to_owned()));
                    return true;
                } else {
                    self.lazy_call_map.insert(
                        head.get_lazy_call_name().unwrap().into(),
                        vec![(head.to_owned(), body.to_owned())],
                    );
                    return true;
                }
            }
            Definition::StructCall { head, body, .. } => {
                if self
                    .struct_map
                    .contains_key(&head.get_struct_call_name().unwrap())
                {
                    self.struct_map
                        .get_mut(&head.get_struct_call_name().unwrap())
                        .unwrap()
                        .push((head.to_owned(), body.to_owned()));
                    return true;
                } else {
                    self.struct_map.insert(
                        head.get_struct_call_name().unwrap().into(),
                        vec![(head.to_owned(), body.to_owned())],
                    );
                    return true;
                }
            }
        }
    }
}

impl Definition {
    fn display_as_tree(&self, n: usize) {
        let f = |s: &str, head: &Expression, body: &Expression, local: &Localization| {
            println!(
                " {s} @ {}:{} -- {}:{}",
                local.start_line, local.start_col, local.end_line, local.end_col
            );
            (0..n).for_each(|_| print!("--"));
            println!(" Head :");
            head.display_as_tree(n + 1);
            (0..n).for_each(|_| print!("--"));
            println!(" Body :");
            body.display_as_tree(n + 1);
        };
        (0..n).for_each(|_| print!("--"));
        match self {
            Definition::Atom { head, body, local } => f("Const Def", head, body, local),
            Definition::Call { head, body, local } => f("Call Def", head, body, local),
            Definition::LazyCall { head, body, local } => f("Lazy Call Def", head, body, local),
            Definition::StructCall { head, body, local } => f("Struct Call Def", head, body, local),
        }
    }
    fn get_head(&self) -> Expression {
        match self {
            Definition::Atom { head, body, local }
            | Definition::Call { head, body, local }
            | Definition::LazyCall { head, body, local }
            | Definition::StructCall { head, body, local } => head.clone(),
        }
    }
    fn get_body(&self) -> Expression {
        match self {
            Definition::Atom { body, .. }
            | Definition::Call { body, .. }
            | Definition::LazyCall { body, .. }
            | Definition::StructCall { body, .. } => body.clone(),
        }
    }
    fn get_local(&self) -> Localization {
        match self {
            Definition::Atom { head, body, local }
            | Definition::Call { head, body, local }
            | Definition::LazyCall { head, body, local }
            | Definition::StructCall { head, body, local } => *local,
        }
    }
    fn is_atom(&self) -> bool {
        match self {
            Self::Atom { .. } => true,
            _ => false,
        }
    }
    fn is_call(&self) -> bool {
        match self {
            Self::Call { .. } => true,
            _ => false,
        }
    }
    fn is_lazy_call(&self) -> bool {
        match self {
            Self::LazyCall { .. } => true,
            _ => false,
        }
    }
    fn is_struct_call(&self) -> bool {
        match self {
            Self::StructCall { .. } => true,
            _ => false,
        }
    }

    fn from_expression(head: &Expression, body: &Expression, local: Localization) -> Option<Self> {
        match head {
            Expression::Number { .. } => None,
            Expression::Call { .. } => Self::call(head.clone(), body.clone(), local),
            Expression::LazyCall { .. } => Self::lazy_call(head.clone(), body.clone(), local),
            Expression::StructCall { .. } => Self::struct_call(head.clone(), body.clone(), local),
            Expression::Variable { .. } => Self::atom(head.clone(), body.clone(), local),
        }
    }
    fn atom(head: Expression, body: Expression, local: Localization) -> Option<Self> {
        if head.is_variable() {
            Some(Self::Atom { head, body, local })
        } else {
            None
        }
    }
    fn call(head: Expression, body: Expression, local: Localization) -> Option<Self> {
        //todo
        match head {
            Expression::Call { .. } => Some(Self::Call { head, body, local }),
            _ => None,
        }
        .filter(|x| {
            x.get_head()
                .get_call_args()
                .unwrap()
                .into_iter()
                .all(|x| x.is_number() || x.is_variable() || x.is_struct_call())
        })
    }

    fn lazy_call(head: Expression, body: Expression, local: Localization) -> Option<Self> {
        //todo
        match head {
            Expression::LazyCall { .. } => Some(Self::LazyCall { head, body, local }),
            _ => None,
        }
    }
    fn struct_call(head: Expression, body: Expression, local: Localization) -> Option<Self> {
        //todo
        match head {
            Expression::StructCall { .. } => Some(Self::StructCall { head, body, local }),
            _ => None,
        }
        .filter(|x| {
            x.get_head()
                .get_struct_call_args()
                .unwrap()
                .into_iter()
                .all(|x| x.is_number() || x.is_variable() || x.is_struct_call())
        })
    }
}

impl Expression {
    fn eval_primitive(name: &str, args: &Vec<Expression>) -> impl Fn(usize, usize) -> usize {
        let bin = match (name, args.len()) {
            ("+", 2) => |x, y| x + y,
            ("-", 2) => |x, y| x - y,
            ("/", 2) => |x, y| x / y,
            ("*", 2) => |x, y| x * y,
            ("mod", 2) => |x, y| x % y,
            ("max", 2) => |x: usize, y| x.max(y),
            ("min", 2) => |x: usize, y| x.min(y),
            ("band", 2) => |x, y| x & (y),
            ("bor", 2) => |x, y| x | (y),
            ("bxor", 2) => |x, y| x ^ (y),
            ("shr", 2) => |x, y| x >> (y),
            ("shl", 2) => |x, y| x << (y),
            ("lesser", 2) => |x, y| (x < y) as usize,
            ("greater", 2) => |x, y| (x > y) as usize,
            ("leq", 2) => |x, y| (x <= y) as usize,
            ("geq", 2) => |x, y| (x >= y) as usize,
            ("eq", 2) => |x, y| (x == y) as usize,
            _ => panic!("this shouldnt happen"),
        };
        bin
    }

    fn eval(&self, env: &Env) -> Result<Self, EvalError> {
        match self.eval_aux(env) {
            Ok(x) => {
                if !x.is_call() && !x.is_lazy_call() && !x.is_variable() {
                    return Ok(x);
                } else {
                    return x.eval_aux(env);
                }
            }
            err @ Err(_) => return err.clone(),
        }
    }

    fn eval_aux(&self, env: &Env) -> Result<Self, EvalError> {
        let MAP: HashSet<&str> = HashSet::from([
            "+", "-", "/", "*", "mod", "max", "min", "band", "bor", "bxor", "shr", "shl", "lesser",
            "greater", "leq", "geq", "eq",
        ]);

        let ret = match self {
            expr @ Expression::Number { value, local } => Ok(expr.clone()),

            Expression::Call { name, args, local } => {
                if env.call_map.contains_key(name) || MAP.contains(&name.as_str()) {
                    let mut new_args = Vec::new();
                    new_args.reserve(args.len());
                    for arg in args {
                        new_args.push(arg.eval(env)?);
                    }
                    if MAP.contains(&name.as_str())
                        && new_args.len() == 2
                        && new_args.iter().all(Expression::is_number)
                    {
                        let a = new_args.get(0).unwrap().get_int_val().unwrap();
                        let b = new_args.get(1).unwrap().get_int_val().unwrap();
                        let c = Self::eval_primitive(name.as_str(), &new_args)(a, b);
                        return Ok(Expression::number(c, *local));
                    }
                    if !env.call_map.contains_key(name) {
                        return Err(EvalError::GenericErr(
                            *local,
                            "Call <{name}> is not defined",
                        ));
                    }
                    let new_expr = Expression::call(name.to_owned(), new_args, *local);

                    for (head, body) in env.call_map.get(name).unwrap().iter() {
                        let mut bindings = HashMap::default();
                        if head.pattern_match(&new_expr, &mut bindings) {
                            return Ok(body.replace_var(&bindings));
                        }
                    }
                    return Err(EvalError::GenericErr(
                        *local,
                        "Call  did not match any of the patterns",
                    ));
                } else {
                    return Err(EvalError::GenericErr(
                        *local,
                        "Call <{name}> is not defined",
                    ));
                }
            }
            expr @ Expression::LazyCall { name, local, .. } => {
                if env.lazy_call_map.contains_key(name) {
                    for (head, body) in env.lazy_call_map.get(name).unwrap().iter() {
                        let mut bindings = HashMap::default();

                        if head.pattern_match(expr, &mut bindings) {
                            return Ok(body.replace_var(&bindings));
                        }
                    }
                    return Err(EvalError::GenericErr(
                        *local,
                        "Lazy Call did not match any of the patterns",
                    ));
                } else {
                    return Err(EvalError::GenericErr(*local, "Lazy Call is not defined"));
                }
            }
            Expression::StructCall { name, local, args } => {
                if env.struct_map.contains_key(name) {
                    if env
                        .struct_map
                        .get(name)
                        .unwrap()
                        .iter()
                        .any(|(pat, _)| pat.get_struct_call_args().unwrap().len() == args.len())
                    {
                        let iterator = args
                            .iter()
                            .map(|x| x.eval(env))
                            .filter(|x| x.is_ok())
                            .map(|x| x.unwrap());
                        if iterator.clone().count() == args.len() {
                            return Ok(Expression::struct_call(
                                name.into(),
                                iterator.collect(),
                                *local,
                            ));
                        }
                    }
                }
                {
                    return Err(EvalError::GenericErr(*local, "Struct is not defined"));
                }
            }
            expr @ Expression::Variable { name, .. } => {
                if env.const_map.contains_key(name) {
                    Ok(env.const_map.get(name).unwrap().clone())
                } else {
                    Ok(expr.clone())
                }
            }
        };

        ret
    }

    fn get_all_call_args(&self) -> Option<Vec<Self>> {
        match self {
            Expression::Number { .. } => None,
            Expression::Call { args, .. } => Some(args.clone()),
            Expression::LazyCall { args, .. } => Some(args.clone()),
            Expression::StructCall { args, .. } => Some(args.clone()),
            Expression::Variable { .. } => None,
        }
    }
    fn get_all_call_names(&self) -> Option<String> {
        match self {
            Expression::Number { .. } => None,
            Expression::Call { name, .. } => Some(name.clone()),
            Expression::LazyCall { name, .. } => Some(name.clone()),
            Expression::StructCall { name, .. } => Some(name.clone()),
            Expression::Variable { .. } => None,
        }
    }
    fn get_all_names(&self) -> Option<String> {
        match self {
            Expression::Number { .. } => None,
            Expression::Call { name, .. } => Some(name.clone()),
            Expression::LazyCall { name, .. } => Some(name.clone()),
            Expression::StructCall { name, .. } => Some(name.clone()),
            Expression::Variable { name, .. } => Some(name.clone()),
        }
    }
    fn get_variable_name(&self) -> Option<String> {
        match self {
            Expression::Variable { name, .. } => Some(name.clone()),
            _ => None,
        }
    }
    fn get_call_name(&self) -> Option<String> {
        match self {
            Expression::Call { name, .. } => Some(name.clone()),
            _ => None,
        }
    }
    fn get_lazy_call_name(&self) -> Option<String> {
        match self {
            Expression::LazyCall { name, .. } => Some(name.clone()),
            _ => None,
        }
    }
    fn get_struct_call_name(&self) -> Option<String> {
        match self {
            Expression::StructCall { name, .. } => Some(name.clone()),
            _ => None,
        }
    }
    fn get_call_args(&self) -> Option<Vec<Self>> {
        match self {
            Expression::Call { args, .. } => Some(args.clone()),
            _ => None,
        }
    }
    fn get_int_val(&self) -> Option<usize> {
        match self {
            Expression::Number { value, .. } => Some(*value),
            _ => None,
        }
    }

    fn get_lazy_call_args(&self) -> Option<Vec<Self>> {
        match self {
            Expression::LazyCall { args, .. } => Some(args.clone()),
            _ => None,
        }
    }
    fn get_struct_call_args(&self) -> Option<Vec<Self>> {
        match self {
            Expression::StructCall { args, .. } => Some(args.clone()),
            _ => None,
        }
    }

    fn is_variable(&self) -> bool {
        match self {
            Self::Variable { .. } => true,
            _ => false,
        }
    }
    fn is_number(&self) -> bool {
        match self {
            Self::Number { .. } => true,
            _ => false,
        }
    }
    fn is_call(&self) -> bool {
        match self {
            Self::Call { .. } => true,
            _ => false,
        }
    }
    fn is_lazy_call(&self) -> bool {
        match self {
            Self::LazyCall { .. } => true,
            _ => false,
        }
    }
    fn is_struct_call(&self) -> bool {
        match self {
            Self::StructCall { .. } => true,
            _ => false,
        }
    }

    fn equal(&self, other: &Self) -> bool {
        match (self, other) {
            (Expression::Number { value: v1, .. }, Expression::Number { value: v2, .. }) => {
                v1 == v2
            }
            (
                Expression::Call {
                    name: n1, args: a1, ..
                },
                Expression::Call {
                    name: n2, args: a2, ..
                },
            ) => n1 == n2 && a1.iter().zip(a2).all(|(x, y)| x.equal(y)),
            (
                Expression::LazyCall {
                    name: n1, args: a1, ..
                },
                Expression::LazyCall {
                    name: n2, args: a2, ..
                },
            ) => n1 == n2 && a1.iter().zip(a2).all(|(x, y)| x.equal(y)),
            (
                Expression::StructCall {
                    name: n1, args: a1, ..
                },
                Expression::StructCall {
                    name: n2, args: a2, ..
                },
            ) => n1 == n2 && a1.iter().zip(a2).all(|(x, y)| x.equal(y)),
            (Expression::Variable { name: n1, .. }, Expression::Variable { name: n2, .. }) => {
                n1 == n2
            }
            _ => false,
        }
    }

    fn pattern_match(&self, other: &Self, bindings: &mut HashMap<String, Expression>) -> bool {
        match (self, other) {
            (
                Expression::Number {
                    value: v1,
                    local: _,
                },
                Expression::Number {
                    value: v2,
                    local: _,
                },
            ) => v1 == v2,
            (
                Expression::Call {
                    name: n1,
                    args: a1,
                    local: _,
                },
                Expression::Call {
                    name: n2,
                    args: a2,
                    local: _,
                },
            )
            | (
                Expression::StructCall {
                    name: n1,
                    args: a1,
                    local: _,
                },
                Expression::StructCall {
                    name: n2,
                    args: a2,
                    local: _,
                },
            ) => {
                if n1 != n2 || a1.len() != a2.len() {
                    return false;
                }
                for (a, b) in a1.iter().zip(a2.iter()) {
                    if !a.pattern_match(b, bindings) {
                        return false;
                    }
                }
                return true;
            }

            (
                Expression::LazyCall {
                    name: n1,
                    args: a1,
                    local: _,
                },
                Expression::Call {
                    name: n2,
                    args: a2,
                    local: _,
                },
            )
            | (
                Expression::LazyCall {
                    name: n1,
                    args: a1,
                    local: _,
                },
                Expression::LazyCall {
                    name: n2,
                    args: a2,
                    local: _,
                },
            ) => {
                if n1 != n2 || a1.len() != a2.len() {
                    return false;
                }
                for (a, b) in a1.iter().zip(a2.iter()) {
                    if !a.pattern_match(b, bindings) {
                        return false;
                    }
                }
                return true;
            }
            (
                Expression::Variable { name, local: _ },
                expr @ Expression::Number { value: _, local: _ },
            ) => {
                if bindings.contains_key(name) {
                    return expr.equal(bindings.get(name.into()).unwrap());
                }
                bindings.insert(name.into(), expr.clone());
                return true;
            }
            (
                Expression::Variable { name, local: _ },
                expr @ Expression::Call {
                    name: _,
                    args: _,
                    local: _,
                },
            ) => {
                if bindings.contains_key(name) {
                    return expr.equal(bindings.get(name.into()).unwrap());
                }
                bindings.insert(name.into(), expr.clone());
                return true;
            }
            (
                Expression::Variable { name, local: _ },
                expr @ Expression::StructCall {
                    name: _,
                    args: _,
                    local: _,
                },
            ) => {
                if bindings.contains_key(name) {
                    return expr.equal(bindings.get(name.into()).unwrap());
                }
                bindings.insert(name.into(), expr.clone());
                return true;
            }
            (
                Expression::Variable { name, local: _ },
                expr @ Expression::Variable { name: _, local: _ },
            ) => {
                bindings.insert(name.into(), expr.clone());
                return true;
            }
            _ => false,
        }
    }

    fn replace_var(&self, bindings: &HashMap<String, Expression>) -> Self {
        match self {
            expr @ Expression::Number { value: _, local: _ } => expr.to_owned(),
            Expression::Call { name, args, local } => Expression::Call {
                name: name.into(),
                args: args.iter().map(|x| x.replace_var(bindings)).collect(),
                local: *local,
            },
            Expression::LazyCall { name, args, local } => Expression::LazyCall {
                name: name.into(),
                args: args.iter().map(|x| x.replace_var(bindings)).collect(),
                local: *local,
            },
            Expression::StructCall { name, args, local } => Expression::StructCall {
                name: name.into(),
                args: args.iter().map(|x| x.replace_var(bindings)).collect(),
                local: *local,
            },
            expr @ Expression::Variable { name, local: _ } => {
                if bindings.contains_key(name) {
                    bindings.get(name).unwrap().clone()
                } else {
                    expr.clone()
                }
            }
        }
    }

    fn number(value: usize, local: Localization) -> Self {
        Self::Number { value, local }
    }
    fn call(name: String, args: Vec<Expression>, local: Localization) -> Self {
        Self::Call { name, args, local }
    }
    fn lazy_call(name: String, args: Vec<Expression>, local: Localization) -> Self {
        Self::LazyCall { name, args, local }
    }
    fn struct_call(name: String, args: Vec<Expression>, local: Localization) -> Self {
        Self::StructCall { name, args, local }
    }

    fn variable(name: impl Into<String>, local: Localization) -> Self {
        Self::Variable {
            name: name.into(),
            local,
        }
    }

    fn get_location(&self) -> Localization {
        match self {
            Expression::Number { local, .. }
            | Expression::Call { local, .. }
            | Expression::LazyCall { local, .. }
            | Expression::StructCall { local, .. }
            | Expression::Variable { local, .. } => *local,
        }
    }

    fn display_as_tree(&self, n: usize) {
        (0..n).for_each(|_| print!("--"));
        match self {
            Expression::Number { value, local } => {
                println!(
                    " {value} : Number @ {}:{} -- {}:{}",
                    local.start_line, local.start_col, local.end_line, local.end_col
                )
            }
            Expression::Call { name, args, local } => {
                println!(
                    "Call <{name}> @ {}:{} -- {}:{}",
                    local.start_line, local.start_col, local.end_line, local.end_col
                );
                args.iter().for_each(|e| e.display_as_tree(n + 2))
            }
            Expression::LazyCall { name, args, local } => {
                println!(
                    "LazyCall <{name}> @ {}:{} -- {}:{}",
                    local.start_line, local.start_col, local.end_line, local.end_col
                );
                args.iter().for_each(|e| e.display_as_tree(n + 2))
            }
            Expression::StructCall { name, args, local } => {
                println!(
                    "StructCall <{name}> @ {}:{} -- {}:{}",
                    local.start_line, local.start_col, local.end_line, local.end_col
                );
                args.iter().for_each(|e| e.display_as_tree(n + 2))
            }
            Expression::Variable { name, local } => {
                println!(
                    " {name} : Variable @ {}:{} -- {}:{}",
                    local.start_line, local.start_col, local.end_line, local.end_col
                )
            }
        }
    }
}

use parser_combinator::either::Either;
//Parsers
use parser_combinator::parser::{match_anything, match_literal};
use parser_combinator::*;

#[derive(Clone, Debug)]
pub enum ParseErrors<'a> {
    GenericErr(Localization, &'static str, Chars<'a>),
    Empty,
}

#[derive(Clone, Debug)]
pub enum EvalError {
    GenericErr(Localization, &'static str),
    Empty,
}

//Tokens
pub fn whitespace<'a>(
    input: Chars<'a>,
    state: Localization,
) -> parser_combinator::ParseResult<Chars<'a>, Localization, char, ParseErrors> {
    let space = match_anything(|local: Localization| local.advance_by(0, 1)).validate(
        |character: &char| character == &' ',
        "alphabetic character".to_string(),
    );
    let newline = match_anything(|local: Localization| local.advance_by(1, 0)).validate(
        |character: &char| character == &'\n',
        "alphabetic character".to_string(),
    );

    space
        .or_else(newline)
        .with_error_using_state(|(_a, _b), state, rest| {
            ParseErrors::GenericErr(state, "whitespace failed", rest)
        })
        .parse(input, state)
}

macro_rules! token_implementer {
    ($type:ident,$repr:literal) => {
        #[derive(Clone, Debug, Eq, PartialEq)]
        pub struct $type;

        impl<'a> Parse<'a, Chars<'a>, Localization, (String, Localization), ParseErrors<'a>>
            for $type
        {
            fn parse(
                &self,
                input: Chars<'a>,
                state: Localization,
            ) -> ParseResult<'a, Chars<'a>, Localization, (String, Localization), ParseErrors<'a>>
            {
                match_literal($repr.chars(), |local: Localization| {
                    local.advance_by(0, $repr.len())
                })
                .transform_with_state(|id, state| (id.as_str().to_string(), state))
                .with_error_using_state(|_, state, input| {
                    ParseErrors::GenericErr(state, $repr, input)
                })
                .skip(whitespace)
                .parse(input, state)
            }
        }
    };

    ($type:ident,$repr:expr) => {
        #[derive(Clone, Debug, Eq, PartialEq)]
        pub struct $type;

        impl<'a> Parse<'a, Chars<'a>, Localization, (String, Localization), ParseErrors<'a>>
            for $type
        {
            fn parse(
                &self,
                input: Chars<'a>,
                state: Localization,
            ) -> ParseResult<'a, Chars<'a>, Localization, (String, Localization), ParseErrors<'a>>
            {
                let predicate = $repr;
                (token_with_predicate(predicate).skip(whitespace)).parse(input, state)
            }
        }
    };
}

fn token_with_predicate(
    predicate: impl Fn(&char) -> bool,
) -> impl for<'a> Fn(
    Chars<'a>,
    Localization,
)
    -> ParseResult<'a, Chars<'a>, Localization, (String, Localization), ParseErrors<'a>> {
    move |mut input: Chars<'_>, mut state: Localization| {
        let old_state = state;
        let state_transformer = |_: &char, local: Localization| local.advance_by(0, 1);
        let mut orig = input.clone();
        let mut c = 0;
        match input.next() {
            Some(x) => {
                if !predicate(&x) {
                    return Err(ParseErrors::GenericErr(state, "lowercase variable", orig));
                }
            }
            None => return Err(ParseErrors::GenericErr(state, "lowercase variable", orig)),
        }
        let orig2 = orig.clone();
        let mut orig3 = orig.clone();
        loop {
            match orig.next() {
                Some(x) => {
                    if !predicate(&x) {
                        break;
                    } else {
                        c += 1;
                        state = state_transformer(&x, state);
                        orig3.next();
                    }
                }
                None => break,
            }
        }
        let s = orig2.as_str();
        Ok(((s[..c].to_string(), old_state), state, orig3))
    }
}

token_implementer!(PlusToken, "+");
token_implementer!(MinusToken, "-");
token_implementer!(StarToken, "*");
token_implementer!(SlashToken, "/");
token_implementer!(CarrotToken, "^");
token_implementer!(LParenToken, "(");
token_implementer!(RParenToken, ")");
token_implementer!(LBracketToken, "[");
token_implementer!(RBracketToken, "]");
token_implementer!(LBracetToken, "{");
token_implementer!(RBracetToken, "}");

token_implementer!(DotToken, ".");
token_implementer!(CommaToken, ",");
token_implementer!(EqualToken, "=");
token_implementer!(SemiColonToken, ";");
token_implementer!(LowerCaseToken, |character: &char| character.is_alphabetic()
    && character.is_ascii_lowercase());
token_implementer!(IntegerToken, |character: &char| character.is_numeric());

//Grammar

macro_rules! grammar_snippet {
    //TODO s
    // well just think about this more it could be represented more elegantly.
    // It would be cool if we could merge snippets into one big grammar macro with all kinds of special syntax
    //  But thats for future
    //  An easy quality of life improvement would using closures fix variable names in the macro itself for production and error
    //  And therefore allievieating need to supply a closure
    ($type:ident :=  $rule:expr ; production := $production:expr ) => {
        #[derive(Clone, Debug, Eq, PartialEq)]
        pub struct $type;

        impl<'a> Parse<'a, Chars<'a>, Localization, Expression, ParseErrors<'a>> for $type {
            fn parse(
                &self,
                input: Chars<'a>,
                state: Localization,
            ) -> ParseResult<'a, Chars<'a>, Localization, Expression, ParseErrors<'a>> {
                $rule
                    .transform_with_state($production)
                    .skip(whitespace)
                    .parse(input, state)
            }
        }
    };

    ($type:ident :=  $rule:expr  ; precondition :=$cond:expr ;error :=   $err:expr ) => {
        #[derive(Clone, Debug, Eq, PartialEq)]
        pub struct $type;

        impl<'a> Parse<'a, Chars<'a>, Localization, Expression, ParseErrors<'a>> for $type {
            fn parse(
                &self,
                input: Chars<'a>,
                state: Localization,
            ) -> ParseResult<'a, Chars<'a>, Localization, Expression, ParseErrors<'a>> {
                if !$cond(input.clone(), state.clone()) {
                    return $err;
                }
                $rule
                    .skip(whitespace)
                    /*.transform(|x| {
                        println!("{} :    {:?}", stringify!($type), x);
                        x
                    })*/
                    .parse(input, state)
            }
        }
    };

    ($type:ident :=  $rule:expr  ; error :=   $err:expr) => {
        #[derive(Clone, Debug, Eq, PartialEq)]
        pub struct $type;

        impl<'a> Parse<'a, Chars<'a>, Localization, Expression, ParseErrors<'a>> for $type {
            fn parse(
                &self,
                input: Chars<'a>,
                state: Localization,
            ) -> ParseResult<'a, Chars<'a>, Localization, Expression, ParseErrors<'a>> {
                $rule
                    .with_error_using_state($err)
                    /*.transform(|x| {
                        println!("{} :    {:?}", stringify!($type), x);
                        x
                    })*/
                    .skip(whitespace)
                    .parse(input, state)
            }
        }
    };

    ($type:ident :=  $rule:expr  ; production := $production:expr ; error :=   $err:expr     ) => {
        #[derive(Clone, Debug, Eq, PartialEq)]
        pub struct $type;

        impl<'a> Parse<'a, Chars<'a>, Localization, Expression, ParseErrors<'a>> for $type {
            fn parse(
                &self,
                input: Chars<'a>,
                state: Localization,
            ) -> ParseResult<'a, Chars<'a>, Localization, Expression, ParseErrors<'a>> {
                $rule
                    .transform_with_state($production)
                    .with_error_using_state($err)
                    /*.transform(|x| {
                        println!("{} :    {:?}", stringify!($type), x);
                        x
                    })*/
                    .skip(whitespace)
                    .parse(input, state)
            }
        }
    };

    ($type:ident of $retType:ident :=  $rule:expr  ; production := $production:expr ; error :=   $err:expr     ) => {
        #[derive(Clone, Debug, Eq, PartialEq)]
        pub struct $type;

        impl<'a> Parse<'a, Chars<'a>, Localization, $retType, ParseErrors<'a>> for $type {
            fn parse(
                &self,
                input: Chars<'a>,
                state: Localization,
            ) -> ParseResult<'a, Chars<'a>, Localization, $retType, ParseErrors<'a>> {
                $rule
                    .transform_with_state($production)
                    .with_error_using_state($err)
                    /*.transform(|x| {
                            println!("{} :    {:?}", stringify!($type), x);
                            x
                    })*/
                    .skip(whitespace)
                    .parse(input, state)
            }
        }
    };
}

pub fn build_right_assoc<'a>(
    list: (Expression, Vec<((String, Localization), Expression)>),
    state: Localization,
) -> Expression {
    let (first, rest) = list;
    if rest.is_empty() {
        first
    } else {
        rest.into_iter().fold(first, |left, (op, right)| {
            let loc = left.get_location().combine_last(&right.get_location());
            Expression::call(op.0, vec![left, right], loc)
        })
    }
}

pub fn build_left_assoc<'a>(
    tree: Either<(Expression, (String, Localization), Expression), Expression>,
    _state: Localization,
) -> Expression {
    match tree {
        Either::Left((x, name, y)) => {
            let loc = x.get_location().combine_last(&y.get_location());
            let args = vec![x, y];
            Expression::call(name.0, args, loc)
        }
        Either::Right(x) => x,
    }
}

grammar_snippet! {
    Toplevel   of Program      := Def.either(Expr).separated_by(SemiColonToken);
    production := |x,_| {
        let mut exprs  = vec![];
        let mut defs  = vec![];

        match x.0 {
            Either::Left(def) => {defs.push(def.clone())},
            Either::Right(expr) => {exprs.push(expr.clone())},
        }
        x.1.iter().for_each(|(_,a)|
                            match a {
                                Either::Left(def) => {defs.push(def.clone())},
                                Either::Right(expr) => {exprs.push(expr.clone())},
                            }
        );
        Program::from_defs_and_exprs(defs, exprs)
    };
    error      := |err,st,rest|  ParseErrors::Empty
}

grammar_snippet! {
    Def of Definition    :=  (StructCallExpr.or_else(LazyCallExpr).or_else(CallExpr).or_else(VarExpr),EqualToken , Expr)
        .transform_with_state(|x, local |Definition::from_expression(&x.0, &x.2, local))
        .with_error_using_state(|_err,st,rest| ParseErrors::GenericErr(st, "definition", rest))
        .validate(|x| x.is_some(), ParseErrors::Empty);


    production := |x,_local| x.unwrap();

    error      := |_err,st,rest| ParseErrors::GenericErr(st, "definition", rest)
}

grammar_snippet! {
    Expr         := SumExpr;
    precondition := |mut input : Chars,_| input.any(|char| !char.is_whitespace());
    error        :=  Err(ParseErrors::Empty)
}

grammar_snippet! {
    SumExpr    := Self.right_assoc(SubExpr, PlusToken);
    production := build_right_assoc;
    error      := |_err,st,rest| ParseErrors::GenericErr(st, "sum", rest)
}

grammar_snippet! {
    SubExpr    := Self.right_assoc(MulExpr, MinusToken);
    production := build_right_assoc;
    error      := |_err,st,rest| ParseErrors::GenericErr(st, "subtraction", rest)
}

grammar_snippet! {
    MulExpr    := Self.right_assoc(DivExpr, StarToken);
    production := build_right_assoc;
    error      := |_err,st,rest| ParseErrors::GenericErr(st, "multiplication", rest)
}

grammar_snippet! {
    DivExpr    := Self.right_assoc(ExpExpr, SlashToken);
    production := build_right_assoc;
    error      := |_err,st,rest| ParseErrors::GenericErr(st, "division", rest)
}

grammar_snippet! {
    ExpExpr    := Self.left_assoc(CarrotToken, AtomicExpr.or_else(NegExpr));
    production := build_left_assoc;
    error      := |_err,st,rest| ParseErrors::GenericErr(st, "exponentiation", rest)
}

grammar_snippet! {
    NegExpr    := MinusToken.pair(Expr);
    production := |(name,x),_state| { let local = name.1;let args = vec![x];  Expression::call ( name.0.to_string(), args, local )};
    error      := |_err,st,rest| ParseErrors::GenericErr(st, "negation", rest)
}

grammar_snippet! {
    AtomicExpr    := StructCallExpr.or_else(LazyCallExpr).or_else(CallExpr).or_else(NumExpr).or_else(VarExpr).or_else(BracketedExpr);
    error         := |_err,st,rest| ParseErrors::GenericErr(st, "atom", rest)
}

grammar_snippet! {
    BracketedExpr    := LParenToken.triple(Expr, RParenToken).second();
    error            := |_err,st,rest| ParseErrors::GenericErr(st, "bracketed", rest)
}

grammar_snippet! {
    CallExpr    := LowerCaseToken.pair(LParenToken.triple(Expr.separated_by(CommaToken), RParenToken));
    production := | (name,(_, (expr, vec), (_, local2))),_state|
    {
        let mut args = vec![expr];
        args.extend(vec.into_iter().map(|(_,x)| x));
        Expression::call ( name.0,args, local2 )
    };
    error      := |_err,st,rest| ParseErrors::GenericErr(st, "Call", rest)
}

grammar_snippet! {
    LazyCallExpr    := LowerCaseToken.pair(LBracketToken.triple(Expr.separated_by(CommaToken), RBracketToken));
    production := | (name,(_, (expr, vec), (_, local2))),_state|
    {
        let mut args = vec![expr];
        args.extend(vec.into_iter().map(|(_,x)| x));
        Expression::lazy_call ( name.0,args, local2 )
    };
    error      := |_err,st,rest| ParseErrors::GenericErr(st, "Call", rest)
}

grammar_snippet! {
    StructCallExpr    := LowerCaseToken.pair(LBracetToken.triple(Expr.separated_by(CommaToken), RBracetToken));
    production := | (name,(_, (expr, vec), (_, local2))),_state|
    {
        let mut args = vec![expr];
        args.extend(vec.into_iter().map(|(_,x)| x));
        Expression::struct_call ( name.0,args, local2 )
    };
    error      := |_err,st,rest| ParseErrors::GenericErr(st, "Call", rest)
}

grammar_snippet! {
    NumExpr := IntegerToken;
    production := |(val, local), state|

    {
        let value = str::parse::<usize>(&val).unwrap();
                                       Expression::number(value, local.combine_last(&state))
    };
    error      := |_err,st,rest| ParseErrors::GenericErr(st, "Number", rest)
}

grammar_snippet! {
    VarExpr := LowerCaseToken;
    production := |(name, local), state| Expression::variable(name, local.combine_last(&state));
    error      := |_err,st,rest| ParseErrors::GenericErr(st, "Variable", rest)
}

#[test]
fn number_expr_should_suceed() {
    let a = "123    ".chars();
    let res = SumExpr.parse(a, Localization::default()).unwrap();
    println!("{:?}", res);
    assert!(false)
}

#[test]
fn var_expr_should_suceed() {
    let a = "xey    ".chars();
    let res = SumExpr.parse(a, Localization::default()).unwrap();
    println!("{:?}", res);
    assert!(false)
}

#[test]
fn sum_expr_should_suceed() {
    let a = " 1 *  2 + 3 * 4 + 5 +  6 + 7 + 8".chars();
    let res = SumExpr.parse(a, Localization::default()).unwrap();
    println!("{:?}", res);
    assert!(false)
}

fn repl() {
    use std::io::Write;
    use std::io::{self, BufRead};
    let stdin = io::stdin();
    let mut env = Env::default();
    loop {
        let mut buf = String::new();

        stdin.read_line(&mut buf);
        println!("\nParsing___________________________ ");

        println!("\nRead :");
        println!("         {buf}");
        if let Ok(a) = Toplevel.parse(buf.chars(), Localization::default()) {
            println!("\nLeft :");
            println!("         {}", a.2.as_str());
            //println!("{:#?}",&a.0);
            println!("{}", &a.0);
            //a.0.display_as_tree(2);
            println!("\nEvaluating________________________ ");
            a.0.eval_with_env(&mut env)
                .iter()
                .for_each(|result| match result {
                    Ok(x) => {
                        println!("\n   SUCCESS!. ");
                        //println!("{:4^-#?}",&x);
                        println!("    {x}");
                    }
                    Err(err) => {
                        //println!("\n   FAIL!. \n  {:4>#?}", err);
                        println!("\n   FAIL!. \n  {:?}", err);
                    }
                });
        }
        print!("___________________________________________________________________\n>> ");
        io::stdout().flush().unwrap();
    }
}
fn main() {
    repl();
}

fn main10() {
    use std::io::{self, BufRead};
    let stdin = io::stdin();
    let mut env = Env::default();

    for line in stdin.lock().lines() {
        print!("\n>> ");
        let lines = line.unwrap();
        println!("\nRead :");
        println!("       :{lines}");
        if let Ok(a) = Toplevel.parse(lines.chars(), Localization::default()) {
            println!("\nLeft :");
            println!("     : {} \nResult\n", a.2.as_str());
            println!("{:#?}", &a.0);
            //a.0.display_as_tree(2);
            a.0.eval_with_env(&mut env)
                .iter()
                .for_each(|result| match result {
                    Ok(x) => {
                        println!("\nSUCCESS!. \n");
                        println!("{:#?}", &x);
                    }
                    Err(err) => {
                        println!("\nFAIL!. \n\n\n{:?}", err);
                    }
                });
        }
        print!("\n>> ");
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut fun = |name: &String, args: &Vec<Expression>, sep1: &str, sep2: &str| {
            write!(f, " {name}{sep1}");
            let s: String = args
                .iter()
                .map(|x| format!(" {} ", x))
                .intersperse(",".to_string())
                .collect();
            write!(f, "{}", s);
            write!(f, "{sep2} ")
        };
        match self {
            Expression::Number { value, local } => write!(f, " {value} "),
            Expression::Call { name, args, local } => fun(name, args, "(", ")"),
            Expression::LazyCall { name, args, local } => fun(name, args, "[", "]"),
            Expression::StructCall { name, args, local } => fun(name, args, "{", "}"),
            Expression::Variable { name, local } => write!(f, " {name} "),
        }
    }
}

impl Display for Definition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Definition::Atom { head, body, .. }
            | Definition::Call { head, body, .. }
            | Definition::LazyCall { head, body, .. }
            | Definition::StructCall { head, body, .. } => write!(f, "{head} = {body}"),
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Program         :\n")?;
        write!(f, "    Definitions :\n")?;
        for x in self.defs.as_slice() {
            write!(f, "                   {x}")?;
        }
        write!(f, "\n    Expressions :\n")?;
        for x in self.exprs.as_slice() {
            write!(f, "                   {x}")?;
        }
        write!(f, "\nEnd")
    }
}

fn main3() {
    use std::io::{self, BufRead};
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        print!(">> ");
        let lines = line.unwrap();
        println!("Result :");
        println!("       :{lines}");
        let a = Expr.parse(lines.chars(), Localization::default()).unwrap();
        println!("       :{}", a.2.as_str());
        println!("       :{:#?}", a.0);
        //a.0.display_as_tree(0)
    }
}

fn build_ui() -> impl Widget<()> {
    Label::new("Hello world")
}

fn main2() {
    let main_window = WindowDesc::new(|| build_ui())
        .window_size((600.0, 400.0))
        .title("My first Druid App");
    let initial_data = ();

    AppLauncher::with_window(main_window)
        .launch(initial_data)
        .expect("Failed to launch application");
}
