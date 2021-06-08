//! The eval module to evaluate abyss expressions
#![allow(dead_code)]

use crate::abyss;
use abyss::object::Object;
use abyss::object::Env;
use crate::logic::Unifiable;
//use super::parser;
use std::rc::Rc;
use abyss::config::HashMap;
use abyss::eval::core::*;
pub use abyss::object::EvalError;
use std::borrow::Borrow;

//(let ((fact (lambda (n) (case n ((0 1) (n (+ n (fact (- n 1))))))))) (fact 10000))




fn eval_atom(op: Object, a: Object, b: Object, env: &mut Env) -> Result<Object> {
    use Object::*;
    use atom::*;
    match op {
        Var(s) => {
            let mut xs = vec![];
            xs.push(evaluate(a, env)?);
            xs.push(evaluate(b, env)?);
            match &s[..] {
                "+" | "-" | "*" | "/" => eval_arith(&s, &xs),
                "<"  => eval_lt(&xs),
                ">"  => eval_gt(&xs),
                "<=" => eval_le(&xs),
                ">=" => eval_ge(&xs),
                "==" => eval_eq(&xs),
                "/=" => eval_ne(&xs),
                unknown => Err(EvalError { msg: format!("Atom evaluation error: unknown atom operator: {:?}", unknown) })
            }
        },
        _ => Err(EvalError { msg: format!("Atom evaluation error: evaluating {:?}", op) })
    }
}



/// Evaluate arithmetic expressions.
fn eval_if(cond: Object, x: Object, y: Object, env: &mut Env) -> Result<Object> {
    use Object::*;

    let cond = evaluate(cond, env)?;
    match cond {
        Var(s) if &s[..] == "True"  => evaluate(x, env),
        Var(s) if &s[..] == "False" => evaluate(y, env),
        _ => Err(EvalError { msg: format!("If expression error: {:?}", cond) })
    }
}


#[inline]
fn eval_case(expr: Object, pat: Object, res: Object, env: &mut Env) -> Option<Result<Object>> {
    let bind = pat.unify(&expr);
    if let Ok(mut bind) = bind {
        let bind: HashMap<String, Rc<Object>> = bind.iter_mut().map(|(k, v)| (k.clone(), Rc::new(v.clone()))).collect();
        env.extend(bind);
        Some(evaluate(res, env))
    } else {
        None
    }
}

/// Evaluate case(match) expressions
fn eval_cases(expr: Object, cases: &[Object], env: &mut Env) -> Result<Object> {
    //println!("\ncases");
    use Object::*;
    let expr = evaluate(expr, env)?;
    for case in cases {
        match case {
            List(xs) => match &xs[..] {
                [pat, res] => {
                    match eval_case(expr.clone(), pat.clone(), res.clone(), env) {
                        None => continue,
                        Some(res) => return res,
                    }
                },
                _ => return Err(EvalError {
                    msg: format!("Case bindings should have format of `[pat result]`, instead of {:?}", case) 
                }),
            },
            _ => return Err(EvalError { 
                msg: format!("Case bindings should have format of `[pat result]`, instead of {:?}", case) 
            }),
        }
    }
    Err(EvalError { msg: format!("Case expression error") })
}


pub fn bind(left: Object, right: Object, env: &mut Env) -> Result<()> {
    use Object::*;
    match (left, right) {
        (Var(s), expr) => {
            let v = evaluate(expr, env)?;
            //let v = wrap(expr.clone(), env.clone());
            let v = match v {
                Closure(None, ps, expr, env) => {
                    Closure(Some(s.clone()), ps, expr, env)
                },
                others => others,
            };
            env.insert(s.clone(), Rc::new(v.clone()));
        }
        (List(xs), expr) => match &xs[..] {
            [Var(f), ps @ ..] => {
                let lambda = weak(ps.to_vec(), expr.clone());
                bind(Var(f.clone()), lambda, env)?
            },
            _ => return Err(EvalError { msg: format!("Binding error: Invalid Binding {:?} and {:?}", List(xs), expr) })
        }
        (x, y) => return Err(EvalError { msg: format!("Binding error: Binding {:?} and {:?}", x, y) })
    }
    Ok(())
}



/// Handle bindings
fn bindings(bindings: &[Object], env: &mut Env) -> Result<()> {
    use Object::*;
    for binding in bindings {
        match binding {
            List(xs) => match &xs[..] {
                [def, expr] => {
                    bind(def.clone(), expr.clone(), env)?
                }
                _ => return Err(EvalError { msg: format!("Binding error: {:?}", binding) })
            },
            _ => return Err(EvalError { msg: format!("Binding error: {:?}", binding) })
        }
    }
    Ok(())
}


//(let ((gen (lambda (s n) (case n ((0 ()) (n (:: s (gen s (- n 1))))))))) (gen 'T_T 200))
#[inline]
fn apply_env(f: Object, name: &Option<String>, pat: &Object, x: &Object, env: &mut Env) -> Result<()> {
    let bind = pat.unify(&x);
    if let Ok(bind) = bind {
        let bind: HashMap<String, Rc<Object>> = bind.into_iter().map(|(k, v)| (k, Rc::new(v))).collect();
        if let Some(name) = name {
            env.insert(name.clone(), Rc::new(f));
        }
        env.extend(bind);
        Ok(())
    } else {
        Err(EvalError { msg: format!("Application error: Unification error: unifying {:?} and {:?}", pat, x) })
    }
}


#[inline]
fn wash_type(pat: &Object) -> &Object {
    use Object::*;
    match pat {
        List(xs) if match &xs[..] { 
            [Var(s), _x, _type] if &s[..] == ":" => true,
            _ => false,
         } => {
            &xs[1]
        },
        _ => pat
    }
}

/// Handle basic function application `(f x)`
/// 
/// `f` and `x` should have been evaluated (lazy?)!
fn apply(f: Object, x: Object) -> Result<Object> {
    use Object::*;
    
    //let g = f.clone();
    match &f {
        Closure(name, params, expr, env) => {
            // Unification should be invoked here, but we only allow single variable here to debug...
            let mut env = env.clone();
            
            match params.borrow() {
                List(ps) => match &ps[..] {
                    [ ] => if x == Nil {
                        if let Some(name) = name {
                            env.insert(name.clone(), Rc::new(f.clone()));
                        }
                        evaluate(Object::clone(expr.borrow()), &mut env)
                    } else {
                        Err(EvalError { msg: format!("Applying error: unexpected parameter: {}", x) })
                    },
                    [pat] => {
                        apply_env(f.clone(), name, wash_type(pat), &x, &mut env)?;
                        evaluate(Object::clone(expr.borrow()), &mut env)
                    },
                    [pat, ss @ ..] => {
                        apply_env(f.clone(), name, wash_type(pat), &x, &mut env)?;
                        Ok(Closure(None, Rc::new(List(ss.to_vec())), expr.clone(), env.clone()))
                    }
                },
                others => Err(EvalError { msg: format!("Function parameters should be a list instead of {:?}", others) })
            }
        }
        _ => Err(EvalError { msg: format!("Function application error: {:?}", f) })
    }
}



/// Handle cons expression
fn eval_cons(x: Object, xs: Object, env: &mut Env) -> Result<Object> {
    use Object::*;
    let xs = evaluate(xs, env)?;
    match xs {
        List(xs) => Ok(List(vec![Cons("::".into()), evaluate(x, env)?, List(xs)])),
        _ => Err(EvalError {msg: format!("Cons error: {:?}", xs)})
    }
}

/// Handle list constructions
fn eval_list(xs: &[Object], env: &mut Env) -> Result<Object> {
    let mut v = vec![];
    for x in xs {
        let x = evaluate(x.clone(), env)?;
        v.push(x);
    }
    Ok(Object::List(v))
}

/*
fn eval_head(xs: &Object, env: &mut Env) -> Result<Object> {
    match evaluate(xs, env)? {
        Object::List(xs) => match &xs[..] {
            [x, ..] => Ok(x.clone()),
            _ => Err(EvalError { msg: format!("Eval error: Can not get head of an empty list.") })
        },
        others => Err(EvalError { msg: format!("Eval error: Can not get head from {:?}", others) })
    }
}

fn eval_tail(xs: &Object, env: &mut Env) -> Result<Object> {
    //println!("{:?}", xs);
    match evaluate(xs, env)? {
        Object::List(xs) => match &xs[..] {
            [_, xs @ ..] => Ok(Object::List(xs.to_vec())),
            _ => Err(EvalError { msg: format!("Eval error: Can not get tail of an empty list.") })
        },
        others => Err(EvalError { msg: format!("Eval error: Can not get tail from {:?}", others) })
    }
}
*/

/// Thunk evaluation
#[inline]
pub fn eval_thunk(thunk: Object) -> Result<Object> {
    //println!("\nthunk: {:?}", thunk);
    use Object::*;
    match thunk {
        Thunk(name, thunk, mut env) => {
            if let Some(name) = name {
                let thunk = Thunk(Some(name.clone()), thunk.clone(), env.clone());
                env.insert(name.clone(), Rc::new(thunk));
            }
            if *thunk.evaluated.borrow() == true {
                return Ok(thunk.value());
            }
            let mut res = evaluate(thunk.value(), &mut env)?;
            if let Thunk(_, _, _) = res {
                res = eval_thunk(res)?;
            }
            let result = res.clone();
            //let p: &Thunker = expr.borrow();
            *thunk.as_ref().expr.borrow_mut() = res;
            Ok(result)
        },
        _ => Err(EvalError { msg: format!("Error while evaluating thunk: {:?}", thunk) })
    }
}


macro_rules! get {
    ($sx: expr, $msg: expr) => {
        match $sx {
            Some(x) => Ok(x),
            None => Err(EvalError { msg: format!("Getting {}", $msg) })
        }
    };
}


/// The main evaluate function to calculate all abyss expressions
pub fn evaluate(expr: Object, env: &mut Env) -> Result<Object> {
    use Object::*;
    //println!("\neval: {} in {:?}", expr, env);
    let ans = match expr {
        Nil         => Ok(Nil),
        Var(ref s) if atom::is_atom(s) => Ok(expr.clone()),
        Var(ref s)  => env.get(s).map(|x| (**x).clone()).ok_or(EvalError { msg: format!("No such variable: {}", s) }),
        Symbol(_)   => Ok(expr),
        Integer(_)  => Ok(expr),
        Real(_)     => Ok(expr),
        Str(_)      => Ok(expr),
        Thunk(_, _, _) => eval_thunk(expr),
        List(xs)       => {
            let mut it = xs.into_iter();
            let op = it.next();
            if let None = op {
                return Ok(List(vec![]));
            }
            match op.unwrap() {
                Var(op) if atom::is_atom_op(&op) => {
                    let (a, b) = (
                        get!(it.next(), format!("2nd argument of function: {}", op))?,
                        get!(it.next(), format!("3rd argument of function: {}", op))?,
                    );
                    eval_atom(Var(op), a, b, env)
                }
                Var(op) if &op[..] == "lambda" => {
                    let (ps, expr) = (
                        get!(it.next(), "parameters of lambda")?,
                        get!(it.next(), "expression of lambda")?,
                    );
                    Ok(Object::closure(ps, expr, env.clone()))
                },
                Var(op) if &op[..] == "lazy" => {
                    let x = get!(it.next(), "parameter of function: lazy")?;
                    wrap(None, x, env.clone())
                }
                Var(op) if &op[..] == "!" => {
                    let x = get!(it.next(), "parameter of function: !")?;
                    evaluate(x, env)
                }
                Var(op) if &op[..] == "if" => {
                    let (cond, x, y) = (
                        get!(it.next(), "condition of function: if"   )?,
                        get!(it.next(), "true branch of function: if" )?,
                        get!(it.next(), "false branch of function: if")?,
                    );
                    eval_if(cond, x, y, env)
                }
                Var(op) if &op[..] == "let" => {
                    let (bs, expr) = (
                        get!(it.next(), "bindings of let expression"  )?,
                        get!(it.next(), "expression of let expression")?,
                    );
                    match &bs {
                        List(bs) => {
                            bindings(bs, env)?;
                            evaluate(expr, env)
                        }
                        _ => Err(EvalError { msg: format!("Wrong format of bindings: {:?}", bs) })
                    }
                },
                Var(op) if &op[..] == "case" => {
                    let (expr, cases) = (
                        get!(it.next(), "cases of case expression")?,
                        get!(it.next(), "expression of case expression")?,
                    );
                    match &cases {
                        List(cases) => {
                            eval_cases(expr, cases, env)
                        }
                        _ => Err(EvalError { msg: format!("Wrong format of cases: {:?}", cases) })
                    }
                },
                Var(op) if &op[..] == "::" => {
                    let (x, xs) = (
                        get!(it.next(), "1st parameter of function: ::")?,
                        get!(it.next(), "2nd parameter of function: ::")?,
                    );
                    eval_cons(x, xs, env)
                },
                Var(op) if &op[..] == "list" => {
                    let mut xs = vec![];
                    for x in it {
                        xs.push(x);
                    }
                    eval_list(&xs, env)
                },
                Cons(f) => {
                    let mut xs = vec![Cons(f)];
                    xs.extend(it);
                    Ok(List(xs))
                },
                f if it.clone().peekable().peek() == None => {
                    let f = evaluate(f, env)?;
                    apply(f, Nil)
                },
                f /* normal function application */ => {
                    let mut f = evaluate(f, env)?;
                    for x in it {
                        //let xv = evaluate(x, env)?;
                        let x = evaluate(x, env)?;
                        f = apply(f, x)?;
                    }
                    Ok(f)
                }
            }
        },
        _ => Err(EvalError { msg: format!("Unknow expression: {:?}", expr) })
    };
    ans
}







#[cfg(test)]
mod tests {
    use super::*;
    use Object::*;

    #[test]
    fn simple_recursive() {
        let mut env = env();
        let src = String::from("(let ((gen (lambda (s n) (case n ((0 ()) (n (:: s (gen s (- n 1))))))))) (gen 'T_T 3))");
        if let Ok(ast) = src.parse::<Object>() {
            let res = evaluate(ast, &mut env);
            //assert_eq!(res.ok(), Some(List(vec![Symbol("T_T".into()), Symbol("T_T".into()), Symbol("T_T".into())])));
            assert_eq!(res.ok(), Some(List(vec![
                Cons("::".into()), Symbol("T_T".into()), List(vec![
                    Cons("::".into()), Symbol("T_T".into()), List(vec![
                        Cons("::".into()), Symbol("T_T".into()), List(vec![])
                    ])
                ])
            ])));
        }
    }

    #[test]
    fn complex_recursive() {
        let mut env = env();
        let src = String::from("(let ((fact (lambda (n) (case n ((0 1) (n (+ n (fact (- n 1))))))))) (fact 10))");
        if let Ok(ast) = src.parse::<Object>() {
            let res = evaluate(ast, &mut env);
            assert_eq!(res.ok(), Some(Integer(56)));
        }
    }
}








/*
/// Test if the arithmetic operator is valid
fn is_arith(op: &str) -> bool {
    ["+", "-", "*", "/"].iter().any(|&x| x == op)
}


/// Evaluate arithmetic expressions.
fn eval_arith(expr: &Object, env: &mut Env) -> Result<Object, EvalError> {
    use Object::*;
    
    let binary_integer: HashMap<&str, &fn(i64, i64) -> i64> = atom::BINARY_ARITH_INTEGER.iter().map(|(k, v)| (*k, v)).collect();
    let binary_real: HashMap<&str, &fn(f64, f64) -> f64> = atom::BINARY_ARITH_REAL.iter().map(|(k, v)| (*k, v)).collect();
    match expr {
        List(xs) => match &xs[..] {
            [Var(op), x, y] if is_arith(&op) => {
                let x = evaluate(x, env)?;
                let y = evaluate(y, env)?;
                match (x, y) {
                    (Integer(x), Integer(y)) => Ok(Integer((binary_integer[&op[..]])(x, y))),
                    (Real(x), Real(y)) => Ok(Real((binary_real[&op[..]])(x, y))),
                    others => Err(EvalError { msg: format!("Arith error: evaluating {:?}", others) })
                }
            },
            _ => Err(EvalError { msg: format!("eval arithmetic error!") })
        },
        _ => Err(EvalError { msg: format!("eval arithmetic error!") })
    }
}


/// Evaluate arithmetic expressions.
fn eval_if(cond: &Object, x: &Object, y: &Object, env: &mut Env) -> Result<Object, EvalError> {
    use Object::*;

    let cond = evaluate(cond, env)?;
    match cond {
        Var(s) if &s[..] == "True"  => evaluate(x, env),
        Var(s) if &s[..] == "False" => evaluate(y, env),
        _ => Err(EvalError { msg: format!("If expression error: {:?}", cond) })
    }
}



/// Evaluate case(match) expressions
fn eval_cases(expr: &Object, cases: &[Object], env: &mut Env) -> Result<Object, EvalError> {
    //println!("\ncases");
    use Object::*;
    let expr = evaluate(expr, env)?;
    for case in cases {
        match case {
            List(xs) => match &xs[..] {
                [pat, res] => {
                    let bind = pat.unify(&expr);
                    if let Ok(mut bind) = bind {
                        let bind: HashMap<String, Rc<Object>> = bind.iter_mut().map(|(k, v)| (k.clone(), Rc::new(v.clone()))).collect();
                        env.extend(bind);
                        return evaluate(res, env)
                    } else {
                        continue;
                    }
                },
                _ => return Err(EvalError { 
                    msg: format!("Case bindings should have format of `[pat result]`, instead of {:?}", case) 
                }),
            },
            _ => return Err(EvalError { 
                msg: format!("Case bindings should have format of `[pat result]`, instead of {:?}", case) 
            }),
        }
    }
    Err(EvalError { msg: format!("Case expression error") })
}


/// Handle bindings
fn bindings(bindings: &[Object], env: &mut Env) -> Result<(), EvalError> {
    use Object::*;
    for binding in bindings {
        match binding {
            List(xs) => match &xs[..] {
                [Var(s), expr] => {
                    let v = evaluate(expr, env)?;
                    //let v = wrap(expr.clone(), env.clone());
                    let v = match v {
                        Closure(None, ps, expr, env) => {
                            Closure(Some(s.clone()), ps, expr, env)
                        },
                        others => others,
                    };
                    env.insert(s.clone(), Rc::new(v.clone()));
                }
                _ => todo!() //unsupported yet!
            },
            _ => return Err(EvalError { msg: format!("Binding error: {:?}", binding) })
        }
    }
    //println!(">> {:?}", env);
    Ok(())
}
//(let ((gen (lambda (s n) (case n ((0 ()) (n (cons s (gen s (- n 1))))))))) (gen 'T_T 200))
/// Handle basic function application `(f x)`
/// 
/// `f` and `x` should have been evaluated!
fn apply(f: Object, x: Object) -> Result<Object, EvalError> {
    use Object::*;
    
    match f {
        Closure(name, params, expr, env) => {
            // Unification should be invoked here, but we only allow single variable here to debug...
            let mut env = env;
            
            match *params {
                List(ps) => match &ps[..] {
                    [ ] => evaluate(&*expr, &mut env),
                    [pat] => {
                        let bind = pat.unify(&x);
                        if let Ok(bind) = bind {
                            let bind: HashMap<String, Rc<Object>> = bind.into_iter().map(|(k, v)| (k, Rc::new(v))).collect();
                            if let Some(name) = name {
                                env.insert(
                                    name.clone(), 
                                    Rc::new(Closure(Some(name), Box::new(List(ps.to_vec())), expr.clone(), env.clone()))
                                );
                            }
                            //println!("{:?}", ps);
                            env.extend(bind);
                            //println!("\napply: {:?}", env);
                            evaluate(&*expr, &mut env)
                        } else {
                            Err(EvalError { msg: format!("Unification error") })
                        }
                    },
                    [pat, ss @ ..] => {
                        let bind = pat.unify(&x);
                        if let Ok(bind) = bind {
                            let bind: HashMap<String, Rc<Object>> = bind.into_iter().map(|(k, v)| (k, Rc::new(v))).collect();
                            if let Some(name) = name.clone() {
                                env.insert(
                                    name.clone(),
                                    Rc::new(Closure(Some(name), Box::new(List(ps.to_vec())), expr.clone(), env.clone()))
                                );
                            }
                            
                            env.extend(bind);
                            //println!("\napply: {:?}", env);
                            Ok(Closure(None, Box::new(List(ss.to_vec())), expr, env))
                        } else {
                            Err(EvalError { msg: format!("Unification error") })
                        }
                    }
                },
                _ => todo!()
            }
        }
        _ => Err(EvalError { msg: format!("Function application error: {:?}", f) })
    }
}

/// Check if variable is atom (normal form)
fn is_atom(s: &str) -> bool {
    let atoms = ["True", "False"];
    atoms.iter().any(|&x| x == s)
}

/// Handle cons expression
fn eval_cons(x: &Object, xs: &Object, env: &mut Env) -> Result<Object, EvalError> {
    let xs = evaluate(xs, env)?;
    match xs {
        //Object::Nil => Ok(Object::List(vec![evaluate(x, env)?])),
        Object::List(xs) => Ok(Object::List(vec![evaluate(x, env)?].into_iter().chain(xs.into_iter()).collect())),
        _ => Err(EvalError {msg: format!("Cons error: {:?}", xs)})
    }
}

/// Handle list constructions
fn eval_list(xs: &[Object], env: &mut Env) -> Result<Object, EvalError> {
    let mut v = vec![];
    for x in xs {
        let x = evaluate(x, env)?;
        v.push(x);
    }
    Ok(Object::List(v))
}


fn eval_head(xs: &Object, env: &mut Env) -> Result<Object, EvalError> {
    match evaluate(xs, env)? {
        Object::List(xs) => match &xs[..] {
            [x, ..] => Ok(x.clone()),
            _ => Err(EvalError { msg: format!("Eval error: Can not get head of an empty list.") })
        },
        others => Err(EvalError { msg: format!("Eval error: Can not get head from {:?}", others) })
    }
}

fn eval_tail(xs: &Object, env: &mut Env) -> Result<Object, EvalError> {
    //println!("{:?}", xs);
    match evaluate(xs, env)? {
        Object::List(xs) => match &xs[..] {
            [_, xs @ ..] => Ok(Object::List(xs.to_vec())),
            _ => Err(EvalError { msg: format!("Eval error: Can not get tail of an empty list.") })
        },
        others => Err(EvalError { msg: format!("Eval error: Can not get tail from {:?}", others) })
    }
}


fn eval_thunk(thunk: &Object) -> Result<Object, EvalError> {
    //println!("\nthunk: {:?}", thunk);
    use Object::*;
    match thunk.clone() {
        Thunk(None, expr, mut env) => {
            //println!("{:?}", env);
            evaluate(&expr, &mut env)
        },
        Thunk(Some(name), expr, mut env) => {
            env.insert(name.clone(), Rc::new(thunk.clone()));
            evaluate(&expr, &mut env)
        },
        _ => Err(EvalError { msg: format!("Error while evaluating thunk: {:?}", thunk) })
    }
}







/// The main evaluate function to calculate all abyss expressions
pub fn evaluate(expr: &Object, env: &mut Env) -> Result<Object, EvalError> {
    use Object::*;
    //println!("\neval: {} in {:?}", expr, env);
    let expr = expr.clone();
    let ans = match expr {
        Nil         => Ok(Nil),
        Var(s) if is_atom(&s) => Ok(Var(s)),
        Var(s)      => env.get(&s).map(|x| (**x).clone()).ok_or(EvalError { msg: format!("No such variable: {}", s) }),
        Symbol(_)   => Ok(expr),
        Integer(_)  => Ok(expr),
        Real(_)     => Ok(expr),
        Str(_)      => Ok(expr),
        Thunk(_, _, _) => eval_thunk(&expr),
        List(ref xs)    => match &xs[..] {
            // Empty list
            [] => Ok(List(xs.clone())),

            // Lambda abstraction
            [Var(op), ps, expr] if &op[..] == "lambda" => {
                Ok(Object::closure(ps.clone(), expr.clone(), env.clone()))
            },

            // Force evaluation
            [Var(op), x] if &op[..] == "lazy" => {
                wrap(None, x.clone(), env.clone())
            }
            
            // Basic arithmetic
            [Var(op), _, _] if is_arith(&op) => eval_arith(&expr, env),

            // If expressions
            [Var(op), cond, x, y] if &op[..] == "if" => eval_if(cond, x, y, env),

            // Let bindings
            [Var(op), List(bs), expr] if &op[..] == "let" => {
                bindings(bs, env)?;
                //println!("{:?}", env);
                evaluate(expr, env)
            },

            // Case (Match) expression
            [Var(op), expr, List(cases)] if &op[..] == "case" => eval_cases(expr, cases, env),

            // List operations
            [Var(op), x, xs]   if &op[..] == "cons" => eval_cons(x, xs, env),
            [Var(op), xs]      if &op[..] == "head" => eval_head(xs, env),
            [Var(op), xs]      if &op[..] == "tail" => eval_tail(xs, env),
            [Var(op), xs @ ..] if &op[..] == "list" => eval_list(xs, env),

            // Normal function application
            [f, xs @ .. ] => {
                let mut fv = evaluate(f, env)?;
                for x in xs {
                    let xv = evaluate(x, env)?;
                    //let xv = wrap(None, x.clone(), env.clone())?;
                    fv = apply(fv, xv)?;
                }
                Ok(fv)
            }
        },
        _ => Err(EvalError { msg: format!("Unknow expression: {:?}", expr) })
    };
    ans
}






*/


