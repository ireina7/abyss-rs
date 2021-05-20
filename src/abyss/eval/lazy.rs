#![allow(dead_code)]

use crate::abyss;
use crate::logic::Unifiable;
use abyss::object::Object;
use abyss::object::Env;
use std::rc::Rc;
use abyss::config::HashMap;
use abyss::eval::core::*;
pub use abyss::object::EvalError;
use std::borrow::Borrow;




fn eval_atom(expr: &Object, env: &mut Env) -> Result<Object> {
    use Object::*;
    use atom::*;
    match expr {
        Var(s) if ["True", "False"].iter().any(|&x| x == s) => Ok(expr.clone()),
        List(xs) => match &xs[..] {
            [] => Ok(List(vec![])),
            [Var(op), xs @ .. ] => {
                let mut ys = vec![];
                for x in xs {
                    ys.push(force(x, env)?);
                }
                match &op[..] {
                    "+" | "-" | "*" | "/" => eval_arith(op, &ys),
                    "<"  => eval_lt(&ys),
                    ">"  => eval_gt(&ys),
                    "<=" => eval_le(&ys),
                    ">=" => eval_ge(&ys),
                    "==" => eval_eq(&ys),
                    "/=" => eval_ne(&ys),
                    unknown => Err(EvalError { msg: format!("Atom evaluation error: unknown atom operator: {:?}", unknown) })
                }
            },
            _ => Err(EvalError { msg: format!("Atom evaluation error: evaluating {:?}", expr) })
        },
        _ => Err(EvalError { msg: format!("Atom evaluation error: evaluating {:?}", expr) })
    }
}

/// Evaluate arithmetic expressions.
fn eval_if(cond: &Object, x: &Object, y: &Object, env: &mut Env) -> Result<Object> {
    use Object::*;

    let cond = force(cond, env)?;
    match cond {
        Var(s) if &s[..] == "True"  => evaluate(x, env),
        Var(s) if &s[..] == "False" => evaluate(y, env),
        _ => Err(EvalError { msg: format!("If expression error: {:?}", cond) })
    }
}



/// Evaluate case(match) expressions
fn eval_cases(expr: &Object, cases: &[Object], env: &mut Env) -> Result<Object> {
    //println!("\ncases");
    use Object::*;
    let expr = force(expr, env)?;
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



fn bind(left: &Object, right: &Object, env: &mut Env) -> Result<()> {
    use Object::*;
    match (left, right) {
        (Var(s), expr) => {
            let v = match expr {
                List(xs) => match &xs[..] {
                    [Var(op), ..] if &op[..] == "lambda" => evaluate(expr, env)?,
                    others => List(others.to_vec())
                }
                others => others.clone()
            };
            //let v = wrap(expr.clone(), env.clone());
            let v = match v {
                Closure(None, ps, expr, env) => {
                    Closure(Some(s.clone()), ps, expr, env)
                },
                others => {
                    let thunk = wrap(Some(s.clone()), others.clone(), env.clone())?;
                    //env.insert(s.clone(), thunk.clone());
                    thunk
                }
            };
            env.insert(s.clone(), Rc::new(v.clone()));
        }
        (List(xs), expr) => match &xs[..] {
            [Var(f), ps @ ..] => {
                let lambda = List(vec![Var("lambda".into()), List(ps.to_vec()), expr.clone()]);
                bind(&Var(f.clone()), &lambda, env)?
            },
            _ => return Err(EvalError { msg: format!("Binding error: Invalid Binding {:?} and {:?}", left, right) })
        }
        _ => return Err(EvalError { msg: format!("Binding error: Binding {:?} and {:?}", left, right) })
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
                    bind(def, expr, env)?;
                }
                _ => return Err(EvalError { msg: format!("Binding error: {:?}", binding) })
            },
            _ => return Err(EvalError { msg: format!("Binding error: {:?}", binding) })
        }
    }
    Ok(())
}








/// Handle basic function application `(f x)`
/// 
/// `f` and `x` should have been evaluated (lazy?)!
fn apply(f: Object, x: Object) -> Result<Object> {
    use Object::*;
    
    match f {
        Closure(name, params, expr, env) => {
            // Unification should be invoked here, but we only allow single variable here to debug...
            let mut env = env;
            
            match params.borrow() {
                List(ps) => match &ps[..] {
                    [ ] => evaluate(&*expr, &mut env),
                    [pat] => {
                        let bind = pat.unify(&x);
                        if let Ok(bind) = bind {
                            let bind: HashMap<String, Rc<Object>> = bind.into_iter().map(|(k, v)| (k, Rc::new(v))).collect();
                            if let Some(name) = name {
                                env.insert(
                                    name.clone(),
                                    Rc::new(Closure(Some(name), Rc::new(List(ps.to_vec())), expr.clone(), env.clone()))
                                );
                            }
                            //println!("{:?}", ps);
                            env.extend(bind);
                            //println!("\napply: {:?}", env);
                            evaluate(&*expr, &mut env)
                        } else {
                            Err(EvalError { msg: format!("Application error: Unification error: unifying {:?} and {:?}", pat, x) })
                        }
                    },
                    [pat, ss @ ..] => {
                        let bind = pat.unify(&x);
                        if let Ok(bind) = bind {
                            let bind: HashMap<String, Rc<Object>> = bind.into_iter().map(|(k, v)| (k, Rc::new(v))).collect();
                            if let Some(name) = name {
                                env.insert(
                                    name.clone(),
                                    Rc::new(Closure(Some(name), Rc::new(List(ps.to_vec())), expr.clone(), env.clone()))
                                );
                            }
                            env.extend(bind);
                            Ok(Closure(None, Rc::new(List(ss.to_vec())), expr, env))
                        } else {
                            Err(EvalError { msg: format!("Application error: Unification error: unifying {:?} and {:?}", pat, x) })
                        }
                    }
                },
                others => Err(EvalError { msg: format!("Function parameters should be a list instead of {:?}", others) })
            }
        }
        _ => Err(EvalError { msg: format!("Function application error: {:?}", f) })
    }
}



/// Handle cons expression
fn eval_cons(x: &Object, xs: &Object, env: &mut Env) -> Result<Object> {
    let xs = evaluate(xs, env)?;
    match xs {
        //Object::Nil => Ok(Object::List(vec![evaluate(x, env)?])),
        Object::List(xs) => Ok(Object::List(vec![wrap(None, x.clone(), env.clone())?].into_iter().chain(xs.into_iter()).collect())),
        _ => Err(EvalError {msg: format!("Cons error: {:?}", xs)})
    }
}

/// Handle list constructions
fn eval_list(xs: &[Object], env: &mut Env) -> Result<Object> {
    let mut v = vec![];
    for x in xs {
        let x = wrap(None, x.clone(), env.clone())?;
        v.push(x);
    }
    Ok(Object::List(v))
}


fn eval_head(xs: &Object, env: &mut Env) -> Result<Object> {
    match force(xs, env)? {
        Object::List(xs) => match &xs[..] {
            [x, ..] => Ok(x.clone()),
            _ => Err(EvalError { msg: format!("Eval error: Can not get head of an empty list.") })
        },
        others => Err(EvalError { msg: format!("Eval error: Can not get head from {:?}", others) })
    }
}

fn eval_tail(xs: &Object, env: &mut Env) -> Result<Object> {
    //println!("{:?}", xs);
    match force(xs, env)? {
        Object::List(xs) => match &xs[..] {
            [_, xs @ ..] => Ok(Object::List(xs.to_vec())),
            _ => Err(EvalError { msg: format!("Eval error: Can not get tail of an empty list.") })
        },
        others => Err(EvalError { msg: format!("Eval error: Can not get tail from {:?}", others) })
    }
}

/// Thunk evaluation
fn eval_thunk(thunk: &Object) -> Result<Object> {
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

/// Force strict evaluation
fn force(obj: &Object, env: &mut Env) -> Result<Object> {
    //println!("\nforce: {} in {:?}", thunk, env);
    use Object::*;
    let obj = evaluate(obj, env)?;
    match obj {
        Thunk(_, _, _) => eval_thunk(&obj),
        others => Ok(others)
    }
}




/// The main evaluate function to calculate all abyss expressions
pub fn evaluate(expr: &Object, env: &mut Env) -> Result<Object> {
    use Object::*;
    //println!("\neval: {} in {:?}", expr, env);
    let ans = match expr {
        Nil         => Ok(Nil),
        Var(s) if atom::is_atom(s) => Ok(expr.clone()),
        Var(s)      => env.get(s).map(|x| (**x).clone()).ok_or(EvalError { msg: format!("No such variable: {}", s) }),
        Symbol(_)   => Ok(expr.clone()),
        Integer(_)  => Ok(expr.clone()),
        Real(_)     => Ok(expr.clone()),
        Str(_)      => Ok(expr.clone()),
        Thunk(_, _, _) => Ok(expr.clone()),
        List(xs)    => match &xs[..] {
            // Empty list
            [] => Ok(expr.clone()),

            // Basic atom evaluation (should use strict evaluation)
            [Var(op), ..] if atom::is_atom_op(&op) => eval_atom(expr, env),

            // Lambda abstraction
            [Var(op), ps, expr] if &op[..] == "lambda" => {
                Ok(Object::closure(ps.clone(), expr.clone(), env.clone()))
            },

            // Delay evaluation
            [Var(op), x] if &op[..] == "lazy" => {
                wrap(None, x.clone(), env.clone())
            }

            // Force evaluation
            [Var(op), x] if &op[..] == "!" => {
                force(x, env)
            }
            
            // If expressions
            [Var(op), cond, x, y] if &op[..] == "if" => eval_if(cond, x, y, env),

            // Let bindings
            [Var(op), List(bs), expr] if &op[..] == "let" => {
                bindings(bs, env)?;
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
                let mut fv = force(f, env)?;
                for x in xs {
                    //let xv = evaluate(x, env)?;
                    let xv = wrap(None, x.clone(), env.clone())?;
                    fv = apply(fv, xv)?;
                }
                Ok(fv)
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
        let src = String::from("(let ((gen (lambda (s n) (case n ((0 ()) (n (cons s (gen s (- n 1))))))))) (gen 'T_T 3))");
        if let Ok(ast) = src.parse::<Object>() {
            let res = evaluate(&ast, &mut env);
            assert_eq!(res.ok(), Some(List(vec![Symbol("T_T".into()), Symbol("T_T".into()), Symbol("T_T".into())])));
        }
    }

    #[test]
    fn complex_recursive() {
        let mut env = env();
        let src = String::from("(let ((fact (lambda (n) (case n ((0 1) (n (+ n (fact (- n 1))))))))) (fact 10))");
        if let Ok(ast) = src.parse::<Object>() {
            let res = evaluate(&ast, &mut env);
            assert_eq!(res.ok(), Some(Integer(56)));
        }
    }
}