use crate::abyss;
use crate::logic::Unifiable;
use abyss::object::Object;
use abyss::object::Env;
use std::rc::Rc;
use abyss::config::HashMap;
use abyss::eval::core::*;
pub use abyss::object::EvalError;




/// Test if the arithmetic operator is valid
fn is_arith(op: &str) -> bool {
    ["+", "-", "*", "/"].iter().any(|&x| x == op)
}


/// Evaluate arithmetic expressions.
fn eval_arith(expr: &Object, env: &mut Env) -> Result<Object, EvalError> {
    use Object::*;

    let binary_integer: HashMap<&str, &fn(i64, i64) -> i64> = 
        atom::BINARY_ARITH_INTEGER.iter().map(|(k, v)| (*k, v)).collect();
    let binary_real: HashMap<&str, &fn(f64, f64) -> f64> = 
        atom::BINARY_ARITH_REAL.iter().map(|(k, v)| (*k, v)).collect();
    match expr {
        List(xs) => match &xs[..] {
            [Var(op), x, y] if is_arith(&op) => {
                let x = force(x, env)?;
                let y = force(y, env)?;
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

    let cond = force(cond, env)?;
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


/// Handle bindings
fn bindings(bindings: &[Object], env: &mut Env) -> Result<(), EvalError> {
    use Object::*;
    for binding in bindings {
        match binding {
            List(xs) => match &xs[..] {
                [Var(s), expr] => {
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
/// `f` and `x` should have been evaluated (lazy?)!
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
                            if let Some(name) = name {
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
                others => Err(EvalError { msg: format!("Function parameters should be a list instead of {:?}", others) })
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

/// Thunk evaluation
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

/// Force strict evaluation
fn force(thunk: &Object, env: &mut Env) -> Result<Object, EvalError> {
    //println!("\nforce: {} in {:?}", thunk, env);
    use Object::*;
    let thunk_ = thunk.clone();
    //println!("\n{:?} ==> {:?}", thunk, env);
    match thunk_ {
        Thunk(_, _, _) => eval_thunk(thunk),
        Var(s) => {
            let v = env.get(&s).map(|x| (**x).clone()).ok_or(EvalError { msg: format!("No such variable: {}", s) })?;
            match &v {
                Thunk(_, _, _) => eval_thunk(&v),
                _ => Ok(v)
            }
        }
        _ => evaluate(&thunk, env)
    }
}




/// The main evaluate function to calculate all abyss expressions
pub fn evaluate(expr: &Object, env: &mut Env) -> Result<Object, EvalError> {
    use Object::*;
    //println!("\neval: {} in {:?}", expr, env);
    let ans = match expr {
        Nil         => Ok(Nil),
        Var(s) if is_atom(s) => Ok(expr.clone()),
        Var(_)      => force(expr, env),//env.get(s).map(|x| x.clone()).ok_or(EvalError { msg: format!("No such variable: {}", s) }),
        Symbol(_)   => Ok(expr.clone()),
        Integer(_)  => Ok(expr.clone()),
        Real(_)     => Ok(expr.clone()),
        Str(_)      => Ok(expr.clone()),
        Thunk(_, _, _) => Ok(expr.clone()),
        List(xs)    => match &xs[..] {
            // Empty list
            [] => Ok(expr.clone()),

            // Lambda abstraction
            [Var(op), ps, expr] if &op[..] == "lambda" => {
                Ok(Object::closure(ps.clone(), expr.clone(), env.clone()))
            },

            // Force evaluation
            [Var(op), x] if &op[..] == "lazy" => {
                wrap(None, x.clone(), env.clone())
            }
            
            // Basic arithmetic
            [Var(op), _, _] if is_arith(&op) => eval_arith(expr, env),

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