#![allow(dead_code)]

use crate::abyss;
use crate::logic::Unifiable;
use crate::utils::error::Backtrace;
use abyss::object::*;
//use abyss::object::Env;
use std::rc::Rc;
//use std::cell::RefCell;
use abyss::config::HashMap;
use abyss::eval::core::*;
pub use abyss::object::EvalError;
use std::borrow::Borrow;




fn eval_atom(op: Object, a: Object, b: Object, env: &mut Env, backtrace: &mut Backtrace) -> Result<Object> {
    use Object::*;
    use atom::*;
    match op {
        Var(s) => {
            let mut xs = vec![];
            xs.push(force(a, env, backtrace)?);
            xs.push(force(b, env, backtrace)?);
            match &s[..] {
                "+" | "-" | "*" | "/" => eval_arith(&s, &xs),
                "<"  => eval_lt(&xs),
                ">"  => eval_gt(&xs),
                "<=" => eval_le(&xs),
                ">=" => eval_ge(&xs),
                "==" => eval_eq(&xs),
                "/=" => eval_ne(&xs),
                unknown => Err(EvalError::new(format!("Atom evaluation error: unknown atom operator: {:?}", unknown), backtrace.clone()))
            }
        },
        _ => Err(EvalError::new(format!("Atom evaluation error: evaluating {:?}", op), backtrace.clone()))
    }
}

/// Evaluate arithmetic expressions.
#[inline]
fn eval_if(cond: Object, x: Object, y: Object, env: &mut Env, backtrace: &mut Backtrace) -> Result<Object> {
    use Object::*;

    let cond = force(cond, env, backtrace)?;
    match cond {
        Var(s) if &s[..] == "True"  => evaluate(x, env, backtrace),
        Var(s) if &s[..] == "False" => evaluate(y, env, backtrace),
        _ => Err(EvalError::new(format!("If expression error: {:?}", cond), backtrace.clone()))
    }
}


#[inline]
fn eval_case(expr: Object, pat: Object, res: Object, env: &mut Env, backtrace: &mut Backtrace) -> Option<Result<Object>> {
    let bind = pat.unify(&expr);
    if let Ok(mut bind) = bind {
        let bind: HashMap<String, Rc<Object>> = bind.iter_mut().map(|(k, v)| (k.clone(), Rc::new(v.clone()))).collect();
        env.extend(bind);
        Some(evaluate(res, env, backtrace))
    } else {
        //println!("{:?}", bind); need to finish error conversion!!
        None
    }
}

/// Evaluate case(match) expressions
fn eval_cases(expr: Object, cases: &[Object], env: &mut Env, backtrace: &mut Backtrace) -> Result<Object> {
    //println!("\ncases");
    use Object::*;
    let expr = evaluate(expr, env, backtrace)?;
    for case in cases {
        match case {
            List(xs) => match &xs[..] {
                [pat, res] => {
                    match eval_case(expr.clone(), pat.clone(), res.clone(), env, backtrace) {
                        None => continue,
                        Some(res) => return res,
                    }
                },
                _ => return Err(EvalError::new(
                    format!("Case bindings should have format of `[pat result]`, instead of {:?}", case),
                    backtrace.clone()
                )),
            },
            _ => return Err(EvalError::new(
                format!("Case bindings should have format of `[pat result]`, instead of {:?}", case),
                backtrace.clone() 
            )),
        }
    }
    Err(EvalError::new(format!("Case expression error, expr: {}, cases: {}", expr, cases.len()), backtrace.clone()))
}



pub fn bind(left: Object, right: Object, env: &mut Env, backtrace: &mut Backtrace) -> Result<()> {
    use Object::*;
    match (left, right) {
        (Var(s), expr) => {
            let v = match expr {
                List(xs) => match &xs[..] {
                    [Var(op), ..] if &op[..] == "lambda" => evaluate(List(xs), env, backtrace)?,
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
                    //env.insert(s.clone(), Rc::new(thunk.clone()));
                    thunk
                }
            };
            env.insert(s.clone(), Rc::new(v));
        }
        (List(xs), expr) => match &xs[..] {
            [Var(f), ps @ ..] => {
                let lambda = weak(ps.to_vec(), expr.clone());
                bind(Var(f.clone()), lambda, env, backtrace)?
            },
            _ => return Err(EvalError::new(format!("Binding error: Invalid Binding {:?} and {:?}", List(xs), expr), backtrace.clone()))
        }
        (x, y) => return Err(EvalError::new(format!("Binding error: Binding {:?} and {:?}", x, y), backtrace.clone()))
    }
    Ok(())
}

/// Handle bindings
fn bindings(bindings: &[Object], env: &mut Env, backtrace: &mut Backtrace) -> Result<()> {
    use Object::*;
    for binding in bindings {
        match binding {
            List(xs) => match &xs[..] {
                [def, expr] => {
                    bind(def.clone(), expr.clone(), env, backtrace)?;
                }
                _ => return Err(EvalError::new(format!("Binding error: {:?}", binding), backtrace.clone()))
            },
            _ => return Err(EvalError::new(format!("Binding error: {:?}", binding), backtrace.clone()))
        }
    }
    Ok(())
}





#[inline]
fn apply_env(
    f: Object, name: &Option<String>, 
    pat: &Object, x: &Object, 
    env: &mut Env, backtrace: &mut Backtrace
) -> Result<()> {

    let bind = pat.unify(&x);
    if let Ok(bind) = bind {
        let bind: HashMap<String, Rc<Object>> = bind.into_iter().map(|(k, v)| (k, Rc::new(v))).collect();
        if let Some(name) = name {
            env.insert(name.clone(), Rc::new(f));
        }
        env.extend(bind);
        Ok(())
    } else {
        //println!("{:?}", bind);
        Err(EvalError::new(format!("Application error: Unification error: unifying {:?} and {:?}", pat, x), backtrace.clone()))
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
fn apply(f: Object, x: Object, backtrace: &mut Backtrace) -> Result<Object> {
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
                            //backtrace.push(format!("Apply function: {}", name));
                            env.insert(name.clone(), Rc::new(f.clone()));
                        }
                        evaluate(Object::clone(expr.borrow()), &mut env, backtrace)
                    } else {
                        Err(EvalError::new(format!("Applying error: unexpected parameter: {}", x), backtrace.clone()))
                    },
                    [pat] => {
                        /*
                        if let Some(name) = name {
                            backtrace.push(format!("Apply function: {}", name));
                        }*/
                        apply_env(f.clone(), name, wash_type(pat), &x, &mut env, backtrace)?;
                        evaluate(Object::clone(expr.borrow()), &mut env, backtrace)
                    },
                    [pat, ss @ ..] => {/*
                        if let Some(name) = name {
                            backtrace.push(format!("Apply function: {}", name));
                        }*/
                        apply_env(f.clone(), name, wash_type(pat), &x, &mut env, backtrace)?;
                        Ok(Closure(None, Rc::new(List(ss.to_vec())), Rc::clone(expr), env.clone()))
                    }
                },
                others => Err(EvalError::new(format!("Function parameters should be a list instead of {:?}", others), backtrace.clone()))
            }
        }
        _ => Err(EvalError::new(format!("Function application error: Applying {:?}", f), backtrace.clone()))
    }
}



/// Handle cons expression
#[inline]
fn eval_cons(x: Object, xs: Object, env: &mut Env) -> Result<Object> {
    use Object::*;
    //let xs = evaluate(xs, env)?;
    let xs = wrap(None, xs, env.clone())?;
    let x  = wrap(None, x , env.clone())?;
    Ok(Object::List(vec![Cons("::".into()), x, xs]))
}

/// Handle list constructions
#[inline]
fn eval_list(xs: &[Object], env: &mut Env) -> Result<Object> {
    let mut v = vec![];
    for x in xs {
        let x = wrap(None, x.clone(), env.clone())?;
        v.push(x);
    }
    Ok(Object::List(v))
}

/*
fn eval_head(xs: &Object, env: &mut Env) -> Result<Object> {
    use Object::*;
    match force(xs, env)? {
        List(xs) => match &xs[..] {
            [x, ..] => Ok(x.clone()),
            _ => Err(EvalError { msg: format!("Eval error: Can not get head of an empty list.") })
        },
        others => Err(EvalError { msg: format!("Eval error: Can not get head from {:?}", others) })
    }
}

fn eval_tail(xs: &Object, env: &mut Env) -> Result<Object> {
    //println!("{:?}", xs);
    use Object::*;
    match force(xs, env)? {
        List(xs) => match &xs[..] {
            [_, xs @ ..] => Ok(Object::List(xs.to_vec())),
            _ => Err(EvalError { msg: format!("Eval error: Can not get tail of an empty list.") })
        },
        others => Err(EvalError { msg: format!("Eval error: Can not get tail from {:?}", others) })
    }
}
*/

/// Thunk evaluation
#[inline]
pub fn eval_thunk(thunk: Object, backtrace: &mut Backtrace) -> Result<Object> {
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
            let mut res = evaluate(thunk.value(), &mut env, backtrace)?;

            // Flatten thunk, never generate recursive wrapped thunk!
            if let Thunk(_, _, _) = res {
                res = eval_thunk(res, backtrace)?;
            }
            let result = res.clone();
            //let p: &Thunker = expr.borrow();
            *thunk.as_ref().expr.borrow_mut() = res;
            Ok(result)
        },
        _ => Err(EvalError::new(format!("Error while evaluating thunk: {:?}", thunk), backtrace.clone()))
    }
}

/// Force strict evaluation
#[inline]
pub fn force(obj: Object, env: &mut Env, backtrace: &mut Backtrace) -> Result<Object> {
    //println!("\nforce: {} in {:?}", thunk, env);
    use Object::*;
    let e = obj.clone();
    let v = evaluate(obj, env, backtrace)?;
    match v {
        Thunk(_, _, _) => {
            let v = eval_thunk(v, backtrace)?;
            match e {
                Var(s) => {
                    env.insert(s, Rc::new(v.clone()));
                },
                _ => {}
            }
            Ok(v)
        },
        others => Ok(others.clone())
    }
}

#[inline]
/// Very bad, never use!
fn force_value(obj: Object, env: &mut Env, backtrace: &mut Backtrace) -> Result<Object> {
    //println!("force value: {:?}", obj);
    use Object::*;
    match obj {
        Thunk(_, _, _) => force_value(eval_thunk(obj, backtrace)?, env, backtrace),
        List(xs) => match &xs[..] {
            [Cons(cons), xs @ ..] => {
                let mut ys = vec![Cons(cons.clone())];
                for x in xs {
                    let x = force_value(x.clone(), env, backtrace)?;
                    ys.push(x);
                }
                Ok(List(ys))
            }
            others => Ok(List(others.to_vec()))
        }
        others => Ok(others.clone())
    }
}








/// The main evaluate function to calculate all abyss expressions
pub fn evaluate(expr: Object, env: &mut Env, backtrace: &mut Backtrace) -> Result<Object> {
    use Object::*;
    macro_rules! get {
        ($sx: expr, $msg: expr) => {
            match $sx {
                Some(x) => Ok(x),
                None => Err(EvalError::new(format!("Getting {}", $msg), backtrace.clone()))
            }
        };
    }
    //println!("\neval: {:?} in {:?}", expr, 0);
    let ans = match expr {
        Nil            => Ok(Nil),
        Var(ref s) if atom::is_atom(s) => Ok(expr),
        Var(ref s)     => env.get(s).map(|x| (**x).clone()).ok_or(EvalError::new(format!("No such variable: {}", s), backtrace.clone())),
        Symbol(_)      => Ok(expr),
        Integer(_)     => Ok(expr),
        Real(_)        => Ok(expr),
        Str(_)         => Ok(expr),
        Closure(_, _, _, _) => Ok(expr),
        Thunk(_, _, _) => Ok(expr),
        List(xs)       => {
            let mut it = xs.into_iter();
            let op = it.next();
            if let None = op {
                return Ok(List(vec![]));
            }
            match op.unwrap() {
                Var(op) if atom::is_atom_op(&op) && it.clone().count() == 2 => {
                    let (a, b) = (
                        get!(it.next(), format!("2nd argument of function: {}", op))?,
                        get!(it.next(), format!("3rd argument of function: {}", op))?,
                    );
                    eval_atom(Var(op), a, b, env, backtrace)
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
                    force_value(evaluate(x, env, backtrace)?, env, backtrace)
                }
                Var(op) if &op[..] == "if" => {
                    let (cond, x, y) = (
                        get!(it.next(), "condition of function: if"   )?,
                        get!(it.next(), "true branch of function: if" )?,
                        get!(it.next(), "false branch of function: if")?,
                    );
                    eval_if(cond, x, y, env, backtrace)
                }
                Var(op) if &op[..] == "let" => {
                    let (bs, expr) = (
                        get!(it.next(), "bindings of let expression"  )?,
                        get!(it.next(), "expression of let expression")?,
                    );
                    match &bs {
                        List(bs) => {
                            bindings(bs, env, backtrace)?;
                            evaluate(expr, env, backtrace)
                        }
                        _ => Err(EvalError::new(format!("Wrong format of bindings: {:?}", bs), backtrace.clone()))
                    }
                },
                Var(op) if &op[..] == "case" => {
                    let (expr, cases) = (
                        get!(it.next(), "cases of case expression")?,
                        get!(it.next(), "expression of case expression")?,
                    );
                    match &cases {
                        List(cases) => {
                            eval_cases(expr, cases, env, backtrace)
                        }
                        _ => Err(EvalError::new(format!("Wrong format of cases: {:?}", cases), backtrace.clone()))
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
                    let f = force(f, env, backtrace)?;
                    apply(f, Nil, backtrace)
                },
                f /* normal function application */ => {
                    let mut f = force(f, env, backtrace)?;
                    for x in it {
                        //let xv = evaluate(x, env)?;
                        let x = wrap(None, x, env.clone())?;
                        f = apply(f, x, backtrace)?;
                    }
                    Ok(f)
                }
            }
        },
        _ => Err(EvalError::new(format!("Evaluation error: Unknow expression: {:?}", expr), backtrace.clone()))
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
        let mut bt = Backtrace::new();
        let src = String::from("(! (let ((gen (lambda (s n) (case n ((0 ()) (n (:: s (gen s (- n 1))))))))) (gen 'T_T 3)))");
        if let Ok(ast) = src.parse::<Object>() {
            let res = evaluate(ast, &mut env, &mut bt);
            assert_eq!(res, Ok(List(vec![
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
        let mut bt = Backtrace::new();
        let src = String::from("(let ((fact (lambda (n) (case n ((0 1) (n (+ n (fact (- n 1))))))))) (fact 10))");
        if let Ok(ast) = src.parse::<Object>() {
            let res = evaluate(ast, &mut env, &mut bt);
            assert_eq!(res, Ok(Integer(56)));
        }
    }
}


