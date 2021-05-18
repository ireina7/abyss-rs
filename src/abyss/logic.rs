use super::object::Object;
//use super::env::Environment;
use crate::logic::*;
use std::collections::HashMap;


impl Unifiable for Object {

    type Key = String;
    fn unify(&self, other: &Self) -> Result<HashMap<Self::Key, Self>, UnifyError>
    where Self: Sized 
    {
        use Object::*;
        let mut env = HashMap::new();
        let res = unify_objects(self, other, &mut env);
        if let Err(err) = res {
            return Err(err)
        }
        let mut ans = HashMap::new();
        let keys: Vec<_> = env.keys().collect();
        for key in keys {
            if let Var(s) = key {
                let v = subst(&env, &key);
                match v {
                    Some(o) => {
                        ans.insert(s.clone(), o);
                    },
                    None => return Err(UnifyError { msg: format!("Unification error: No value!") })
                }
            }
        }
        Ok(ans)
    }
}




type Env = HashMap<Object, Object>;

//#[allow(dead_code)]
fn subst(map: &HashMap<Object, Object>, obj: &Object) -> Option<Object> {
    use Object::*;
    match obj {
        Var(_x) => {
            if let Some(o) = map.get(obj) {
                if let Var(_y) = o { subst(map, o) } else { Some(o.clone()) }
            } else {
                None
            }
        },
        _ => Some(obj.clone())
    }
}



fn unify_var(var: &Object, v: &Object, env: &mut Env) -> Result<(), UnifyError> {
    use Object::*;

    match (var, v) {
        (Var(_), _) if env.contains_key(var) => {
            let next = env[var].clone();
            unify_objects(&next, v, env)
        },
        (Var(_), _) if env.contains_key(v) => {
            let next = env[v].clone();
            unify_objects(var, &next, env)
        },
        (Var(_), _) if check_occurs(var, v, env) => 
            Err(UnifyError { msg: format!("Unification error: check occurence error!") }),
        (Var(_), _) => {
            env.insert(var.clone(), v.clone());
            Ok(())
        }
        _ => Err(UnifyError { msg: format!("Unification error: Unifying varibles") })
    }
}

fn check_occurs(v: &Object, term: &Object, env: &Env) -> bool {
    use Object::*;

    if v == term {
        return true;
    }
    match (v, term) {
        (Var(_), Var(_)) if env.contains_key(term) => {
            check_occurs(v, &env[term], env)
        },
        (Var(_), List(xs)) => xs.iter().any(|x| check_occurs(v, x, env)),
        _ => false
    }
}




fn unify_objects(this: &Object, other: &Object, env: &mut Env) -> Result<(), UnifyError> {
    use Object::*;

    match (this, other) {
        (Nil, Nil) => Ok(()),
        (Integer(x), Integer(y)) if x == y => Ok(()),
        (Real   (x), Real   (y)) if x == y => Ok(()),
        (Str    (x), Str    (y)) if x == y => Ok(()),
        (Symbol (x), Symbol (y)) if x == y => Ok(()),
        (Var(_), _) => {
            unify_var(&this, &other, env)
        },
        (_, Var(_)) => {
            unify_var(&other, &this, env)
        },
        (List(xs), List(ys)) => {
            for (x, y) in xs.iter().zip(ys.iter()) {
                let res = unify_objects(x, y, env);
                if let Err(_) = res {
                    return res;
                }
            }
            Ok(())
        },
        (Closure(_, _, _), _) | (_, Closure(_, _, _)) => 
             Err(UnifyError { msg: format!("Unification error: Comparing {:?} with {:?}.", this, other) }),
        _ => Err(UnifyError { msg: format!("Unification error: Comparing {:?} with {:?}.", this, other) }),
    }
}











#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_unification() {
        use Object::*;
        let lhs = List(vec![Var("a".into()), Var("b".into()), Str("test_str".into())]);
        let rhs = List(vec![Var("b".into()), Integer(7), Str("test_str".into())]);
        let res = lhs.unify(&rhs);

        assert_eq!(res.ok(), None);
    }
}
