use super::object::Object;
//use super::env::Environment;
use crate::logic::*;
use std::collections::HashMap;
use super::eval::lazy;


impl Unifiable for Object {

    type Key = String;
    fn unify(&self, other: &Self) -> Result<HashMap<Self::Key, Self>, UnifyError>
    where Self: Sized 
    {
        use Object::*;
        let mut unv = HashMap::new(); // the environment for unification
        let res = unify_objects(self, other, &mut unv);
        if let Err(err) = res {
            return Err(err)
        }
        let mut ans = HashMap::new();
        let keys: Vec<_> = unv.keys().collect();
        for key in keys {
            if let Var(s) = key {
                let v = subst(&unv, &key);
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



fn unify_var(var: &Object, v: &Object, unv: &mut Env) -> Result<(), UnifyError> {
    use Object::*;

    match (var, v) {
        (Var(_), _) if unv.contains_key(var) => {
            let next = unv[var].clone();
            unify_objects(&next, v, unv)
        },
        (Var(_), _) if unv.contains_key(v) => {
            let next = unv[v].clone();
            unify_objects(var, &next, unv)
        },
        (Var(_), _) if check_occurs(var, v, unv) => 
            Err(UnifyError { msg: format!("Unification error: check occurence error!") }),
        (Var(_), _) => {
            unv.insert(var.clone(), v.clone());
            Ok(())
        }
        _ => Err(UnifyError { msg: format!("Unification error: Unifying varibles") })
    }
}

fn check_occurs(v: &Object, term: &Object, unv: &Env) -> bool {
    use Object::*;

    if v == term {
        return true;
    }
    match (v, term) {
        (Var(_), Var(_)) if unv.contains_key(term) => {
            check_occurs(v, &unv[term], unv)
        },
        (Var(_), List(xs)) => xs.iter().any(|x| check_occurs(v, x, unv)),
        _ => false
    }
}




fn unify_objects(this: &Object, other: &Object, unv: &mut Env) -> Result<(), UnifyError> {
    use Object::*;
    //println!("Unifying: {:?} and {:?}", this, other);
    match (this, other) {
        (Nil, Nil) => Ok(()),
        (Integer(x), Integer(y)) if x == y => Ok(()),
        (Real   (x), Real   (y)) if x == y => Ok(()),
        (Str    (x), Str    (y)) if x == y => Ok(()),
        (Symbol (x), Symbol (y)) if x == y => Ok(()),
        (Cons   (x), Cons   (y)) if x == y => Ok(()), 
        (Var(_), _) => {
            unify_var(&this, &other, unv)
        },
        (_, Var(_)) => {
            unify_var(&other, &this, unv)
        },
        (_, thunk @ Thunk(_, _, _)) => {
            let v = lazy::eval_thunk(thunk);
            match v {
                Ok(v) => unify_objects(this, &v, unv),
                Err(_err) => Err(UnifyError { msg: format!("Unfinished unification error") })
            }
        }
        (thunk @ Thunk(_, _, _), _) => {
            let v = lazy::eval_thunk(thunk);
            match v {
                Ok(v) => unify_objects(&v, this, unv),
                Err(_err) => Err(UnifyError { msg: format!("Unfinished unification error") })
            }
        }
        (List(xs), List(ys)) if 
            !xs.is_empty() && xs[0] == Cons("::".into()) && 
            !ys.is_empty() && ys[0] != Cons("::".into()) => {
            
            unify_objects(this, &unpack(other.clone()), unv)
        },

        (List(xs), List(ys)) if xs.len() == ys.len() => {
            for (x, y) in xs.iter().zip(ys.iter()) {
                let res = unify_objects(x, y, unv);
                if let Err(_) = res {
                    return res;
                }
            }
            Ok(())
        },
        (Closure(_, _, _, _), _) | (_, Closure(_, _, _, _)) => 
             Err(UnifyError { msg: format!("Unification error: Comparing {:?} with {:?}.", this, other) }),
        _ => Err(UnifyError { msg: format!("Unification error: Comparing {:?} with {:?}.", this, other) }),
    }
}


/// Pack list
#[inline]
pub fn unpack(xs: Object) -> Object {
    use Object::*;
    match xs {
        List(xs) => match &xs[..] {
            [] => List(xs),
            [x, xs @ ..] => List(vec![Cons("::".into()), x.clone(), List(xs.to_vec())]),
        }
        others => others,
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

        let mut ans = HashMap::new();
        ans.insert("a".to_string(), Integer(7));
        ans.insert("b".to_string(), Integer(7));
        assert_eq!(res.ok(), Some(ans));
    }
}
