use super::object::Object;
use crate::logic::*;
use std::collections::HashMap;


impl Unifiable for Object {

    type Key = Object;
    fn unify(&self, other: &Self) -> Result<HashMap<Self::Key, Self>, UnifyError>
    where Self: Sized {
        unify_objects(self, other)
    }
}



#[allow(dead_code)]
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


fn unify_objects(this: &Object, other: &Object) -> Result<HashMap<Object, Object>, UnifyError> {
    use Object::*;
    let mut ans = HashMap::new();

    match (this, other) {
        (Nil, Nil) => {},
        (Integer(x), Integer(y)) if x == y => {},
        (Real   (x), Real   (y)) if x == y => {},
        (Str    (x), Str    (y)) if x == y => {},
        (Symbol (x), Symbol (y)) if x == y => {},
        (Var(_), _) => {
            ans.insert(this.clone(), other.clone());
        },
        (_, Var(_)) => {
            ans.insert(other.clone(), this.clone());
        },
        (List(xs), List(ys)) => for (x, y) in xs.iter().zip(ys.iter()) {
            let res = unify_objects(x, y);
            if let Ok(map) = res {
                ans.extend(map);
            } else {
                return res;
            }
        },
        (Closure(_, _, _), _) => return Err(UnifyError { msg: format!("Unification error: Comparing {:?} with {:?}.", this, other) }),
        (_, Closure(_, _, _)) => return Err(UnifyError { msg: format!("Unification error: Comparing {:?} with {:?}.", this, other) }),
        _ => return Err(UnifyError { msg: format!("Unification error: Comparing {:?} with {:?}.", this, other) }),
    }
    Ok(ans)
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

        assert_eq!(res, Ok(HashMap::new()));
    }
}
