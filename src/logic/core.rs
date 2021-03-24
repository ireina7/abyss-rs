#[allow(dead_code)]
struct LogicError {
    msg: String
}

#[allow(dead_code)]
enum LogicObject<T: PartialEq> {
    Var(String),
    Lit(T)
}
