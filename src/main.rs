mod abyss;

fn main() {
    let mut memo = abyss::HashMap::new();
    memo.insert(0, abyss::Object::Real(7.0));
    memo.insert(1, abyss::Object::Integer(7));
    memo.insert(2, abyss::Object::Str(String::from("test")));
    memo.insert(3, abyss::Object::Nil);
    println!("This is abyss");
    println!("{:?}", memo);
}
