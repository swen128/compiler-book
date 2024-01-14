use std::io::Read;

use compiler_book::parse;

fn main() {
    let mut src = String::new();
    std::io::stdin().read_to_string(&mut src).unwrap();

    println!("{}", src);
    println!("{:?}", parse(&src));
}
