mod ast;
mod source;
mod token;
mod tokenizer;

fn main() {
    use crate::source::Source;

    println!(
        "{:?}",
        tokenizer::parse(Source::new_orphan("Hello, world!"))
    );
}
