mod node {
    use crate::token::Token;

    #[derive(Debug)]
    pub struct Node<V> {
        start: usize,
        end: usize,
        line: u32,
        column: u32,
        value: V,
    }

    impl<V> Node<V> {
        pub fn new(value: V, first_token: &Token, last_token: &Token) -> Self {
            Node {
                value,
                start: first_token.position,
                end: last_token.position + last_token.lexeme.len(),
                line: first_token.line,
                column: first_token.column,
            }
        }
    }
}
