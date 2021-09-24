pub fn to_string(errors: Vec<String>) -> String {
    errors.join("\n\n----------------------------------------\n\n")
}
