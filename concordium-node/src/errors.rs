error_chain! {
    errors {
        QueueingError(t: String) {
            description("can't queue message")
            display("can't queue message: '{}'", t)
        }
        ProcessControlError(t: String) {
            description("can't stop process")
            display("can't stop process '{}'", t)
        }
    }
}