// https://github.com/rust-lang/rfcs/issues/3479
macro_rules! assert_panic {
    ($expr:expr) => {
        match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| $expr)) {
            Ok(_) => panic!("Expression did not trigger panic"),
            Err(_) => (),
        }
    };
    ($expr:expr, $expected_msg:expr) => {
        match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| $expr)) {
            Ok(_) => panic!("Expression did not trigger panic"),
            Err(err) => {
                let expected_msg_str = $expected_msg.to_string();
                if let Some(msg) = err.downcast_ref::<&'static str>() {
                    assert_eq!(
                        *msg, expected_msg_str,
                        "Panic message does not match expected"
                    );
                } else if let Some(msg) = err.downcast_ref::<String>() {
                    assert_eq!(
                        *msg, expected_msg_str,
                        "Panic message does not match expected"
                    );
                } else {
                    panic!(
                        "Expected panic message not found, expected panic message: {}",
                        expected_msg_str
                    );
                }
            }
        }
    };
}
pub(crate) use assert_panic;
