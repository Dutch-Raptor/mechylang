use crate::test_utils::test_eval_ok;

#[test]
fn test_variable_scoping() {
    test_eval_ok(
        r#"
        let x = 1;
        assert_eq(x, 1);
        { // This is a new scope
            assert_eq(x, 1); // x is accessible from this scope
            x = 2; // x can be updated from this scope
            assert_eq(x, 2);
        } // The scope ends here
        // in the outer scope, x is also 2 now as it was updated from the inner scope
        assert_eq(x, 2);
        "#,
    );
}

#[test]
fn test_shadowing() {
    test_eval_ok(
        r#"
        let x = 1;
        assert_eq(x, 1);
        { 
            assert_eq(x, 1); // x is accessible from inner scope
            let x = 2; // now x is shadowed
            assert_eq(x, 2);
            x = 3; // we have updated the value of x in the inner scope
            assert_eq(x, 3);
        } // inner scope ends here
        // In the outer scope, x is still 1
        assert_eq(x, 1);
        "#,
    );
}

#[test]
fn test_shadowing_within_if_else() {
    test_eval_ok(
        r#"
        let x = 1;
        assert_eq(x, 1);
        if true {
            assert_eq(x, 1); // x is accessible from inner scope
            let x = 2; // now x is shadowed
            assert_eq(x, 2);
            x = 3; // we have updated the value of x in the inner scope
            assert_eq(x, 3);
        } else {
            assert_eq(x, 1); // x is still accessible from inner scope
            let x = 4; // now x is shadowed
            assert_eq(x, 4);
            x = 5; // we have updated the value of x in the inner scope
            assert_eq(x, 5);
        }
        // inner scope ends here
        // In the outer scope, x is still 1
        assert_eq(x, 1);
        "#,
    );
}

#[test]
fn test_shadowing_within_for_loop() {
    test_eval_ok(
        r#"
        let x = 1;
        assert_eq(x, 1);
        for i in 1..5 {
            assert_eq(x, 1); // x is accessible from inner scope
            let x = 2; // now x is shadowed
            assert_eq(x, 2);
            x = i; // we have updated the value of x in the inner scope
            assert_eq(x, i);
        }
        // inner scope ends here
        // In the outer scope, x is still 1
        assert_eq(x, 1);
        "#,
    );
}

#[test]
fn test_shadowing_within_while_loop() {
    test_eval_ok(
        r#"
        let x = 1;
        assert_eq(x, 1);
        let counter = 0;
        while true {
            assert_eq(x, 1); // x is accessible from inner scope
            let x = 2; // now x is shadowed
            assert_eq(x, 2);
            x = 3; // we have updated the value of x in the inner scope
            assert_eq(x, 3);
            
            counter += 1;
            if counter == 5 {
                break;
            }
        }
        // inner scope ends here
        // In the outer scope, x is still 1
        assert_eq(x, 1);
        "#,
    );
}

#[test]
fn test_scoping_within_closure() {
    test_eval_ok(
        r#"
        let shared_counter = 0;
        let f = fn() {
            assert_eq(shared_counter, 0);
            shared_counter += 1;
            assert_eq(shared_counter, 1);
            
            let shared_counter = 0; // shadowing the outer variable
            shared_counter = 42; // updating the outer variable
            assert_eq(shared_counter, 42);
        };
        f();
        assert_eq(shared_counter, 1);
        "#,
    );
}

#[test]
fn test_captured_environment_is_not_dropped() {
    test_eval_ok(
        r#"
        let f_with_captured_vars = {
            let captured_var = [1, 2, 3];
            fn() {
                captured_var.push(captured_var[captured_var.len() - 1] + 1);
                return captured_var;
            }
        } // captured_var is no longer accessible here, except from the returned function f_with_captured_vars
        
        assert_eq(f_with_captured_vars(), [1, 2, 3, 4]);
        assert_eq(f_with_captured_vars(), [1, 2, 3, 4, 5]);
        assert_eq(f_with_captured_vars(), [1, 2, 3, 4, 5, 6]);
        "#,
    );
}