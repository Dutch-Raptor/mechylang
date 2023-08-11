struct Defer<F: FnOnce()>(Option<F>);
impl<F: FnOnce()> Drop for Defer<F> {
    fn drop(&mut self) {
        if let Some(f) = self.0.take() {
            f();
        }
    }
}

pub fn defer<F: FnOnce()>(f: F) -> impl Drop {
    Defer(Some(f))
}

static mut INDENT: usize = 0;

pub fn start_trace(name: &str) -> &str {
    unsafe {
        println!("{}Begin {}", " ".repeat(INDENT * 2), name);
        INDENT += 1;
    }
    return name;
}

pub fn stop_trace(name: &str) {
    unsafe {
        INDENT -= 1;
        println!("{}End {}", " ".repeat(INDENT * 2), name);
    }
}

#[macro_export]
macro_rules! trace {
    ($name:expr) => {{
        $crate::tracer::start_trace($name);
        $crate::tracer::defer(|| $crate::tracer::stop_trace($name))
    }};
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, rc::Rc};

    use super::*;

    #[test]
    fn test_defer() {
        let x = Rc::new(RefCell::new(0));
        {
            let _y = defer(|| *x.borrow_mut() += 1);
            assert_eq!(*x.borrow(), 0);
        }
        assert_eq!(*x.borrow(), 1);
    }
}
