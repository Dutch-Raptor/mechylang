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
#[cfg(not(test))]
const TRACE_ENABLED: bool = false;
#[cfg(test)]
const TRACE_ENABLED: bool = false;

pub fn reset_trace() {
    unsafe {
        INDENT = 0;
    }
}

pub fn start_trace(name: &str) -> &str {
    if !TRACE_ENABLED {
        return name;
    }
    unsafe {
        eprintln!("{}Begin {}", " ".repeat(INDENT * 2), name);
        INDENT += 1;
    }
    return name;
}

pub fn stop_trace(name: &str) {
    if !TRACE_ENABLED {
        return;
    }
    unsafe {
        INDENT -= 1;
        eprintln!("{}End {}", " ".repeat(INDENT * 2), name);
    }
}

#[macro_export]
macro_rules! trace {
    ($name:expr) => {{
        let name = $name.to_string();
        $crate::tracer::start_trace(name.as_str());
        $crate::tracer::defer(move || {
            $crate::tracer::stop_trace(name.as_str());
        })
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
