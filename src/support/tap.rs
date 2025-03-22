pub trait TapOwned<T> {
    fn tap_owned(self, tap: impl FnOnce(T) -> T) -> T;
}

impl<T> TapOwned<T> for T {
    fn tap_owned(self, tap: impl FnOnce(T) -> T) -> T {
        tap(self)
    }
}

