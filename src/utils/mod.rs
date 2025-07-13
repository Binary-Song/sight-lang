use std::cell::RefCell;
pub mod interning;

pub struct PushGuard<T>(pub std::rc::Rc<RefCell<Vec<T>>>);
pub struct PushGuards<T>(pub Vec<PushGuard<T>>);

impl<T> Drop for PushGuard<T> {
    fn drop(&mut self) {
        self.0.borrow_mut().pop();
    }
}

impl<T> Drop for PushGuards<T> {
    fn drop(&mut self) {
        while let Some(guard) = self.0.pop() {
            drop(guard);
        }
    }
}

impl<T> PushGuards<T> {
    pub fn new() -> Self {
        PushGuards(Vec::new())
    }

    pub fn push(&mut self, guard: PushGuard<T>) {
        self.0.push(guard);
    }
}
