use crate::LiteralValue;
use std::cell::{RefCell, RefMut};
use std::collections::HashMap;
use std::fmt::format;
use std::hash::Hash;
use std::marker::PhantomData;

/// An internable type allows for interning.
///
/// See [Interned] for more details.
pub trait Internable: Eq + Hash + Sized + Clone {}

pub trait StaticInternable: Internable + 'static {
    fn interner() -> &'static std::thread::LocalKey<RefCell<Interner<Self>>>;
    fn intern(self) -> Interned<Self> {
        static_intern(self)
    }
}

pub struct Interner<T: Internable> {
    data: Vec<T>,
    indices: HashMap<T, usize>,
}

pub type InternString = Interned<String>;

/// An id to a thread-locally deduped (aka. interned) object of type `T`.
/// Interning is a technique to store only one copy of each distinct value,
/// which can save memory and improve performance.
/// See [wikipedia](https://en.wikipedia.org/wiki/Interning_(computer_science)) for more on interning.
///
/// To get the original value back, use the [`unintern`] function.
pub struct Interned<T: Internable>(usize, PhantomData<T>);

impl<T: StaticInternable> Interned<T> {
    pub fn static_unintern(self) -> T {
        static_unintern(self)
    }
}

impl<T: Internable> std::fmt::Debug for Interned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Interned({:?})", self.0)
    }
}

impl<T: Internable> Interner<T> {
    pub fn new() -> Self {
        Interner {
            data: Vec::new(),
            indices: HashMap::new(),
        }
    }

    pub fn intern(&mut self, s: T) -> Interned<T> {
        if let Some(index) = self.indices.get(&s) {
            return Interned(*index, PhantomData);
        }
        let index = self.data.len();
        self.indices.insert(s.clone(), index);
        self.data.push(s);
        Interned(index, PhantomData)
    }

    pub fn unintern(&self, t: Interned<T>) -> T {
        if let Some(value) = self.data.get(t.0) {
            value.clone()
        } else {
            panic!("Attempted to unintern an invalid index: {}", t.0);
        }
    }
}

fn static_intern<T: StaticInternable>(t: T) -> Interned<T> {
    T::interner().with(|interner| {
        let mut interner = interner.borrow_mut();
        interner.intern(t)
    })
}

fn static_unintern<T: StaticInternable>(t: Interned<T>) -> T {
    T::interner().with(|interner| {
        let mut interner = interner.borrow_mut();
        interner.unintern(t)
    })
}

impl Internable for String {}

impl StaticInternable for String {
    fn interner() -> &'static std::thread::LocalKey<RefCell<Interner<String>>> {
        thread_local! {
            static INTERNER: RefCell<Interner<String>> = RefCell::new(Interner::<String>::new());
        }
        let t: &'static _ = &INTERNER;
        t
    }
}

impl InternString {
    pub fn from_string(s: String) -> Self {
        static_intern(s)
    }

    pub fn from_str(s: &str) -> Self {
        static_intern(s.to_string())
    }

    pub fn to_string(self) -> String {
        static_unintern(self)
    }
}

impl<T: Internable> Interned<T> {
    pub fn unintern(self, interner: &mut Interner<T>) -> T {
        interner.unintern(self)
    }
}

impl<T: Internable> LiteralValue for Interned<T> {
    fn literal_value(&self) -> String {
        format!("Interned({})", self.0.literal_value())
    }
}

impl<T: Internable> Clone for Interned<T> {
    fn clone(&self) -> Self {
        Interned(self.0, PhantomData)
    }
}

impl<T: Internable> Copy for Interned<T> {}

impl<T: Internable> PartialEq for Interned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T: Internable> Eq for Interned<T> {}

impl<T: Internable> std::hash::Hash for Interned<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}
