use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::format;

use crate::LiteralValue;

struct StringInterner {
    map: HashMap<String, InternString>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InternString(usize);

thread_local! {
    static INTERNER: RefCell<StringInterner> = RefCell::new(StringInterner::new());
}

impl LiteralValue for InternString {
    fn literal_value(&self) -> String {
        format!("InternString::from_str({:?})", self.to_string())
    }
}

impl StringInterner {
    fn new() -> Self {
        StringInterner {
            map: HashMap::new(),
        }
    }

    fn intern(&mut self, s: &str) -> InternString {
        if let Some(interned) = self.map.get(s) {
            return interned.clone();
        }
        let index = self.map.len();
        let interned = InternString(index);
        self.map.insert(s.to_string(), interned.clone());
        interned
    }
}

impl InternString {
    pub fn from_str(s: &str) -> Self {
        INTERNER.with(|interner| {
            let mut interner = interner.borrow_mut();
            interner.intern(s)
        })
    }

    pub fn to_string(&self) -> String {
        INTERNER.with(|interner| {
            let interner = interner.borrow();
            interner
                .map
                .iter()
                .find_map(|(k, v)| if v.0 == self.0 { Some(k.clone()) } else { None })
                .expect("InternString not found")
        })
    }
}
