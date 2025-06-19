pub struct IdMap<T> {
    map: std::collections::HashMap<usize, Option<T>>,
}

impl<T> IdMap<T> {
    pub fn new() -> Self {
        IdMap {
            map: std::collections::HashMap::new(),
        }
    }

    pub fn push(&mut self, value: T) -> usize {
        self.map.insert(self.map.len(), Some(value));
        self.map.len() - 1
    }

    pub fn push_none(&mut self) -> usize {
        self.map.insert(self.map.len(), None);
        self.map.len() - 1
    }

    pub fn get(&self, id: usize) -> Option<&T> {
        self.map.get(&id).and_then(|opt| opt.as_ref())
    }

    pub fn set(&mut self, id: usize, value: T) {
        self.map.insert(id, Some(value));
    }

    pub fn set_none(&mut self, id: usize) {
        self.map.insert(id, None);
    }
}
