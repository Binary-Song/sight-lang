#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub struct VocabKey(u32);

pub trait Vocab
{
    fn get(&self, key: VocabKey) -> Option<&str>;
    fn insert(&mut self, text: &str) -> VocabKey;
}

pub struct VocabImpl {
    pub dict: std::collections::HashMap<String, VocabKey>,
    pub next_id: u32,
}

impl Vocab for VocabImpl {
    fn get(&self, key: VocabKey) -> Option<&str> {
        self.dict.iter().find_map(|(k, v)| if *v == key { Some(k.as_str()) } else { None })
    }

    fn insert(&mut self, text: &str) -> VocabKey {
        if let Some(key) = self.dict.get(text) {
            return *key;
        }
        let key = VocabKey(self.next_id);
        self.dict.insert(text.to_string(), key);
        self.next_id += 1;
        key
    }
}
