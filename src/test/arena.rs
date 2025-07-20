use crate::container::Arena;
use crate::container::Container;
use crate::{
    container::{EncodeError, Interner},
    multicontainer,
};

#[test]
fn simple_test() {
    let f1 = || -> Option<()> {
        let mut arena = multicontainer!(
            Arena::<i32>::default(),
            Arena::<u32>::default(),
            Arena::<bool>::default(),
            Arena::<Vec<String>>::default(),
            Interner::<String>::default()
        );

        let a = arena.encode(32i32)?;
        let b = arena.encode(64u32)?;
        let c = arena.encode(true)?;
        let d = arena.encode(vec!["foo".to_string(), "bar".to_string()])?;

        assert_eq!(arena.decode(a)?, 32i32);
        assert_eq!(arena.decode(c)?, true);
        assert_eq!(arena.decode(d)?, vec!["foo".to_string(), "bar".to_string()]);
        assert_eq!(arena.decode(b)?, 64u32);
        Some(())
    };
    f1().unwrap();
}

#[test]
fn bad_type() {
    let mut arena = Arena::<i32>::default();
    match arena.encode_extra("") {
        Err(EncodeError::UnsupportedType(t)) => {
            // ok, expected
        }
        _ => panic!("Expected UnsupportedType error"),
    }
}

#[test]
fn interning_should_work() {
    let mut interner = Interner::<String>::default();
    let id1 = interner.encode("hello".to_string()).unwrap();
    let id2 = interner.encode("world".to_string()).unwrap();
    let id3 = interner.encode("hello".to_string()).unwrap(); // should return the same ID as id1

    assert_eq!(id1, id3);
    assert_ne!(id1, id2);

    assert_eq!(interner.decode(id1).unwrap(), "hello");
    assert_eq!(interner.decode(id2).unwrap(), "world");
}
