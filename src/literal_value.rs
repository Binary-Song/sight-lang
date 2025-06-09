use std::fmt::format;

pub trait LiteralValue {
    fn literal_value(&self) -> String;
}
mod simple_type_impls {
    use super::LiteralValue;

    impl LiteralValue for i32 {
        fn literal_value(&self) -> String {
            self.to_string()
        }
    }

    impl LiteralValue for bool {
        fn literal_value(&self) -> String {
            self.to_string()
        }
    }

    impl LiteralValue for u8 {
        fn literal_value(&self) -> String {
            self.to_string()
        }
    }

    impl LiteralValue for u16 {
        fn literal_value(&self) -> String {
            self.to_string()
        }
    }

    impl LiteralValue for u32 {
        fn literal_value(&self) -> String {
            self.to_string()
        }
    }

    impl LiteralValue for u64 {
        fn literal_value(&self) -> String {
            self.to_string()
        }
    }

    impl LiteralValue for u128 {
        fn literal_value(&self) -> String {
            self.to_string()
        }
    }

    impl LiteralValue for usize {
        fn literal_value(&self) -> String {
            self.to_string()
        }
    }

    impl LiteralValue for i8 {
        fn literal_value(&self) -> String {
            self.to_string()
        }
    }

    impl LiteralValue for i16 {
        fn literal_value(&self) -> String {
            self.to_string()
        }
    }

    impl LiteralValue for i64 {
        fn literal_value(&self) -> String {
            self.to_string()
        }
    }

    impl LiteralValue for i128 {
        fn literal_value(&self) -> String {
            self.to_string()
        }
    }

    impl LiteralValue for isize {
        fn literal_value(&self) -> String {
            self.to_string()
        }
    }

    impl LiteralValue for f32 {
        fn literal_value(&self) -> String {
            self.to_string()
        }
    }

    impl LiteralValue for f64 {
        fn literal_value(&self) -> String {
            self.to_string()
        }
    }

    impl LiteralValue for char {
        fn literal_value(&self) -> String {
            format!("{:?}", self)
        }
    }

    impl LiteralValue for String {
        fn literal_value(&self) -> String {
            format!("{:?}.to_string()", self)
        }
    }
}

mod tuple_impls {
    use super::LiteralValue;

    impl LiteralValue for () {
        fn literal_value(&self) -> String {
            "()".to_string()
        }
    }

    impl<T1: LiteralValue, T2: LiteralValue> LiteralValue for (T1, T2) {
        fn literal_value(&self) -> String {
            format!("({}, {})", self.0.literal_value(), self.1.literal_value())
        }
    }

    impl<T1: LiteralValue, T2: LiteralValue, T3: LiteralValue> LiteralValue for (T1, T2, T3) {
        fn literal_value(&self) -> String {
            format!(
                "({}, {}, {})",
                self.0.literal_value(),
                self.1.literal_value(),
                self.2.literal_value()
            )
        }
    }

    impl<T1: LiteralValue, T2: LiteralValue, T3: LiteralValue, T4: LiteralValue> LiteralValue
        for (T1, T2, T3, T4)
    {
        fn literal_value(&self) -> String {
            format!(
                "({}, {}, {}, {})",
                self.0.literal_value(),
                self.1.literal_value(),
                self.2.literal_value(),
                self.3.literal_value()
            )
        }
    }

    impl<
            T1: LiteralValue,
            T2: LiteralValue,
            T3: LiteralValue,
            T4: LiteralValue,
            T5: LiteralValue,
        > LiteralValue for (T1, T2, T3, T4, T5)
    {
        fn literal_value(&self) -> String {
            format!(
                "({}, {}, {}, {}, {})",
                self.0.literal_value(),
                self.1.literal_value(),
                self.2.literal_value(),
                self.3.literal_value(),
                self.4.literal_value()
            )
        }
    }
}

mod generic_impls {
    use super::LiteralValue;

    impl<T: LiteralValue> LiteralValue for Vec<T> {
        fn literal_value(&self) -> String {
            let values: Vec<String> = self.iter().map(|v| v.literal_value()).collect();
            format!("vec![{}]", values.join(", "))
        }
    }

    impl<T: LiteralValue> LiteralValue for Box<T> {
        fn literal_value(&self) -> String {
            format!("Box::new({})", self.as_ref().literal_value())
        }
    }
    
    impl<T: LiteralValue> LiteralValue for Option<T> {
        fn literal_value(&self) -> String {
            match self {
                Some(value) => format!("Some({value})", value = value.literal_value()),
                None => "None".to_string(),
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::LiteralValue;
    use sight_macros::LiteralValue;

    #[test]
    fn test_string_literal_value() {
        let s = "Hello, World!".to_string();
        assert_eq!(s.literal_value(), r#""Hello, World!".to_string()"#);
    }

    #[test]
    fn test_i32_literal_value() {
        let i = 42;
        assert_eq!(i.literal_value(), "42");
    }

    #[test]
    fn test_bool_literal_value() {
        let b = true;
        assert_eq!(b.literal_value(), "true");
    }

    #[test]
    fn test_unit_literal_value() {
        let u = ();
        assert_eq!(u.literal_value(), "()".to_string());
    }

    #[test]
    fn test_option_some_literal_value() {
        let opt: Option<i32> = Some(42);
        assert_eq!(opt.literal_value(), "Some(42)");
    }

    #[test]
    fn test_option_none_literal_value() {
        let opt: Option<i32> = None;
        assert_eq!(opt.literal_value(), "None");
    }

    #[test]
    fn test_struct() {
        #[derive(LiteralValue)]
        struct X {
            a: String,
            b: i32,
            c: bool,
        }

        let x = X {
            a: "Hello".to_string(),
            b: 42,
            c: true,
        };

        assert_eq!(
            x.literal_value(),
            "X{a:\"Hello\".to_string(), b:42, c:true}"
        );
    }

    #[test]
    fn test_enum() {
        #[derive(LiteralValue)]
        enum X {
            A,
            B(String, i32),
            C { x: bool, y: i32 },
        }
        let x1 = X::A;
        let x2 = X::B("Hello".to_string(), 42);
        let x3 = X::C { x: true, y: 100 };
        assert_eq!(x1.literal_value(), "X::A");
        assert_eq!(x2.literal_value(), "X::B(\"Hello\".to_string(), 42)");
        assert_eq!(x3.literal_value(), "X::C{x:true, y:100}");
    }
}
