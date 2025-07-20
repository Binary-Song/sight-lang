use std::ops::Add;
use sight_macros::LiteralValue;

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct Span(pub usize, pub usize);

impl Add for Span {
    type Output = Span;

    fn add(self, other: Span) -> Span {
        let min = self.0.min(other.0);
        let max = self.1.max(other.1);
        Span(min, max)
    }
}

pub trait GetSpanRef {
    fn get_span_ref(&self) -> &Option<Span>;
    fn join_spans(&self, other: &Self) -> Option<Span> {
        let self_span = self.get_span_ref();
        let other_span = other.get_span_ref();
        match (self_span, other_span) {
            (Some(s1), Some(s2)) => Some(Span(s1.0, s2.1)),
            _ => None,
        }
    }
    fn get_span(&self) -> Option<Span> {
        self.get_span_ref().clone()
    }
}

pub trait GetSpanMut: GetSpanRef {
    fn get_span_mut(&mut self) -> &mut Option<Span>;
}
