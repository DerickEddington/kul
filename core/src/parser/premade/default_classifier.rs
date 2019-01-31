use crate::parser::CharClassifier;


/// A [`CharClassifier`](trait.CharClassifier.html) that uses the common `{`,
/// `}`, and `\` characters and the Unicode whitespace property.
pub struct DefaultCharClassifier;

impl CharClassifier for DefaultCharClassifier {
    #[inline]
    fn is_nest_start(&self, c: char) -> bool {
        '{' == c
    }

    #[inline]
    fn is_nest_end(&self, c: char) -> bool {
        '}' == c
    }

    #[inline]
    fn is_nest_escape(&self, c: char) -> bool {
        '\\' == c
    }

    #[inline]
    fn is_whitespace(&self, c: char) -> bool {
        c.is_whitespace()
    }
}
