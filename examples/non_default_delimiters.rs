#![allow(missing_docs, clippy::dbg_macro, clippy::print_stdout)]

use kul::{
    Parser, Datum, Text as _,
    parser::CharClassifier,
    common::inmem
};


#[derive(Clone, Copy, Debug)]
pub struct CustomCharClassifier;

impl CharClassifier for CustomCharClassifier {
    fn is_nest_start(&self, c: char) -> bool {
        '⟪' == c
    }

    fn is_nest_end(&self, c: char) -> bool {
        '⟫' == c
    }

    fn is_nest_escape(&self, c: char) -> bool {
        '␛' == c
    }

    fn is_whitespace(&self, c: char) -> bool {
        c.is_whitespace()
    }
}


fn main() {
    let input = r#"
Using non-default delimiters:

⟪⟪source-code Rust⟫
    use kul::common::inmem::parse_str;

    fn main() {
        let input = "Escaped the {bold non-default} delimiters: ␛⟪, ␛⟫, ␛␛";
        dbg!(parse_str(input));
    }
⟫
"#;
    let mut parser = Parser {
        classifier: CustomCharClassifier,
        allocator: inmem::DatumAllocator::<'_, ()>::default(),
        bindings: inmem::OperatorBindings::<'_, _, ()>::default(),
    };
    let ast = parser.parse(inmem::Text::from_str(input).iter()).collect::<Vec<_>>();
    dbg!(&ast);

    if let Ok(Datum::Combination{operands, ..}) = &ast[1] {
        if let Datum::List{elem, ..} = &**operands {
            if let Datum::Text(text) = &**elem {
                let selected: String = text.chars().collect();
                println!("\nSelected text:\n{}", selected);
            }
        }
    }
}
