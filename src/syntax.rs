use std::rc::Rc;
use cake::{*, tree::*};
use cake_derive::RuleContainer;

#[derive(RuleContainer)]
pub struct Main {
    main: Element,
}

impl Module for Main {
    fn new() -> Self {
        add_rules!{
            main := !Symbol::whitespace_or_newline().zero_or_more() +
                Item::item().optional() + g!{!Symbol::newline().one_or_more() + Item::item()}.zero_or_more() +
                !Symbol::whitespace_or_newline().zero_or_more();
        }
    }
}

#[derive(RuleContainer)]
pub struct Symbol {
    whitespace: Element,
    newline: Element,
    whitespace_or_newline: Element,
}

impl Module for Symbol {
    fn new() -> Self {
        add_rules!{
            whitespace := str(" ") | str("\t");
            newline := Symbol::whitespace().zero_or_more() + str("\n") + Symbol::whitespace().zero_or_more();
            whitespace_or_newline := Symbol::newline() | Symbol::whitespace();
        }
    }
}

#[derive(RuleContainer)]
pub struct Item {
    item: Element,
    module: Element,
    visibility: Element,
}

impl Module for Item {
    fn new() -> Self {
        let whitespace = |min: usize| !Symbol::whitespace().min(min);
        let newlines = || !Symbol::newline().one_or_more();

        let run_visibility: ElementCallback = |option | if let Some(children) = option {
            Some(children)
        } else {
            Some(vec![leaf!("private".to_string())])
        };

        add_rules!{
            item := Item::module();
            module := g!{Item::visibility() + whitespace(0)}.optional() + !str("mod") + whitespace(1) + Identifier::identifier() + newlines() +
                g!{g!{Item::item() + newlines()}.zero_or_more()}.name("items") +
                !str("end");
            visibility := str("pub").optional().run(run_visibility);
        }
    }
}

#[derive(RuleContainer)]
pub struct Expression {
    expression: Element,
}

impl Module for Expression {
    fn new() -> Self {
        add_rules!{
            expression := Literal::literal();
        }
    }
}

#[derive(RuleContainer)]
pub struct Identifier {
    identifier: Element,
    pascal_case: Element,
    snake_case: Element,
}

impl Module for Identifier {
    fn new() -> Self {
        add_rules!{
            identifier := Identifier::pascal_case() | Identifier::snake_case();
            pascal_case := g!{regex("[A-Z]") + regex("[a-zA-Z0-9_]").min(0)}.join();
            snake_case := g!{regex("[a-z]") + regex("[a-zA-Z0-9_]").min(0)}.join();
        }
    }
}

#[derive(RuleContainer)]
pub struct Literal {
    literal: Element,
    boolean: Element,
}

impl Module for Literal {
    fn new() -> Self {
        add_rules!{
            literal := Literal::boolean();
            boolean := str("true") | str("false");
        }
    }
}
