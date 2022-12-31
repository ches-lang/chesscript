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
            main := !Misc::whitespace_or_newline().zero_or_more() +
                Item::item().optional() + g!{!Misc::newline().one_or_more() + Item::item()}.zero_or_more() +
                !Misc::whitespace_or_newline().zero_or_more();
        }
    }
}

#[derive(RuleContainer)]
pub struct Misc {
    visibility: Element,
    whitespace: Element,
    newline: Element,
    whitespace_or_newline: Element,
}

impl Module for Misc {
    fn new() -> Self {
        let run_visibility: ElementCallback = |option | if let Some(children) = option {
            Some(children)
        } else {
            Some(
                vec![
                    leaf!("private".to_string()),
                ],
            )
        };

        add_rules!{
            visibility := str("pub").optional().run(run_visibility);
            whitespace := str(" ") | str("\t");
            newline := Misc::whitespace().zero_or_more() + str("\n") + Misc::whitespace().zero_or_more();
            whitespace_or_newline := Misc::newline() | Misc::whitespace();
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

#[derive(RuleContainer)]
pub struct Item {
    item: Element,
    module: Element,
}

impl Module for Item {
    fn new() -> Self {
        let whitespace = |min: usize| !Misc::whitespace().min(min);
        let newlines = || !Misc::newline().one_or_more();

        add_rules!{
            item := Item::module();
            module := g!{Misc::visibility() + whitespace(0)}.optional() + !str("mod") + whitespace(1) + Identifier::identifier() + newlines() +
                g!{g!{Expression::expression() + newlines()}.zero_or_more()}.name("expressions") +
                !str("end");
        }
    }
}
