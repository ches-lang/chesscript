use std::rc::Rc;
use cake::{*, tree::*};
use cake_derive::RuleContainer;

/* Main */

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

/* Symbol */

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

/* Item */

#[derive(RuleContainer)]
pub struct Item {
    item: Element,
    module: Element,
    function: Element,
    function_argument_group: Element,
    function_argument: Element,
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
            function := g!{Item::visibility() + whitespace(0)}.optional() + !str("fn") + whitespace(1) + Identifier::identifier() + whitespace(0) +
                Item::function_argument_group() + whitespace(0) +
                DataType::annotation().optional() + newlines() +
                g!{g!{Expression::expression() + newlines()}.zero_or_more()}.name("expressions") +
                !str("end");
            function_argument_group := !str("(") + whitespace(0) +
                g!{
                    Item::function_argument() + g!{whitespace(0) + !str(",") + whitespace(0) + Item::function_argument()}.zero_or_more()
                }.optional() + whitespace(0) +
                !str(")");
            function_argument := Identifier::identifier() + whitespace(0) + DataType::annotation();
            visibility := str("pub").optional().run(run_visibility);
        }
    }
}

/* Expression */

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

/* Keyword */

#[derive(RuleContainer)]
pub struct Keyword {
    keyword: Element,
}

impl Module for Keyword {
    fn new() -> Self {
        add_rules!{
            keyword := DataType::primitive().expand();
        }
    }
}

/* Identifier */

#[derive(RuleContainer)]
pub struct Identifier {
    identifier: Element,
    pascal_case: Element,
    snake_case: Element,
}

impl Module for Identifier {
    fn new() -> Self {
        add_rules!{
            identifier := g!{Identifier::pascal_case() | Identifier::snake_case()}.except(Keyword::keyword());
            pascal_case := g!{regex("[A-Z]") + regex("[a-zA-Z0-9_]").min(0)}.join();
            snake_case := g!{regex("[a-z]") + regex("[a-zA-Z0-9_]").min(0)}.join();
        }
    }
}

/* DataType */

#[derive(RuleContainer)]
pub struct DataType {
    data_type: Element,
    primitive: Element,
    annotation: Element,
}

impl Module for DataType {
    fn new() -> Self {
        add_rules!{
            data_type := DataType::primitive();
            primitive := str_choices(vec![
                "bool",
                "s8", "s16", "s32", "s64", "ssize",
                "u8", "u16", "u32", "u64", "usize",
                "f32", "f64",
                "char", "str",
            ]);
            annotation := !str(":") + !Symbol::whitespace().zero_or_more() + DataType::data_type();
        }
    }
}

/* Literal */

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
