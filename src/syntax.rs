use std::rc::Rc;
use cake::{*, tree::*};
use cake_derive::RuleContainer;

const WHITESPACE: fn(usize) -> Element = |min: usize| !Symbol::whitespace().min(min);
const NEWLINES: fn() -> Element = || !Symbol::newline().one_or_more();

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
    // fix: rename to formal_argument_group
    function_argument_group: Element,
    function_argument: Element,
    visibility: Element,
}

impl Module for Item {
    fn new() -> Self {
        let run_visibility: ElementCallback = |option | if let Some(children) = option {
            Some(children)
        } else {
            Some(vec![leaf!("private".to_string())])
        };

        add_rules!{
            item := Item::module() | Item::function();
            module := g!{Item::visibility() + WHITESPACE(0)}.optional() + !str("mod") + WHITESPACE(1) + Identifier::identifier() + NEWLINES() +
                g!{g!{Item::item() + NEWLINES()}.zero_or_more()}.name("items") +
                !str("end");
            function := g!{Item::visibility() + WHITESPACE(0)}.optional() + !str("fn") + WHITESPACE(1) + Identifier::identifier() + WHITESPACE(0) +
                Item::function_argument_group() + WHITESPACE(0) +
                DataType::annotation().optional() + NEWLINES() +
                g!{g!{Expression::expression() + NEWLINES()}.zero_or_more()}.name("expressions") +
                !str("end");
            function_argument_group :=
                !str("(") +
                WHITESPACE(0) +
                g!{
                    Item::function_argument() + g!{WHITESPACE(0) + !str(",") + WHITESPACE(0) + Item::function_argument()}.zero_or_more()
                }.optional() +
                WHITESPACE(0) +
                !str(")");
            function_argument := Identifier::identifier() + WHITESPACE(0) + DataType::annotation();
            visibility := str("pub").optional().run(run_visibility);
        }
    }
}

/* Expression */

#[derive(RuleContainer)]
pub struct Expression {
    expression: Element,
    expression_element: Element,
}

impl Module for Expression {
    fn new() -> Self {
        add_rules!{
            expression := Expression::expression_element().expand() +
                g!{!str(".") + Expression::expression_element().expand()}.zero_or_more();
            expression_element :=
                Literal::literal() |
                Function::call() |
                DataType::data_type() |
                Identifier::identifier();
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
            // todo: add keywords
            keyword := DataType::primitive().expand() | str_choices(vec![
                "end", "false", "fn", "mod", "pub", "true",
            ]);
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
    integer: Element,
    float: Element,
    annotation: Element,
}

impl Module for DataType {
    fn new() -> Self {
        add_rules!{
            data_type := DataType::primitive();
            primitive := str_choices(vec!["bool", "char", "str"]) | DataType::integer().expand() | DataType::float().expand();
            integer := str_choices(vec![
                "s8", "s16", "s32", "s64", "ssize",
                "u8", "u16", "u32", "u64", "usize",
            ]);
            float := str_choices(vec!["f32", "f64"]);
            annotation := !str(":") + !Symbol::whitespace().zero_or_more() + DataType::data_type();
        }
    }
}

/* Literal */

#[derive(RuleContainer)]
pub struct Literal {
    literal: Element,
    boolean: Element,
    integer: Element,
    based_integer: Element,
    float: Element,
    character: Element,
    string: Element,

    /* Materials */
    single_character: Element,
    escape_sequence: Element,
    general_escape_suffix: Element,
    unicode_escape_suffix: Element,
}

impl Module for Literal {
    fn new() -> Self {
        let number_chain = |digit: Element| g!{
            g!{
                digit.clone().min(1).max(3) +
                g!{
                    !str("_") + digit.clone().times(3)
                }.one_or_more()
            } |
            digit.one_or_more()
        }.join();

        add_rules!{
            literal :=
                Literal::boolean() |
                Literal::float() |
                Literal::integer() |
                Literal::character() |
                Literal::string();
            boolean := str("true") | str("false");
            integer :=
                g!{
                    Literal::based_integer().expand() | number_chain(regex("[0-9]")).group().name("dec")
                } +
                g!{
                    DataType::integer().expand() |
                    DataType::float().expand()
                }.name("data_type_suffix").optional();
            based_integer :=
                g!{
                    !str("0b") + number_chain(regex("[0-1]"))
                }.name("bin") |
                g!{
                    !str("0o") + number_chain(regex("[0-7]"))
                }.name("oct") |
                g!{
                    !str("0x") + number_chain(regex("[0-9a-f]"))
                }.name("hex");
            float :=
                g!{
                    number_chain(regex("[0-9]")).group().name("integer") + !str(".") + number_chain(regex("[0-9]")).group().name("decimal")
                } +
                DataType::float().expand().name("data_type_suffix").optional();
            character := !str("'") + Literal::single_character().except(str("'") | str("\\\"")) + !str("'");
            string := !str("\"") + g!{Literal::single_character().except(str("\n") | str("\"") | str("\\'"))}.group().zero_or_more() + !str("\"");

            /* Materials */
            single_character := Literal::escape_sequence() | wildcard().except(str("\n") | str("\\"));
            escape_sequence := !str("\\") + g!{
                Literal::general_escape_suffix() |
                Literal::unicode_escape_suffix()
            };
            general_escape_suffix := str_choices(vec![
                // todo: Add characters.
                "0", "n", "t", "\\",
            ]);
            unicode_escape_suffix := !str("u{") + regex("[0-9a-f]").min(1).max(6).group().join() + !str("}");
        }
    }
}

/* Function */

#[derive(RuleContainer)]
pub struct Function {
    call: Element,
    actual_argument_group: Element,
    actual_argument: Element,
}

impl Module for Function {
    fn new() -> Self {
        add_rules!{
            call := Identifier::identifier() + WHITESPACE(0) + Function::actual_argument_group();
            actual_argument_group :=
                !str("(") +
                WHITESPACE(0) +
                g!{
                    Function::actual_argument() + g!{WHITESPACE(0) + !str(",") + WHITESPACE(0) + Function::actual_argument()}.zero_or_more()
                }.optional() +
                WHITESPACE(0) +
                !str(")");
            actual_argument := Expression::expression();
        }
    }
}
