use cake::{*, tree::*};

use crate::hir::HirIdentifierKind;

pub(crate) struct SyntaxElementGenerator;

impl SyntaxElementGenerator {
    pub fn main(content: SyntaxChild) -> SyntaxChild {
        node!{
            "Main::main" => vec![
                content,
            ]
        }
    }

    pub fn identifier(id_kind: HirIdentifierKind, id: &str) -> SyntaxChild {
        let node_name = match id_kind {
            HirIdentifierKind::PascalCase => "pascal_case",
            HirIdentifierKind::SnakeCase => "snake_case",
        };

        node!{
            "Identifier::identifier" => vec![
                node!{
                    format!("Identifier::{}", node_name) => vec![
                        leaf!(id),
                    ]
                },
            ]
        }
    }

    pub fn visibility(visibility: &str) -> SyntaxChild {
        node!{
            "Misc::visibility" => vec![
                leaf!(visibility),
            ]
        }
    }

    pub fn item(content: SyntaxChild) -> SyntaxChild {
        node!{
            "Item::item" => vec![
                content,
            ]
        }
    }

    pub fn module(visibility: &str, id: SyntaxChild, items: Vec<SyntaxChild>) -> SyntaxChild {
        node!{
            "Item::module" => vec![
                SyntaxElementGenerator::visibility(visibility),
                id,
                node!{
                    "items" => items
                },
            ]
        }
    }

    pub fn expression(content: SyntaxChild) -> SyntaxChild {
        node!{
            "Expression::expression" => vec![
                content,
            ]
        }
    }

    pub fn literal(content: SyntaxChild) -> SyntaxChild {
        node!{
            "Literal::literal" => vec![
                content,
            ]
        }
    }

    pub fn boolean_literal(value: &str) -> SyntaxChild {
        SyntaxElementGenerator::literal(node!{
            "Literal::boolean" => vec![
                leaf!(value),
            ]
        })
    }
}
