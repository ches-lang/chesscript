use crate::hir::HirIdentifierKind;
use cake::{*, tree::*};

pub(crate) struct SyntaxElementGenerator;

impl SyntaxElementGenerator {
    pub fn main(content: SyntaxChild) -> SyntaxChild {
        node!{
            "Main::main" => vec![
                content,
            ]
        }
    }

    pub fn keyword(value: &str) -> SyntaxChild {
        node!{
            "Keyword::keyword" => vec![
                leaf!(value),
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

    pub fn data_type(content: SyntaxChild) -> SyntaxChild {
        node!{
            "DataType::data_type" => vec![
                content,
            ]
        }
    }

    pub fn primitive_data_type(name: &str) -> SyntaxChild {
        node!{
            "DataType::primitive" => vec![
                leaf!(name),
            ]
        }
    }

    pub fn data_type_annotation(data_type: SyntaxChild) -> SyntaxChild {
        node!{
            "DataType::annotation" => vec![
                data_type,
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

    pub fn integer_literal(base: &str, int_value: &str, type_suffix: Option<&str>) -> SyntaxChild {
        let int_node = node!{
            base => vec![
                leaf!(int_value),
            ]
        };

        let children = if let Some(type_suffix) = type_suffix {
            vec![
                int_node,
                node!{
                    "data_type_suffix" => vec![
                        leaf!(type_suffix),
                    ]
                },
            ]
        } else {
            vec![
                int_node
            ]
        };

        SyntaxElementGenerator::literal(node!{
            "Literal::integer" => children
        })
    }

    pub fn float_literal(int_value: &str, decimal_value: &str, type_suffix: Option<&str>) -> SyntaxChild {
        let int_node = node!{
            "integer" => vec![
                leaf!(int_value),
            ]
        };

        let decimal_node = node!{
            "decimal" => vec![
                leaf!(decimal_value),
            ]
        };

        let children = if let Some(type_suffix) = type_suffix {
            vec![
                int_node,
                decimal_node,
                node!{
                    "data_type_suffix" => vec![
                        leaf!(type_suffix),
                    ]
                }
            ]
        } else {
            vec![
                int_node,
                decimal_node,
            ]
        };

        SyntaxElementGenerator::literal(node!{
            "Literal::float" => children
        })
    }

    pub fn escape_sequence(suffix: SyntaxChild) -> SyntaxChild {
        node!{
            "Literal::escape_sequence" => vec![
                suffix,
            ]
        }
    }

    pub fn general_escape_sequence(character: &str) -> SyntaxChild {
        SyntaxElementGenerator::escape_sequence(
            node!{
                "Literal::general_escape_suffix" => vec![
                    leaf!(character),
                ]
            },
        )
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

    pub fn function(
        visibility: SyntaxChild,
        id: SyntaxChild,
        arg_group: SyntaxChild,
        return_type_annotation: Option<SyntaxChild>,
        expressions: Vec<SyntaxChild>,
    ) -> SyntaxChild {
        let function_node = if let Some(return_type_annotation) = return_type_annotation {
            vec![
                visibility,
                id,
                arg_group,
                return_type_annotation,
                node!{
                    "expressions" => expressions
                },
            ]
        } else {
            vec![
                visibility,
                id,
                arg_group,
                node!{
                    "expressions" => expressions
                },
            ]
        };

        node!{
            "Item::function" => function_node
        }
    }

    pub fn function_argument_group(args: Vec<SyntaxChild>) -> SyntaxChild {
        node!{
            "Item::function_argument_group" => args
        }
    }

    pub fn function_argument(id: SyntaxChild, data_type_annotation: SyntaxChild) -> SyntaxChild {
        node!{
            "Item::function_argument" => vec![
                id,
                data_type_annotation,
            ]
        }
    }

    pub fn visibility(visibility: &str) -> SyntaxChild {
        node!{
            "Item::visibility" => vec![
                leaf!(visibility),
            ]
        }
    }
}
