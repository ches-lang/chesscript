use std::{result::Result, num::IntErrorKind};
use cake::tree::*;

use crate::{compiler::log::{CompilerLog, CompilerWarningLog, CompilerErrorLog}, syntax::Expression};

#[derive(Clone, Debug, PartialEq)]
pub enum HirGeneratorError {}

pub type HirGeneratorResult<T> = Result<T, HirGeneratorError>;

#[derive(Clone, Debug, PartialEq)]
pub struct HirGeneratorOptions;

#[derive(Clone, Debug, PartialEq)]
pub enum HirIdentifierKind {
    PascalCase,
    SnakeCase,
}

pub struct HirGenerator<'a> {
    options: &'a HirGeneratorOptions,
    logs: Vec<CompilerLog>,
}

impl<'a> HirGenerator<'a> {
    pub fn new(options: &'a HirGeneratorOptions) -> HirGenerator {
        HirGenerator {
            options: options,
            logs: Vec::new(),
        }
    }

    pub fn generate(mut self, tree: &SyntaxTree) -> HirGeneratorResult<(Hir, Vec<CompilerLog>)> {
        let item_nodes = tree.root.children.iter();
        let mut items = Vec::new();

        for each_item in item_nodes {
            items.push(self.item(each_item.into_node())?);
        }

        let hir = Hir {
            items: items,
        };

        Ok((hir, self.logs))
    }

    pub fn item(&mut self, node: &SyntaxNode) -> HirGeneratorResult<HirItem> {
        let child_node = node.child_node_at(0);

        let item = match child_node.name.as_str() {
            "Item::module" => HirItem::Module(self.module(child_node)?),
            _ => unreachable!(),
        };

        Ok(item)
    }

    pub fn module(&mut self, node: &SyntaxNode) -> HirGeneratorResult<HirModule> {
        let identifier_node  = node.search_node("Identifier::identifier").unwrap();
        let identifier = self.identifier(identifier_node, HirIdentifierKind::PascalCase);
        let visibility = HirVisibility::Private;
        let mut items = Vec::new();

        for each_item in &node.search_node("items").unwrap().children {
            items.push(self.item(each_item.into_node())?);
        }

        let module = HirModule {
            identifier: identifier,
            visibility: visibility,
            items: items,
        };

        Ok(module)
    }

    pub fn identifier(&mut self, node: &SyntaxNode, valid_identifier_kind: HirIdentifierKind) -> String {
        let child_node = node.child_node_at(0);

        let identifier_kind = match child_node.name.as_str() {
            "Identifier::pascal_case" => HirIdentifierKind::PascalCase,
            "Identifier::snake_case" => HirIdentifierKind::SnakeCase,
            _ => unreachable!(),
        };

        let identifier = &child_node.child_leaf_at(0).value;

        if valid_identifier_kind != identifier_kind {
            let new_log = CompilerLog::Warning(CompilerWarningLog::IdentifierShouldBePascalCase { id: identifier.to_string() });
            self.logs.push(new_log);
        }

        identifier.to_string()
    }

    pub fn expression(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        let child_node = node.child_node_at(0);

        let expression = match child_node.name.as_str() {
            "Literal::literal" => HirExpression::Literal(self.literal(child_node)?),
            "DataType::data_type" => HirExpression::DataType(self.data_type(child_node)),
            _ => unreachable!(),
        };

        Some(expression)
    }

    pub fn data_type(&mut self, node: &SyntaxNode) -> HirDataType {
        let child_node = node.child_node_at(0);

        match child_node.name.as_str() {
            "DataType::primitive" => {
                let primitive = HirPrimitiveDataType::from(
                    child_node.child_leaf_at(0).value.as_str(),
                );
                HirDataType::Primitive(primitive)
            },
            _ => unreachable!(),
        }
    }

    pub fn literal(&mut self, node: &SyntaxNode) -> Option<HirLiteral> {
        let child_node = node.child_node_at(0);

        let literal = match child_node.name.as_str() {
            "Literal::boolean" => HirLiteral::Boolean { value: child_node.child_leaf_at(0).value.clone() },
            "Literal::integer" => {
                let based_integer_node = child_node.child_node_at(0);
                let integer = based_integer_node.child_leaf_at(0).value.clone();

                let base = match based_integer_node.name.as_str() {
                    "bin" => HirNumberBase::Binary,
                    "dec" => HirNumberBase::Decimal,
                    "oct" => HirNumberBase::Octal,
                    "hex" => HirNumberBase::Hexadecimal,
                    _ => unreachable!(),
                };

                let data_type = match child_node.search_node("data_type_suffix") {
                    Some(type_suffix_node) => {
                        let type_suffix = HirDataType::Primitive(
                            HirPrimitiveDataType::from(
                                type_suffix_node.child_leaf_at(0).value.as_str(),
                            ),
                        );

                        Some(type_suffix)
                    },
                    None => None,
                };

                HirLiteral::Integer { base: base, integer: integer, data_type: data_type }
            },
            "Literal::float" => {
                let integer = child_node.search_node("integer").unwrap().child_leaf_at(0).value.clone();
                let decimal = child_node.search_node("decimal").unwrap().child_leaf_at(0).value.clone();

                let data_type = match child_node.search_node("data_type_suffix") {
                    Some(type_suffix_node) => {
                        let type_suffix = HirDataType::Primitive(
                            HirPrimitiveDataType::from(
                                type_suffix_node.child_leaf_at(0).value.as_str(),
                            ),
                        );

                        Some(type_suffix)
                    },
                    None => None,
                };

                HirLiteral::Float { integer: integer, decimal: decimal, data_type: data_type }
            },
            "Literal::character" => match self.single_character(child_node.child_node_at(0)) {
                Some(v) => HirLiteral::Character { character: v },
                None => return None,
            },
            "Literal::string" => {
                let mut string = String::new();

                for each_character in &child_node.children {
                    let new_character = match self.single_character(each_character.into_node()) {
                        Some(v) => v,
                        None => return None,
                    };

                    string.push(new_character);
                }

                HirLiteral::String { string: string }
            },
            _ => unimplemented!(),
        };

        Some(literal)
    }

    pub fn single_character(&mut self, node: &SyntaxNode) -> Option<char> {
        let character = match node.search_node("Literal::escape_sequence") {
            Some(escape_sequence_node) => {
                let escape_suffix_node = escape_sequence_node.child_node_at(0);

                match escape_suffix_node.name.as_str() {
                    "Literal::general_escape_suffix" => match escape_suffix_node.child_leaf_at(0).value.as_str() {
                        // todo: Add characters.
                        "0" => '\0',
                        "n" => '\n',
                        "t" => '\t',
                        "\\" => '\\',
                        _ => unreachable!(),
                    },
                    "Literal::unicode_escape_suffix" => {
                        let unicode = escape_suffix_node.child_leaf_at(0).value.as_str();

                        match u32::from_str_radix(unicode, 16) {
                            Ok(v) => match char::from_u32(v) {
                                Some(v) => v,
                                None => {
                                    self.logs.push(CompilerLog::Error(CompilerErrorLog::UnicodeEscapeSequenceIsInvalid { unicode: unicode.to_string() }));
                                    return None;
                                },
                            },
                            Err(_) => unreachable!(),
                        }
                    },
                    _ => unreachable!(),
                }
            },
            None => node.child_leaf_at(0).value.chars().next().unwrap(),
        };

        Some(character)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Hir {
    pub items: Vec<HirItem>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum HirVisibility {
    Public,
    Private,
}

#[derive(Clone, Debug, PartialEq)]
pub enum HirItem {
    Module(HirModule),
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirModule {
    pub identifier: String,
    pub visibility: HirVisibility,
    pub items: Vec<HirItem>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum HirExpression {
    DataType(HirDataType),
    Literal(HirLiteral),
}

#[derive(Clone, Debug, PartialEq)]
pub enum HirNumberBase {
    Binary,
    Octal,
    Decimal,
    Hexadecimal,
}

#[derive(Clone, Debug, PartialEq)]
pub enum HirLiteral {
    Boolean { value: String },
    Integer { base: HirNumberBase, integer: String, data_type: Option<HirDataType> },
    Float { integer: String, decimal: String, data_type: Option<HirDataType> },
    Character { character: char },
    String { string: String },
}

#[derive(Clone, Debug, PartialEq)]
pub enum HirDataType {
    Primitive(HirPrimitiveDataType),
    Named(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum HirPrimitiveDataType {
    Boolean,
    S8,
    S16,
    S32,
    S64,
    SSize,
    U8,
    U16,
    U32,
    U64,
    USize,
    F32,
    F64,
    Character,
    String,
}

impl From<&str> for HirPrimitiveDataType {
    fn from(v: &str) -> Self {
        match v {
            "bool" => HirPrimitiveDataType::Boolean,
            "s8" => HirPrimitiveDataType::S8,
            "s16" => HirPrimitiveDataType::S16,
            "s32" => HirPrimitiveDataType::S32,
            "s64" => HirPrimitiveDataType::S64,
            "ssize" => HirPrimitiveDataType::SSize,
            "u8" => HirPrimitiveDataType::U8,
            "u16" => HirPrimitiveDataType::U16,
            "u32" => HirPrimitiveDataType::U32,
            "u64" => HirPrimitiveDataType::U64,
            "usize" => HirPrimitiveDataType::USize,
            "f32" => HirPrimitiveDataType::F32,
            "f64" => HirPrimitiveDataType::F64,
            "char" => HirPrimitiveDataType::Character,
            "str" => HirPrimitiveDataType::String,
            _ => unreachable!(),
        }
    }
}
