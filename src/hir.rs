use std::result::Result;
use cake::tree::*;

use crate::{compiler::log::{CompilerLog, CompilerWarningLog}};

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

    pub fn data_type(&mut self, node: &SyntaxNode) -> HirDataType {
        let child_node = node.child_node_at(0);

        match child_node.name.as_str() {
            "DataType::primitive" => {
                let value = &child_node.child_leaf_at(0).value;

                let primitive = match value.as_str() {
                    "bool" => HirPrimitiveDataType::Bool,
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
                };

                HirDataType::Primitive(primitive)
            },
            _ => unreachable!(),
        }
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
pub enum HirDataType {
    Primitive(HirPrimitiveDataType),
    Named(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum HirPrimitiveDataType {
    Bool,
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
