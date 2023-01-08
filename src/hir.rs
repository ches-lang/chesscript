use std::{result::Result, fmt::{Display, Formatter}, num::IntErrorKind};
use cake::tree::*;
use crate::{compiler::log::{CompilerLog, CompilerWarningLog, CompilerErrorLog}};

#[derive(Clone, Debug, PartialEq)]
pub enum HirGeneratorError {}

pub type HirGeneratorResult<T> = Result<T, HirGeneratorError>;

#[derive(Clone, Debug, PartialEq)]
pub struct HirGeneratorOptions;

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
            "Item::function" => HirItem::Function(self.function(child_node)?),
            _ => unreachable!(),
        };

        Ok(item)
    }

    pub fn module(&mut self, node: &SyntaxNode) -> HirGeneratorResult<HirModule> {
        let id_node  = node.search_node("Identifier::identifier").unwrap();
        let id = self.identifier(id_node, Some(HirIdentifierKind::PascalCase));
        let visibility = self.visibility(node.search_node("Item::visibility").unwrap());
        let mut items = Vec::new();

        for each_item in &node.search_node("items").unwrap().children {
            items.push(self.item(each_item.into_node())?);
        }

        let module = HirModule {
            id: id,
            visibility: visibility,
            items: items,
        };

        Ok(module)
    }

    pub fn function(&mut self, node: &SyntaxNode) -> HirGeneratorResult<HirFunction> {
        let id_node  = node.search_node("Identifier::identifier").unwrap();
        let id = self.identifier(id_node, Some(HirIdentifierKind::SnakeCase));
        let visibility = self.visibility(node.search_node("Item::visibility").unwrap());
        let arg_group = node.search_node("Item::function_argument_group").unwrap();
        let args = arg_group.children.iter().map(|v| self.formal_argument(v.into_node())).collect();

        let return_type = match node.search_node("DataType::annotation") {
            Some(v) => Some(self.data_type_annotation(v)),
            None => None,
        };

        let mut exprs = Vec::new();

        for each_expr_node in &node.search_node("expressions").unwrap().children {
            if let Some(v) = self.expression(each_expr_node.into_node()) {
                exprs.push(v);
            }
        }

        let function = HirFunction {
            id: id,
            visibility: visibility,
            args: args,
            return_type: return_type,
            exprs: exprs,
        };

        Ok(function)
    }

    pub fn formal_argument(&mut self, node: &SyntaxNode) -> HirFormalArgument {
        let id_node  = node.search_node("Identifier::identifier").unwrap();
        let id = self.identifier(id_node, Some(HirIdentifierKind::PascalCase));
        let data_type = self.data_type_annotation(node.search_node("DataType::annotation").unwrap());

        HirFormalArgument {
            id: id,
            data_type: data_type,
        }
    }

    pub fn visibility(&mut self, node: &SyntaxNode) -> HirVisibility {
        match node.child_leaf_at(0).value.as_str() {
            "pub" => HirVisibility::Public,
            "private" => HirVisibility::Private,
            _ => unreachable!(),
        }
    }

    // fix: validate identifier kind after HIRification
    pub fn identifier(&mut self, node: &SyntaxNode, valid_identifier_kind: Option<HirIdentifierKind>) -> String {
        let child_node = node.child_node_at(0);

        let identifier_kind = match child_node.name.as_str() {
            "Identifier::pascal_case" => HirIdentifierKind::PascalCase,
            "Identifier::snake_case" => HirIdentifierKind::SnakeCase,
            _ => unreachable!(),
        };

        let identifier = &child_node.child_leaf_at(0).value;

        if let Some(valid_identifier_kind) = valid_identifier_kind {
            if valid_identifier_kind != identifier_kind {
                let new_log = CompilerLog::Warning(CompilerWarningLog::IdentifierShouldBePascalCase { id: identifier.to_string() });
                self.logs.push(new_log);
            }
        }

        identifier.to_string()
    }

    pub fn expression_element(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        let expr = match node.name.as_str() {
            "Literal::literal" => HirExpression::Literal(
                match self.literal(node) {
                    Some(v) => v,
                    None => return None,
                }
            ),
            "DataType::data_type" => HirExpression::DataType(self.data_type(node)),
            "Function::call" => HirExpression::FunctionCall(self.function_call(node)),
            "Identifier::identifier" => HirExpression::Identifier(self.identifier(node, None)),
            _ => unreachable!(),
        };

        Some(expr)
    }

    pub fn expression(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        if node.children.len() == 1 {
            let child_node = node.child_node_at(0);
            self.expression_element(child_node)
        } else {
            let exprs: Vec<Option<HirExpression>> = node.children.iter().map(|v| self.expression_element(v.into_node())).collect();
            Some(HirExpression::Chain(exprs))
        }
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

    pub fn data_type_annotation(&mut self, node: &SyntaxNode) -> HirDataType {
        self.data_type(node.search_node("DataType::data_type").unwrap())
    }

    pub fn literal(&mut self, node: &SyntaxNode) -> Option<HirLiteral> {
        let child_node = node.child_node_at(0);

        let literal = match child_node.name.as_str() {
            "Literal::boolean" => {
                let str_value = child_node.child_leaf_at(0).value.clone();
                HirLiteral::Boolean { value: str_value == "true" }
            },
            "Literal::integer" => {
                let based_integer_node = child_node.child_node_at(0);
                let base = match based_integer_node.name.as_str() {
                    "bin" => HirIntegerBase::Binary,
                    "dec" => HirIntegerBase::Decimal,
                    "oct" => HirIntegerBase::Octal,
                    "hex" => HirIntegerBase::Hexadecimal,
                    _ => unreachable!(),
                };

                let str_value = &based_integer_node.child_leaf_at(0).value;

                let value = match u64::from_str_radix(str_value, base.to_radix()) {
                    Ok(v) => v,
                    Err(e) => match e.kind() {
                        IntErrorKind::PosOverflow | IntErrorKind::NegOverflow => {
                            self.logs.push(CompilerLog::Error(CompilerErrorLog::NumberLiteralIsTooLarge));
                            return None;
                        },
                        _ => unreachable!(),
                    },
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

                HirLiteral::Integer { base: base, value: value, data_type: data_type }
            },
            "Literal::float" => {
                // fix: join integer and decimal at syntax definition
                let integer = child_node.search_node("integer").unwrap().child_leaf_at(0).value.clone();
                let decimal = child_node.search_node("decimal").unwrap().child_leaf_at(0).value.clone();
                let str_value = format!("{}.{}", integer, decimal);
                let value = match str_value.parse::<f64>() {
                    Ok(v) => v,
                    Err(e) => panic!("{:?}", e),
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

                HirLiteral::Float { value: value, data_type: data_type }
            },
            "Literal::character" => match self.single_character(child_node.child_node_at(0)) {
                Some(v) => HirLiteral::Character { value: v },
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

                HirLiteral::String { value: string }
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

    pub fn function_call(&mut self, node: &SyntaxNode) -> HirFunctionCall {
        let id_node  = node.search_node("Identifier::identifier").unwrap();
        let id = self.identifier(id_node, None);
        let arg_group = node.search_node("Function::actual_argument_group").unwrap();
        let args = arg_group.children.iter().map(|v| self.actual_argument(v.into_node())).collect();

        HirFunctionCall {
            id: id,
            args: args,
        }
    }

    pub fn actual_argument(&mut self, node: &SyntaxNode) -> HirActualArgument {
        let expr_node = node.search_node("Expression::expression").unwrap();
        let expr = self.expression(expr_node);

        HirActualArgument {
            expr: expr,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Hir {
    pub items: Vec<HirItem>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirIdentifier {
    kind: HirIdentifierKind,
    id: String,
}

#[derive(Clone, Debug, PartialEq)]
pub enum HirIdentifierKind {
    PascalCase,
    SnakeCase,
}

#[derive(Clone, Debug, PartialEq)]
pub enum HirVisibility {
    Public,
    Private,
}

#[derive(Clone, Debug, PartialEq)]
pub enum HirItem {
    Module(HirModule),
    Function(HirFunction),
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirModule {
    // fix: string to HirIdentifier
    pub id: String,
    pub visibility: HirVisibility,
    pub items: Vec<HirItem>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirFunction {
    pub id: String,
    pub visibility: HirVisibility,
    pub args: Vec<HirFormalArgument>,
    pub return_type: Option<HirDataType>,
    pub exprs: Vec<HirExpression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirFormalArgument {
    pub id: String,
    pub data_type: HirDataType,
}

#[derive(Clone, Debug, PartialEq)]
pub enum HirExpression {
    Chain(Vec<Option<HirExpression>>),
    DataType(HirDataType),
    Literal(HirLiteral),
    FunctionCall(HirFunctionCall),
    Identifier(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum HirIntegerBase {
    Binary,
    Octal,
    Decimal,
    Hexadecimal,
}

impl HirIntegerBase {
    pub fn to_radix(&self) -> u32 {
        match self {
            HirIntegerBase::Binary => 2,
            HirIntegerBase::Octal => 8,
            HirIntegerBase::Decimal => 10,
            HirIntegerBase::Hexadecimal => 16,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum HirLiteral {
    Boolean { value: bool },
    Integer { base: HirIntegerBase, value: u64, data_type: Option<HirDataType> },
    Float { value: f64, data_type: Option<HirDataType> },
    Character { value: char },
    String { value: String },
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

impl Display for HirPrimitiveDataType {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let s = match self {
            HirPrimitiveDataType::Boolean => "bool",
            HirPrimitiveDataType::S8 => "s8",
            HirPrimitiveDataType::S16 => "s16",
            HirPrimitiveDataType::S32 => "s32",
            HirPrimitiveDataType::S64 => "s64",
            HirPrimitiveDataType::SSize => "ssize",
            HirPrimitiveDataType::U8 => "u8",
            HirPrimitiveDataType::U16 => "u16",
            HirPrimitiveDataType::U32 => "u32",
            HirPrimitiveDataType::U64 => "u64",
            HirPrimitiveDataType::USize => "usize",
            HirPrimitiveDataType::F32 => "f32",
            HirPrimitiveDataType::F64 => "f64",
            HirPrimitiveDataType::Character => "char",
            HirPrimitiveDataType::String => "str",
        };

        write!(f, "{}", s)
    }
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

#[derive(Clone, Debug, PartialEq)]
pub struct HirFunctionCall {
    pub id: String,
    pub args: Vec<HirActualArgument>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirActualArgument {
    pub expr: Option<HirExpression>,
}
