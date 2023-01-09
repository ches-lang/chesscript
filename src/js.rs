use std::{fmt::Debug, collections::VecDeque};
use crate::hir::*;

#[derive(Clone, Debug, PartialEq)]
pub struct JsGeneratorOptions {
    pub minify: bool,
    pub target: JsTarget,
    pub module_style: JsModuleStyle,
}

#[derive(Clone, Debug, PartialEq)]
pub enum JsTarget {
    Es2015,
    Playground,
}

#[derive(Clone, Debug, PartialEq)]
pub enum JsModuleStyle {
    CommonJs,
    Es2015,
    NoModules,
}

pub struct JsGenerator<'a> {
    options: &'a JsGeneratorOptions,
    item_id_pool: Vec<String>,
    module_stack: Vec<&'a HirModule>,
}

impl<'a> JsGenerator<'a> {
    pub fn new(options: &'a JsGeneratorOptions) -> Self {
        JsGenerator {
            options: options,
            item_id_pool: Vec::new(),
            module_stack: Vec::new(),
        }
    }

    pub fn generate(&mut self, hir: &'a Hir) -> Js {
        let mut stmts: VecDeque<JsStatement> = hir.items.iter().map(|v| self.item(v)).collect();

        if self.options.module_style == JsModuleStyle::CommonJs {
            let export_ids = self.item_id_pool.iter().map(
                |v| JsExpression::Identifier(v.clone()),
            ).collect();

            let export_stmt = JsStatement::Substitution {
                left: Box::new(
                    JsExpression::Chain(vec![
                        JsExpression::Identifier("module".to_string()),
                        JsExpression::Identifier("exports".to_string()),
                    ]),
                ),
                right: Box::new(JsExpression::Literal(JsLiteral::Object { exprs: export_ids })),
            };

            stmts.push_back(export_stmt);
        }

        if self.options.module_style == JsModuleStyle::NoModules {
            let stdlib = "CSR.println=(msg)=>{libcall('log',[msg]);};";
            let entry_point_call = "CSR.main();";

            stmts.push_front(JsStatement::JsSource(stdlib.to_string()));
            stmts.push_back(JsStatement::JsSource(entry_point_call.to_string()));

            stmts = vec![JsStatement::NamespaceDefinition { no_modules: true, es_export: false, id: "CSR".to_string(), stmts: stmts.into() }].into();
        }

        Js {
            stmts,
        }
    }

    pub fn is_es_module(&self) -> bool {
        self.options.module_style == JsModuleStyle::Es2015
    }

    pub fn is_no_modules(&self) -> bool {
        self.options.module_style == JsModuleStyle::NoModules
    }

    pub fn item(&mut self, item: &'a HirItem) -> JsStatement {
        match item {
            HirItem::Module(module) => self.module(module),
            HirItem::Function(function) => self.function(function),
        }
    }

    pub fn module(&mut self, module: &'a HirModule) -> JsStatement {
        self.module_stack.push(module);

        if self.module_stack.len() == 1 {
            self.item_id_pool.push(module.id.clone());
        }

        let stmts = module.items.iter().map(|v| self.item(v)).collect();
        JsStatement::NamespaceDefinition { no_modules: self.is_no_modules(), es_export: self.is_es_module(), id: module.id.clone(), stmts }
    }

    pub fn function(&mut self, function: &'a HirFunction) -> JsStatement {
        let args = function.args.iter().map(|v| v.id.clone()).collect();

        let mut stmts: Vec<JsStatement> = if function.exprs.len() == 0 {
            Vec::new()
        } else {
            function.exprs[0..function.exprs.len() - 1].iter().map(|v| self.expression(v)).collect()
        };

        if let Some(v) = function.exprs.last() {
            let return_expr = self.expression(v).into_expression();
            let last_stmt = JsStatement::ReturnValue { expr: return_expr };
            stmts.push(last_stmt);
        }

        JsStatement::FunctionDefinition {
            no_modules: self.is_no_modules(),
            es_export: self.is_es_module(),
            parent_id: match self.module_stack.last() {
                Some(v) => Some(v.id.clone()),
                None => None,
            },
            id: function.id.clone(),
            args: args,
            stmts: stmts,
        }
    }

    pub fn expression(&mut self, expr: &'a HirExpression) -> JsStatement {
        match expr {
            HirExpression::Chain(exprs) => {
                JsStatement::Expression(
                    JsExpression::Chain(
                        exprs.iter().map(|v| {
                            match v {
                                Some(v) => self.expression(v).into_expression(),
                                None => unreachable!(),
                            }
                        }).collect(),
                    ),
                )
            },
            HirExpression::DataType(data_type) => match data_type {
                HirDataType::Primitive(primitive) => JsStatement::Expression(JsExpression::Identifier(primitive.to_string())),
                HirDataType::Named(id) => JsStatement::Expression(JsExpression::Identifier(id.clone())),
            },
            HirExpression::Literal(literal) => {
                let js_literal = match literal {
                    HirLiteral::Boolean { value } => JsLiteral::Boolean { value: *value },
                    HirLiteral::Integer { base, value, data_type } => JsLiteral::Integer { base: base.into(), value: *value },
                    HirLiteral::Float { value, data_type } => JsLiteral::Float { value: *value },
                    HirLiteral::Character { value } => JsLiteral::Character { value: *value },
                    HirLiteral::String { value } => JsLiteral::String { value: value.clone() },
                };

                JsStatement::Expression(JsExpression::Literal(js_literal))
            },
            HirExpression::FunctionCall(call) => JsStatement::Expression(
                JsExpression::FunctionCall(
                    JsFunctionCall {
                        id: call.id.clone(),
                        // fix: unwrap() of expression
                        args: call.args.iter().map(|v| self.expression(v.expr.as_ref().unwrap()).into_expression()).collect(),
                    }
                ),
            ),
            HirExpression::Identifier(id) => JsStatement::Expression(JsExpression::Identifier(id.clone())),
        }
    }
}

pub trait JsStringifier: Clone + Debug + PartialEq {
    fn stringify(&self) -> String;
}

macro_rules! or_value {
    ($condition:expr, $value1:expr, $value2:expr $(,)?) => {(
        if $condition {
            $value1
        } else {
            $value2
        }
    )};
}

macro_rules! stringify_vec {
    ($vec:expr) => {(
        stringify_vec!($vec, "")
    )};

    ($vec:expr, $separator:expr) => {(
        $vec.iter().map(|v| v.stringify()).collect::<Vec<String>>().join($separator)
    )};
}

#[derive(Clone, Debug, PartialEq)]
pub struct Js {
    stmts: VecDeque<JsStatement>,
}

impl JsStringifier for Js {
    fn stringify(&self) -> String {
        stringify_vec!(self.stmts)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum JsStatement {
    JsSource(String),
    Expression(JsExpression),
    FunctionDefinition { no_modules: bool, es_export: bool, parent_id: Option<String>, id: String, args: Vec<String>, stmts: Vec<JsStatement> },
    NamespaceDefinition { no_modules: bool, es_export: bool, id: String, stmts: Vec<JsStatement> },
    Substitution { left: Box<JsExpression>, right: Box<JsExpression> },
    ReturnValue { expr: JsExpression },
}

impl JsStatement {
    pub fn into_expression(self) -> JsExpression {
        match self {
            JsStatement::Expression(expr) => expr,
            _ => panic!("Cannot convert statement into expression."),
        }
    }
}

impl JsStringifier for JsStatement {
    fn stringify(&self) -> String {
        match self {
            JsStatement::JsSource(src) => src.clone(),
            JsStatement::Expression(expr) => format!("{};", expr.stringify()),
            JsStatement::FunctionDefinition { no_modules, es_export, parent_id, id, args, stmts } => {
                let str_stmts = stringify_vec!(stmts);
                let str_args = args.join(",");

                if *no_modules {
                    if let Some(parent_id) = parent_id {
                        format!("{}.{}=({})=>{{{}}};", parent_id, id, str_args, str_stmts)
                    } else {
                        format!("{}=({})=>{{{}}};", id, str_args, str_stmts)
                    }
                } else {
                    let str_es_export = or_value!(*es_export, "export ", "");
                    format!("{}function {}({}){{{}}}", str_es_export, id, str_args, str_stmts)
                }
            },
            JsStatement::NamespaceDefinition { no_modules, es_export, id, stmts: statements } => {
                let str_stmts = stringify_vec!(statements);

                if *no_modules {
                    format!("var {0};(({0})=>{{{1}}})({0}||({0}={{}}));", id, str_stmts)
                } else {
                    let str_es_export = or_value!(*es_export, "export ", "");
                    format!("{}namespace {}{{{}}}", str_es_export, id, str_stmts)
                }
            },
            JsStatement::Substitution { left, right } => {
                format!("{}={};", left.stringify(), right.stringify())
            },
            JsStatement::ReturnValue { expr } => {
                format!("return {};", expr.stringify())
            },
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum JsExpression {
    Chain(Vec<JsExpression>),
    Identifier(String),
    Literal(JsLiteral),
    FunctionCall(JsFunctionCall),
}

impl JsStringifier for JsExpression {
    fn stringify(&self) -> String {
        match self {
            JsExpression::Chain(exprs) => stringify_vec!(exprs, "."),
            JsExpression::Identifier(id) => id.clone(),
            JsExpression::Literal(literal) => literal.stringify(),
            JsExpression::FunctionCall(call) => call.stringify(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum JsLiteral {
    Boolean { value: bool },
    Integer { base: JsIntegerBase, value: u64 },
    Float { value: f64 },
    Character { value: char },
    String { value: String },
    Object { exprs: Vec<JsExpression> },
}

impl JsStringifier for JsLiteral {
    fn stringify(&self) -> String {
        match self {
            JsLiteral::Boolean { value } => value.to_string(),
            JsLiteral::Integer { base, value } => match base {
                JsIntegerBase::Binary => format!("0b{:b}", value),
                JsIntegerBase::Octal => format!("0o{:o}", value),
                JsIntegerBase::Decimal => format!("{}", value),
                JsIntegerBase::Hexadecimal => format!("0x{:x}", value),
            },
            JsLiteral::Float { value } => value.to_string(),
            // fix: escape sequence
            JsLiteral::Character { value } => format!("'{}'", value),
            JsLiteral::String { value } => format!("\"{}\"", value),
            JsLiteral::Object { exprs } => format!("{{{}}}", stringify_vec!(exprs, ",")),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum JsIntegerBase {
    Binary,
    Octal,
    Decimal,
    Hexadecimal,
}

impl From<&HirIntegerBase> for JsIntegerBase {
    fn from(v: &HirIntegerBase) -> Self {
        match v {
            HirIntegerBase::Binary => JsIntegerBase::Binary,
            HirIntegerBase::Octal => JsIntegerBase::Octal,
            HirIntegerBase::Decimal => JsIntegerBase::Decimal,
            HirIntegerBase::Hexadecimal => JsIntegerBase::Hexadecimal,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct JsFunctionCall {
    id: String,
    args: Vec<JsExpression>,
}

impl JsStringifier for JsFunctionCall {
    fn stringify(&self) -> String {
        let str_args = stringify_vec!(self.args, ",");
        format!("{}({})", self.id, str_args)
    }
}
