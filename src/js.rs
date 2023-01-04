use std::fmt::Debug;
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
}

#[derive(Clone, Debug, PartialEq)]
pub enum JsModuleStyle {
    CommonJs,
    Es2015,
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
        let mut stmts: Vec<JsStatement> = hir.items.iter().map(|v| self.item(v)).collect();

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

            stmts.push(export_stmt);
        }

        Js {
            stmts,
        }
    }

    pub fn is_es_module(&self) -> bool {
        self.options.module_style == JsModuleStyle::Es2015
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
        JsStatement::NamespaceDefinition { es_export: self.is_es_module(), id: module.id.clone(), stmts }
    }

    pub fn function(&mut self, function: &'a HirFunction) -> JsStatement {
        let args = function.args.iter().map(|v| v.id.clone()).collect();
        let mut stmts: Vec<JsStatement> = function.exprs[0..function.exprs.len() - 1].iter().map(|v| self.expression(v)).collect();

        if let Some(v) = function.exprs.last() {
            let return_expr = match self.expression(v) {
                JsStatement::Expression(expr) => expr,
                _ => unreachable!(),
            };

            let last_stmt = JsStatement::ReturnValue { expr: return_expr };
            stmts.push(last_stmt);
        }

        JsStatement::FunctionDefinition { es_export: self.is_es_module(), id: function.id.clone(), args: args, stmts: stmts }
    }

    pub fn expression(&mut self, expr: &'a HirExpression) -> JsStatement {
        match expr {
            HirExpression::DataType(data_type) => match data_type {
                HirDataType::Primitive(primitive) => JsStatement::Expression(JsExpression::Identifier(primitive.to_string())),
                HirDataType::Named(id) => JsStatement::Expression(JsExpression::Identifier(id.clone())),
            },
            HirExpression::Literal(literal) => {
                let js_literal = match literal {
                    HirLiteral::Boolean { value } => JsLiteral::Boolean { value: *value },
                    // fix: use base
                    HirLiteral::Integer { base, value, data_type } => JsLiteral::Integer { value: *value },
                    HirLiteral::Float { value, data_type } => JsLiteral::Float { value: *value },
                    HirLiteral::Character { value } => JsLiteral::Character { value: *value },
                    HirLiteral::String { value } => JsLiteral::String { value: value.clone() },
                };

                JsStatement::Expression(JsExpression::Literal(js_literal))
            },
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
    stmts: Vec<JsStatement>,
}

impl JsStringifier for Js {
    fn stringify(&self) -> String {
        stringify_vec!(self.stmts)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum JsStatement {
    Expression(JsExpression),
    FunctionDefinition { es_export: bool, id: String, args: Vec<String>, stmts: Vec<JsStatement> },
    NamespaceDefinition { es_export: bool, id: String, stmts: Vec<JsStatement> },
    Substitution { left: Box<JsExpression>, right: Box<JsExpression> },
    ReturnValue { expr: JsExpression },
}

impl JsStringifier for JsStatement {
    fn stringify(&self) -> String {
        match self {
            JsStatement::Expression(expr) => format!("{};", expr.stringify()),
            JsStatement::FunctionDefinition { es_export, id, args, stmts } => {
                let str_es_export = or_value!(*es_export, "export ", "");
                let str_stmts = stringify_vec!(stmts);
                let args = args.join(",");
                format!("{}function {}({}){{{}}}", str_es_export, id, args, str_stmts)
            },
            JsStatement::NamespaceDefinition { es_export, id, stmts: statements } => {
                let str_es_export = or_value!(*es_export, "export ", "");
                let str_stmts = stringify_vec!(statements);
                format!("{}namespace {}{{{}}}", str_es_export, id, str_stmts)
            },
            JsStatement::Substitution { left, right } => (
                format!("{}={};", left.stringify(), right.stringify())
            ),
            JsStatement::ReturnValue { expr } => (
                format!("return {};", expr.stringify())
            ),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum JsExpression {
    Chain(Vec<JsExpression>),
    Identifier(String),
    Literal(JsLiteral),
}

impl JsStringifier for JsExpression {
    fn stringify(&self) -> String {
        match self {
            JsExpression::Chain(exprs) => stringify_vec!(exprs, "."),
            JsExpression::Identifier(id) => id.clone(),
            JsExpression::Literal(literal) => literal.stringify(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum JsLiteral {
    Boolean { value: bool },
    // fix: add base
    Integer { value: u64 },
    Float { value: f64 },
    Character { value: char },
    String { value: String },
    Object { exprs: Vec<JsExpression> },
}

impl JsStringifier for JsLiteral {
    fn stringify(&self) -> String {
        match self {
            JsLiteral::Boolean { value } => value.to_string(),
            JsLiteral::Integer { value } => value.to_string(),
            JsLiteral::Float { value } => value.to_string(),
            // fix: escape sequence
            JsLiteral::Character { value } => format!("'{}'", value),
            JsLiteral::String { value } => format!("\"{}\"", value),
            JsLiteral::Object { exprs } => format!("{{{}}}", stringify_vec!(exprs, ",")),
        }
    }
}
