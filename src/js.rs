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

    pub fn item(&mut self, item: &'a HirItem) -> JsStatement {
        match item {
            HirItem::Module(module) => self.module(module),
        }
    }

    pub fn module(&mut self, module: &'a HirModule) -> JsStatement {
        self.module_stack.push(module);

        if self.module_stack.len() == 1 {
            self.item_id_pool.push(module.id.clone());
        }

        let es_export = self.options.module_style == JsModuleStyle::Es2015;
        let stmts = module.items.iter().map(|v| self.item(v)).collect();
        JsStatement::NamespaceDefinition { es_export: es_export, id: module.id.clone(), statements: stmts }
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
    NamespaceDefinition { es_export: bool, id: String, statements: Vec<JsStatement> },
    Substitution { left: Box<JsExpression>, right: Box<JsExpression> },
}

impl JsStringifier for JsStatement {
    fn stringify(&self) -> String {
        match self {
            JsStatement::Expression(expr) => format!("{};", expr.stringify()),
            JsStatement::NamespaceDefinition { es_export, id, statements } => {
                let str_es_export = or_value!(*es_export, "export ", "");
                let str_stmts = statements.iter().map(|v| v.stringify()).collect::<Vec<String>>().join("");
                format!("{}namespace {}{{{}}}", str_es_export, id, str_stmts)
            },
            JsStatement::Substitution { left, right } => (
                format!("{}={};", left.stringify(), right.stringify())
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
    Boolean { value: String },
    // fix: add base
    Integer { integer: u64 },
    Float { float: f64 },
    Character { character: char },
    String { string: String },
    Object { exprs: Vec<JsExpression> },
}

impl JsStringifier for JsLiteral {
    fn stringify(&self) -> String {
        match self {
            JsLiteral::Boolean { value } => value.to_string(),
            JsLiteral::Integer { integer } => integer.to_string(),
            JsLiteral::Float { float } => float.to_string(),
            // fix: escape sequence
            JsLiteral::Character { character } => format!("'{}'", character),
            JsLiteral::String { string } => format!("\"{}\"", string),
            JsLiteral::Object { exprs } => format!("{{{}}}", stringify_vec!(exprs, ",")),
        }
    }
}
