use crate::hir::*;

macro_rules! or_value {
    ($condition:expr, $value1:expr, $value2:expr $(,)?) => {(
        if $condition {
            $value1
        } else {
            $value2
        }
    )};
}

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
    item_identifiers: Vec<String>,
}

impl<'a> JsGenerator<'a> {
    pub fn new(options: &'a JsGeneratorOptions) -> Self {
        JsGenerator {
            options: options,
            item_identifiers: Vec::new(),
        }
    }

    pub fn generate(&mut self, hir: &Hir) -> String {
        let js = hir.items.iter().map(|v| self.item(v)).collect::<Vec<String>>().join("");
        let exports = or_value!(
            self.options.module_style == JsModuleStyle::CommonJs,
            format!("module.exports={{{}}};", self.item_identifiers.join(",")),
            String::new(),
        );
        format!("{}{}", js, exports)
    }

    pub(crate) fn item(&mut self, item: &HirItem) -> String {
        match item {
            HirItem::Module(module) => self.module(module),
        }
    }

    pub(crate) fn module(&mut self, module: &HirModule) -> String {
        let es_export = or_value!(self.options.module_style == JsModuleStyle::Es2015, "export ", "");
        self.item_identifiers.push(module.identifier.clone());
        let sub_items = module.items.iter().map(|v| self.item(v)).collect::<Vec<String>>().join("");
        format!("{}namespace {}{{{}}}", es_export, module.identifier, sub_items)
    }
}
