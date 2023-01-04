use crate::{js::*, hir::*};
use speculate::speculate;

speculate!{
    before {
        #[allow(unused)]
        let default_js_options = &JsGeneratorOptions {
            minify: true,
            target: JsTarget::Es2015,
            module_style: JsModuleStyle::Es2015,
        };
    }

    describe "jsify item" {
        describe "jsify item > module" {
            test "include exports keyword" {
                let mut generator = JsGenerator::new(default_js_options);

                assert_eq!(
                    generator.module(
                        &HirModule {
                            id: "Module".to_string(),
                            visibility: HirVisibility::Private,
                            items: vec![
                                HirItem::Module(HirModule {
                                    id: "SubModule".to_string(),
                                    visibility: HirVisibility::Private,
                                    items: vec![],
                                }),
                            ],
                        },
                    ).stringify(),
                    "export namespace Module{export namespace SubModule{}}".to_string(),
                );
            }

            test "include substitution of module.exports" {
                let mut generator = JsGenerator::new(
                    &JsGeneratorOptions {
                        minify: true,
                        target: JsTarget::Es2015,
                        module_style: JsModuleStyle::CommonJs,
                    },
                );

                let module = HirModule {
                    id: "Module".to_string(),
                    visibility: HirVisibility::Private,
                    items: vec![
                        HirItem::Module(HirModule {
                            id: "SubModule".to_string(),
                            visibility: HirVisibility::Private,
                            items: vec![],
                        }),
                    ],
                };

                assert_eq!(
                    generator.generate(
                        &Hir {
                            items: vec![HirItem::Module(module)],
                        },
                    ).stringify(),
                    "namespace Module{namespace SubModule{}}module.exports={Module};".to_string(),
                );
            }
        }

        describe "jsify item > function" {
            test "has an argument and expressions including return statement" {
                let mut generator = JsGenerator::new(default_js_options);

                assert_eq!(
                    generator.function(
                        &HirFunction {
                            id: "f".to_string(),
                            visibility: HirVisibility::Private,
                            args: vec![
                                HirFunctionFormalArgument {
                                    id: "arg".to_string(),
                                    data_type: HirDataType::Primitive(HirPrimitiveDataType::S32),
                                },
                            ],
                            return_type: None,
                            exprs: vec![
                                HirExpression::Literal(HirLiteral::Boolean { value: true }),
                                HirExpression::Literal(HirLiteral::Boolean { value: true }),
                            ],
                        },
                    ).stringify(),
                    "export function f(arg){true;return true;}".to_string(),
                );
            }
        }
    }
}
