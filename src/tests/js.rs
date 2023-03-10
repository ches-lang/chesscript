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
            test "ES module includes exports keyword" {
                let mut generator = JsGenerator::new(default_js_options);

                assert_eq!(
                    generator.module(
                        &HirModule {
                            id: HirIdentifier::unresolved_from(HirIdentifierKind::PascalCase, "Module"),
                            visibility: HirVisibility::Private,
                            items: vec![
                                HirItem::Module(HirModule {
                                    id: HirIdentifier::unresolved_from(HirIdentifierKind::PascalCase, "SubModule"),
                                    visibility: HirVisibility::Private,
                                    items: vec![],
                                }),
                            ],
                        },
                    ).stringify(),
                    "export namespace Module{export namespace SubModule{}}".to_string(),
                );
            }

            test "CommonJS module includes substitution of module.exports" {
                let mut generator = JsGenerator::new(
                    &JsGeneratorOptions {
                        minify: true,
                        target: JsTarget::Es2015,
                        module_style: JsModuleStyle::CommonJs,
                    },
                );

                let module = HirModule {
                    id: HirIdentifier::unresolved_from(HirIdentifierKind::PascalCase, "Module"),
                    visibility: HirVisibility::Private,
                    items: vec![
                        HirItem::Module(HirModule {
                            id: HirIdentifier::unresolved_from(HirIdentifierKind::PascalCase, "SubModule"),
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

            test "has no-module style namespace" {
                let mut generator = JsGenerator::new(
                    &JsGeneratorOptions {
                        minify: true,
                        target: JsTarget::Es2015,
                        module_style: JsModuleStyle::NoModules,
                    },
                );

                let module = HirModule {
                    id: HirIdentifier::unresolved_from(HirIdentifierKind::PascalCase, "Module"),
                    visibility: HirVisibility::Private,
                    items: vec![
                        HirItem::Module(HirModule {
                            id: HirIdentifier::unresolved_from(HirIdentifierKind::PascalCase, "SubModule"),
                            visibility: HirVisibility::Private,
                            items: vec![],
                        }),
                    ],
                };

                let generate_namespace = |id: &str, stmts: &str| format!("var {0};(({0})=>{{{1}}})({0}||({0}={{}}));", id, stmts);

                assert_eq!(
                    generator.module(&module).stringify(),
                    generate_namespace("Module", &generate_namespace("SubModule", "")).to_string(),
                );
            }
        }

        describe "jsify item > function" {
            test "has an argument and expressions including return statement" {
                let mut generator = JsGenerator::new(default_js_options);

                assert_eq!(
                    generator.function(
                        &HirFunction {
                            id: HirIdentifier::unresolved_from(HirIdentifierKind::SnakeCase, "f"),
                            visibility: HirVisibility::Private,
                            args: vec![
                                HirFormalArgument {
                                    id: HirIdentifier::unresolved_from(HirIdentifierKind::SnakeCase, "arg"),
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

    describe "jsify expression" {
        test "has two chained expressions" {
            let mut generator = JsGenerator::new(default_js_options);

            assert_eq!(
                generator.expression(
                    &HirExpression::Chain(
                        vec![
                            Some(HirExpression::Identifier(HirIdentifier::unresolved_from(HirIdentifierKind::SnakeCase, "a"))),
                            Some(HirExpression::Identifier(HirIdentifier::unresolved_from(HirIdentifierKind::SnakeCase, "b"))),
                        ],
                    ),
                ).into_expression().stringify(),
                "a.b".to_string(),
            );
        }

        describe "jsify expression > literal" {
            describe "jsify expression > literal > boolean" {
                test "is true" {
                    let mut generator = JsGenerator::new(default_js_options);

                    assert_eq!(
                        generator.expression(
                            &HirExpression::Literal(
                                HirLiteral::Boolean { value: true },
                            ),
                        ).into_expression().stringify(),
                        "true".to_string(),
                    );
                }
            }

            describe "jsify expression > literal > integer" {
                test "is binary format" {
                    let mut generator = JsGenerator::new(default_js_options);

                    assert_eq!(
                        generator.expression(
                            &HirExpression::Literal(
                                HirLiteral::Integer { base: HirIntegerBase::Binary, value: 255, data_type: None },
                            ),
                        ).into_expression().stringify(),
                        "0b11111111".to_string(),
                    );
                }

                test "is octal format" {
                    let mut generator = JsGenerator::new(default_js_options);

                    assert_eq!(
                        generator.expression(
                            &HirExpression::Literal(
                                HirLiteral::Integer { base: HirIntegerBase::Octal, value: 255, data_type: None },
                            ),
                        ).into_expression().stringify(),
                        "0o377".to_string(),
                    );
                }

                test "is decimal format" {
                    let mut generator = JsGenerator::new(default_js_options);

                    assert_eq!(
                        generator.expression(
                            &HirExpression::Literal(
                                HirLiteral::Integer { base: HirIntegerBase::Decimal, value: 255, data_type: None },
                            ),
                        ).into_expression().stringify(),
                        "255".to_string(),
                    );
                }

                test "is hexadecimal format" {
                    let mut generator = JsGenerator::new(default_js_options);

                    assert_eq!(
                        generator.expression(
                            &HirExpression::Literal(
                                HirLiteral::Integer { base: HirIntegerBase::Hexadecimal, value: 255, data_type: None },
                            ),
                        ).into_expression().stringify(),
                        "0xff".to_string(),
                    );
                }
            }

            // todo: add more tests
        }

        describe "jsify expression > function call" {
            test "has an id and comma-separated arguments" {
                let mut generator = JsGenerator::new(default_js_options);

                assert_eq!(
                    generator.expression(
                        &HirExpression::FunctionCall(
                            HirFunctionCall {
                                id: HirIdentifier::unresolved_from(HirIdentifierKind::SnakeCase, "f"),
                                args: vec![
                                    HirActualArgument { expr: Some(HirExpression::Literal(HirLiteral::Boolean { value: true })) },
                                    HirActualArgument { expr: Some(HirExpression::Literal(HirLiteral::Boolean { value: true })) },
                                ],
                            },
                        ),
                    ).into_expression().stringify(),
                    "f(true,true)".to_string(),
                );
            }
        }

        describe "jsify expression > identifier" {
            test "matches identifier" {
                let mut generator = JsGenerator::new(default_js_options);

                assert_eq!(
                    generator.expression(
                        &HirExpression::Identifier(HirIdentifier::unresolved_from(HirIdentifierKind::SnakeCase, "a"))
                    ).into_expression().stringify(),
                    "a".to_string(),
                );
            }
        }
    }
}
