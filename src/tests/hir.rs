use crate::{hir::*, tests::generator::*};
use cake::{*, tree::*};
use speculate::speculate;

speculate!{
    before {
        let default_hir_options = HirGeneratorOptions {};
    }

    describe "hirify item" {
        describe "hirify item > module" {
            test "has private visibility" {
                let mut generator = HirGenerator::new(&default_hir_options);

                assert_eq!(
                    generator.module(
                        &SyntaxElementGenerator::module(
                            "private",
                            SyntaxElementGenerator::identifier(HirIdentifierKind::PascalCase, "Module"),
                            vec![],
                        ).into_node(),
                    ).unwrap(),
                    HirModule {
                        id: "Module".to_string(),
                        visibility: HirVisibility::Private,
                        items: vec![],
                    },
                );
            }

            test "has public visibility" {
                let mut generator = HirGenerator::new(&default_hir_options);

                assert_eq!(
                    generator.module(
                        &SyntaxElementGenerator::module(
                            "pub",
                            SyntaxElementGenerator::identifier(HirIdentifierKind::PascalCase, "Module"),
                            vec![],
                        ).into_node(),
                    ).unwrap(),
                    HirModule {
                        id: "Module".to_string(),
                        visibility: HirVisibility::Public,
                        items: vec![],
                    },
                );
            }

            test "has child items" {
                let mut generator = HirGenerator::new(&default_hir_options);

                assert_eq!(
                    generator.module(
                        &SyntaxElementGenerator::module(
                            "private",
                            SyntaxElementGenerator::identifier(HirIdentifierKind::PascalCase, "Module"),
                            vec![
                                SyntaxElementGenerator::item(
                                    SyntaxElementGenerator::module(
                                        "private",
                                        SyntaxElementGenerator::identifier(HirIdentifierKind::PascalCase, "SubModule"),
                                        vec![],
                                    ),
                                ),
                            ],
                        ).into_node(),
                    ).unwrap(),
                    HirModule {
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
                );
            }
        }

        describe "hirify item > function" {
            test "has public visibility" {
                let mut generator = HirGenerator::new(&default_hir_options);

                assert_eq!(
                    generator.function(
                        &SyntaxElementGenerator::function(
                            SyntaxElementGenerator::visibility("pub"),
                            SyntaxElementGenerator::identifier(HirIdentifierKind::SnakeCase, "main"),
                            SyntaxElementGenerator::function_argument_group(vec![]),
                            None,
                            vec![],
                        ).into_node(),
                    ).unwrap(),
                    HirFunction {
                        id: "main".to_string(),
                        visibility: HirVisibility::Public,
                        args: Vec::new(),
                        return_type: None,
                        exprs: Vec::new(),
                    },
                );
            }

            test "has private visibility" {
                let mut generator = HirGenerator::new(&default_hir_options);

                assert_eq!(
                    generator.function(
                        &SyntaxElementGenerator::function(
                            SyntaxElementGenerator::visibility("private"),
                            SyntaxElementGenerator::identifier(HirIdentifierKind::SnakeCase, "main"),
                            SyntaxElementGenerator::function_argument_group(vec![]),
                            None,
                            vec![],
                        ).into_node(),
                    ).unwrap(),
                    HirFunction {
                        id: "main".to_string(),
                        visibility: HirVisibility::Private,
                        args: Vec::new(),
                        return_type: None,
                        exprs: Vec::new(),
                    },
                );
            }

            test "has return type" {
                let mut generator = HirGenerator::new(&default_hir_options);

                let return_type_annotation = SyntaxElementGenerator::data_type_annotation(
                    SyntaxElementGenerator::data_type(
                        SyntaxElementGenerator::primitive_data_type("s32"),
                    ),
                );

                assert_eq!(
                    generator.function(
                        &SyntaxElementGenerator::function(
                            SyntaxElementGenerator::visibility("pub"),
                            SyntaxElementGenerator::identifier(HirIdentifierKind::SnakeCase, "main"),
                            SyntaxElementGenerator::function_argument_group(vec![]),
                            Some(return_type_annotation),
                            vec![],
                        ).into_node(),
                    ).unwrap(),
                    HirFunction {
                        id: "main".to_string(),
                        visibility: HirVisibility::Public,
                        args: Vec::new(),
                        return_type: Some(HirDataType::Primitive(HirPrimitiveDataType::S32)),
                        exprs: Vec::new(),
                    },
                );
            }

            test "has arguments" {
                let mut generator = HirGenerator::new(&default_hir_options);

                assert_eq!(
                    generator.function(
                        &SyntaxElementGenerator::function(
                            SyntaxElementGenerator::visibility("pub"),
                            SyntaxElementGenerator::identifier(HirIdentifierKind::SnakeCase, "main"),
                            SyntaxElementGenerator::function_argument_group(
                                vec![
                                    SyntaxElementGenerator::function_argument(
                                        SyntaxElementGenerator::identifier(HirIdentifierKind::SnakeCase, "arg1"),
                                        SyntaxElementGenerator::data_type_annotation(
                                            SyntaxElementGenerator::data_type(
                                                SyntaxElementGenerator::primitive_data_type("s32"),
                                            ),
                                        ),
                                    ),
                                    SyntaxElementGenerator::function_argument(
                                        SyntaxElementGenerator::identifier(HirIdentifierKind::SnakeCase, "arg2"),
                                        SyntaxElementGenerator::data_type_annotation(
                                            SyntaxElementGenerator::data_type(
                                                SyntaxElementGenerator::primitive_data_type("s32"),
                                            ),
                                        ),
                                    ),
                                ],
                            ),
                            None,
                            vec![],
                        ).into_node(),
                    ).unwrap(),
                    HirFunction {
                        id: "main".to_string(),
                        visibility: HirVisibility::Public,
                        args: vec![
                            HirFunctionFormalArgument {
                                id: "arg1".to_string(),
                                data_type: HirDataType::Primitive(HirPrimitiveDataType::S32),
                            },
                            HirFunctionFormalArgument {
                                id: "arg2".to_string(),
                                data_type: HirDataType::Primitive(HirPrimitiveDataType::S32),
                            },
                        ],
                        return_type: None,
                        exprs: Vec::new(),
                    },
                );
            }

            test "has expression" {
                let mut generator = HirGenerator::new(&default_hir_options);

                assert_eq!(
                    generator.function(
                        &SyntaxElementGenerator::function(
                            SyntaxElementGenerator::visibility("pub"),
                            SyntaxElementGenerator::identifier(HirIdentifierKind::SnakeCase, "main"),
                            SyntaxElementGenerator::function_argument_group(vec![]),
                            None,
                            vec![
                                SyntaxElementGenerator::expression(
                                    SyntaxElementGenerator::boolean_literal("true"),
                                ),
                            ],
                        ).into_node(),
                    ).unwrap(),
                    HirFunction {
                        id: "main".to_string(),
                        visibility: HirVisibility::Public,
                        args: Vec::new(),
                        return_type: None,
                        exprs: vec![HirExpression::Literal(HirLiteral::Boolean { value: true })],
                    },
                );
            }

            describe "hirify item > function > formal argument" {
                test "has an argument" {
                    let mut generator = HirGenerator::new(&default_hir_options);

                    assert_eq!(
                        generator.formal_argument(
                            &SyntaxElementGenerator::function_argument(
                                SyntaxElementGenerator::identifier(HirIdentifierKind::SnakeCase, "arg"),
                                SyntaxElementGenerator::data_type_annotation(
                                    SyntaxElementGenerator::data_type(
                                        SyntaxElementGenerator::primitive_data_type("s32"),
                                    ),
                                ),
                            ).into_node(),
                        ),
                        HirFunctionFormalArgument {
                            id: "arg".to_string(),
                            data_type: HirDataType::Primitive(HirPrimitiveDataType::S32),
                        },
                    );
                }
            }
        }

        describe "hirify item > visibility" {
            test "has private visibility" {
                let mut generator = HirGenerator::new(&default_hir_options);

                assert_eq!(
                    generator.visibility(
                        &SyntaxElementGenerator::visibility("private").into_node(),
                    ),
                    HirVisibility::Private,
                );
            }
        }
    }

    describe "hirify expression" {
        describe "hirify expression > data type" {
            test "has primitive data type" {
                let mut generator = HirGenerator::new(&default_hir_options);

                assert_eq!(
                    generator.expression(
                        &SyntaxElementGenerator::expression(
                            SyntaxElementGenerator::data_type(
                                SyntaxElementGenerator::primitive_data_type("bool"),
                            ),
                        ).into_node(),
                    ),
                    Some(HirExpression::DataType(HirDataType::Primitive(HirPrimitiveDataType::Boolean))),
                );
            }

            test "has bool in annotation" {
                let mut generator = HirGenerator::new(&default_hir_options);

                assert_eq!(
                    generator.data_type_annotation(
                        &SyntaxElementGenerator::data_type_annotation(
                            SyntaxElementGenerator::data_type(
                                SyntaxElementGenerator::primitive_data_type("bool"),
                            ),
                        ).into_node(),
                    ),
                    HirDataType::Primitive(HirPrimitiveDataType::Boolean),
                );
            }
        }

        describe "hirify expression > literal" {
            describe "hirify expression > literal > boolean" {
                test "match the specified value" {
                    let mut generator = HirGenerator::new(&default_hir_options);

                    assert_eq!(
                        generator.expression(
                            &SyntaxElementGenerator::expression(
                                SyntaxElementGenerator::boolean_literal("true"),
                            ).into_node(),
                        ),
                        Some(HirExpression::Literal(HirLiteral::Boolean { value: true })),
                    );
                }
            }

            describe "hirify expression > literal > integer" {
                test "match the specified value" {
                    let mut generator = HirGenerator::new(&default_hir_options);

                    assert_eq!(
                        generator.expression(
                            &SyntaxElementGenerator::expression(
                                SyntaxElementGenerator::integer_literal("dec", "012", None),
                            ).into_node(),
                        ),
                        Some(
                            HirExpression::Literal(
                                HirLiteral::Integer {
                                    base: HirNumberBase::Decimal,
                                    value: 12,
                                    data_type: None,
                                },
                            ),
                        ),
                    );
                }

                test "has the specified type suffix" {
                    let mut generator = HirGenerator::new(&default_hir_options);

                    assert_eq!(
                        generator.expression(
                            &SyntaxElementGenerator::expression(
                                SyntaxElementGenerator::integer_literal("dec", "012", Some("s32")),
                            ).into_node(),
                        ),
                        Some(
                            HirExpression::Literal(
                                HirLiteral::Integer {
                                    base: HirNumberBase::Decimal,
                                    value: 12,
                                    data_type: Some(HirDataType::Primitive(HirPrimitiveDataType::S32)),
                                },
                            ),
                        ),
                    );
                }
            }

            describe "hirify expression > literal > float" {
                test "match the specified value" {
                    let mut generator = HirGenerator::new(&default_hir_options);

                    assert_eq!(
                        generator.expression(
                            &SyntaxElementGenerator::expression(
                                SyntaxElementGenerator::float_literal("012", "012", None),
                            ).into_node(),
                        ),
                        Some(
                            HirExpression::Literal(
                                HirLiteral::Float {
                                    value: 12.012,
                                    data_type: None,
                                },
                            ),
                        ),
                    );
                }

                test "has the specified type suffix" {
                    let mut generator = HirGenerator::new(&default_hir_options);

                    assert_eq!(
                        generator.expression(
                            &SyntaxElementGenerator::expression(
                                SyntaxElementGenerator::float_literal("012", "012", Some("f32")),
                            ).into_node(),
                        ),
                        Some(
                            HirExpression::Literal(
                                HirLiteral::Float {
                                    value: 12.012,
                                    data_type: Some(HirDataType::Primitive(HirPrimitiveDataType::F32)),
                                },
                            ),
                        ),
                    );
                }
            }

            describe "hirify expression > literal > single character" {
                test "match the specified value" {
                    let mut generator = HirGenerator::new(&default_hir_options);

                    assert_eq!(
                        generator.single_character(
                            SyntaxElementGenerator::single_character(leaf!("a")).into_node(),
                        ),
                        Some('a'),
                    );
                }

                test "process multibyte character as a character" {
                    let mut generator = HirGenerator::new(&default_hir_options);

                    assert_eq!(
                        generator.single_character(
                            SyntaxElementGenerator::single_character(leaf!("あ")).into_node(),
                        ),
                        Some('あ'),
                    );
                }

                test "match the specified escape character" {
                    let mut generator = HirGenerator::new(&default_hir_options);

                    assert_eq!(
                        generator.single_character(
                            SyntaxElementGenerator::single_character(
                                SyntaxElementGenerator::general_escape_sequence("n"),
                            ).into_node(),
                        ),
                        Some('\n'),
                    );
                }

                test "match the specified unicode escape" {
                    let mut generator = HirGenerator::new(&default_hir_options);

                    assert_eq!(
                        generator.single_character(
                            SyntaxElementGenerator::single_character(
                                SyntaxElementGenerator::unicode_escape_sequence("0"),
                            ).into_node(),
                        ),
                        Some('\0'),
                    );
                }

                test "cannot hirify out-of-range unicode escape" {
                    let mut generator = HirGenerator::new(&default_hir_options);

                    assert_eq!(
                        generator.single_character(
                            SyntaxElementGenerator::single_character(
                                SyntaxElementGenerator::unicode_escape_sequence("ffffff"),
                            ).into_node(),
                        ),
                        None,
                    );
                }
            }

            describe "hirify expression > literal > character" {
                test "match the specified value" {
                    let mut generator = HirGenerator::new(&default_hir_options);

                    assert_eq!(
                        generator.expression(
                            &SyntaxElementGenerator::expression(
                                SyntaxElementGenerator::character_literal(
                                    SyntaxElementGenerator::single_character(leaf!("a")),
                                ),
                            ).into_node(),
                        ),
                        Some(HirExpression::Literal(HirLiteral::Character { value: 'a' })),
                    );
                }
            }

            describe "hirify expression > literal > string" {
                test "match the specified value" {
                    let mut generator = HirGenerator::new(&default_hir_options);

                    assert_eq!(
                        generator.expression(
                            &SyntaxElementGenerator::expression(
                                SyntaxElementGenerator::string_literal(
                                    vec![
                                        SyntaxElementGenerator::single_character(
                                            leaf!("a"),
                                        ),
                                        SyntaxElementGenerator::single_character(
                                            SyntaxElementGenerator::general_escape_sequence("n"),
                                        ),
                                        SyntaxElementGenerator::single_character(
                                            SyntaxElementGenerator::unicode_escape_sequence("0"),
                                        ),
                                    ],
                                ),
                            ).into_node(),
                        ),
                        Some(HirExpression::Literal(HirLiteral::String { value: "a\n\0".to_string() })),
                    );
                }
            }
        }
    }
}
