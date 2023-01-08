use crate::{compiler::{CsCompiler}, hir::{HirIdentifierKind}, tests::generator::SyntaxElementGenerator};
use cake::{*, parser::{Parser, ParserResult, ParserError}, RuleId, tree::*};
use speculate::speculate;

speculate!{
    before {
        let cake = CsCompiler::bake_cake();

        let assert_ast = |input: &str, rule_id: &str, expected: ParserResult|
            assert_eq!(Parser::parse_rule(&cake, &RuleId(rule_id.to_string()), input, 1024), expected);

        #[allow(unused)]
        let expect_success = |input: &str, rule_id: &str, expected: SyntaxTree|
            assert_ast(input, rule_id, Ok(expected));

        #[allow(unused)]
        let expect_failure = |input: &str, rule_id: &str, expected: ParserError|
            assert_ast(input, rule_id, Err(expected));
    }

    describe "parse main" {
        test "has no item" {
            expect_success("\n\n", "Main::main", tree!{
                node!{
                    "Main::main" => vec![]
                }
            });
        }

        test "has an item" {
            expect_success("mod Module\nend", "Main::main", tree!{
                SyntaxElementGenerator::main(
                    SyntaxElementGenerator::item(
                        SyntaxElementGenerator::module(
                            "private",
                            SyntaxElementGenerator::identifier(HirIdentifierKind::PascalCase, "Module"),
                            vec![],
                        ),
                    ),
                )
            });
        }
    }

    describe "parse keyword" {
        test "has the name of data type" {
            expect_success("s32", "Keyword::keyword", tree!{
                SyntaxElementGenerator::keyword("s32")
            });
        }
    }

    describe "parse identifir" {
        test "has the identifier" {
            expect_success("id", "Identifier::identifier", tree!{
                SyntaxElementGenerator::identifier(HirIdentifierKind::SnakeCase, "id")
            });
        }

        test "except reserved keyword from identifier" {
            // todo: Review
            expect_failure("s32", "Identifier::identifier", ParserError::UnexpectedEndOfInput);
        }
    }

    describe "parse item" {
        // fix: not to parse module and function directly, but parse item.

        describe "module item" {
            test "has private visibility" {
                expect_success("mod Module\nend", "Item::module", tree!{
                    SyntaxElementGenerator::module(
                        "private",
                        SyntaxElementGenerator::identifier(HirIdentifierKind::PascalCase, "Module"),
                        vec![],
                    )
                });
            }

            test "has public visibility" {
                expect_success("pub mod Module\nend", "Item::module", tree!{
                    SyntaxElementGenerator::module(
                        "pub",
                        SyntaxElementGenerator::identifier(HirIdentifierKind::PascalCase, "Module"),
                        vec![],
                    )
                });
            }

            test "has child items" {
                expect_success("mod Module\nmod SubModule\nend\nfn f()\nend\nend", "Item::module", tree!{
                    SyntaxElementGenerator::module(
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
                            SyntaxElementGenerator::item(
                                SyntaxElementGenerator::function_definition(
                                    "private",
                                    SyntaxElementGenerator::identifier(HirIdentifierKind::SnakeCase, "f"),
                                    SyntaxElementGenerator::formal_function_argument_group(vec![]),
                                    None,
                                    vec![],
                                ),
                            ),
                        ],
                    )
                });
            }
        }

        describe "parse item > function" {
            test "has public visibility" {
                expect_success("pub fn main()\nend", "Item::function", tree!{
                    SyntaxElementGenerator::function_definition(
                        "pub",
                        SyntaxElementGenerator::identifier(HirIdentifierKind::SnakeCase, "main"),
                        SyntaxElementGenerator::formal_function_argument_group(vec![]),
                        None,
                        vec![],
                    )
                });
            }

            test "has return type" {
                let return_type_annotation = SyntaxElementGenerator::data_type_annotation(
                    SyntaxElementGenerator::data_type(
                        SyntaxElementGenerator::primitive_data_type("s32"),
                    ),
                );

                expect_success("pub fn main(): s32\nend", "Item::function", tree!{
                    SyntaxElementGenerator::function_definition(
                        "pub",
                        SyntaxElementGenerator::identifier(HirIdentifierKind::SnakeCase, "main"),
                        SyntaxElementGenerator::formal_function_argument_group(vec![]),
                        Some(return_type_annotation),
                        vec![],
                    )
                });
            }

            test "has expression" {
                expect_success("pub fn main()\ntrue\nend", "Item::function", tree!{
                    SyntaxElementGenerator::function_definition(
                        "pub",
                        SyntaxElementGenerator::identifier(HirIdentifierKind::SnakeCase, "main"),
                        SyntaxElementGenerator::formal_function_argument_group(vec![]),
                        None,
                        vec![
                            SyntaxElementGenerator::expression(
                                SyntaxElementGenerator::boolean_literal("true"),
                            ),
                        ],
                    )
                });
            }

            describe "parse item > function > argument" {
                test "has no argument" {
                    expect_success("()", "Item::function_argument_group", tree!{
                        SyntaxElementGenerator::formal_function_argument_group(vec![])
                    });
                }

                test "has three arguments" {
                    expect_success("(arg1: s32, arg2: s32, arg3: s32)", "Item::function_argument_group", tree!{
                        SyntaxElementGenerator::formal_function_argument_group(vec![
                            SyntaxElementGenerator::formal_function_argument(
                                SyntaxElementGenerator::identifier(HirIdentifierKind::SnakeCase, "arg1"),
                                SyntaxElementGenerator::data_type_annotation(
                                    SyntaxElementGenerator::data_type(
                                        SyntaxElementGenerator::primitive_data_type("s32"),
                                    ),
                                ),
                            ),
                            SyntaxElementGenerator::formal_function_argument(
                                SyntaxElementGenerator::identifier(HirIdentifierKind::SnakeCase, "arg2"),
                                SyntaxElementGenerator::data_type_annotation(
                                    SyntaxElementGenerator::data_type(
                                        SyntaxElementGenerator::primitive_data_type("s32"),
                                    ),
                                ),
                            ),
                            SyntaxElementGenerator::formal_function_argument(
                                SyntaxElementGenerator::identifier(HirIdentifierKind::SnakeCase, "arg3"),
                                SyntaxElementGenerator::data_type_annotation(
                                    SyntaxElementGenerator::data_type(
                                        SyntaxElementGenerator::primitive_data_type("s32"),
                                    ),
                                ),
                            ),
                        ])
                    });
                }

                test "parse function argument" {
                    expect_success("arg: s32", "Item::function_argument", tree!{
                        SyntaxElementGenerator::formal_function_argument(
                            SyntaxElementGenerator::identifier(HirIdentifierKind::SnakeCase, "arg"),
                            SyntaxElementGenerator::data_type_annotation(
                                SyntaxElementGenerator::data_type(
                                    SyntaxElementGenerator::primitive_data_type("s32"),
                                ),
                            ),
                        )
                    });
                }
            }
        }

        describe "parse item > visibility" {
            test "has public visibility" {
                expect_success("pub", "Item::visibility", tree!{
                    SyntaxElementGenerator::visibility("pub")
                });
            }

            test "parse private visibility" {
                expect_success("", "Item::visibility", tree!{
                    SyntaxElementGenerator::visibility("private")
                });
            }
        }

        describe "parse data type" {
            test "has the primitive type" {
                expect_success("s32", "DataType::data_type", tree!{
                    SyntaxElementGenerator::data_type(
                        SyntaxElementGenerator::primitive_data_type("s32"),
                    )
                });
            }

            test "has specified type in annotation" {
                expect_success(": s32", "DataType::annotation", tree!{
                    SyntaxElementGenerator::data_type_annotation(
                        SyntaxElementGenerator::data_type(
                            SyntaxElementGenerator::primitive_data_type("s32"),
                        ),
                    )
                });
            }
        }
    }
    
    describe "parse expression" {
        describe "parse expression > data type" {
            test "has the name of primitive data type" {
                expect_success("s32", "Expression::expression", tree!{
                    SyntaxElementGenerator::expression(
                        SyntaxElementGenerator::data_type(
                            SyntaxElementGenerator::primitive_data_type("s32"),
                        ),
                    )
                });
            }
        }

        describe "parse expression > literal" {
            describe "parse expression > literal > boolean" {
                test "has true" {
                    expect_success("true", "Expression::expression", tree!{
                        SyntaxElementGenerator::expression(
                            SyntaxElementGenerator::boolean_literal("true"),
                        )
                    });
                }
            }

            describe "parse expression > literal > integer" {
                test "can use decimal digits" {
                    expect_success("0_123_456_789", "Expression::expression", tree!{
                        SyntaxElementGenerator::expression(
                            SyntaxElementGenerator::integer_literal("dec", "0123456789", None),
                        )
                    });
                }

                test "can use binary digits" {
                    expect_success("0b0_111", "Expression::expression", tree!{
                        SyntaxElementGenerator::expression(
                            SyntaxElementGenerator::integer_literal("bin", "0111", None),
                        )
                    });
                }

                test "can use octal digits" {
                    expect_success("0o01_234_567", "Expression::expression", tree!{
                        SyntaxElementGenerator::expression(
                            SyntaxElementGenerator::integer_literal("oct", "01234567", None),
                        )
                    });
                }

                test "can use hexadecimal digits" {
                    expect_success("0x0_123_456_789_abc_def", "Expression::expression", tree!{
                        SyntaxElementGenerator::expression(
                            SyntaxElementGenerator::integer_literal("hex", "0123456789abcdef", None),
                        )
                    });
                }

                test "can specify integer suffix to integer" {
                    expect_success("0s32", "Expression::expression", tree!{
                        SyntaxElementGenerator::expression(
                            SyntaxElementGenerator::integer_literal("dec", "0", Some("s32")),
                        )
                    });
                }

                test "can speficy float suffix to integer" {
                    expect_success("0f32", "Expression::expression", tree!{
                        SyntaxElementGenerator::expression(
                            SyntaxElementGenerator::integer_literal("dec", "0", Some("f32")),
                        )
                    });
                }

                test "does not separator number" {
                    expect_success("1000", "Expression::expression", tree!{
                        SyntaxElementGenerator::expression(
                            SyntaxElementGenerator::integer_literal("dec", "1000", None),
                        )
                    });
                }

                test "separate number" {
                    expect_success("10_000_000", "Expression::expression", tree!{
                        SyntaxElementGenerator::expression(
                            SyntaxElementGenerator::integer_literal("dec", "10000000", None),
                        )
                    });
                }
            }

            describe "parse expression > literal > float" {
                test "has the float number" {
                    expect_success("0.0", "Expression::expression", tree!{
                        SyntaxElementGenerator::expression(
                            SyntaxElementGenerator::float_literal("0", "0", None),
                        )
                    });
                }

                test "separate integer and decimal" {
                    expect_success("0_000.0_000", "Expression::expression", tree!{
                        SyntaxElementGenerator::expression(
                            SyntaxElementGenerator::float_literal("0000", "0000", None),
                        )
                    });
                }

                test "can specify float suffix to float" {
                    expect_success("0.0f32", "Expression::expression", tree!{
                        SyntaxElementGenerator::expression(
                            SyntaxElementGenerator::float_literal("0", "0", Some("f32")),
                        )
                    });
                }

                test "cannot specify integer suffix to float" {
                    expect_failure("0.0s32", "Expression::expression", ParserError::ExpectedEndOfInput);
                }
            }

            describe "parse expression > literal > character" {
                test "has the character" {
                    expect_success("'a'", "Expression::expression", tree!{
                        SyntaxElementGenerator::expression(
                            SyntaxElementGenerator::character_literal(
                                SyntaxElementGenerator::single_character(leaf!("a")),
                            ),
                        )
                    });
                }

                test "has the escape character" {
                    expect_success("'\\u{0}'", "Expression::expression", tree!{
                        SyntaxElementGenerator::expression(
                            SyntaxElementGenerator::character_literal(
                                SyntaxElementGenerator::single_character(
                                    SyntaxElementGenerator::unicode_escape_sequence("0"),
                                ),
                            )
                        )
                    });
                }

                test "cannot include newline" {
                    expect_failure("'\n'", "Expression::expression", ParserError::UnexpectedEndOfInput);
                }

                test "cannot include single quotation" {
                    expect_failure("'''", "Expression::expression", ParserError::UnexpectedEndOfInput);
                }

                test "cannot include double quotation escape" {
                    expect_failure("'\\\"'", "Expression::expression", ParserError::UnexpectedEndOfInput);
                }
            }

            describe "parse expression > literal > string" {
                test "has no character" {
                    expect_success("\"\"", "Expression::expression", tree!{
                        SyntaxElementGenerator::expression(
                            SyntaxElementGenerator::string_literal(vec![]),
                        )
                    });
                }

                test "has normal and escape sequence character" {
                    expect_success("\"a\\n\"", "Expression::expression", tree!{
                        SyntaxElementGenerator::expression(
                            SyntaxElementGenerator::string_literal(vec![
                                SyntaxElementGenerator::single_character(leaf!("a")),
                                SyntaxElementGenerator::single_character(
                                    SyntaxElementGenerator::general_escape_sequence("n"),
                                ),
                            ]),
                        )
                    });
                }

                test "cannot include newline" {
                    expect_failure("\"\n\"", "Expression::expression", ParserError::UnexpectedEndOfInput);
                }

                test "cannot include double quotation" {
                    expect_failure("\"\"\"", "Expression::expression", ParserError::ExpectedEndOfInput);
                }

                test "cannot include single quotation" {
                    expect_failure("\"\\'\"", "Expression::expression", ParserError::UnexpectedEndOfInput);
                }
            }

            describe "parse expression > literal > escape sequence" {
                test "has only escape character" {
                    expect_success("\\n", "Literal::escape_sequence", tree!{
                        SyntaxElementGenerator::general_escape_sequence("n")
                    });
                }

                test "has hexadecimal unicode body" {
                    expect_success("\\u{012def}", "Literal::escape_sequence", tree!{
                        SyntaxElementGenerator::unicode_escape_sequence("012def")
                    });
                }
            }
        }

        describe "parse expression > function call" {
            test "has function name and argument group" {
                expect_success("f()", "Function::call", tree!{
                    SyntaxElementGenerator::function_call(
                        SyntaxElementGenerator::identifier(HirIdentifierKind::SnakeCase, "f"),
                        SyntaxElementGenerator::actual_argument_group(vec![]),
                    )
                });
            }

            describe "parse expression > function call > argument" {
                test "has no argument" {
                    expect_success("f()", "Function::call", tree!{
                        SyntaxElementGenerator::function_call(
                            SyntaxElementGenerator::identifier(HirIdentifierKind::SnakeCase, "f"),
                            SyntaxElementGenerator::actual_argument_group(vec![]),
                        )
                    });
                }

                test "has three arguments" {
                    expect_success("f(true, 0, ' ')", "Function::call", tree!{
                        SyntaxElementGenerator::function_call(
                            SyntaxElementGenerator::identifier(HirIdentifierKind::SnakeCase, "f"),
                            SyntaxElementGenerator::actual_argument_group(
                                vec![
                                    SyntaxElementGenerator::actual_argument(
                                        SyntaxElementGenerator::expression(
                                            SyntaxElementGenerator::boolean_literal("true"),
                                        ),
                                    ),
                                    SyntaxElementGenerator::actual_argument(
                                        SyntaxElementGenerator::expression(
                                            SyntaxElementGenerator::integer_literal("dec", "0", None),
                                        ),
                                    ),
                                    SyntaxElementGenerator::actual_argument(
                                        SyntaxElementGenerator::expression(
                                            SyntaxElementGenerator::character_literal(
                                                SyntaxElementGenerator::single_character(leaf!(" ")),
                                            ),
                                        ),
                                    ),
                                ],
                            ),
                        )
                    });
                }
            }
        }
    }
}
