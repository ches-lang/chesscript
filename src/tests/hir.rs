use crate::{hir::*, tests::generator::*};
use cake::{*, tree::*};
use speculate::speculate;

speculate!{
    before {
        let default_hir_options = HirGeneratorOptions {};
    }

    it "hirize module" {
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
                identifier: "Module".to_string(),
                visibility: HirVisibility::Private,
                items: vec![
                    HirItem::Module(HirModule {
                        identifier: "SubModule".to_string(),
                        visibility: HirVisibility::Private,
                        items: vec![],
                    }),
                ],
            }
        );
    }

    it "hirize data type expression" {
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

    it "hirize boolean literal expression" {
        let mut generator = HirGenerator::new(&default_hir_options);

        assert_eq!(
            generator.expression(
                &SyntaxElementGenerator::expression(
                    SyntaxElementGenerator::boolean_literal("true"),
                ).into_node(),
            ),
            Some(HirExpression::Literal(HirLiteral::Boolean { value: "true".to_string() })),
        );
    }

    it "hirize integer literal expression" {
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
                        integer: "012".to_string(),
                        data_type: None,
                    },
                ),
            ),
        );
    }

    it "hirize integer literal expression with data type suffix" {
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
                        integer: "012".to_string(),
                        data_type: Some(HirDataType::Primitive(HirPrimitiveDataType::S32)),
                    },
                ),
            ),
        );
    }

    it "hirize single character" {
        let mut generator = HirGenerator::new(&default_hir_options);

        assert_eq!(
            generator.single_character(
                SyntaxElementGenerator::single_character(leaf!("a")).into_node(),
            ),
            Some('a'),
        );
    }

    it "hirize multibyte single character" {
        let mut generator = HirGenerator::new(&default_hir_options);

        assert_eq!(
            generator.single_character(
                SyntaxElementGenerator::single_character(leaf!("あ")).into_node(),
            ),
            Some('あ'),
        );
    }

    it "hirize single character of general escape sequence" {
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

    it "hirize single character of unicode escape sequence" {
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

    it "hirize single character of out-of-range unicode escape sequence" {
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

    it "hirize character literal expression" {
        let mut generator = HirGenerator::new(&default_hir_options);

        assert_eq!(
            generator.expression(
                &SyntaxElementGenerator::expression(
                    SyntaxElementGenerator::character_literal(
                        SyntaxElementGenerator::single_character(leaf!("a")),
                    ),
                ).into_node(),
            ),
            Some(HirExpression::Literal(HirLiteral::Character { character: 'a' })),
        );
    }

    it "hirize string literal expression" {
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
            Some(HirExpression::Literal(HirLiteral::String { string: "a\n\0".to_string() })),
        );
    }
}
