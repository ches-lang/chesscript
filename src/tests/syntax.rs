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

    /* Main */

    it "parse empty code" {
        expect_success("\n\n", "Main::main", tree!{
            node!{
                "Main::main" => vec![]
            }
        });
    }

    /* Keyword */

    it "parse keyword" {
        expect_success("s32", "Keyword::keyword", tree!{
            SyntaxElementGenerator::keyword("s32")
        });
    }

    /* Identifier */

    it "parse identifier" {
        expect_success("id", "Identifier::identifier", tree!{
            SyntaxElementGenerator::identifier(HirIdentifierKind::SnakeCase, "id")
        });
    }

    it "parse keyword as identifier" {
        // todo: Review
        expect_failure("s32", "Identifier::identifier", ParserError::UnexpectedEndOfInput);
    }

    /* DataType */

    it "parse primitive type" {
        expect_success("s32", "DataType::data_type", tree!{
            SyntaxElementGenerator::data_type(
                SyntaxElementGenerator::primitive_data_type("s32"),
            )
        });
    }

    /* Literal */

    it "parse boolean literal" {
        expect_success("true", "Literal::literal", tree!{
            SyntaxElementGenerator::boolean_literal("true")
        });
    }

    it "parse integer literal" {
        expect_success("000", "Literal::literal", tree!{
            SyntaxElementGenerator::integer_literal("dec", "000", None)
        });
    }

    it "parse bin integer literal" {
        expect_success("0b0_111", "Literal::literal", tree!{
            SyntaxElementGenerator::integer_literal("bin", "0111", None)
        });
    }

    it "parse oct integer literal" {
        expect_success("0o01_234_567", "Literal::literal", tree!{
            SyntaxElementGenerator::integer_literal("oct", "01234567", None)
        });
    }

    it "parse hex integer literal" {
        expect_success("0x0_123_456_789_abc_def", "Literal::literal", tree!{
            SyntaxElementGenerator::integer_literal("hex", "0123456789abcdef", None)
        });
    }

    it "parse integer literal with integer type suffix" {
        expect_success("000s32", "Literal::literal", tree!{
            SyntaxElementGenerator::integer_literal("dec", "000", Some("s32"))
        });
    }

    it "parse integer literal with float type suffix" {
        expect_failure("0f32", "Literal::literal", ParserError::ExpectedEndOfInput);
    }

    it "parse number chain without separator" {
        expect_success("1000", "Literal::literal", tree!{
            SyntaxElementGenerator::integer_literal("dec", "1000", None)
        });
    }

    it "parse number chain with separators" {
        expect_success("10_000_000", "Literal::literal", tree!{
            SyntaxElementGenerator::integer_literal("dec", "10000000", None)
        });
    }

    it "parse float literal" {
        expect_success("000.000", "Literal::literal", tree!{
            SyntaxElementGenerator::float_literal("000", "000", None)
        });
    }

    it "parse float literal with and without separator" {
        expect_failure("000.000_0", "Literal::literal", ParserError::ExpectedEndOfInput);
    }

    it "parse float literal with float type suffix" {
        expect_success("000.000f32", "Literal::literal", tree!{
            SyntaxElementGenerator::float_literal("000", "000", Some("f32"))
        });
    }

    it "parse float literal with integer type suffix" {
        expect_failure("000.000s32", "Literal::literal", ParserError::ExpectedEndOfInput);
    }

    it "parse general escape sequence" {
        expect_success("\\n", "Literal::escape_sequence", tree!{
            SyntaxElementGenerator::general_escape_sequence("n")
        });
    }

    /* Item */

    it "parse item" {
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

    /* Module Item */

    it "parse private module" {
        expect_success("mod Module\nend", "Item::module", tree!{
            SyntaxElementGenerator::module(
                "private",
                SyntaxElementGenerator::identifier(HirIdentifierKind::PascalCase, "Module"),
                vec![],
            )
        });
    }

    it "parse public module" {
        expect_success("pub mod Module\nend", "Item::module", tree!{
            SyntaxElementGenerator::module(
                "pub",
                SyntaxElementGenerator::identifier(HirIdentifierKind::PascalCase, "Module"),
                vec![],
            )
        });
    }

    it "parse sub item in module" {
        expect_success("mod Module\nmod SubModule\nend\nend", "Item::module", tree!{
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
                ],
            )
        });
    }

    /* Function Item */

    it "parse function" {
        expect_success("pub fn main()\nend", "Item::function", tree!{
            SyntaxElementGenerator::function(
                SyntaxElementGenerator::visibility("pub"),
                SyntaxElementGenerator::identifier(HirIdentifierKind::SnakeCase, "main"),
                SyntaxElementGenerator::function_argument_group(vec![]),
                None,
                vec![],
            )
        });
    }

    it "parse function with return type" {
        let return_type_annotation = SyntaxElementGenerator::data_type_annotation(
            SyntaxElementGenerator::data_type(
                SyntaxElementGenerator::primitive_data_type("s32"),
            ),
        );

        expect_success("pub fn main(): s32\nend", "Item::function", tree!{
            SyntaxElementGenerator::function(
                SyntaxElementGenerator::visibility("pub"),
                SyntaxElementGenerator::identifier(HirIdentifierKind::SnakeCase, "main"),
                SyntaxElementGenerator::function_argument_group(vec![]),
                Some(return_type_annotation),
                vec![],
            )
        });
    }

    it "parse function with expression" {
        expect_success("pub fn main()\ntrue\nend", "Item::function", tree!{
            SyntaxElementGenerator::function(
                SyntaxElementGenerator::visibility("pub"),
                SyntaxElementGenerator::identifier(HirIdentifierKind::SnakeCase, "main"),
                SyntaxElementGenerator::function_argument_group(vec![]),
                None,
                vec![
                    SyntaxElementGenerator::expression(
                        SyntaxElementGenerator::boolean_literal("true"),
                    ),
                ],
            )
        });
    }

    it "parse function argument group with no argument" {
        expect_success("()", "Item::function_argument_group", tree!{
            SyntaxElementGenerator::function_argument_group(vec![])
        });
    }

    it "parse function argument group with multiple arguments" {
        expect_success("(arg1: s32, arg2: s32, arg3: s32)", "Item::function_argument_group", tree!{
            SyntaxElementGenerator::function_argument_group(vec![
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
                SyntaxElementGenerator::function_argument(
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

    it "parse function argument" {
        expect_success("arg: s32", "Item::function_argument", tree!{
            SyntaxElementGenerator::function_argument(
                SyntaxElementGenerator::identifier(HirIdentifierKind::SnakeCase, "arg"),
                SyntaxElementGenerator::data_type_annotation(
                    SyntaxElementGenerator::data_type(
                        SyntaxElementGenerator::primitive_data_type("s32"),
                    ),
                ),
            )
        });
    }

    /* Item Visibility */

    it "parse public visibility" {
        expect_success("pub", "Item::visibility", tree!{
            SyntaxElementGenerator::visibility("pub")
        });
    }

    it "parse private visibility" {
        expect_success("", "Item::visibility", tree!{
            SyntaxElementGenerator::visibility("private")
        });
    }

    /* Data Type */

    it "parse primitive data type" {
        expect_success("s32", "DataType::data_type", tree!{
            SyntaxElementGenerator::data_type(
                SyntaxElementGenerator::primitive_data_type("s32"),
            )
        });
    }

    it "parse data type annoation" {
        expect_success(": s32", "DataType::annotation", tree!{
            SyntaxElementGenerator::data_type_annotation(
                SyntaxElementGenerator::data_type(
                    SyntaxElementGenerator::primitive_data_type("s32"),
                ),
            )
        });
    }
}
