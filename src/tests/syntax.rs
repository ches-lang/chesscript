use crate::{compiler::{CsCompiler}, hir::{HirIdentifierKind}, tests::generator::SyntaxElementGenerator};
use cake::{*, parser::{Parser, ParserResult, ParserError}, RuleId, tree::*};
use speculate::speculate;

speculate!{
    before {
        let cake = CsCompiler::bake_cake();

        let assert_ast = |input: &str, rule_id: &str, expected: ParserResult|
            assert_eq!(Parser::parse_rule(&cake, &RuleId(rule_id.to_string()), input, 1024), expected);

        let expect_success = |input: &str, rule_id: &str, expected: SyntaxTree|
            assert_ast(input, rule_id, Ok(expected));

        let expect_failure = |input: &str, rule_id: &str, expected: ParserError|
            assert_ast(input, rule_id, Err(expected));
    }

    it "parse public visibility" {
        expect_success("pub", "Misc::visibility", tree!{
            SyntaxElementGenerator::visibility("pub")
        });
    }

    it "parse private visibility" {
        expect_success("", "Misc::visibility", tree!{
            SyntaxElementGenerator::visibility("private")
        });
    }

    it "parse empty code" {
        expect_success("\n\n", "Main::main", tree!{
            node!{
                "Main::main" => vec![]
            }
        });
    }

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

    it "parse private module" {
        expect_success("mod Module\ntrue\nfalse\nend", "Item::module", tree!{
            SyntaxElementGenerator::module(
                "private",
                SyntaxElementGenerator::identifier(HirIdentifierKind::PascalCase, "Module"),
                vec![
                    SyntaxElementGenerator::expression(SyntaxElementGenerator::boolean_literal("true")),
                    SyntaxElementGenerator::expression(SyntaxElementGenerator::boolean_literal("false")),
                ],
            )
        });
    }

    it "parse public module with non-pascal-case identifier" {
        expect_success("pub mod module\nend", "Item::module", tree!{
            SyntaxElementGenerator::module(
                "pub",
                SyntaxElementGenerator::identifier(HirIdentifierKind::SnakeCase, "module"),
                vec![],
            )
        });
    }
}
