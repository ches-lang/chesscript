use crate::{hir::*, tests::generator::*};
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

    it "hirize data type" {
        let mut generator = HirGenerator::new(&default_hir_options);

        assert_eq!(
            generator.data_type(
                &SyntaxElementGenerator::data_type(
                    SyntaxElementGenerator::primitive_data_type("bool"),
                ).into_node(),
            ),
            HirDataType::Primitive(HirPrimitiveDataType::Bool),
        );
    }
}
