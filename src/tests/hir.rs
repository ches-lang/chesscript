use crate::{hir::*, tests::generator::*};
use speculate::speculate;

speculate!{
    it "generate module" {
        let options = HirGeneratorOptions {};
        let mut generator = HirGenerator::new(&options);

        let left = generator.module(
            &SyntaxElementGenerator::module(
                "private",
                SyntaxElementGenerator::identifier(HirIdentifierKind::PascalCase, "Module"),
                vec![],
            ).into_node(),
        ).unwrap();

        let right = HirModule {
            identifier: "Module".to_string(),
            visibility: HirVisibility::Private,
        };

        assert_eq!(left, right);
    }
}
