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

    it "jsify ES module" {
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

    it "jsify CommonJS module including module.exports" {
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
