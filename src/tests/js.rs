use crate::{js::{JsModuleStyle, JsTarget, JsGeneratorOptions, JsGenerator}, hir::*};
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

    it "generate ES module" {
        let mut generator = JsGenerator::new(default_js_options);

        assert_eq!(
            generator.module(
                &HirModule {
                    identifier: "Module".to_string(),
                    visibility: HirVisibility::Private,
                    items: vec![
                        HirItem::Module(HirModule {
                            identifier: "SubModule".to_string(),
                            visibility: HirVisibility::Private,
                            items: vec![],
                        }),
                    ],
                },
            ),
            "export namespace Module{export namespace SubModule{}}".to_string(),
        );
    }

    it "generate CommonJS module including module.exports" {
        let mut generator = JsGenerator::new(
            &JsGeneratorOptions {
                minify: true,
                target: JsTarget::Es2015,
                module_style: JsModuleStyle::CommonJs,
            },
        );

        let module = HirModule {
            identifier: "Module".to_string(),
            visibility: HirVisibility::Private,
            items: vec![
                HirItem::Module(HirModule {
                    identifier: "SubModule".to_string(),
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
            ),
            "namespace Module{namespace SubModule{}}module.exports={Module};".to_string(),
        );
    }
}