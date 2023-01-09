use crate::{compiler::{CsCompiler, CsOptimizationOptions, CsCompilerOptions, CsInputFile, CsOutputFile, CsFileHierarchy}, js::{JsModuleStyle, JsTarget, JsGeneratorOptions}};
use speculate::speculate;

struct CsCompilerGenerator {
    input: Vec<CsInputFile>,
    compiler_options: Option<CsCompilerOptions<JsGeneratorOptions>>,
    js_options: Option<JsGeneratorOptions>,
}

impl CsCompilerGenerator {
    pub fn new(input_files: Vec<CsInputFile>) -> Self {
        CsCompilerGenerator {
            input: input_files,
            compiler_options: None,
            js_options: None,
        }
    }

    pub fn apply_js_options(mut self, js_options: JsGeneratorOptions) -> Self {
        self.js_options = Some(js_options);
        self
    }

    pub fn apply_compiler_options(mut self, compiler_options: CsCompilerOptions<JsGeneratorOptions>) -> Self {
        self.compiler_options = Some(compiler_options);
        self
    }

    pub fn compile(self) -> Vec<CsOutputFile> {
        let js_options = self.js_options.unwrap_or(JsGeneratorOptions {
            minify: true,
            target: JsTarget::Es2015,
            module_style: JsModuleStyle::Es2015,
        });

        let compiler_options = self.compiler_options.unwrap_or(CsCompilerOptions {
            optimization_options: CsOptimizationOptions {
                loop_unrolling: false,
            },
            generator_options: js_options,
        });

        let compiler = CsCompiler::new(self.input, &compiler_options);
        compiler.generate_js().unwrap()
    }
}

speculate!{
    before {
        let hierarchy = CsFileHierarchy {
            package_name: "ches_test".to_string(),
            modules: vec![],
        };

        let assert_output = |
            input: &str,
            output: &str,
            callback: fn(generator: CsCompilerGenerator) -> CsCompilerGenerator,
        | {
            let mut generator = CsCompilerGenerator::new(
                vec![
                    CsInputFile {
                        hierarchy: hierarchy.clone(),
                        src: input.to_string(),
                    },
                ],
            );

            generator = callback(generator);

            assert_eq!(generator.compile(), vec![
                CsOutputFile {
                    hierarchy: hierarchy.clone(),
                    src: output.to_string(),
                    logs: Vec::new(),
                },
            ]);
        };
    }

    test "compile module into ES module" {
        assert_output("mod Module\nend", "export namespace Module{}", |v| v);
    }

    test "compile module into CommonJS module" {
        assert_output("mod Module\nend", "namespace Module{}module.exports={Module};", |generator| {
            generator.apply_js_options(JsGeneratorOptions {
                minify: true,
                target: JsTarget::Es2015,
                module_style: JsModuleStyle::CommonJs,
            })
        });
    }

    test "compile function" {
        // fix: CSR.main = ...

        assert_output("fn main()\nend", "var CSR;((CSR)=>{var main;main=()=>{};CSR.main();})(CSR||(CSR={}));", |generator| {
            generator.apply_js_options(JsGeneratorOptions {
                minify: true,
                target: JsTarget::Playground,
                module_style: JsModuleStyle::NoModules,
            })
        });
    }
}
