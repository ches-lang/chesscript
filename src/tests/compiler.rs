use crate::{compiler::{CsCompiler, CsOptimizationOptions, CsCompilerOptions, log::CompilerLog}, js::{JsModuleStyle, JsTarget, JsGeneratorOptions}};
use speculate::speculate;

struct CsCompilerGenerator<'a> {
    input: &'a str,
    compiler_options: Option<CsCompilerOptions<JsGeneratorOptions>>,
    js_options: Option<JsGeneratorOptions>,
}

impl<'a> CsCompilerGenerator<'a> {
    pub fn new(input: &'a str) -> Self {
        CsCompilerGenerator {
            input: input,
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

    pub fn compile(self) -> (String, Vec<CompilerLog>) {
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

        let compiler = CsCompiler::new(Box::new(self.input), &compiler_options);
        compiler.generate_js().unwrap()
    }
}

speculate!{
    test "compile module into ES module" {
        let compiler = CsCompilerGenerator::new("mod Module\nend");

        assert_eq!(compiler.compile(), (
            "export namespace Module{}".to_string(),
            Vec::new(),
        ));
    }

    test "compile module into CommonJS module" {
        let compiler = CsCompilerGenerator::new("mod Module\nend")
            .apply_js_options(JsGeneratorOptions {
                minify: true,
                target: JsTarget::Es2015,
                module_style: JsModuleStyle::CommonJs,
            });

        assert_eq!(compiler.compile(), (
            "namespace Module{}module.exports={Module};".to_string(),
            Vec::new(),
        ));
    }

    test "compile function" {
        let compiler = CsCompilerGenerator::new("fn main()\nend");

        assert_eq!(compiler.compile(), (
            "export function main(){}".to_string(),
            Vec::new(),
        ));
    }
}
