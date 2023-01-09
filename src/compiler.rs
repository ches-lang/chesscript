pub mod log;

use std::result::Result;
use crate::{syntax::*, js::{JsGenerator, JsGeneratorOptions, JsStringifier}, hir::{HirGenerator, HirGeneratorOptions, Hir, HirGeneratorError}};
use cake::{Cake, RuleId, Module, parser::ParserError, tree::SyntaxTree};
use self::log::CompilerLog;

#[derive(Clone, Debug, PartialEq)]
pub struct CsFileHierarchy {
    pub package_name: String,
    pub modules: Vec<String>,
}

#[derive(Debug, PartialEq)]
pub struct CsInputFile {
    pub hierarchy: CsFileHierarchy,
    pub src: String,
}

#[derive(Debug, PartialEq)]
pub struct CsOutputFile {
    pub hierarchy: CsFileHierarchy,
    pub src: String,
    pub logs: Vec<CompilerLog>,
}

pub type CsCompilerResult<T> = Result<T, CsCompilerError>;

#[derive(Clone, Debug, PartialEq)]
pub enum CsCompilerError {
    ParserError(ParserError),
    HirGeneratorError(HirGeneratorError),
}

pub struct CsOptimizationOptions {
    pub loop_unrolling: bool,
}

pub struct CsCompilerOptions<GeneratorOptions> {
    pub optimization_options: CsOptimizationOptions,
    pub generator_options: GeneratorOptions,
}

pub struct CsCompiler<'a> {
    input: Vec<CsInputFile>,
    options: &'a CsCompilerOptions<JsGeneratorOptions>,
}

impl<'a> CsCompiler<'a> {
    pub fn new(input: Vec<CsInputFile>, options: &'a CsCompilerOptions<JsGeneratorOptions>) -> Self {
        CsCompiler {
            input: input,
            options: options,
        }
    }

    pub fn bake_cake() -> Cake {
        let mut cake = Cake::new(RuleId("Main::main".to_string()));
        cake.add_module(Main::new());
        cake.add_module(Symbol::new());
        cake.add_module(Item::new());
        cake.add_module(Expression::new());
        cake.add_module(Keyword::new());
        cake.add_module(Identifier::new());
        cake.add_module(DataType::new());
        cake.add_module(Literal::new());
        cake.add_module(Function::new());
        cake
    }

    pub fn generate_js(&self) -> CsCompilerResult<Vec<CsOutputFile>> {
        let cake = CsCompiler::bake_cake();
        let mut output = Vec::new();

        for each_input in &self.input {
            let tree = self.parse(&cake, each_input)?;
            let (hir, logs) = self.generate_hir(&tree)?;
            let mut generator = JsGenerator::new(&self.options.generator_options);
            let js = generator.generate(&hir).stringify();

            let new_output = CsOutputFile {
                hierarchy:  each_input.hierarchy.clone(),
                src: js,
                logs: logs,
            };

            output.push(new_output);
        }

        Ok(output)
    }

    pub fn parse(&self, cake: &Cake, input: &CsInputFile) -> CsCompilerResult<SyntaxTree> {
        match cake.parse(&input.src, 128) {
            Ok(v) => Ok(v),
            Err(e) => return Err(CsCompilerError::ParserError(e)),
        }
    }

    pub fn generate_hir(&self, tree: &SyntaxTree) -> CsCompilerResult<(Hir, Vec<CompilerLog>)> {
        let options = HirGeneratorOptions {};
        let generator = HirGenerator::new(&options);

        match generator.generate(tree) {
            Ok(v) => Ok(v),
            Err(e) => Err(CsCompilerError::HirGeneratorError(e)),
        }
    }
}
