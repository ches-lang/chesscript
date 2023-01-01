pub mod log;

use std::result::Result;
use crate::{syntax::*, js::{JsGenerator, JsGeneratorOptions}, hir::{HirGenerator, HirGeneratorOptions, Hir, HirGeneratorError}};
use cake::{Cake, RuleId, Module, parser::ParserError, tree::SyntaxTree};

use self::log::CompilerLog;

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
    input: Box<&'a str>,
    options: &'a CsCompilerOptions<JsGeneratorOptions>,
}

impl<'a> CsCompiler<'a> {
    pub fn new(input: Box<&'a str>, options: &'a CsCompilerOptions<JsGeneratorOptions>) -> Self {
        CsCompiler {
            input: input,
            options: options,
        }
    }

    pub fn bake_cake() -> Cake {
        let mut cake = Cake::new(RuleId("Main::main".to_string()));
        cake.add_module(Main::new());
        cake.add_module(Symbol::new());
        cake.add_module(Expression::new());
        cake.add_module(Keyword::new());
        cake.add_module(Identifier::new());
        cake.add_module(DataType::new());
        cake.add_module(Literal::new());
        cake.add_module(Item::new());
        cake
    }

    pub fn generate_js(&self) -> CsCompilerResult<(String, Vec<CompilerLog>)> {
        let cake = CsCompiler::bake_cake();
        let tree = self.parse(&cake)?;
        let (hir, logs) = self.generate_hir(&tree)?;
        let mut generator = JsGenerator::new(&self.options.generator_options);
        let js = generator.generate(&hir);
        Ok((js, logs))
    }

    pub fn parse(&self, cake: &Cake) -> CsCompilerResult<SyntaxTree> {
        match cake.parse(*self.input, 128) {
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
