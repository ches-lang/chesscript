#[derive(Clone, Debug, PartialEq)]
pub enum CompilerLog {
    Error(CompilerErrorLog),
    Warning(CompilerWarningLog),
    Notice(CompilerNoticeLog),
}

#[derive(Clone, Debug, PartialEq)]
pub enum CompilerErrorLog {
    NumberLiteralIsTooLarge,
    UnicodeEscapeSequenceIsInvalid { unicode: String },
}

#[derive(Clone, Debug, PartialEq)]
pub enum CompilerWarningLog {
    IdentifierShouldBePascalCase { id: String },
}

#[derive(Clone, Debug, PartialEq)]
pub enum CompilerNoticeLog {
}
