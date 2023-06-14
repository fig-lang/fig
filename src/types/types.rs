use wasm_encoder::ValType;

use crate::codegen::codegen::CompilerError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    I32,
    I64,

    F32,
    F64,

    String,

    Unknown,
}

impl From<String> for Type {
    fn from(value: String) -> Self {
        match value.as_str() {
            "i32" => Self::I32,
            "i64" => Self::I64,
            "f32" => Self::F32,
            "f64" => Self::F64,
            "string" => Self::String,
            _ => Self::Unknown,
        }
    }
}

impl TryInto<ValType> for Type {
    type Error = CompilerError;

    fn try_into(self) -> Result<ValType, Self::Error> {
        match self {
            Self::I32 => Ok(ValType::I32),
            Self::I64 => Ok(ValType::I64),
            Self::F32 => Ok(ValType::F32),
            Self::F64 => Ok(ValType::F64),

            // pointer of the string
            Self::String => Ok(ValType::I32),

            _t => Err(CompilerError::NotDefined(
                "type is not supported".to_string(),
            )),
        }
    }
}
