use wasm_encoder::{Instruction, ValType};

use super::codegen::CodeManager;

/// Allocates some chunk of memory
/// and push pointer to the stack
pub fn malloc<'a>(code_manager: &mut CodeManager, offset_glob_id: u32) -> Vec<Instruction<'a>> {
    use wasm_encoder::Instruction::*;

    code_manager.add_local(ValType::I64);

    vec![
        GlobalGet(offset_glob_id),
        LocalSet(1),
        LocalGet(1),
        LocalGet(0),
        I64Add,
        GlobalSet(offset_glob_id),
        LocalGet(1),
        Return,
        End,
    ]
}
