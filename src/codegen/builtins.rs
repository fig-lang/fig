use wasm_encoder::Instruction;

pub fn malloc<'a>() -> Vec<Instruction<'a>> {
    use wasm_encoder::Instruction::*;

    vec![
        GlobalGet(0),
        LocalSet(1),
        LocalGet(1),
        LocalGet(0),
        I32Add,
        GlobalSet(0),
        LocalGet(1),
        Return,
        End,
    ]
}

pub fn free<'a>() -> Vec<Instruction<'a>> {
    use wasm_encoder::Instruction::*;

    vec![
        GlobalGet(0),
        LocalGet(0),
        I32Sub,
        GlobalSet(0),
        End,
    ]
}
