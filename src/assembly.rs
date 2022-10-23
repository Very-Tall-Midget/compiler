use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct Assembly {
    asm: Vec<OpCode>,
}

impl Assembly {
    pub fn new() -> Self {
        Assembly { asm: Vec::new() }
    }

    pub fn push(&mut self, op: OpCode) {
        self.asm.push(op);
    }

    pub fn push_asm(&mut self, asm: &Assembly) {
        self.asm.extend(asm.asm.iter().cloned());
    }

    pub fn to_string(&self) -> Result<String, String> {
        let mut res = String::new();
        for op in &self.asm {
            match op {
                OpCode::GlobalVariable(id, value) => {
                    if *value == 0 {
                        res.push_str(&format!(
                            "    .globl {0}\n    .bss\n    .align 4\n{0}:\n    .zero 4\n",
                            id
                        ));
                    } else {
                        res.push_str(&format!(
                            "    .globl {0}\n    .data\n    .align 4\n{0}:\n    .long {1}\n",
                            id, value
                        ));
                    }
                }
                OpCode::FunctionDecl(name) => {
                    res.push_str(&format!("    .globl {}\n    .text\n", name))
                }
                OpCode::StartFunction(name) => res.push_str(&format!("{}:\n", name)),
                OpCode::Label(label) => res.push_str(&format!("{}:\n", label)),
                OpCode::Call(name) => res.push_str(&format!("    call {}\n", name)),
                OpCode::PreserveStack => res.push_str("    push %rbp\n    mov %rsp, %rbp\n"),
                OpCode::RestoreStack => res.push_str("    mov %rbp, %rsp\n    pop %rbp\n"),
                OpCode::Push(location) => res.push_str(&format!(
                    "    push{1} {0}\n",
                    location,
                    location.size.to_string()
                )),
                OpCode::PushImmediate(value) => res.push_str(&format!("    push ${}\n", value)),
                OpCode::Pop(location) => res.push_str(&format!(
                    "    pop{1} {0}\n",
                    location,
                    location.size.to_string()
                )),
                OpCode::Mov(from, to) => {
                    if from.size != to.size {
                        return Err(format!("Cannot move {} into {}, different sizes", from, to));
                    }
                    res.push_str(&format!("    mov {}, {}\n", from, to));
                }
                OpCode::MovImmediate(value, location) => res.push_str(&format!(
                    "    mov{2} ${0}, {1}\n",
                    value,
                    location,
                    location.size.to_string()
                )),
                OpCode::Compare(left, right) => {
                    if left.size != right.size {
                        return Err(format!(
                            "Cannot compare {} and {}, different sizes",
                            left, right
                        ));
                    }
                    res.push_str(&format!("    cmp {}, {}\n", left, right));
                }
                OpCode::CompareImmediate(value, location) => res.push_str(&format!(
                    "    cmp{2} ${0}, {1}\n",
                    value,
                    location,
                    location.size.to_string()
                )),
                OpCode::Jump(condition, label) => {
                    res.push_str(&format!("    j{} {}\n", condition.to_string(), label));
                }
                OpCode::Add(left, right) => {
                    if left.size != right.size {
                        return Err(format!(
                            "Cannot add {} and {}, different sizes",
                            left, right
                        ));
                    }
                    res.push_str(&format!("    add {}, {}\n", left, right));
                }
                OpCode::AddImmediate(value, location) => res.push_str(&format!(
                    "    add{2} ${0}, {1}\n",
                    value,
                    location,
                    location.size.to_string()
                )),
                OpCode::Sub(left, right) => {
                    if left.size != right.size {
                        return Err(format!(
                            "Cannot subtract {} and {}, different sizes",
                            left, right
                        ));
                    }
                    res.push_str(&format!("    sub {}, {}\n", left, right));
                }
                OpCode::Mult(left, right) => {
                    if left.size != right.size {
                        return Err(format!(
                            "Cannot multiply {} and {}, different sizes",
                            left, right
                        ));
                    }
                    res.push_str(&format!("    imul {}, {}\n", left, right));
                }
                OpCode::IDiv(location) => res.push_str(&format!("    idiv {}\n", location)),
                OpCode::SignExtend => res.push_str("    cqo\n"),
                OpCode::ShiftLeft(left, right) => {
                    if left.size != LocationSize::Byte {
                        return Err(format!(
                            "Cannot shift {1} left by {0}, wrong size ({0} must be a byte)",
                            left, right
                        ));
                    }
                    res.push_str(&format!("    shl {}, {}\n", left, right));
                }
                OpCode::ShiftRight(left, right) => {
                    if left.size != LocationSize::Byte {
                        return Err(format!(
                            "Cannot shift {1} right by {0}, wrong size ({0} must be a byte)",
                            left, right
                        ));
                    }
                    res.push_str(&format!("    shr {}, {}\n", left, right));
                }
                OpCode::Increment(location) => res.push_str(&format!(
                    "    inc{} {}\n",
                    location.size.to_string(),
                    location
                )),
                OpCode::Decrement(location) => res.push_str(&format!(
                    "    dec{} {}\n",
                    location.size.to_string(),
                    location
                )),
                OpCode::And(left, right) => {
                    if left.size != right.size {
                        return Err(format!(
                            "Cannot and {} and {}, different sizes",
                            left, right
                        ));
                    }
                    res.push_str(&format!("    and {}, {}\n", left, right));
                }
                OpCode::Xor(left, right) => {
                    if left.size != right.size {
                        return Err(format!(
                            "Cannot xor {} and {}, different sizes",
                            left, right
                        ));
                    }
                    res.push_str(&format!("    xor {}, {}\n", left, right));
                }
                OpCode::Or(left, right) => {
                    if left.size != right.size {
                        return Err(format!("Cannot or {} and {}, different sizes", left, right));
                    }
                    res.push_str(&format!("    or {}, {}\n", left, right));
                }
                OpCode::Negation(location) => res.push_str(&format!(
                    "    neg{} {}\n",
                    location.size.to_string(),
                    location
                )),
                OpCode::Not(location) => res.push_str(&format!(
                    "    not{} {}\n",
                    location.size.to_string(),
                    location
                )),
                OpCode::Set(condition, location) => {
                    if location.size != LocationSize::Byte {
                        return Err(format!(
                            "Cannot set {0} with set{1}, wrong size ({0} must be a byte)",
                            location,
                            condition.to_string()
                        ));
                    }
                    res.push_str(&format!("    set{} {}\n", condition.to_string(), location));
                }
                OpCode::Return => res.push_str("    ret\n"),
            }
        }
        Ok(res)
    }

    pub fn optimised(&self) -> Self {
        let mut res = Assembly::new();
        let mut last_mov: Option<OpCode> = None;
        for opcode in &self.asm {
            match opcode {
                OpCode::Mov(from, to) => {
                    if let Some(last_mov_op) = &last_mov {
                        match last_mov_op {
                            OpCode::Mov(last_mov_from, last_mov_to) => {
                                if last_mov_to == from {
                                    last_mov = Some(OpCode::Mov(last_mov_from.clone(), to.clone()));
                                } else {
                                    res.push(last_mov_op.clone());
                                    last_mov = Some(opcode.clone());
                                }
                            }
                            OpCode::MovImmediate(last_mov_val, last_mov_reg) => {
                                if last_mov_reg == from {
                                    last_mov = Some(OpCode::MovImmediate(*last_mov_val, to.clone()));
                                } else {
                                    res.push(last_mov_op.clone());
                                    last_mov = Some(opcode.clone());
                                }
                            }
                            _ => unreachable!(),
                        }
                    } else {
                        last_mov = Some(opcode.clone());
                    }
                }
                OpCode::MovImmediate(_, _) => {
                    if let Some(last_mov_op) = &last_mov {
                        res.push(last_mov_op.clone());
                        last_mov = Some(opcode.clone());
                    } else {
                        last_mov = Some(opcode.clone());
                    }
                }
                _ => {
                    if let Some(last_mov_op) = &last_mov {
                        res.push(last_mov_op.clone());
                        last_mov = None;
                    }
                    res.push(opcode.clone());
                }
            }
        }
        res
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Location {
    pub name: String,
    pub size: LocationSize,
}

impl Location {
    pub fn rax() -> Self {
        Location {
            name: "%rax".to_string(),
            size: LocationSize::Qword,
        }
    }

    pub fn rcx() -> Self {
        Location {
            name: "%rcx".to_string(),
            size: LocationSize::Qword,
        }
    }

    pub fn rdx() -> Self {
        Location {
            name: "%rdx".to_string(),
            size: LocationSize::Qword,
        }
    }

    pub fn rsp() -> Self {
        Location {
            name: "%rsp".to_string(),
            size: LocationSize::Qword,
        }
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LocationSize {
    Byte,
    Word,
    Dword,
    Qword,
}

impl ToString for LocationSize {
    fn to_string(&self) -> String {
        match self {
            LocationSize::Byte => "b",
            LocationSize::Word => "w",
            LocationSize::Dword => "l",
            LocationSize::Qword => "q",
        }
        .to_string()
    }
}

#[derive(Debug, Clone)]
pub enum OpCode {
    GlobalVariable(String, i32), // name, value
    FunctionDecl(String),        // name
    StartFunction(String),       // name
    Label(String),               // name
    Call(String),                // name

    PreserveStack,
    RestoreStack,

    Push(Location),                  // from
    PushImmediate(i32),              // value
    Pop(Location),                   // to
    Mov(Location, Location),         // from, to
    MovImmediate(i32, Location),     // value, to
    Compare(Location, Location),     // left, right
    CompareImmediate(i32, Location), // value, loction
    Jump(JumpCondition, String),     // condition, label

    Add(Location, Location),     // left, right
    AddImmediate(i32, Location), // value, loction
    Sub(Location, Location),     // left, right
    Mult(Location, Location),    // left, right
    IDiv(Location),              // loction
    SignExtend,
    ShiftLeft(Location, Location),  // left, right
    ShiftRight(Location, Location), // left, right
    Increment(Location),            // location
    Decrement(Location),            // location

    And(Location, Location), // left, right
    Xor(Location, Location), // left, right
    Or(Location, Location),  // left, right
    Negation(Location),      // location
    Not(Location),           // location

    Set(SetCondition, Location), // condition, loction

    Return,
}

#[derive(Debug, Clone)]
pub enum JumpCondition {
    Unconditional,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

impl ToString for JumpCondition {
    fn to_string(&self) -> String {
        match self {
            JumpCondition::Unconditional => "mp",
            JumpCondition::Equal => "e",
            JumpCondition::NotEqual => "ne",
            JumpCondition::LessThan => "l",
            JumpCondition::LessThanOrEqual => "le",
            JumpCondition::GreaterThan => "g",
            JumpCondition::GreaterThanOrEqual => "ge",
        }
        .to_string()
    }
}

#[derive(Debug, Clone)]
pub enum SetCondition {
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

impl ToString for SetCondition {
    fn to_string(&self) -> String {
        match self {
            SetCondition::Equal => "e",
            SetCondition::NotEqual => "ne",
            SetCondition::LessThan => "l",
            SetCondition::LessThanOrEqual => "le",
            SetCondition::GreaterThan => "g",
            SetCondition::GreaterThanOrEqual => "ge",
        }
        .to_string()
    }
}
