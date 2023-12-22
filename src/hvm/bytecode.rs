use std::fmt::Display;


#[derive(Debug, Clone)]
pub enum Instruction {
    Return,
    Constant,
}

impl Into<usize> for Instruction {
    fn into(self) -> usize {
        match self {
            Self::Return => 0,
            Self::Constant => 1,
        }
    }
}

impl From<usize> for Instruction {
    fn from(value: usize) -> Self {
        match value {
            0 => Self::Return,
            1 => Self::Constant,
            _ => panic!(),
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Self::Return => "Return",
            Self::Constant => "Constant",
        })
    }
}

type Value = i32;

pub struct Chunk {
    pub name: String,
    pub code: Vec<usize>,
    values: Vec<Value>,
}

impl Chunk {
    pub fn new(name: &str) -> Self {
        Self { name: name.to_string(), code: vec![], values: vec![] }
    }

    pub fn add_constant(&mut self, constant: Value) -> usize {
        self.values.push(constant);
        self.values.len() - 1
    }
}

impl Display for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Chunk `{}`:", self.name)?;
        let mut idx = 0;
        while idx < self.code.len() {
            let curr_inst = self.code[idx].into();
            match curr_inst {
                Instruction::Return => writeln!(f, "    {idx}: {curr_inst}")?,
                Instruction::Constant => {
                    let operand = self.code[idx + 1];
                    writeln!(f, "    {idx}: {curr_inst} {operand}")?;
                    idx += 1;
                }
            }

            idx += 1;
        }        

        Ok(())
    }
}
