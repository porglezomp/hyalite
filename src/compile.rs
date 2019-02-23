use crate::ast::{BExpr, Inst, Stmt};
use std::fmt;

#[derive(Debug, Clone)]
pub struct Cfg {
    entry: Id,
    blocks: Vec<BasicBlock>,
}

#[derive(Debug, Clone, Copy)]
pub struct Id(usize);

impl Cfg {
    fn add_new_block(&mut self) -> Id {
        self.blocks.push(BasicBlock::new());
        Id(self.blocks.len() - 1)
    }

    pub fn entry(&self) -> Id {
        self.entry
    }

    pub fn get_block(&self, Id(id): Id) -> Option<&BasicBlock> {
        self.blocks.get(id)
    }
}

#[derive(Debug, Clone)]
pub enum Terminator {
    Halt,
    Jump(Id),
    Cond { cond: BExpr, t_id: Id, f_id: Id },
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub insts: Vec<Inst>,
    pub terminator: Terminator,
}

impl BasicBlock {
    fn new() -> Self {
        BasicBlock {
            insts: Vec::new(),
            terminator: Terminator::Halt,
        }
    }
}

pub fn compile(s: &Stmt) -> Cfg {
    let mut cfg = Cfg {
        entry: Id(0),
        blocks: vec![BasicBlock::new()],
    };
    compile_stmt(&mut cfg, Id(0), s);
    cfg
}

fn compile_stmt(cfg: &mut Cfg, entry: Id, s: &Stmt) -> Id {
    match s {
        Stmt::If(cond, t, f) => {
            let tail = cfg.add_new_block();
            let t_block = cfg.add_new_block();
            let t_exit = compile_stmt(cfg, t_block, t);
            cfg.blocks[t_exit.0].terminator = Terminator::Jump(tail);
            let f_exit = if let Some(f) = f {
                let f_block = cfg.add_new_block();
                let f_exit = compile_stmt(cfg, f_block, f);
                cfg.blocks[f_exit.0].terminator = Terminator::Jump(tail);
                f_exit
            } else {
                tail
            };
            cfg.blocks[entry.0].terminator = Terminator::Cond {
                cond: cond.clone(),
                t_id: t_exit,
                f_id: f_exit,
            };
            tail
        }
        Stmt::Seq(stmts) => {
            let mut block = entry;
            for stmt in stmts {
                block = compile_stmt(cfg, block, stmt);
            }
            block
        }
        Stmt::Inst(inst) => {
            cfg.blocks[entry.0].insts.push(inst.clone());
            entry
        }
    }
}

impl fmt::Display for Id {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "bb{}", self.0)
    }
}

impl fmt::Display for Terminator {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Terminator::Halt => write!(fmt, "halt"),
            Terminator::Jump(id) => write!(fmt, "jump {}", id),
            Terminator::Cond { cond, t_id, f_id } => {
                write!(fmt, "cond {}, {}, {}", cond, t_id, f_id)
            }
        }
    }
}

impl fmt::Display for BasicBlock {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        for inst in &self.insts {
            writeln!(fmt, "  {}", inst)?;
        }
        write!(fmt, "  {}", self.terminator)
    }
}

impl fmt::Display for Cfg {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "entry = {}", self.entry)?;
        for (i, block) in self.blocks.iter().enumerate() {
            write!(fmt, "\n\n{}:\n{}", i, block)?;
        }
        Ok(())
    }
}
