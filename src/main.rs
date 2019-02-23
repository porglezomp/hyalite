#![feature(slice_patterns)]

use crate::{
    ast::{BExpr, Inst, NExpr, Stmt, Var},
    compile::{Cfg, Id, Terminator},
};
use std::collections::VecDeque;

mod ast;
mod compile;

fn main() {
    let (x, y, z) = (Var::fresh(), Var::fresh(), Var::fresh());
    let prog = Stmt::Seq(vec![
        Stmt::assign(x, 69),
        Stmt::declare(y),
        Stmt::assume(BExpr::eq(NExpr::add(x, y), 420)),
        Stmt::if_(
            BExpr::eq(NExpr::add(x, y), 420),
            vec![
                Stmt::declare(z),
                Stmt::assign(x, NExpr::add(x, z)),
                Stmt::assume(BExpr::eq(x, 666)),
            ],
            None,
        ),
    ]);
    println!("{}", prog);
    println!();
    let cfg = compile::compile(&prog);
    println!("{}", cfg);
    let mut queue = make_queue(&cfg);
    let results = run_queue(&cfg, &mut queue, 10);
    for (i, trace) in results.iter().enumerate() {
        println!("Trace {}:", i);
        for inst in trace {
            println!("  {}", inst);
        }
    }
}

fn make_queue(cfg: &Cfg) -> VecDeque<(Id, Vec<Inst>)> {
    let mut queue = VecDeque::new();
    queue.push_back((cfg.entry(), Vec::new()));
    queue
}

fn run_queue(cfg: &Cfg, queue: &mut VecDeque<(Id, Vec<Inst>)>, mut fuel: usize) -> Vec<Vec<Inst>> {
    let mut results = Vec::new();
    while fuel > 0 && !queue.is_empty() {
        let (bb, mut trace) = queue.pop_front().unwrap();
        fuel = fuel.saturating_sub(1);
        let block = cfg.get_block(bb).unwrap();
        for inst in &block.insts {
            trace.push(inst.clone());
            if let Inst::Assert(_) = inst {
                fuel = fuel.saturating_sub(1);
            }
        }

        match block.terminator {
            Terminator::Halt => results.push(trace),
            Terminator::Jump(id) => queue.push_back((id, trace)),
            Terminator::Cond {
                ref cond,
                t_id,
                f_id,
            } => {
                let mut trace_f = trace.clone();
                trace.push(Inst::Assume(cond.clone()));
                trace_f.push(Inst::Assume(BExpr::not(cond.clone())));
                queue.push_back((t_id, trace));
                queue.push_back((f_id, trace_f));
            }
        }
    }
    results
}
