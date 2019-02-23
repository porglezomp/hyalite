#![feature(slice_patterns)]

use crate::{
    ast::{BExpr, Inst, NExpr, Stmt, Var},
    compile::{Cfg, Id, Terminator},
};
use std::{
    collections::{BTreeMap, VecDeque},
    sync::atomic::{AtomicU32, Ordering},
};

use z3;

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
        Stmt::while_(BExpr::lt(0, x), vec![Stmt::assign(x, NExpr::sub(x, 413))]),
        Stmt::assert(BExpr::le(x, 0)),
    ]);
    println!("{}", prog);
    println!();
    let cfg = compile::compile(&prog);
    println!("{}", cfg);
    let mut queue = make_queue(&cfg);
    let results = run_queue(&cfg, &mut queue, 25);

    let mut config = z3::Config::new();
    config.set_model_generation(true);
    let ctx = z3::Context::new(&config);
    for (i, trace) in results.iter().enumerate() {
        let (sat, vars, decls) = make_sat(&ctx, &trace);
        println!("Trace {}:", i);
        sat.check();
        print_assert(&trace, &sat, &vars, &decls);
    }
}

fn make_queue(cfg: &Cfg) -> VecDeque<(Id, Vec<Inst>)> {
    let mut queue = VecDeque::new();
    queue.push_back((cfg.entry(), Vec::new()));
    queue
}

static Z3_FRESH: AtomicU32 = AtomicU32::new(0);

fn make_var(ctx: &z3::Context, v: Var) -> z3::Ast {
    ctx.named_bitvector_const(
        &format!("{}@{}", v, Z3_FRESH.fetch_add(1, Ordering::Relaxed)),
        32,
    )
}

fn make_sat<'ctx>(
    ctx: &'ctx z3::Context,
    trace: &[Inst],
) -> (
    z3::Solver<'ctx>,
    BTreeMap<Var, z3::Ast<'ctx>>,
    BTreeMap<Var, z3::Ast<'ctx>>,
) {
    let solver = z3::Solver::new(&ctx);
    let mut decls = BTreeMap::new();
    let mut vars = BTreeMap::new();
    for inst in trace {
        match inst {
            Inst::Declare(v) => {
                let new_var = make_var(&ctx, *v);
                decls.insert(*v, new_var.clone());
                vars.insert(*v, new_var);
            }
            Inst::Assign(v, x) => {
                let new_var = make_var(&ctx, *v);
                solver.assert(&new_var._eq(&x.to_ast(ctx, &vars)));
                vars.insert(*v, new_var);
            }
            Inst::Assume(b) => {
                solver.assert(&b.to_ast(ctx, &vars));
            }
            Inst::Assert(b) => {
                solver.assert(&b.to_ast(ctx, &vars).not());
            }
        }
    }
    (solver, vars, decls)
}

impl NExpr {
    fn to_ast<'ctx>(
        &self,
        ctx: &'ctx z3::Context,
        vars: &BTreeMap<Var, z3::Ast<'ctx>>,
    ) -> z3::Ast<'ctx> {
        match self {
            NExpr::Add(l, r) => l.to_ast(ctx, vars).bvadd(&r.to_ast(ctx, vars)),
            NExpr::Sub(l, r) => l.to_ast(ctx, vars).bvsub(&r.to_ast(ctx, vars)),
            NExpr::Mul(l, r) => l.to_ast(ctx, vars).bvmul(&r.to_ast(ctx, vars)),
            NExpr::Div(l, r) => l.to_ast(ctx, vars).bvsdiv(&r.to_ast(ctx, vars)),
            NExpr::Mod(l, r) => l.to_ast(ctx, vars).bvsmod(&r.to_ast(ctx, vars)),
            NExpr::Var(v) => vars[v].clone(),
            NExpr::OfBool(b) => {
                let bv32 = ctx.bitvector_sort(32);
                b.to_ast(ctx, vars)
                    .ite(&bv32.from_i64(1), &bv32.from_i64(0))
            }
            NExpr::Lit(i) => {
                let bv32 = ctx.bitvector_sort(32);
                bv32.from_i64(*i as i64)
            }
        }
    }
}

impl BExpr {
    fn to_ast<'ctx>(
        &self,
        ctx: &'ctx z3::Context,
        vars: &BTreeMap<Var, z3::Ast<'ctx>>,
    ) -> z3::Ast<'ctx> {
        match self {
            BExpr::Eq(l, r) => l.to_ast(ctx, vars)._eq(&r.to_ast(ctx, vars)),
            BExpr::Lt(l, r) => l.to_ast(ctx, vars).bvslt(&r.to_ast(ctx, vars)),
            BExpr::And(l, r) => l.to_ast(ctx, vars).and(&[&r.to_ast(ctx, vars)]),
            BExpr::Or(l, r) => l.to_ast(ctx, vars).or(&[&r.to_ast(ctx, vars)]),
            BExpr::Not(b) => b.to_ast(ctx, vars).not(),
            BExpr::OfNum(n) => {
                let bv32 = ctx.bitvector_sort(32);
                n.to_ast(ctx, vars)._eq(&bv32.from_i64(0)).not()
            }
            BExpr::Lit(b) => ctx.from_bool(*b),
        }
    }
}

fn run_queue(cfg: &Cfg, queue: &mut VecDeque<(Id, Vec<Inst>)>, mut fuel: usize) -> Vec<Vec<Inst>> {
    let mut results = Vec::new();
    let mut config = z3::Config::new();
    config.set_model_generation(true);
    let ctx = z3::Context::new(&config);
    'queue: while fuel > 0 && !queue.is_empty() {
        let (bb, mut trace) = queue.pop_front().unwrap();
        let block = cfg.get_block(bb).unwrap();
        match block.terminator {
            // Don't use fuel on forwarding blocks
            // (TODO: Need to ensure that we don't get stuck in a loop of forwarding blocks)
            Terminator::Jump(_) if block.insts.is_empty() => (),
            _ => fuel = fuel.saturating_sub(1),
        }

        for inst in &block.insts {
            match inst {
                Inst::Assume(_) => {
                    assert!(make_sat(&ctx, &trace).0.check());
                    trace.push(inst.clone());
                    let (sat, vars, decls) = make_sat(&ctx, &trace);
                    if !sat.check() {
                        println!("Assumption contradiction!");
                        print_assert(&trace, &sat, &vars, &decls);
                        continue 'queue;
                    }
                }
                Inst::Assert(b) => {
                    fuel = fuel.saturating_sub(1);
                    let mut assert_trace = trace.clone();
                    assert_trace.push(inst.clone());
                    let (sat, vars, decls) = make_sat(&ctx, &assert_trace);
                    if sat.check() {
                        println!("Assertion failure:");
                        print_assert(&assert_trace, &sat, &vars, &decls);
                        continue 'queue;
                    }
                    // results.push(assert_trace);
                    trace.push(Inst::Assume(b.clone()));
                }
                inst => trace.push(inst.clone()),
            }
        }

        match block.terminator {
            Terminator::Halt => {
                if make_sat(&ctx, &trace).0.check() {
                    results.push(trace)
                } else {
                    println!("Infeasible path reached halt");
                }
            }
            Terminator::Jump(id) => queue.push_back((id, trace)),
            Terminator::Cond {
                ref cond,
                t_id,
                f_id,
            } => {
                let mut trace_f = trace.clone();
                trace.push(Inst::Assume(cond.clone()));
                trace_f.push(Inst::Assume(BExpr::not(cond.clone())));

                if make_sat(&ctx, &trace).0.check() {
                    queue.push_back((t_id, trace));
                } else {
                    println!("Discarding inconsistent path: {}", trace.last().unwrap());
                }

                if make_sat(&ctx, &trace_f).0.check() {
                    queue.push_back((f_id, trace_f));
                } else {
                    println!("Discarding inconsistent path: {}", trace_f.last().unwrap());
                }
            }
        }
    }
    results
}

fn print_assert(
    trace: &[Inst],
    sat: &z3::Solver,
    vars: &BTreeMap<Var, z3::Ast>,
    decls: &BTreeMap<Var, z3::Ast>,
) {
    // println!("\n{}", sat);
    for inst in trace {
        println!("{}", inst);
    }
    let model = sat.get_model();
    println!("===");
    for (var, exp) in decls {
        if let Some(val) = model.eval(&exp) {
            println!("{}: {} ({})", var, exp, val.as_i64().unwrap() as i32);
        } else {
            println!("{}: {} (???)", var, exp);
        }
    }
    println!("=>");
    for (var, exp) in vars {
        if let Some(val) = model.eval(&exp) {
            println!("{}: {} ({})", var, exp, val.as_i64().unwrap() as i32);
        } else {
            println!("{}: {} (???)", var, exp);
        }
    }
}
