use crate::{
    ast::{BExpr, Inst, NExpr, Var, MEM_VAR},
    compile::{Cfg, Id, Terminator},
};
use std::{
    collections::{BTreeMap, VecDeque},
    sync::atomic::{AtomicU32, Ordering},
};

pub fn make_queue(cfg: &Cfg) -> VecDeque<(Id, Vec<Inst>)> {
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

pub struct SmtResult<'ctx> {
    sat: z3::Solver<'ctx>,
    vars: BTreeMap<Var, z3::Ast<'ctx>>,
    decls: BTreeMap<Var, z3::Ast<'ctx>>,
    mem: z3::Ast<'ctx>,
}

fn make_mem(ctx: &z3::Context) -> z3::Ast {
    let arr = ctx.array_sort(&ctx.bitvector_sort(32), &ctx.bitvector_sort(32));
    z3::Ast::new_const(
        &z3::Symbol::from_string(
            ctx,
            &format!("@MEM@{}", Z3_FRESH.fetch_add(1, Ordering::Relaxed)),
        ),
        &arr,
    )
}

pub fn make_sat<'ctx>(ctx: &'ctx z3::Context, trace: &[Inst]) -> SmtResult<'ctx> {
    let solver = z3::Solver::new(&ctx);
    let mut decls = BTreeMap::new();
    let mut vars = BTreeMap::new();
    let mut mem = make_mem(&ctx);
    vars.insert(MEM_VAR, mem.clone());
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
            Inst::Store(addr, n) => {
                let new_mem = make_mem(&ctx);
                solver.assert(
                    &new_mem._eq(&mem.store(&addr.to_ast(ctx, &vars), &n.to_ast(ctx, &vars))),
                );
                mem = new_mem;
                vars.insert(MEM_VAR, mem.clone());
            }
            Inst::Assume(b) => {
                solver.assert(&b.to_ast(ctx, &vars));
            }
            Inst::Assert(b) => {
                solver.assert(&b.to_ast(ctx, &vars).not());
            }
        }
    }
    SmtResult {
        sat: solver,
        vars,
        decls,
        mem,
    }
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
            NExpr::Load(addr) => vars[&MEM_VAR].select(&addr.to_ast(ctx, vars)),
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

pub fn run_queue(
    cfg: &Cfg,
    queue: &mut VecDeque<(Id, Vec<Inst>)>,
    mut fuel: usize,
) -> Vec<Vec<Inst>> {
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
                    assert!(make_sat(&ctx, &trace).sat.check());
                    trace.push(inst.clone());
                    let res = make_sat(&ctx, &trace);
                    if !res.sat.check() {
                        println!("Assumption contradiction!");
                        print_assert(&trace, &res);
                        continue 'queue;
                    }
                }
                Inst::Assert(b) => {
                    fuel = fuel.saturating_sub(1);
                    let mut assert_trace = trace.clone();
                    assert_trace.push(inst.clone());
                    let res = make_sat(&ctx, &assert_trace);
                    if res.sat.check() {
                        println!("Assertion failure:");
                        print_assert(&assert_trace, &res);
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
                if make_sat(&ctx, &trace).sat.check() {
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

                if make_sat(&ctx, &trace).sat.check() {
                    queue.push_back((t_id, trace));
                } else {
                    println!("Discarding inconsistent path: {}", trace.last().unwrap());
                }

                if make_sat(&ctx, &trace_f).sat.check() {
                    queue.push_back((f_id, trace_f));
                } else {
                    println!("Discarding inconsistent path: {}", trace_f.last().unwrap());
                }
            }
        }
    }
    results
}

pub fn print_assert(trace: &[Inst], res: &SmtResult) {
    res.sat.check();
    for inst in trace {
        println!("{}", inst);
    }
    let model = res.sat.get_model();
    println!("===");
    for (var, exp) in &res.decls {
        if let Some(val) = model.eval(&exp) {
            println!("{}: {} ({})", var, exp, val);
        } else {
            println!("{}: {} (???)", var, exp);
        }
    }
    println!("=>");
    for (var, exp) in &res.vars {
        if *var == MEM_VAR {
            continue;
        }
        if let Some(val) = model.eval(&exp) {
            println!("{}: {} ({})", var, exp, val);
        } else {
            println!("{}: {} (???)", var, exp);
        }
    }
    println!("===");
    println!("Memory: {}", model.eval(&res.mem).unwrap());
}
