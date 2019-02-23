#![feature(slice_patterns)]

use crate::{
    ast::{BExpr, NExpr, Stmt, Var},
    smt::{make_queue, make_sat, print_assert, run_queue},
};

mod ast;
mod compile;
mod smt;

use z3;

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
        Stmt::while_(
            BExpr::lt(0, x),
            vec![
                Stmt::store(0, x),
                Stmt::store(x, 13),
                Stmt::assign(x, NExpr::sub(x, 413)),
            ],
        ),
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
        let res = make_sat(&ctx, &trace);
        println!("Trace {}:", i);
        print_assert(&trace, &res);
    }
}
