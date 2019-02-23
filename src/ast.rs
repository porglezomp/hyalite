use std::{
    fmt,
    rc::Rc,
    sync::atomic::{AtomicU32, Ordering},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Var(u32);

pub static MEM_VAR: Var = Var(0);
static NEXT_FRESH_VAR: AtomicU32 = AtomicU32::new(1);

impl Var {
    pub fn fresh() -> Var {
        Var(NEXT_FRESH_VAR.fetch_add(1, Ordering::Relaxed))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BExpr {
    Eq(Rc<NExpr>, Rc<NExpr>),
    Lt(Rc<NExpr>, Rc<NExpr>),
    And(Rc<BExpr>, Rc<BExpr>),
    Or(Rc<BExpr>, Rc<BExpr>),
    Not(Rc<BExpr>),
    OfNum(Rc<NExpr>),
    Lit(bool),
}

impl BExpr {
    pub fn eq(n1: impl Into<NExpr>, n2: impl Into<NExpr>) -> BExpr {
        BExpr::Eq(Rc::new(n1.into()), Rc::new(n2.into()))
    }

    pub fn ne(n1: impl Into<NExpr>, n2: impl Into<NExpr>) -> BExpr {
        BExpr::not(BExpr::Eq(Rc::new(n1.into()), Rc::new(n2.into())))
    }

    pub fn lt(n1: impl Into<NExpr>, n2: impl Into<NExpr>) -> BExpr {
        BExpr::Lt(Rc::new(n1.into()), Rc::new(n2.into()))
    }

    pub fn gt(n1: impl Into<NExpr>, n2: impl Into<NExpr>) -> BExpr {
        BExpr::Lt(Rc::new(n2.into()), Rc::new(n1.into()))
    }

    pub fn le(n1: impl Into<NExpr>, n2: impl Into<NExpr>) -> BExpr {
        BExpr::not(BExpr::gt(n1, n2))
    }

    pub fn ge(n1: impl Into<NExpr>, n2: impl Into<NExpr>) -> BExpr {
        BExpr::not(BExpr::lt(n1, n2))
    }

    pub fn not(b: impl Into<BExpr>) -> BExpr {
        BExpr::Not(Rc::new(b.into()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NExpr {
    Add(Rc<NExpr>, Rc<NExpr>),
    Sub(Rc<NExpr>, Rc<NExpr>),
    Mul(Rc<NExpr>, Rc<NExpr>),
    Div(Rc<NExpr>, Rc<NExpr>),
    Mod(Rc<NExpr>, Rc<NExpr>),
    Load(Rc<NExpr>),
    Var(Var),
    OfBool(Rc<BExpr>),
    Lit(i32),
}

impl NExpr {
    pub fn add(n1: impl Into<NExpr>, n2: impl Into<NExpr>) -> NExpr {
        NExpr::Add(Rc::new(n1.into()), Rc::new(n2.into()))
    }

    pub fn sub(n1: impl Into<NExpr>, n2: impl Into<NExpr>) -> NExpr {
        NExpr::Sub(Rc::new(n1.into()), Rc::new(n2.into()))
    }

    pub fn load(addr: impl Into<NExpr>) -> NExpr {
        NExpr::Load(Rc::new(addr.into()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Inst {
    Declare(Var),
    Assign(Var, NExpr),
    Store(NExpr, NExpr),
    Assume(BExpr),
    Assert(BExpr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    If(BExpr, Rc<Stmt>, Option<Rc<Stmt>>),
    While(BExpr, Rc<Stmt>),
    Seq(Vec<Stmt>),
    Inst(Inst),
}

impl Stmt {
    pub fn if_(b: impl Into<BExpr>, t: impl Into<Stmt>, f: impl Into<Option<Stmt>>) -> Stmt {
        Stmt::If(b.into(), Rc::new(t.into()), f.into().map(Rc::new))
    }

    pub fn while_(b: impl Into<BExpr>, s: impl Into<Stmt>) -> Stmt {
        Stmt::While(b.into(), Rc::new(s.into()))
    }

    pub fn declare(v: Var) -> Stmt {
        Stmt::Inst(Inst::Declare(v))
    }

    pub fn assign(v: Var, x: impl Into<NExpr>) -> Stmt {
        Stmt::Inst(Inst::Assign(v, x.into()))
    }

    pub fn store(addr: impl Into<NExpr>, n: impl Into<NExpr>) -> Stmt {
        Stmt::Inst(Inst::Store(addr.into(), n.into()))
    }

    pub fn assume(p: impl Into<BExpr>) -> Stmt {
        Stmt::Inst(Inst::Assume(p.into()))
    }

    pub fn assert(p: impl Into<BExpr>) -> Stmt {
        Stmt::Inst(Inst::Assert(p.into()))
    }
}

macro_rules! intos {
    ($($From:ty => $To:ty = $cvt:expr;)*) => {$(
        impl Into<$To> for $From {
            fn into(self) -> $To { $cvt(self) }
        }
    )*};
}

intos! {
    bool => BExpr = |b| BExpr::Lit(b);
    NExpr => BExpr = |n| BExpr::OfNum(Rc::new(n));
    i32 => NExpr = |i| NExpr::Lit(i);
    Var => NExpr = |v| NExpr::Var(v);
    BExpr => NExpr = |b| NExpr::OfBool(Rc::new(b));
    Option<Stmt> => Stmt = |s: Option<Stmt>| s.unwrap_or_else(|| Stmt::Seq(vec![]));
    Vec<Stmt> => Stmt = |v| Stmt::Seq(v);
}

impl fmt::Display for Var {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "x{}", self.0)
    }
}

impl fmt::Display for BExpr {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BExpr::Eq(l, r) => write!(fmt, "{} = {}", l, r),
            BExpr::Lt(l, r) => write!(fmt, "{} < {}", l, r),
            BExpr::And(l, r) => write!(fmt, "({} and {})", l, r),
            BExpr::Or(l, r) => write!(fmt, "({} or {})", l, r),
            BExpr::Not(b) => write!(fmt, "not({})", b),
            BExpr::OfNum(n) => write!(fmt, "bool({})", n),
            BExpr::Lit(b) => write!(fmt, "{}", b),
        }
    }
}

impl fmt::Display for NExpr {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NExpr::Add(l, r) => write!(fmt, "({} + {})", l, r),
            NExpr::Sub(l, r) => write!(fmt, "({} - {})", l, r),
            NExpr::Mul(l, r) => write!(fmt, "({} * {})", l, r),
            NExpr::Div(l, r) => write!(fmt, "({} / {})", l, r),
            NExpr::Mod(l, r) => write!(fmt, "({} % {})", l, r),
            NExpr::Load(addr) => write!(fmt, "[{}]", addr),
            NExpr::Var(v) => write!(fmt, "{}", v),
            NExpr::OfBool(b) => write!(fmt, "int({})", b),
            NExpr::Lit(n) => write!(fmt, "{}", n),
        }
    }
}

impl fmt::Display for Inst {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Inst::Declare(v) => write!(fmt, "var {}", v),
            Inst::Assign(v, n) => write!(fmt, "{} := {}", v, n),
            Inst::Store(addr, n) => write!(fmt, "[{}] := {}", addr, n),
            Inst::Assume(p) => write!(fmt, "assume {}", p),
            Inst::Assert(p) => write!(fmt, "assert {}", p),
        }
    }
}

impl fmt::Display for Stmt {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Stmt::If(b, t, f) => {
                let t = format!("  {}", t).replace("\n", "\n  ");
                write!(fmt, "if {} then\n{}", b, t)?;
                if let Some(f) = f {
                    let f = format!("  {}", f).replace("\n", "\n  ");
                    write!(fmt, "\nelse\n{}", f)?;
                }
                write!(fmt, "\nend")
            }
            Stmt::While(b, s) => {
                let s = format!("  {}", s).replace("\n", "\n  ");
                write!(fmt, "while {} do\n{}\nend", b, s)
            }
            Stmt::Seq(s) => match &s[..] {
                &[] => write!(fmt, "skip"),
                &[ref s, ref rest..] => {
                    write!(fmt, "{}", s)?;
                    for f in rest {
                        write!(fmt, " ;\n{}", f)?;
                    }
                    Ok(())
                }
            },
            Stmt::Inst(inst) => write!(fmt, "{}", inst),
        }
    }
}
