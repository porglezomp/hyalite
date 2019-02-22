use std::{
    fmt,
    io::Write,
    rc::Rc,
    sync::atomic::{AtomicU32, Ordering},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Var(u32);

static NEXT_FRESH_VAR: AtomicU32 = AtomicU32::new(0);

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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NExpr {
    Add(Rc<NExpr>, Rc<NExpr>),
    Sub(Rc<NExpr>, Rc<NExpr>),
    Mul(Rc<NExpr>, Rc<NExpr>),
    Div(Rc<NExpr>, Rc<NExpr>),
    Mod(Rc<NExpr>, Rc<NExpr>),
    Var(Var),
    OfBool(Rc<BExpr>),
    Lit(i32),
}

impl NExpr {
    pub fn add(n1: impl Into<NExpr>, n2: impl Into<NExpr>) -> NExpr {
        NExpr::Add(Rc::new(n1.into()), Rc::new(n2.into()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    If(BExpr, Rc<Stmt>, Option<Rc<Stmt>>),
    Seq(Vec<Stmt>),
    Declare(Var),
    Assign(Var, NExpr),
    Assume(BExpr),
    Assert(BExpr),
}

impl Stmt {
    pub fn if_(b: impl Into<BExpr>, t: impl Into<Stmt>, f: impl Into<Option<Stmt>>) -> Stmt {
        Stmt::If(b.into(), Rc::new(t.into()), f.into().map(Rc::new))
    }

    pub fn declare(v: Var) -> Stmt {
        Stmt::Declare(v)
    }

    pub fn assign(v: Var, x: impl Into<NExpr>) -> Stmt {
        Stmt::Assign(v, x.into())
    }

    pub fn assume(p: impl Into<BExpr>) -> Stmt {
        Stmt::Assume(p.into())
    }

    pub fn assert(p: impl Into<BExpr>) -> Stmt {
        Stmt::Assert(p.into())
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
            NExpr::Var(v) => write!(fmt, "{}", v),
            NExpr::OfBool(b) => write!(fmt, "int({})", b),
            NExpr::Lit(n) => write!(fmt, "{}", n),
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
            Stmt::Declare(v) => write!(fmt, "var {}", v),
            Stmt::Assign(v, n) => write!(fmt, "{} := {}", v, n),
            Stmt::Assume(p) => write!(fmt, "assume {}", p),
            Stmt::Assert(p) => write!(fmt, "assert {}", p),
        }
    }
}
