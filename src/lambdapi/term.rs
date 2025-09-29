use std::{borrow::Borrow, path::Path, rc::Rc};

use crate::lambdapi::*;
#[cfg(test)]
use crate::parse::{iterm, statement, statements};

/// Abbreviated constructor for free variable Values.
pub fn vfree(name: Name) -> Value {
    Value::Neutral(Neutral::Free(name))
}

/// Applies a lambda value to an argument value.
/// Panics if `lam` isn't actually a possible lambda.
pub fn vapp(lam: Value, arg: Value) -> Value {
    match lam {
        Value::Lam(f) => (*f)(arg),
        Value::Neutral(n) => Value::Neutral(Neutral::App(Box::new(n), Box::new(arg))),
        _ => panic!()  // No other Values can be lambdas
    }
}

pub fn vpi(vty: impl Borrow<Type>, body: impl Fn(Value) -> Value + 'static) -> Value {
    Value::Pi(Box::new(vty.borrow().clone()), Rc::new(body))
}

pub fn vlam(body: impl Fn(Value) -> Value + 'static) -> Value {
    Value::Lam(Rc::new(body))
}

/// Converts anything that can be borrowed as an ITerm into a CTerm by cloning.
pub fn itoc<T: Borrow<ITerm>>(i: T) -> CTerm {
    CTerm::Inf(Box::new(i.borrow().clone()))
}

/// Abbreviated constructor for lambda-application ITerms.
pub fn iapp<T: Borrow<ITerm>, U: Into<CTerm>>(f: T, x: U) -> ITerm {
    ITerm::App(Box::new(f.borrow().clone()), x.into())
}

pub fn iapps(f: impl Borrow<ITerm>, args: impl IntoIterator<Item=impl Into<CTerm>>) -> ITerm {
    let mut result = f.borrow().clone();
    for arg in args {
        result = iapp(result, arg);
    }
    result
}

/// Abbreviated constructor for type-annotation ITerms.
pub fn iann<T: Into<CTerm>, U: Into<CTerm>>(term: T, ty: U) -> ITerm {
    ITerm::Ann(term.into(), ty.into())
}

/// Abbreviated constructor for pi-type ITerms.
pub fn ipi<T: Into<CTerm>, U: Into<CTerm>>(src: T, trg: U) -> ITerm {
    ITerm::Pi(src.into(), trg.into())
}

/// Abbreviated constructor for lambda CTerms.
pub fn clam<T: Into<CTerm>>(body: T) -> CTerm {
    CTerm::Lam(Box::new(body.into()))
}

/// Abbreviation for bound-variable ITerm.
pub fn bnd(n: usize) -> ITerm {
    ITerm::Bound(n)
}

/// Converts usize to ITerm natural numbers.
pub fn inat(n: usize) -> ITerm {
    if n == 0 {
        ITerm::Zero
    }
    else {
        ITerm::Succ(inat(n-1).into())
    }
}

impl ITerm {
    pub fn subst_free(self, name: &Name, new: &ITerm) -> ITerm {
        match self {
            ITerm::Ann(b, t) => ITerm::Ann(b.subst_free(name, new), t.subst_free(name, new)),
            ITerm::Star => self,
            ITerm::Pi(src, trg) => ITerm::Pi(src.subst_free(name, new), trg.subst_free(name, new)),
            ITerm::Bound(j) => self,
            ITerm::Free(ref n) => if name == n { new.clone() } else { self },
            ITerm::App(f, x) => ITerm::App(Box::new(f.subst_free(name, new)), x.subst_free(name, new)),
            ITerm::Nat => self,
            ITerm::Zero => self,
            ITerm::Succ(cterm) => ITerm::Succ(cterm.subst_free(name, new)),
            ITerm::NatElim(motive, base, ind, k) => ITerm::NatElim(
                        motive.subst_free(name, new), base.subst_free(name, new), ind.subst_free(name, new), k.subst_free(name, new)
                    ),
            ITerm::Fin(cterm) => ITerm::Fin(cterm.subst_free(name, new)),
            ITerm::FinElim(motive, base, ind, n, f) => ITerm::FinElim(
                        motive.subst_free(name, new), base.subst_free(name, new), ind.subst_free(name, new), n.subst_free(name, new), f.subst_free(name, new)
                    ),
            ITerm::FZero(cterm) => ITerm::FZero(cterm.subst_free(name, new)),
            ITerm::FSucc(n, f) => ITerm::FSucc(n.subst_free(name, new), f.subst_free(name, new)),
            ITerm::Eq(a, x, y) => ITerm::Eq(a.subst_free(name, new), x.subst_free(name, new), y.subst_free(name, new)),
            ITerm::Refl(a, x) => ITerm::Refl(a.subst_free(name, new), x.subst_free(name, new)),
            ITerm::EqElim(a, m, mr, x, y, eq) => ITerm::EqElim(a.subst_free(name, new), m.subst_free(name, new), mr.subst_free(name, new), x.subst_free(name, new), y.subst_free(name, new), eq.subst_free(name, new)),
        }
    }

    /// Recursively substitutes in a new ITerm for a bound variable.
    pub fn subst(self, i: usize, new: &ITerm) -> ITerm {
        match self {
            ITerm::Ann(b, t) => ITerm::Ann(b.subst(i, new), t.subst(i, new)),
            ITerm::Star => self,
            ITerm::Pi(src, trg) => ITerm::Pi(src.subst(i, new), trg.subst(i + 1, new)),
            ITerm::Bound(j) => if i == j { new.clone() } else { self },
            ITerm::Free(_) => self,
            ITerm::App(f, x) => ITerm::App(Box::new(f.subst(i, new)), x.subst(i, new)),
            ITerm::Nat => self,
            ITerm::Zero => self,
            ITerm::Succ(cterm) => ITerm::Succ(cterm.subst(i, new)),
            ITerm::NatElim(motive, base, ind, k) => ITerm::NatElim(
                        motive.subst(i, new), base.subst(i, new), ind.subst(i, new), k.subst(i, new)
                    ),
            ITerm::Fin(cterm) => ITerm::Fin(cterm.subst(i, new)),
            ITerm::FinElim(motive, base, ind, n, f) => ITerm::FinElim(
                        motive.subst(i, new), base.subst(i, new), ind.subst(i, new), n.subst(i, new), f.subst(i, new)
                    ),
            ITerm::FZero(cterm) => ITerm::FZero(cterm.subst(i, new)),
            ITerm::FSucc(n, f) => ITerm::FSucc(n.subst(i, new), f.subst(i, new)),
            ITerm::Eq(a, x, y) => ITerm::Eq(a.subst(i, new), x.subst(i, new), y.subst(i, new)),
            ITerm::Refl(a, x) => ITerm::Refl(a.subst(i, new), x.subst(i, new)),
            ITerm::EqElim(a, m, mr, x, y, eq) => ITerm::EqElim(a.subst(i, new), m.subst(i, new), mr.subst(i, new), x.subst(i, new), y.subst(i, new), eq.subst(i, new)),
        }
    }
}


impl CTerm {
    pub fn subst_free(self, name: &Name, new: &ITerm) -> CTerm {
        match self {
            CTerm::Inf(it) => CTerm::Inf(Box::new(it.subst_free(name, new))),
            CTerm::Lam(body) => CTerm::Lam(Box::new(body.subst_free(name, new)))
        }
    }

    /// Recursively substitutes in a new ITerm for a bound variable.
    pub fn subst(self, i: usize, new: &ITerm) -> CTerm {
        match self {
            CTerm::Inf(it) => CTerm::Inf(Box::new(it.subst(i, new))),
            CTerm::Lam(body) => CTerm::Lam(Box::new(body.subst(i + 1, new)))
        }
    }
}

/// Converts a Value into a term, under no binders.
pub fn quote0(val: Value) -> CTerm {
    quote(0, val)
}

/// Converts a Value into a term, under `i` binders.
pub fn quote(i: usize, val: Value) -> CTerm {
    match val {
        Value::Lam(f) => CTerm::Lam(Box::new(quote(i + 1, f(vfree(Name::Quote(i)))))),
        Value::Neutral(n) => itoc(neutral_quote(i, n)),
        Value::Star => itoc(ITerm::Star),
        Value::Pi(src, trg) => itoc(ITerm::Pi(quote(i, *src), quote(i + 1, trg(vfree(Name::Quote(i)))))),
        Value::Nat => itoc(ITerm::Nat),
        Value::Zero => itoc(ITerm::Zero),
        Value::Succ(value) => itoc(ITerm::Succ(quote(i, *value))),
        Value::Fin(value) => itoc(ITerm::Fin(quote(i, *value))),
        Value::FZero(value) => itoc(ITerm::FZero(quote(i, *value))),
        Value::FSucc(n, f) => itoc(ITerm::FSucc(quote(i, *n), quote(i, *f))),
        Value::Eq(a, x, y) => itoc(ITerm::Eq(quote(i, *a), quote(i, *x), quote(i, *y))),
        Value::Refl(a, x) => itoc(ITerm::Refl(quote(i, *a), quote(i, *x))),
    }
}

/// Converts a neutral term (containing free variable) into an ITerm.
pub fn neutral_quote(i: usize, neutral: Neutral) -> ITerm {
    match neutral {
        Neutral::Free(x) => boundfree(i, x),
        Neutral::App(f, x) => ITerm::App(Box::new(neutral_quote(i, *f)), quote(i, *x)),
        Neutral::NatElim(motive, base, ind, k) => ITerm::NatElim(quote(i, *motive), quote(i, *base), quote(i, *ind), itoc(neutral_quote(i, *k))),
        Neutral::FinElim(motive, base, ind, n, f) => ITerm::FinElim(quote(i, *motive), quote(i, *base), quote(i, *ind), quote(i, *n), itoc(neutral_quote(i, *f))),
        Neutral::EqElim(ty, motive, base, x, y, eq) => ITerm::EqElim(quote(i, *ty), quote(i, *motive), quote(i, *base), quote(i, *x), quote(i, *y), itoc(neutral_quote(i, *eq)))
    }
}

/// Quotes a name into a bound or free variable term as appropriate.
pub fn boundfree(i: usize, name: Name) -> ITerm {
    match name {
        Name::Quote(k) => ITerm::Bound(i - k - 1),
        _ => ITerm::Free(name)
    }
}

fn ite(name: &str, term: ITerm) -> (&str, Type, Value) {
    (name, term.infer_type(Context::new(vec![])).unwrap(), term.eval(Context::new(vec![])))
}

pub fn std_env() -> Vec<(Name, Type, Option<Value>)> {
    use Value::*;
    let a = 0;
    let x = 0;
    let m = 0;
    let y = 0;
    let b = 0;
    let eq = 0;
    let predata = vec![
        ("Zero", Nat, Zero),
        ("Succ", vpi(Nat, |_| Nat), vlam(|n| Succ(Box::new(n)))),
        ("Nat", Star, Nat),
        (
            "natElim", 
            vpi(vpi(Nat, |_| Star), move |m| 
                vpi(vapp(m.clone(), Zero), move |_| {
                    let m = m.clone();
                    let m2 = m.clone();
                    vpi(
                        vpi(Nat, move |k| {
                            let m = m.clone();
                            vpi(vapp(m.clone(), k.clone()), move |_| {
                                vapp(m.clone(), Succ(Box::new(k.clone())))
                            })
                        }),
                        move |_| {
                            let m = m2.clone();
                            vpi(Nat, move |n| vapp(m.clone(), n))
                        }
                    )
                })
            ),
            clam(clam(clam(clam(ITerm::NatElim(ITerm::Bound(3).into(), ITerm::Bound(2).into(), ITerm::Bound(1).into(), ITerm::Bound(0).into()))))).eval(Context::new(vec![])),
            // vlam(|)
        ),
        ("FZero", vpi(Nat, |n| Fin(Box::new(Succ(Box::new(n))))), vlam(|n| FZero(Box::new(n)))),
        (
            "FSucc",
            vpi(Nat, |n| {
                let n = n.clone();
                vpi(Fin(Box::new(n.clone())), move |_| Fin(Box::new(Succ(Box::new(n.clone())))))
            }),
            vlam(|n| {let n = n.clone(); vlam(move |f| FSucc(Box::new(n.clone()), Box::new(f)))})
        ),
        ("Fin", vpi(Nat, |_| Star), vlam(|n| Fin(Box::new(n.clone())))),
        (
            "finElim", 
            ipi( // motive
                ipi(ITerm::Nat, ipi(ITerm::Fin(bnd(0).into()), ITerm::Star)), ipi( 
                // motive at f0
                ipi(ITerm::Nat, iapp(iapp(bnd(1), ITerm::Succ(bnd(0).into())), ITerm::FZero(bnd(0).into()))), ipi( 
                // motive at fsucc
                ipi(ITerm::Nat, ipi(ITerm::Fin(bnd(0).into()), ipi(iapp(iapp(bnd(3), bnd(1)), bnd(0)), iapp(iapp(bnd(4), ITerm::Succ(bnd(2).into())), ITerm::FSucc(bnd(2).into(), bnd(1).into()))))), ipi( 
                // number
                ITerm::Nat, ipi(
                // the fin
                ITerm::Fin(bnd(0).into()),
                iapp(iapp(bnd(4), bnd(1)), bnd(0))
            ))))).eval(Context::new(vec![])),
            // vpi(
            //     vpi(Nat, |n| vpi(Fin(Box::new(n)), |_| Star)), move |m| { let m1 = m.clone(); vpi(
            //     vpi(Nat, move |n| vapp(vapp(m.clone(), Succ(Box::new(n.clone()))), FZero(Box::new(n.clone())))), move |_| { let m = m1.clone(); vpi(
            //     vpi(
            //         Nat,
            //         move |n| {
            //             let m = m.clone();
            //             let n = n.clone();
            //             vpi(
            //                 Fin(Box::new(n.clone())),
            //                 move |f| { let m = m.clone(); let n = n.clone(); vpi(
            //                     vapp(vapp(m.clone(), n.clone()), f), 
            //                     move |_| vapp(m.clone(), Succ(Box::new(n.clone())))
            //                 ) }
            //             )
            //         }
            //     ), move |_| { let m = m.clone(); vpi(
            //     Nat, move |n| { let m = m.clone(); vpi(
            //     Fin(Box::new(n.clone())), move |f| { 
            //     vapp(vapp(m.clone(), n.clone()), f.clone()) 
            // })})})})}), 
            clam(clam(clam(clam(clam(ITerm::FinElim(
                ITerm::Bound(4).into(), ITerm::Bound(3).into(), ITerm::Bound(2).into(), ITerm::Bound(1).into(), ITerm::Bound(0).into()
            )))))).eval(Context::new(vec![]))
        ),
        ("False", Value::Star, Value::Fin(Box::new(Value::Zero))),
        ("Void", Value::Star, Value::Fin(Box::new(Value::Zero))),
        ("Not", vpi(Value::Star, |_| Value::Star), vlam(|x| vpi(x, |_| Value::Fin(Box::new(Value::Zero))))),
        ("Eq",
            vpi(Value::Star, |a| vpi(a.clone(), move |x| vpi(a.clone(), |y| Value::Star))), 
            vlam(|a| 
                vlam(move |x| {let a = a.clone(); 
                vlam(move |y| Value::Eq(Box::new(a.clone()), Box::new(x.clone()), Box::new(y))
            )}))
        ),
        ("Refl", 
            vpi(Value::Star, |a| vpi(a.clone(), move |x| Value::Eq(Box::new(a.clone()), Box::new(x.clone()), Box::new(x.clone())))),
            vlam(|a| vlam(move |x| Value::Refl(Box::new(a.clone()), Box::new(x.clone())))) 
        ),
        (
            "eqElim",
            ipi(
                ITerm::Star, ipi( // a
                ipi(bnd(0), ipi(bnd(1), ipi(ITerm::Eq(bnd(2).into(), bnd(1).into(), bnd(0).into()), ITerm::Star))), ipi( // m
                ipi(bnd(1), iapps(bnd(1), [bnd(0), bnd(0), ITerm::Refl(bnd(2).into(), bnd(0).into())])), ipi( // b
                bnd(2), ipi( // x
                bnd(3), ipi( // y
                ITerm::Eq(bnd(4).into(), bnd(1).into(), bnd(0).into()), // eq
                iapps(bnd(4), [bnd(2), bnd(1), bnd(0)])
            )))))).eval(Context::new(vec![])),
            // vpi(
            //     Value::Star, move |a| {let a1 = a.clone(); vpi(
            //     vpi(a1.clone(), move |x| {
            //         let a2 = a1.clone();
            //         let x1 = x.clone();
            //         vpi(a1.clone(), move |y| vpi(Value::Eq(Box::new(a2.clone()), Box::new(x1.clone()), Box::new(y)), |_| Value::Star))
            //     }), move |m| {let a1 = a.clone(); let m1 = m.clone(); vpi(
            //     vpi(a.clone(), move |x| vapp(vapp(vapp(m1.clone(), x.clone()), x.clone()), Value::Refl(Box::new(a1.clone()), Box::new(x.clone())))), move |b| {let a2 = a1.clone(); let b = b.clone(); vpi(
            //     a2.clone(), move |x| {let a3 = a2.clone(); let x1 = x.clone(); vpi(
            //     a2.clone(), move |y| {let m2 = m1.clone(); let x1 = x.clone(); let a1 = a.clone(); let y1 = y.clone(); vpi(
            //     Value::Eq(Box::new(a1.clone()), Box::new(x1.clone()), Box::new(y.clone())), move |eq|
            //     vapp(vapp(vapp(m2.clone(), x1.clone()), y1.clone()), eq)
            // )}
            // )}
            // )}
            // )}
            // )}
            // ),
            clam(clam(clam(clam(clam(clam(ITerm::EqElim(
                bnd(5).into(),
                bnd(4).into(),
                bnd(3).into(),
                bnd(2).into(),
                bnd(1).into(),
                bnd(0).into()
            ))))))).eval(Context::new(vec![]))
            // clam(todo!()).eval(Context::new(vec![]))
        ),
        // ("girard", Value::Fin(Box::new(Value::Zero)), Some(girard_reduced())),
    ];
    predata.into_iter().map(|(n, t, v)| (Name::Global(n.to_owned()), t, Some(v))).collect()
}

pub fn girard_env() -> Vec<(Name, Type, Option<Value>)> {
    let predata = [
        ite("P", sets_of()),
        ite("U", u()),
        ite("tau", tau()),
        ite("sigma", sigma()),
        ite("precedes", precedes()),
        ite("delta", delta()),
        ite("inductive", preomega()),
        ite("delta_ind", delta_inductive()),
        ite("omega", omega()),
        ite("omega_wf", lem0()),
        ite("D", d()),
        ite("lem2", lem2()),
        ite("lem3", lem3()),
    ];
    predata.into_iter().map(|(n, t, v)| (Name::Global(n.to_owned()), t, Some(v))).collect()
}

pub fn full_env() -> Vec<(Name, Type, Option<Value>)> {
    let mut env = std_env();
    env.append(&mut girard_env());
    env
}

#[test]
fn test_stdlib() {
    for (name, ty, val) in std_env() {
        println!("{name:?}");
        let quoted = quote0(val.unwrap());
        quoted.check_type(Context::new(vec![]), ty.clone()).unwrap();
        let iterm = match quoted {
            CTerm::Inf(iterm) => *iterm,
            CTerm::Lam(_) => iann(quoted, quote0(ty)),
        };
        // let annotated = ;
        validate(&format!("{name:?}"), &iterm, Some(true));
        // let ty = val.unwrap().infer_type().unwrap();
        // val.unwrap().
        // validate("t", val, None);
    }
}

#[cfg(test)]
fn test_parsetype(ctx: &mut Context, text: &str) {
    let (rest, stmt) = statement(vec![], text).unwrap();
    assert!(rest.len() == 0);
    match stmt {
        crate::parse::Statement::Let(name, iterm) => {
            let res = ctx.add_free_iterm(name, iterm.clone());
            if res.is_err() {
                panic!("Got error on term {iterm}: {res:?}");
            }
        },
        crate::parse::Statement::Assume(items) => {
            for (name, typ) in items {
                ctx.assume_cterm(name, typ);
            }
        },
        _ => todo!(),
        // crate::parse::Statement::Eval(iterm) => todo!(),
        // crate::parse::Statement::PutStrLn(_) => todo!(),
        // crate::parse::Statement::Out(_) => todo!(),
        // crate::parse::Statement::Set(tag, _) => todo!(),
        // crate::parse::Statement::Command(_) => todo!(),
    }
}

#[test]
fn test_forall() {
    let text = r"let    id = (\ a x -> x) :: forall (a :: *) . (a -> a)";
    let mut ctx = Context::new(std_env());
    test_parsetype(&mut ctx, text);
    ctx.find_free(&Name::Global("id".to_owned())).unwrap();
}

#[test]
fn test_voidelim() {
    let text = r"
        let voidElim =
        ( \ m -> finElim (natElim (\ n -> Fin n -> *)
                                    (\ x -> m x)
                                    (\ _ _ _ -> Fin 1))
                        (\ _ -> FZero 0)
                        (\ _ _ _ -> FZero 0)
                        0 )
        :: forall (m :: Fin 0 -> *) (v :: Fin 0) . m v";
    let mut ctx = Context::new(std_env());
    test_parsetype(&mut ctx, text);
    ctx.find_free(&Name::Global("voidElim".to_owned())).unwrap();
    
}

#[test]
fn test_leibniz() {
    let text = r"
    let leibniz =
    ( \ a b f -> eqElim a
                    (\ x y eq_x_y -> Eq b (f x) (f y))
                    (\ x -> Refl b (f x)) )
    :: forall (a :: *) (b :: *) (f :: a -> b) (x :: a) (y :: a) .
        Eq a x y -> Eq b (f x) (f y)
    ";
    let mut ctx = Context::new(std_env());
    test_parsetype(&mut ctx, text);
    ctx.find_free(&Name::Global("leibniz".to_owned())).unwrap();
}

#[test]
fn test_nat1elim() {
    let text = r"let nat1Elim =
        ( \ m m0 m1 ms -> natElim m m0
                                    (\ p rec -> natElim (\ n -> m (Succ n)) m1 ms p) )
        :: forall (m :: Nat -> *) . m 0 -> m 1 ->
            (forall n :: Nat . m (Succ n) -> m (Succ (Succ n))) ->
            forall (n :: Nat) . m n";
    let mut ctx = Context::new(std_env());
    test_parsetype(&mut ctx, text);
    ctx.find_free(&Name::Global("nat1Elim".to_owned())).unwrap();
}

#[test]
fn test_prelude() {
    let ctx = test_file("files/prelude.lp");
    let (typ, val) = ctx.find_free(&Name::Global("PowerfulUniverse".to_owned())).unwrap();
    match typ {
        Value::Star => println!("yay!"),
        _ => panic!("wrong variant!")
    }
}

#[test]
fn test_girard() {
    let ctx = test_file("files/girard.txt");
    // let (typ2, val3) = ctx.find_free(&Name::Global("lem2".to_owned())).unwrap();
    // let (typ3, val3) = ctx.find_free(&Name::Global("lem3".to_owned())).unwrap();
    let typ = iapp(
        ITerm::Free(Name::Global("lem2".to_owned())), 
        ITerm::Free(Name::Global("lem3".to_owned()))
    ).infer_type(ctx).unwrap();
    let typ = quote0(typ);
    if typ == CTerm::Inf(Box::new(ITerm::Fin(CTerm::Inf(Box::new(ITerm::Zero))))) {
        println!("yay!");
    }
    else {
        panic!("wrong variant! {}", typ)
    }
}

#[cfg(test)]
fn test_file(file: &str) -> Context {
    let file = std::fs::read(file).unwrap();
    let text: String = file.into_iter().map(|i| i as char).collect();
    let (rest, stmts) = statements(&text).expect("Should have parsed");
    println!("Remainder {rest}");
    assert!(rest.len() == 0);
    let mut ctx = Context::new(std_env());
    let mut handled = 0;
    for statement in stmts {
        println!("handling {handled}");
        match statement {
            crate::parse::Statement::Let(name, iterm) => {
                println!("{name:?}");
                let res = ctx.add_free_iterm(name, iterm.clone());
                if res.is_err() {
                    panic!("Got error on term {iterm}: {res:?}");
                }
            },
            crate::parse::Statement::Assume(items) => {
                for (name, typ) in items {
                    ctx.assume_cterm(name, typ);
                }
            },
            _ => todo!()
            // crate::parse::Statement::Eval(iterm) => todo!(),
            // crate::parse::Statement::PutStrLn(_) => todo!(),
            // crate::parse::Statement::Out(_) => todo!(),
            // crate::parse::Statement::Set(tag, _) => todo!(),
            // crate::parse::Statement::Command(_) => todo!(),
        };
        handled += 1;
        // println!("handled {handled}");
    }
    ctx
    // assert!(typ == Value::Star)
}