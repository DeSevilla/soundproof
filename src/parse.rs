use nom::branch::*;
use nom::character::complete::*;
use nom::bytes::complete::*;
use nom::combinator::*;
use nom::error::context;
use nom::error::ParseError;
use nom::multi::*;
use nom::sequence::*;
use nom::AsChar;
use nom::{IResult, Parser};
use crate::lambdapi::ast::*;
use crate::music::notes;

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Let(String, ITerm),
    Assume(Vec<(String, CTerm)>),
    Eval(ITerm),
    PutStrLn(String),
    Out(String),
    Set(Tag, [i32; 4]),
    Clear,
    Command(String),
}

pub fn statements(input: &str) -> IResult<&str, Vec<Statement>> {
    many0(|i| statement(vec![], i)).parse(input)
}

pub fn statement(vars: Vec<String>, input: &str) -> IResult<&str, Statement> {
    use Statement::*;
    let (input, _) = ws(input)?;
    alt((
        |i| {
            let (i, (_, n, _, v, _)) = (tag("let"), identifier, tag("="), |i| iterm(0, vars.clone(), i), ws).parse(i)?;
            Ok((i, Let(n, v)))
        },
        |i| { 
            let (i, _) = tag("assume").parse(i)?;
            let (i, terms) = many1(parens((identifier, tag("::"), |i| cterm(0, vec![], i)))).parse(i)?;
            let terms = terms.into_iter().map(|(n, _, v)| (n, v)).collect();
            Ok((i, Assume(terms)))
        },
        |i| {
            let (i, _) = tag("cmd").parse(i)?;
            let (i, command) = parens(take_while(|c| c != ')')).parse(i)?;
            Ok((i, Command(command.to_owned())))
        },
        |i| {
            let (i, _) = tag("set").parse(i)?;
            let (i, (tag, _, n1, n2, n3, n4)) = parens((node_tag, tag("="), note, note, note, note)).parse(i)?;
            Ok((i, Set(tag, [n1, n2, n3, n4])))
        },
        |i| {
            let (i, _) = tag("clear").parse(i)?;
            Ok((i, Clear))
        },
    )).parse(input)
}

fn node_tag(input: &str) -> IResult<&str, Tag> {
    alt((
        tag("Ann").map(|_| Tag::Ann),
        tag("Star").map(|_| Tag::Type),
        tag("Pi").map(|_| Tag::Pi),
        tag("App").map(|_| Tag::App),
        tag("Bound").map(|_| Tag::Bound),
        tag("Free").map(|_| Tag::Free),
        tag("Zero").map(|_| Tag::Zero),
        tag("Fin").map(|_| Tag::Finite),
        tag("Lam").map(|_| Tag::Lambda),
    )).parse(input)
}

fn note(input: &str) -> IResult<&str, i32> {
    let (i, l) = one_of("ABCDEFG").parse(input)?;
    let note = match l {
        'A' => notes::A,
        'B' => notes::B,
        'C' => notes::C,
        'D' => notes::D,
        'E' => notes::E,
        'F' => notes::F,
        'G' => notes::G,
        _ => unreachable!()
    };
    Ok((i, note))
}

fn parens<'a, O, E>(parser: impl Parser<&'a str, Output=O, Error=E>) -> impl Parser<&'a str, Output=O, Error=E> 
    where 
        E: ParseError<&'a str>
{
    delimited((ws, char('('), ws), parser, (ws, char(')'), ws))
}

fn ident_start(c: char) -> bool {
    c.is_alpha() || c == '_'
}

fn ident_rest(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '\''
}

pub fn reserved(name: &str) -> bool {
    matches!(name, "let" | "forall" | "assume" | "putStrLn" | "out")
}

pub fn identifier(input: &str) -> IResult<&str, String> {
    let (input, _) = ws(input)?;
    // println!("ident \x1b[32m{input}\x1b[0m");
    let (input, id_start) = take_while1(ident_start)(input)?;
    let (input, id_rest) = take_while(ident_rest)(input)?;
    let (input, _) = ws(input)?;
    let ident = id_start.to_owned() + id_rest;
    if reserved(&ident) {
        Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Fail)))
    }
    else {
        Ok((input, ident))
    }
}

pub fn ws<'a, E>(input: &'a str) -> IResult<&'a str, (), E>
    where
        E: ParseError<&'a str>
{
    // take_while1(char::is_whitespace)(input)
    // many0(one_of(" \t\r\n")).map(|v| v.into_iter().collect()).parse(input)
    (multispace0, many0((multispace0, tag("--"), not_line_ending, many0(line_ending)))).map(|_| ()).parse(input)
    // preceded(multispace0, (tag("--"), not_line_ending))
    // space0(input)
}

pub fn lambda(vars: Vec<String>, i: &str) -> IResult<&str, CTerm> {
    let (i, _) = ws(i)?;
    // println!("lambda: \x1b[32m{i}\x1b[0m");
    let (i, _) = tag("\\").parse(i)?;
    let (i, names) = many1(identifier).parse(i)?;
    // println!("lambda got identifiers: {names:?} \x1b[32m{i}\x1b[0m");
    let (i, _) = ws(i)?;
    let (i, _) = tag("->").parse(i)?;
    let (i, _) = ws(i)?;
    
    let mut new_vars: Vec<String> = names.clone().iter().map(|c| c.to_string()).collect();
    new_vars.reverse();
    let mut vars = vars;
    new_vars.append(&mut vars);
    // println!("lambda making body... \x1b[32m{i}\x1b[0m");
    let (i, mut body) = cterm(0, new_vars, i)?;
    for _ in names {
        body = CTerm::Lam(Box::new(body));
    }
    // println!("ok w lambda");
    Ok((i, body))
}

pub fn cterm(n: usize, vars: Vec<String>, input: &str) -> IResult<&str, CTerm> {
    let (input, _) = ws(input)?;
    // println!("cterm {n}: \x1b[32m{input}\x1b[0m");
    if n == 0 {
        // println!("cterm 1");
        context("cterm 0", alt((
            |i| lambda(vars.clone(), i), 
            map(|i| iterm(n, vars.clone(), i), |iterm| CTerm::Inf(Box::new(iterm))),
        ))).parse(input)
    }
    else {
        // println!("cterm 1");
        context("cterm 1", alt((
            parens(|i| lambda(vars.clone(), i)),
            map(|i| iterm(n, vars.clone(), i), |iterm| CTerm::Inf(Box::new(iterm))),
        ))).parse(input)
    }
}

pub fn iterm(n: usize, vars: Vec<String>, input: &str) -> IResult<&str, ITerm> {
    let (input, _) = ws(input)?;
    // println!("iterm {n}: \x1b[32m{input}\x1b[0m");
    if n == 0 {
        let rest = |i, t| {
            // println!("getting implication pi \x1b[32m{i}\x1b[0m");
            let (i, _) = ws(i)?;
            let (i, _) = tag("->").parse(i)?;
            let (i, _) = ws(i)?;
            let mut nvars = vars.clone();
            nvars.insert(0, "".to_owned());
            let (i, body) = cterm(0, nvars, i)?;
            let (i, _) = ws(i)?;
            // println!("ok w implication pi");
            Ok((i, ITerm::Pi(t, body)))
        };
        context("iterm 0", alt((
            |i| {
                // println!("iterm0 opt 1: forall: \x1b[32m{i}\x1b[0m");
                let (i, _) = tag("forall").parse(i)?;
                let (i, _) = ws(i)?;
                // println!("forall getting bindings");
                let (i, (names, values)) = bindings(true, vars.clone(), i)?;
                // println!("forall got bindings");
                let (i, _) = tag(".").parse(i)?;
                // println!("forall getting body");
                let (i, body) = cterm(0, names, i)?;
                // println!("forall got body {body:?} \x1b[32m{i}\x1b[0m");
                let (i, _) = ws(i)?;
                let (head, tail) = values.split_at(1);
                let head = head[0].clone();
                let mut result = ITerm::Pi(head, body);
                for val in tail {
                    result = ITerm::Pi(val.clone(), CTerm::Inf(Box::new(result)))
                }
                // println!("ok w forall {result:?}");
                Ok((i, result))
            },
            |i| {
                // println!("iterm0 opt 2: next level (mb implication): \x1b[32m{i}\x1b[0m");
                let (i, iterm) = iterm(1, vars.clone(), i)?;
                // println!("got higher iterm: {iterm:?} || \x1b[32m{i}\x1b[0m");
                alt((
                    |i| rest(i, CTerm::Inf(Box::new(iterm.clone()))),
                    |i| Ok((i, iterm.clone())), 
                )).parse(i)
            },
            |i| {
                // println!("iterm0 opt 3: parens-lambda || \x1b[32m{i}\x1b[0m");
                let (i, t) = parens(|i| lambda(vars.clone(), i)).parse(i)?;
                rest(i, t)
            }
        ))).parse(input)
    }
    else if n == 1 {
        let rest = |i, t| {
            // println!("can we annotate?");
            let (i, _) = ws(i)?;
            let (i, _) = tag("::").parse(i)?;
            let (i, _) = ws(i)?;
            let (i, ty) = cterm(0, vars.clone(), i)?;
            let (i, _) = ws(i)?;
            // println!("ok w ann");
            Ok((i, ITerm::Ann(t, ty)))
        };
        context("iterm 1", alt((
            |i| {
                // println!("iterm1 opt 1: maybe-annotate || \x1b[32m{i}\x1b[0m");
                let (i, term) = iterm(2, vars.clone(), i)?;
                // println!("got a term: {term:?} || \x1b[32m{i}\x1b[0m");
                alt((
                    |i| rest(i, CTerm::Inf(Box::new(term.clone()))),
                    |i| Ok((i, term.clone()))
                )).parse(i)
                // todo!()
            },
            |i| {
                // println!("iterm1 opt 2: annotated lambda || \x1b[32m{i}\x1b[0m");
                let (i, term) = parens(|i| lambda(vars.clone(), i)).parse(i)?;
                rest(i, term)
            }
        ))).parse(input)
    }
    else if n == 2 {
        let (i, func) = iterm(3, vars.clone(), input)?;
        // println!("got iterm... making args? || \x1b[32m{i}\x1b[0m");
        let (i, args) = context("iterm 2 many", many0(preceded(ws, |i| cterm(3, vars.clone(), i)))).parse(i)?;
        // println!("got args... {args:?} || \x1b[32m{i}\x1b[0m");
        let result = args.into_iter().fold(func, |acc, c| ITerm::App(Box::new(acc), c));
        // println!("ok w var / applications");
        Ok((i, result))
    }
    else if n == 3 {
        alt((
            map(tag("*"), |_| ITerm::Star),
            map(digit1, |n: &str| to_nat(n.parse::<usize>().unwrap())),
            parens(|i| iterm(0, vars.clone(), i)),
            |i| {
                // println!("iterm3 opt 4: checking for variable || \x1b[32m{i}\x1b[0m");
                let (i, name) = identifier(i)?;
                // println!("got identifier {name} \x1b[32m{i}\x1b[0m ");
                // println!("got identifier {name}");
                if let Some(n) = vars.iter().position(|n| *n == name) {
                    // println!("ok bound {n}");
                    Ok((i, ITerm::Bound(n)))
                }
                else {
                    // println!("ok free {name} in ctx {vars:?}");
                    Ok((i, ITerm::Free(Name::Global(name.to_string()))))
                }
            },
        )).parse(input)
    }
    else {
        panic!("Called iterm parser with too large a number: {n}")
    }
}

pub fn one_binding(use_env: bool, vars: Vec<String>, i: &str) -> IResult<&str, (String, CTerm)> {
    // println!("one binding: \x1b[32m{i}\x1b[0m");
    let (i, _) = ws(i)?;
    let (i, name) = identifier(i)?;
    // println!("binding got identifier: {name} || \x1b[32m{i}\x1b[0m");
    let (i, _) = ws(i)?;
    let (i, _) = tag("::").parse(i)?;
    let (i, _) = ws(i)?;
    // println!("binding getting cterm || \x1b[32m{i}\x1b[0m");
    let (i, term) = cterm(0, if use_env { vars } else { println!("going w/o vars"); vec![] }, i)?;
    // println!("binding got cterm {term:?} || \x1b[32m{i}\x1b[0m");
    let (i, _) = ws(i)?;
    // println!("ok one binding");
    Ok((i, (name.to_owned(), term)))
}

pub fn multiple_bindings(use_env: bool, vars: Vec<String>, input: &str) -> IResult<&str, Vec<(String, CTerm)>> {
    if use_env {
        let mut vars = vars;
        let mut bindings = Vec::new();
        let mut rest = input;
        loop {
            // println!("parsing binding with vars {vars:?}");
            let vars1 = vars.clone();
            if let Ok((i, (n, v))) = parens(move |i| one_binding(use_env, vars1.clone(), i)).parse(rest) {
                // println!("got binding {n}: {v}");
                vars.insert(0, n.clone());
                // bindings.insert(0, (n, v));
                bindings.push((n, v));
                rest = i;
            }
            else {
                break;
            }
        }
        if !bindings.is_empty() {
            Ok((rest, bindings))
        }
        else {
            Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Fail)))
        }
    }
    else {
        many1(parens(|i| one_binding(use_env, vars.clone(), i))).parse(input)
    }
}

pub fn bindings(use_env: bool, vars: Vec<String>, input: &str) -> IResult<&str, (Vec<String>, Vec<CTerm>)> {
    // println!("bindings: \x1b[32m{input}\x1b[0m");
    let (i, bindings) = alt((
        (|i| one_binding(use_env, vars.clone(), i)).map(|b| vec![b]),
        |i| multiple_bindings(use_env, vars.clone(), i),
    )).parse(input)?;
    // println!("got bindings: {bindings:?} || \x1b[32m{i}\x1b[0m");
    // this part is fiddly
    let (mut names, values): (Vec<String>, Vec<CTerm>) = bindings.into_iter().rev().unzip();
    let mut vars = vars;
    names.append(&mut vars);
    // println!("ok bindings {names:?}");
    Ok((i, (names, values)))
}


pub fn to_nat(n: usize) -> ITerm {
    let mut t = ITerm::Zero;
    for _ in 0..n {
        t = ITerm::Succ(CTerm::Inf(Box::new(t)));
    }
    t
}

#[cfg(test)]
fn test_iterm(text: &str) {
    let (rest, it) = iterm(0, vec![], text).expect("should have parsed");
    println!("ITerm: {it:?}");
    println!("Rest: {rest}");
    assert!(rest.len() == 0)
}

#[cfg(test)]
fn test_statements(text: &str) {
    let (rest, stmts) = many0(|i| statement(vec![], i)).parse(text).expect("Should have parsed");
    println!("statements: {stmts:?}");
    println!("Rest: {rest}");
    assert!(rest.len() == 0)
}

#[test]
fn test_iterm_forall() {
    test_iterm("forall (x :: Nat) . Nat");
}

#[test]
pub fn test_iterm_pred() {
    let text = r"natElim ( \ _ -> Nat ) Zero ( \ n' _rec -> n' )";
    test_iterm(text);
}

#[test]
pub fn test_iterm_fold() {
    let text = r"( \ m mz ms -> natElim   
    ( \ _ -> m )   mz ( \ n' rec -> ms rec ) ) :: forall (m :: *) . m -> (m -> m) -> Nat -> m";
    test_iterm(text);
}

#[test]
pub fn test_comments() {
    let text = r"( \ m mz ms -> natElim   -- hello world
    ( \ _ -> m )   mz ( \ n' rec -> ms rec ) ) :: forall (m :: *) . m -> (m -> m) -> Nat -> m";
    // let text = r"natElim  -- hello world 
    // ( \ _ -> m )   mz ( \ n' rec -> ms rec ) ) :: forall (m :: *) . m -> (m -> m) -> Nat -> m";
    test_iterm(text);
}

#[test]
pub fn test_iterm_replicate() {
    let text = r"( natElim 
    ( \ n -> 
    forall (a :: *) . a -> Vec a n ) 
        ( \ a _ -> Nil a ) 
        ( \ n' 
     rec_n' a x -> Cons a 
     n' x (rec_n' a x) ) ) 
     :: forall (n :: Nat) . forall (a :: *) . a -> Vec a n";
    test_iterm(text);
    // let (rest, it) = iterm(0, vec![], text).expect("should have parsed");
    // println!("ITerm: {it:?}");
    // println!("Rest: {rest}");
}

#[test]
pub fn test_more_newlines() {
    let text = r"( natElim
        ( \ n -> forall (a :: *) . a -> Vec a n )
         ( \ a _ -> Nil a )
        ( \ n' 
         rec_n' 
         a 
         x -> Cons a n' x (rec_n' a x) )
    ) :: forall (n :: Nat) . forall (a :: *) . a -> Vec a n";
    test_iterm(text); 
    // let (i, _) = space1(i)?;
}

// #[test]
// pub fn test_prelude() {
//     let file = std::fs::read("files/prelude.lp").unwrap();
//     let text: String = file.into_iter().map(|i| i as char).collect();
//     test_statements(&text);
// }

#[test]
pub fn test_a_fn() {
    let text = r"let app = (\x y z -> z y x) :: (* -> * -> (* -> * -> *) -> *) ";
    test_statements(text);
}