#[macro_use]
extern crate rustler;

use rustler::{Env, Term, Error, Encoder};
use rustler::resource::ResourceArc;

rustler_export_nifs!(
    "microtimer",
    [("send_after", 3, send_after),
    ("send_interval", 3, send_interval),
    ("cancel", 1, cancel),
    ],
    Some(on_load)
);

fn on_load(env: Env, _info: Term) -> bool {
    true
}

mod atoms {
    rustler_atoms! {
        atom ok;
        atom error;
        atom nif_error;
    }
}

fn send_after<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    Ok(atoms::ok().encode(env))
}

fn send_interval<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    Ok(atoms::ok().encode(env))
}

fn cancel<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    Ok(atoms::ok().encode(env))
}
