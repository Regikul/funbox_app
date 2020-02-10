#[macro_use]
extern crate rustler;
extern crate primes;

use rustler::{Env, Term, Error, Encoder};

rustler_export_nifs!(
    "primes",
    [("check", 1, check),
    ],
    Some(on_load)
);

fn on_load(_env: Env, _info: Term) -> bool {
    true
}

fn check<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let number:u64 = args[0].decode()?;
    let is_prime = primes::is_prime(number);
    Ok(is_prime.encode(env))
}
