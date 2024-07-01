use debug_derive::CustomDebug;
use std::fmt::Debug;
use std::marker::PhantomData;

type S = String;

#[derive(CustomDebug)]
pub struct Field<T> {
    marker: PhantomData<T>,
    string: S,
    #[debug = "0b{:08b}"]
    bitmask: u8,
}

fn assert_debug<F: Debug>() {}

fn main() {
    struct NotDebug;

    assert_debug::<PhantomData<NotDebug>>();
    assert_debug::<Field<NotDebug>>();
}
