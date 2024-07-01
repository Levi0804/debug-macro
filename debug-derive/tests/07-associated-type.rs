use debug_derive::CustomDebug;
use std::fmt::Debug;

pub trait Trait {
    type Value;
}

#[derive(CustomDebug)]
pub struct Field<T: Trait> {
    values: Vec<T::Value>,
}

fn assert_debug<F: Debug>() {}

fn main() {
    struct Id;

    impl Trait for Id {
        type Value = u8;
    }

    assert_debug::<Field<Id>>();
}
