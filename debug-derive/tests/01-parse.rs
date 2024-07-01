use debug_derive::CustomDebug;

#[derive(CustomDebug)]
pub struct Field {
    name: &'static str,
    bitmask: u16,
}

fn main() {}
