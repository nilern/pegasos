#[allow(non_camel_case_types)]
#[cfg(target_pointer_width = "32")]
pub type fsize = f32;

#[allow(non_camel_case_types)]
#[cfg(target_pointer_width = "64")]
pub type fsize = f64;

// ---

pub trait Bool {
    fn reify() -> bool;
}

pub enum True {}

impl Bool for True {
    fn reify() -> bool { true }
}

pub enum False {}

impl Bool for False {
    fn reify() -> bool { false }
}
