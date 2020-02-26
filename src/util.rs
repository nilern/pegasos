#[cfg(target_pointer_width = "32")]
pub type fsize = f32;

#[cfg(target_pointer_width = "64")]
pub type fsize = f64;

