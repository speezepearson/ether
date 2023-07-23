use bevy::{math::DVec3, prelude::Component};

pub type Timestamp = f64;

#[derive(Component, Clone, Copy, PartialEq, Debug)]
pub struct XV {
    pub x: DVec3,
    pub v: DVec3,
}

pub fn extrapolate_xv(t0: &Timestamp, xv: &XV, t1: &Timestamp) -> XV {
    let dt = t1 - t0;
    let x = xv.x + xv.v * dt;
    let v = xv.v;
    XV { x, v }
}

pub fn dangerously_close(x: f64, y: f64) -> bool {
    let kiloeps = 1000.0 * f64::EPSILON;
    x * (1.0 - kiloeps) < y && y < x * (1.0 + kiloeps)
}

pub const SPEED_OF_LIGHT: f64 = 100.0;
