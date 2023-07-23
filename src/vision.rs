use std::collections::VecDeque;
use std::f64::consts::PI;

use bevy::math::DVec3;

use crate::core::{dangerously_close, extrapolate_xv, Timestamp, SPEED_OF_LIGHT, XV};

fn visible_times_ballistic(
    viewpoint: DVec3,
    view_time: Timestamp,
    (t0, xv0): (Timestamp, XV),
) -> Vec<Timestamp> {
    let (x0, v) = (xv0.x - viewpoint, xv0.v);
    let c = SPEED_OF_LIGHT;
    let t_recv = view_time - t0;

    let qa = c.powi(2) - v.length_squared();
    let qb = -2.0 * (c.powi(2) * t_recv + x0.dot(v));
    let qc = (c * t_recv).powi(2) - x0.length_squared();
    let t_emits = {
        if dangerously_close(c * t_recv, x0.length()) {
            // covers when qc is wrong because of floats
            // println!("object is visible at x0: {} ~= {}", x0.length(), c * t_recv);
            vec![0.0]
        } else if dangerously_close(c, v.length()) {
            // covers when qa is wrong because of floats
            // println!("basically going speed of light");
            if v.angle_between(x0).abs() < 0.00001 {
                // println!("going speed of light away");
                // emits photon at t_emit, at dist r0 + c*t_emit; received at t_recv
                //   t_emit + (r0 + c*t_emit)/c = t_recv
                //   t_emit = (t_recv - r0/c)/2
                vec![(t_recv - x0.length() / c) / 2.0]
            } else if dangerously_close(v.angle_between(x0), PI) {
                // println!("going speed of light towards receiver");
                vec![x0.length() / c]
            } else {
                // println!("going speed of light in some non-special direction");
                vec![-qc / qb] // TODO: not well thought out
            }
        } else {
            // qb might be wrong because of floats, but probably nbd because b/a is small? maybe?
            // println!("default case");
            if dangerously_close(qb.powi(2), 4.0 * qa * qc) {
                vec![-qb / (2.0 * qa)]
            } else if qb.powi(2) < 4.0 * qa * qc {
                vec![]
            } else {
                let sqrt = f64::sqrt(qb.powi(2) - 4.0 * qa * qc);
                [1, -1]
                    .into_iter()
                    .map(|sign| (-qb + sign as f64 * sqrt) / (2.0 * qa))
                    .filter(|x| x.is_finite())
                    .collect()
            }
        }
    };

    t_emits.into_iter()
        .filter(|t_emit| t_emit < &t_recv)
        .map(|t_emit| {
            let x_visible = x0 + v * t_emit;
            // println!("err: {}", (x_visible.length() - c * (t_recv - t_emit)).abs());
            if (x_visible.length() - c * (t_recv - t_emit)).abs() > c * 1.0 {
                println!(
                    "thought x{x0} v{v} emitted photons at ({t_emit}, {x_visible}) visible from origin at {t_recv}, but then x = {} where c dt = {}\n a = {qa}, b = {qb}, c = {qc}\n -b/2a = {}\n b^2 - 4ac = {}\n -c/b = {}",
                    x_visible.length(),
                    c * (t_recv - t_emit),
                    -qb/(2.0*qa),
                    qb.powi(2) - 4.0 * qa * qc,
                    -qa / qb,
                );
            }
            t0 + t_emit
        }).collect()
}

pub fn find_images(
    viewpoint: DVec3,
    view_time: Timestamp,
    trajectory: &VecDeque<(Timestamp, XV)>,
) -> Vec<(Timestamp, XV)> {
    trajectory
        .iter()
        .zip(
            trajectory.iter().skip(1).chain(
                trajectory
                    .back()
                    .map(|(t, xv)| (view_time, extrapolate_xv(t, xv, &view_time)))
                    .iter(),
            ),
        )
        .flat_map(|((t0, x0), (t1, _))| {
            let t_visibles = visible_times_ballistic(viewpoint, view_time, (*t0, *x0));
            t_visibles
                .into_iter()
                .filter(|t| t >= t0 && t <= t1)
                .map(|t| (t, extrapolate_xv(t0, x0, &t)))
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_approx_f64(a: f64, b: f64) {
        assert!(
            (a - b).abs() < 0.0000000000001,
            "assertion failed: {} !~= {}",
            a,
            b
        );
    }

    #[test]
    fn test_visible_times_ballistic_handles_departing_laser() {
        let actual = visible_times_ballistic(
            DVec3::ZERO,
            1.0,
            (
                0.0,
                XV {
                    x: DVec3::ZERO,
                    v: SPEED_OF_LIGHT * DVec3::X,
                },
            ),
        );
        assert_eq!(actual.len(), 1);
        assert_approx_f64(actual[0], 0.5);
    }

    #[test]
    fn test_visible_times_ballistic_does_not_show_incoming_laser() {
        let actual = visible_times_ballistic(
            DVec3::ZERO,
            1.0,
            (
                0.0,
                XV {
                    x: 2.0 * SPEED_OF_LIGHT * DVec3::X,
                    v: SPEED_OF_LIGHT * -DVec3::X,
                },
            ),
        );
        assert_eq!(actual.len(), 0);
    }

    #[test]
    fn test_visible_times_ballistic_does_not_show_distant_tangential_laser() {
        let actual = visible_times_ballistic(
            DVec3::ZERO,
            10.0,
            (
                0.0,
                XV {
                    x: 100.0 * SPEED_OF_LIGHT * -DVec3::X + DVec3::Y,
                    v: SPEED_OF_LIGHT * DVec3::X,
                },
            ),
        );
        assert_eq!(actual.len(), 0);
    }

    #[test]
    fn test_visible_times_ballistic_shows_single_point_for_random_subminal_object() {
        let actual = visible_times_ballistic(
            DVec3::ZERO,
            0.0,
            (
                0.0,
                XV {
                    x: DVec3::X + DVec3::Y,
                    v: (SPEED_OF_LIGHT / 2.0) * DVec3::X,
                },
            ),
        );
        assert_eq!(actual.len(), 1);
    }
}
