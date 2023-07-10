use std::collections::VecDeque;

use bevy::{
    prelude::{
        info, App, Camera2dBundle, Color, Commands, Component, DefaultPlugins, Input, KeyCode,
        ParamSet, Query, Res, ResMut, Resource, Startup, Transform, Update, Vec3, With,
    },
    sprite::{Sprite, SpriteBundle},
    time::{Time, Timer, TimerMode},
};

const PLAYER_ACCELERATION: f32 = 30.0;

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .insert_resource(DrivingTimer(Timer::from_seconds(
            0.01,
            TimerMode::Repeating,
        )))
        .insert_resource(PhysicsTimer(Timer::from_seconds(
            0.01,
            TimerMode::Repeating,
        )))
        .add_systems(Startup, add_player)
        .add_systems(Update, quit_system)
        .add_systems(Update, physics_system)
        .add_systems(Update, driving_system)
        .run();
}

#[derive(Resource)]
struct DrivingTimer(Timer);

#[derive(Resource)]
struct PhysicsTimer(Timer);

#[derive(Component)]
struct Player;

#[derive(Component)]
struct Velocity(Vec3);

#[derive(Component)]
struct Acceleration(Vec3);

#[derive(Component)]
struct PosnTrace(VecDeque<Vec3>);

fn add_player(mut commands: Commands) {
    commands.spawn(Camera2dBundle::default());
    commands.spawn((
        Player,
        Acceleration(Vec3::ZERO),
        Velocity(Vec3::ZERO),
        PosnTrace(VecDeque::from([Vec3::ZERO])),
        SpriteBundle {
            transform: Transform {
                translation: Vec3::ZERO,
                scale: Vec3::new(100.0, 100.0, 0.0),
                ..Default::default()
            },
            sprite: Sprite {
                color: Color::rgb(1.0, 0.0, 0.0),
                ..Default::default()
            },
            ..Default::default()
        },
    ));
}

fn is_quit_input(input: &Res<Input<KeyCode>>) -> bool {
    input.just_pressed(KeyCode::Escape)
        || ((input.pressed(KeyCode::SuperLeft)
            || input.pressed(KeyCode::SuperRight)
            || input.pressed(KeyCode::ControlLeft)
            || input.pressed(KeyCode::ControlRight))
            && (input.just_pressed(KeyCode::Q) || input.just_pressed(KeyCode::W)))
}

fn quit_system(keyboard_input: Res<Input<KeyCode>>) {
    if is_quit_input(&keyboard_input) {
        info!("Quitting");
        std::process::exit(0);
    }
}

fn physics_system(
    time: Res<Time>,
    mut timer: ResMut<PhysicsTimer>,
    mut q: ParamSet<(
        Query<(&mut Velocity, &Acceleration)>,
        Query<(&mut Transform, &Velocity)>,
    )>,
) {
    let dt = time.delta();
    if !timer.0.tick(dt).just_finished() {
        return;
    }
    for (mut velocity, acceleration) in q.p0().iter_mut() {
        velocity.0 += acceleration.0 * dt.as_secs_f32();
    }
    for (mut posn, velocity) in q.p1().iter_mut() {
        posn.translation += velocity.0 * dt.as_secs_f32();
    }
}

/// This system prints 'A' key state
fn driving_system(
    time: Res<Time>,
    mut timer: ResMut<DrivingTimer>,
    keyboard_input: Res<Input<KeyCode>>,
    mut player_transform_query: Query<&mut Acceleration, With<Player>>,
) {
    let dt = time.delta();
    if !timer.0.tick(dt).just_finished() {
        return;
    }
    for mut acceleration in player_transform_query.iter_mut() {
        acceleration.0 = PLAYER_ACCELERATION
            * Vec3::new(
                if keyboard_input.pressed(KeyCode::D) || keyboard_input.pressed(KeyCode::Right) {
                    1.0
                } else if keyboard_input.pressed(KeyCode::A)
                    || keyboard_input.pressed(KeyCode::Left)
                {
                    -1.0
                } else {
                    0.0
                },
                if keyboard_input.pressed(KeyCode::W) || keyboard_input.pressed(KeyCode::Up) {
                    1.0
                } else if keyboard_input.pressed(KeyCode::S)
                    || keyboard_input.pressed(KeyCode::Down)
                {
                    -1.0
                } else {
                    0.0
                },
                0.0,
            )
            .try_normalize()
            .unwrap_or(Vec3::ZERO);
    }
}

mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
