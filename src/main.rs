use std::collections::VecDeque;

use bevy::{
    prelude::{
        info, shape, App, Assets, Camera2dBundle, Color, Commands, Component, DefaultPlugins,
        Input, KeyCode, Mesh, ParamSet, Query, Res, ResMut, Resource, Startup, Transform, Update,
        Vec3, With,
    },
    sprite::{ColorMaterial, MaterialMesh2dBundle},
    time::{Time, Timer, TimerMode},
};

const PLAYER_ACCELERATION: f32 = 30.0;
const PLAYER_RADIUS: f32 = 50.0;
const BULLET_RADIUS: f32 = 10.0;

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
        .add_systems(Startup, setup)
        .add_systems(Update, quit_system)
        .add_systems(Update, collision_system)
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

fn setup(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<ColorMaterial>>,
) {
    commands.spawn(Camera2dBundle::default());
    commands.spawn((
        Player,
        Acceleration(Vec3::ZERO),
        Velocity(Vec3::ZERO),
        PosnTrace(VecDeque::from([Vec3::ZERO])),
        MaterialMesh2dBundle {
            mesh: meshes.add(shape::Circle::default().into()).into(),
            material: materials.add(ColorMaterial::from(Color::rgb(0.0, 0.0, 1.0))),
            transform: Transform {
                translation: Vec3::ZERO,
                scale: Vec3::new(2.0 * PLAYER_RADIUS, 2.0 * PLAYER_RADIUS, 0.0),
                ..Default::default()
            },
            ..Default::default()
        },
    ));
    commands.spawn((
        Bullet,
        MaterialMesh2dBundle {
            mesh: meshes.add(shape::Circle::default().into()).into(),
            material: materials.add(ColorMaterial::from(Color::rgb(1.0, 0.0, 0.0))),
            transform: Transform {
                translation: Vec3::new(200.0, 0.0, 0.0),
                scale: Vec3::new(2.0 * BULLET_RADIUS, 2.0 * BULLET_RADIUS, 0.0),
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

#[derive(Component)]
struct Bullet;

fn collision_system(
    player_query: Query<&Transform, With<Player>>,
    lethal_query: Query<&Transform, With<Bullet>>,
) {
    for player_posn in player_query.iter() {
        for bullet_posn in lethal_query.iter() {
            if (player_posn.translation - bullet_posn.translation).length()
                < PLAYER_RADIUS + BULLET_RADIUS
            {
                info!("Collision!");
            }
        }
    }
}

mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
