use std::{collections::VecDeque, time::Instant};

use bevy::{
    prelude::{
        info, shape, App, Assets, Camera2dBundle, Color, Commands, Component, DefaultPlugins,
        Entity, Input, KeyCode, Mesh, ParamSet, Query, Res, ResMut, Resource, Startup, Transform,
        Update, Vec3, With,
    },
    sprite::{ColorMaterial, MaterialMesh2dBundle},
    time::{Time, Timer, TimerMode},
};

const PLAYER_ACCELERATION: f32 = 30.0;
const PLAYER_RADIUS: f32 = 50.0;
const BULLET_RADIUS: f32 = 10.0;
const SPEED_OF_LIGHT: f32 = 100.0;

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
        .insert_resource(TracingTimer(Timer::from_seconds(
            0.01,
            TimerMode::Repeating,
        )))
        .add_systems(Startup, setup)
        .add_systems(Update, quit_system)
        .add_systems(Update, tracing_system)
        .add_systems(Update, vision_system)
        .add_systems(Update, debug_vision_system)
        .add_systems(Update, collision_system)
        .add_systems(Update, waggler_system)
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
struct Position(Vec3);

#[derive(Component)]
struct Velocity(Vec3);

#[derive(Component)]
struct Acceleration(Vec3);

#[derive(Component)]
struct PositionHistory(VecDeque<(Instant, Vec3)>);

#[derive(Component)]
struct Appearance(MaterialMesh2dBundle<ColorMaterial>);

fn setup(
    time: Res<Time>,
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<ColorMaterial>>,
) {
    commands.spawn(Camera2dBundle::default());
    commands.spawn((
        Player,
        Acceleration(Vec3::ZERO),
        Velocity(Vec3::ZERO),
        Position(Vec3::ZERO),
        PositionHistory(VecDeque::from([(time.startup(), Vec3::ZERO)])),
        Appearance(MaterialMesh2dBundle {
            mesh: meshes.add(shape::Circle::default().into()).into(),
            material: materials.add(ColorMaterial::from(Color::rgb(0.0, 0.0, 1.0))),
            transform: Transform {
                translation: Vec3::ZERO,
                scale: Vec3::new(2.0 * PLAYER_RADIUS, 2.0 * PLAYER_RADIUS, 0.0),
                ..Default::default()
            },
            ..Default::default()
        }),
    ));

    let bullet_posn = Vec3::new(200.0, 0.0, 0.0);
    commands.spawn((
        Bullet,
        Acceleration(Vec3::ZERO),
        Velocity(Vec3::ZERO),
        Position(bullet_posn),
        PositionHistory(VecDeque::from([(time.startup(), bullet_posn)])),
        Appearance(MaterialMesh2dBundle {
            mesh: meshes.add(shape::Circle::default().into()).into(),
            material: materials.add(ColorMaterial::from(Color::rgb(1.0, 0.0, 0.0))),
            transform: Transform {
                scale: Vec3::new(2.0 * BULLET_RADIUS, 2.0 * BULLET_RADIUS, 0.0),
                ..Default::default()
            },
            ..Default::default()
        }),
    ));

    let waggler_posn = Vec3::new(0.0, 100.0, 0.0);
    commands.spawn((
        Waggler,
        Bullet,
        Position(waggler_posn),
        PositionHistory(VecDeque::from([(time.startup(), waggler_posn)])),
        Appearance(MaterialMesh2dBundle {
            mesh: meshes.add(shape::Circle::default().into()).into(),
            material: materials.add(ColorMaterial::from(Color::rgb(1.0, 0.0, 0.0))),
            transform: Transform {
                scale: Vec3::new(2.0 * BULLET_RADIUS, 2.0 * BULLET_RADIUS, 0.0),
                ..Default::default()
            },
            ..Default::default()
        }),
    ));
}

fn waggler_system(time: Res<Time>, mut waggler_query: Query<&mut Position, With<Waggler>>) {
    for mut waggler_posn in waggler_query.iter_mut() {
        let t = time.elapsed().as_secs_f32();
        waggler_posn.0 = Vec3::new(500.0 * t.sin(), 100.0, 0.0);
    }
}

#[derive(Component)]
struct Waggler;

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
        Query<(&mut Position, &Velocity)>,
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
        posn.0 += velocity.0 * dt.as_secs_f32();
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
    player_query: Query<&Position, With<Player>>,
    lethal_query: Query<&Position, With<Bullet>>,
) {
    for player_posn in player_query.iter() {
        for bullet_posn in lethal_query.iter() {
            if (player_posn.0 - bullet_posn.0).length() < PLAYER_RADIUS + BULLET_RADIUS {
                info!("Collision!");
            }
        }
    }
}

#[derive(Resource)]
struct TracingTimer(Timer);

fn tracing_system(
    time: Res<Time>,
    mut timer: ResMut<TracingTimer>,
    mut trace_query: Query<(&mut PositionHistory, &Position)>,
) {
    let dt = time.delta();
    if !timer.0.tick(dt).just_finished() {
        return;
    }
    for (mut position_history, position) in trace_query.iter_mut() {
        position_history
            .0
            .push_back((time.last_update().unwrap_or(time.startup()), position.0));
    }
}

#[derive(Component)]
struct Image;

fn vision_system(
    time: Res<Time>,
    player_position_query: Query<(&Position, &Appearance), With<Player>>,
    mut object_query: Query<(&PositionHistory, &Appearance)>,
    mut image_entity_query: Query<Entity, With<Image>>,
    mut commands: Commands,
) {
    for entity in image_entity_query.iter_mut() {
        commands.entity(entity).despawn();
    }

    let now = time.last_update().unwrap_or(time.startup());
    for (player_position, player_appearance) in player_position_query.iter() {
        let player_bundle = player_appearance.0.clone();
        commands.spawn((
            Image,
            MaterialMesh2dBundle {
                transform: Transform {
                    translation: player_position.0,
                    ..player_bundle.transform
                },
                ..player_bundle
            },
        ));
        for (position_history, appearance) in object_query.iter_mut() {
            // find when, if ever, the object was visible, i.e. when (its distance from player_position) = SPEED_OF_LIGHT*(time ago)
            for ((t0, x0), (t1, x1)) in position_history
                .0
                .iter()
                .zip(position_history.0.iter().skip(1))
            {
                let (dx0, dx1) = (player_position.0 - *x0, player_position.0 - *x1);
                let (r0, r1) = (dx0.length(), dx1.length());

                let (dt0, dt1) = (now.duration_since(*t0), now.duration_since(*t1));
                let (ct0, ct1) = (
                    SPEED_OF_LIGHT * dt0.as_secs_f32(),
                    SPEED_OF_LIGHT * dt1.as_secs_f32(),
                );

                // if it was closer and is now farther away (or vice versa), show it
                if (r0 < ct0 && r1 > ct1) || (r0 > ct0 && r1 < ct1) {
                    let object_bundle = appearance.0.clone();
                    commands.spawn((
                        Image,
                        MaterialMesh2dBundle {
                            transform: Transform {
                                translation: *x0,
                                ..object_bundle.transform
                            },
                            ..object_bundle
                        },
                    ));
                }
            }
        }
    }
}

#[derive(Component)]
struct DebugImage;

fn debug_vision_system(
    object_query: Query<(&Position, &Appearance)>,
    mut image_entity_query: Query<Entity, With<DebugImage>>,
    mut commands: Commands,
    mut materials: ResMut<Assets<ColorMaterial>>,
) {
    for entity in image_entity_query.iter_mut() {
        commands.entity(entity).despawn();
    }

    for (position, appearance) in object_query.iter() {
        let bundle = appearance.0.clone();
        commands.spawn((
            DebugImage,
            MaterialMesh2dBundle {
                material: materials.add(ColorMaterial::from(Color::rgba(0.5, 0.5, 0.5, 0.5))),
                transform: Transform {
                    translation: position.0,
                    ..bundle.transform
                },
                ..bundle
            },
        ));
    }
}

mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
