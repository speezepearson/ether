use std::{
    collections::VecDeque,
    time::{Duration, Instant},
};

use bevy::{
    prelude::{
        info, shape, App, Assets, BuildChildren, Bundle, Camera2dBundle, Color, Commands,
        Component, DefaultPlugins, Entity, Handle, Input, KeyCode, Mesh, Quat, Query, Res, ResMut,
        Resource, Startup, Transform, Update, Vec2, Vec3, With,
    },
    sprite::{ColorMaterial, MaterialMesh2dBundle, SpriteBundle},
    time::{Time, Timer, TimerMode},
};

const PLAYER_ACCELERATION: f32 = 200.0;
const PLAYER_RADIUS: f32 = 50.0;
const PLANET_RADIUS: f32 = 10.0;
const BULLET_RADIUS: f32 = 10.0;
const SPEED_OF_LIGHT: f32 = 100.0;
const WAGGLER_PERIOD_SEC: f32 = 3.0;
const WAGGLER_MAX_SPEED: f32 = SPEED_OF_LIGHT * 5.0;

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
        .add_systems(Update, planet_system)
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

#[derive(Component, Clone, Copy, PartialEq)]
struct Position {
    x: Vec3,
    v: Vec3,
    a: Vec3,
}

impl Position {
    const ZERO: Position = Position {
        x: Vec3::ZERO,
        v: Vec3::ZERO,
        a: Vec3::ZERO,
    };
}

#[derive(Component)]
struct PositionHistory(VecDeque<(Instant, Position)>);

#[derive(Component)]
struct Appearance(MaterialMesh2dBundle<ColorMaterial>);

fn setup(
    time: Res<Time>,
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<ColorMaterial>>,
) {
    let square_mesh = meshes.add(shape::Quad::new(Vec2::new(1.0, 1.0)).into());
    commands.insert_resource(SquareMesh(square_mesh.clone()));
    let circle_mesh = meshes.add(shape::Circle::default().into());
    commands.insert_resource(CircleMesh(circle_mesh.clone()));
    let velocity_dot_material = materials.add(ColorMaterial::from(Color::rgb(1.0, 0.5, 0.0)));
    commands.insert_resource(VelocityDotMaterial(velocity_dot_material));
    let acceleration_dot_material = materials.add(ColorMaterial::from(Color::rgb(1.0, 1.0, 1.0)));
    commands.insert_resource(AccelerationDotMaterial(acceleration_dot_material));

    commands.spawn(Camera2dBundle::default());

    let player_start_posn = Position {
        x: Vec3::X * -100.0,
        v: Vec3::ZERO,
        a: Vec3::ZERO,
    };
    commands.spawn((
        Player,
        player_start_posn,
        PositionHistory(VecDeque::from([(time.startup(), player_start_posn)])),
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

    let planet_start_posn = planet_posn(0.0);
    commands.spawn((
        Planet,
        Bullet,
        planet_start_posn,
        PositionHistory(VecDeque::from(
            (0..1000)
                .rev()
                .map(|step| {
                    let t = step as f32 / 100.0;
                    (time.startup() - Duration::from_secs_f32(t), planet_posn(-t))
                })
                .collect::<Vec<_>>(),
        )),
        Appearance(MaterialMesh2dBundle {
            mesh: meshes.add(shape::Circle::default().into()).into(),
            material: materials.add(ColorMaterial::from(Color::rgb(1.0, 0.0, 0.0))),
            transform: Transform {
                scale: Vec3::new(2.0 * PLANET_RADIUS, 2.0 * PLANET_RADIUS, 0.0),
                ..Default::default()
            },
            ..Default::default()
        }),
    ));
}

#[derive(Resource)]
struct CircleMesh(Handle<Mesh>);

#[derive(Resource)]
struct SquareMesh(Handle<Mesh>);

fn planet_posn(t: f32) -> Position {
    let w = 2.0 * 3.14159 / WAGGLER_PERIOD_SEC;
    // x = (vmax / w) sin(w t)
    // v = vmax cos(w t)
    // a = -w vmax sin(w t)
    Position {
        x: WAGGLER_MAX_SPEED / w * Vec3::new(t.cos(), t.sin(), 0.0),
        v: WAGGLER_MAX_SPEED * 1.0 * Vec3::new(-t.sin(), t.cos(), 0.0),
        a: WAGGLER_MAX_SPEED * w * Vec3::new(-t.cos(), -t.sin(), 0.0),
    }
}

fn planet_system(time: Res<Time>, mut planet_query: Query<&mut Position, With<Planet>>) {
    for mut planet in planet_query.iter_mut() {
        let x = planet_posn(time.elapsed().as_secs_f32());
        planet.x = x.x;
        planet.v = x.v;
        planet.a = x.a;
    }
}

#[derive(Component)]
struct Planet;

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
    mut posn_query: Query<&mut Position>,
) {
    let dt = time.delta();
    if !timer.0.tick(dt).just_finished() {
        return;
    }
    for mut posn in posn_query.iter_mut() {
        let dv = posn.a * dt.as_secs_f32();
        let dx = posn.v * dt.as_secs_f32() + 0.5 * posn.a * dt.as_secs_f32() * dt.as_secs_f32();
        posn.v += dv;
        posn.x += dx;
    }
}

/// This system prints 'A' key state
fn driving_system(
    time: Res<Time>,
    mut timer: ResMut<DrivingTimer>,
    keyboard_input: Res<Input<KeyCode>>,
    mut player_transform_query: Query<&mut Position, With<Player>>,
) {
    let dt = time.delta();
    if !timer.0.tick(dt).just_finished() {
        return;
    }
    for mut position in player_transform_query.iter_mut() {
        position.a = PLAYER_ACCELERATION
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
            if (player_posn.x - bullet_posn.x).length() < PLAYER_RADIUS + BULLET_RADIUS {
                // info!("Collision!");
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
            .push_back((time.last_update().unwrap_or(time.startup()), *position));
    }
}

#[derive(Component)]
struct Image;

#[derive(Resource)]
struct VelocityDotMaterial(Handle<ColorMaterial>);

#[derive(Resource)]
struct AccelerationDotMaterial(Handle<ColorMaterial>);

fn vision_system(
    player_position_query: Query<(&PositionHistory, &Appearance), With<Player>>,
    square_mesh: Res<SquareMesh>,
    velocity_dot_material: Res<VelocityDotMaterial>,
    acceleration_dot_material: Res<AccelerationDotMaterial>,
    mut object_query: Query<(&PositionHistory, &Appearance)>,
    mut image_entity_query: Query<Entity, With<Image>>,
    mut commands: Commands,
) {
    for entity in image_entity_query.iter_mut() {
        commands.entity(entity).despawn();
    }

    for (player_position_history, player_appearance) in player_position_query.iter() {
        let (now, player_position) = player_position_history.0.back().unwrap();
        let player_bundle = player_appearance.0.clone();
        commands.spawn((
            Image,
            MaterialMesh2dBundle {
                transform: Transform {
                    translation: player_position.x,
                    ..player_bundle.transform
                },
                ..player_bundle
            },
        ));
        for (position_history, appearance) in object_query.iter_mut() {
            // find when, if ever, the object was visible, i.e. when (its distance from player_position) = SPEED_OF_LIGHT*(time ago)
            for (i, ((t0, x0), (t1, x1))) in position_history
                .0
                .iter()
                .zip(position_history.0.iter().skip(1))
                .enumerate()
            {
                let (dx0, dx1) = (player_position.x - x0.x, player_position.x - x1.x);
                let (r0, r1) = (dx0.length(), dx1.length());

                let (dt0, dt1) = (now.duration_since(*t0), now.duration_since(*t1));
                let (ct0, ct1) = (
                    SPEED_OF_LIGHT * dt0.as_secs_f32(),
                    SPEED_OF_LIGHT * dt1.as_secs_f32(),
                );

                if (r0 < ct0 && r1 > ct1)
                    || (r0 > ct0 && r1 < ct1)
                    || (t1, x1) == (now, player_position)
                {
                    // println!(
                    //     "{} visible: {} {} {} {} {} {} {}",
                    //     i,
                    //     x0.x,
                    //     x1.x,
                    //     r0,
                    //     r1,
                    //     ct0,
                    //     ct1,
                    //     ct1 - ct0
                    // );
                    let object_bundle = appearance.0.clone();
                    commands
                        .spawn(SpriteBundle {
                            transform: Transform {
                                translation: x0.x,
                                ..Default::default()
                            },
                            ..Default::default()
                        })
                        .with_children(|parent| {
                            parent.spawn((
                                Image,
                                MaterialMesh2dBundle {
                                    transform: Transform {
                                        translation: Vec3::ZERO,
                                        ..object_bundle.transform
                                    },
                                    ..object_bundle.clone()
                                },
                            ));
                            if x0.v.length() > 1.0 {
                                let theta_v = x0.v.y.atan2(x0.v.x);
                                let xscale = 0.2 * x0.v.length();
                                parent.spawn((
                                    Image,
                                    MaterialMesh2dBundle {
                                        mesh: square_mesh.0.clone().into(),
                                        material: velocity_dot_material.0.clone(),
                                        transform: Transform {
                                            scale: Vec3::new(xscale, 1.0, 0.0),
                                            rotation: Quat::from_rotation_z(theta_v),
                                            translation: Vec3::new(
                                                xscale / 2.0 * theta_v.cos(),
                                                xscale / 2.0 * theta_v.sin(),
                                                0.0,
                                            ),
                                        },
                                        ..Default::default()
                                    },
                                ));
                            }
                            if x0.a.length() > 1.0 {
                                let theta_a = x0.a.y.atan2(x0.a.x);
                                let xscale = 0.2 * x0.a.length();
                                parent.spawn((
                                    Image,
                                    MaterialMesh2dBundle {
                                        mesh: square_mesh.0.clone().into(),
                                        material: acceleration_dot_material.0.clone(),
                                        transform: Transform {
                                            scale: Vec3::new(xscale, 1.0, 0.0),
                                            rotation: Quat::from_rotation_z(theta_a),
                                            translation: Vec3::new(
                                                xscale / 2.0 * theta_a.cos(),
                                                xscale / 2.0 * theta_a.sin(),
                                                0.0,
                                            ),
                                        },
                                        ..Default::default()
                                    },
                                ));
                            }
                        });
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
                    translation: position.x + Vec3::Z,
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
