use std::{
    collections::VecDeque,
    time::{Duration, Instant},
};

use bevy::{
    input::mouse::{MouseButtonInput, MouseWheel},
    prelude::{
        info, shape, App, Assets, BuildChildren, Camera, Camera2d, Camera2dBundle, Color, Commands,
        Component, DefaultPlugins, Entity, EventReader, GlobalTransform, Handle, Input,
        IntoSystemConfigs, KeyCode, Mesh, MouseButton, Quat, Query, Res, ResMut, Resource, Startup,
        Transform, Update, Vec2, Vec3, With, Without,
    },
    sprite::{ColorMaterial, MaterialMesh2dBundle, SpriteBundle},
    time::{Time, Timer, TimerMode},
    window::Window,
};

const PLAYER_ACCELERATION: f32 = 200.0;
const PLAYER_RADIUS: f32 = 50.0;
const PLANET_RADIUS: f32 = 10.0;
const BULLET_RADIUS: f32 = 5.0;
const ROCKET_RADIUS: f32 = 5.0;
const SPEED_OF_LIGHT: f32 = 100.0;
const WAGGLER_PERIOD_SEC: f32 = 3.0;
const WAGGLER_MAX_SPEED: f32 = SPEED_OF_LIGHT * 5.0;

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .insert_resource(PlanetTimer(Timer::from_seconds(0.01, TimerMode::Repeating)))
        .add_systems(Startup, setup)
        .add_systems(Update, quit_system)
        .add_systems(Update, camera_follows_player_system)
        .add_systems(Update, vision_system)
        .add_systems(Update, debug_vision_system)
        .add_systems(Update, planet_system.before(physics_system))
        .add_systems(Update, controls_system.before(physics_system))
        .add_systems(Update, physics_system)
        .run();
}

#[derive(Resource)]
struct DrivingTimer(Timer);

#[derive(Resource)]
struct PhysicsTimer(Timer);

#[derive(Component)]
struct Player;

#[derive(Component, Clone, Copy, PartialEq, Debug)]
struct XVA {
    x: Vec3,
    v: Vec3,
    a: Vec3,
}

#[derive(Component)]
struct Trajectory(VecDeque<(Instant, XVA)>);

#[derive(Component)]
struct Physics {
    acceleration: Vec3,
}

#[derive(Component)]
struct Appearance(MaterialMesh2dBundle<ColorMaterial>);

#[derive(Component)]
struct PlayerCamera;

fn setup(
    time: Res<Time>,
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<ColorMaterial>>,
) {
    let square_mesh = meshes.add(shape::Quad::new(Vec2::new(1.0, 1.0)).into());
    commands.insert_resource(SquareMesh(square_mesh.clone()));
    let circle_mesh = CircleMesh(meshes.add(shape::Circle::default().into()));
    commands.insert_resource(circle_mesh.clone());
    let velocity_dot_material = materials.add(ColorMaterial::from(Color::rgb(1.0, 0.5, 0.0)));
    commands.insert_resource(VelocityDotMaterial(velocity_dot_material));
    let acceleration_dot_material = materials.add(ColorMaterial::from(Color::rgb(1.0, 1.0, 1.0)));
    commands.insert_resource(AccelerationDotMaterial(acceleration_dot_material));
    let bullet_material = materials.add(ColorMaterial::from(Color::rgb(1.0, 0.0, 1.0)));
    commands.insert_resource(BulletMaterial(bullet_material));
    let rocket_material = materials.add(ColorMaterial::from(Color::rgb(0.0, 1.0, 1.0)));
    commands.insert_resource(RocketMaterial(rocket_material));

    commands.spawn((Camera2dBundle::default(), PlayerCamera));

    commands.spawn((
        Player,
        Physics {
            acceleration: Vec3::ZERO,
        },
        Trajectory(VecDeque::from([(
            time.startup(),
            XVA {
                x: Vec3::X * -100.0,
                v: Vec3::ZERO,
                a: Vec3::ZERO,
            },
        )])),
        Appearance(MaterialMesh2dBundle {
            mesh: circle_mesh.0.clone().into(),
            material: materials.add(ColorMaterial::from(Color::rgb(0.0, 0.0, 1.0))),
            transform: Transform {
                translation: Vec3::ZERO,
                scale: Vec3::new(2.0 * PLAYER_RADIUS, 2.0 * PLAYER_RADIUS, 0.0),
                ..Default::default()
            },
            ..Default::default()
        }),
    ));

    commands.spawn((
        Planet,
        NoPhysics,
        Trajectory(VecDeque::from(
            (0..1000)
                .rev()
                .map(|step| {
                    let t = step as f32 / 100.0;
                    (time.startup() - Duration::from_secs_f32(t), planet_xva(-t))
                })
                .collect::<Vec<_>>(),
        )),
        Appearance(MaterialMesh2dBundle {
            mesh: circle_mesh.0.clone().into(),
            material: materials.add(ColorMaterial::from(Color::rgb(1.0, 0.0, 0.0))),
            transform: Transform {
                scale: Vec3::new(2.0 * PLANET_RADIUS, 2.0 * PLANET_RADIUS, 0.0),
                ..Default::default()
            },
            ..Default::default()
        }),
    ));
}

#[derive(Resource, Clone)]
struct CircleMesh(Handle<Mesh>);

#[derive(Resource, Clone)]
struct BulletMaterial(Handle<ColorMaterial>);

#[derive(Resource, Clone)]
struct RocketMaterial(Handle<ColorMaterial>);

#[derive(Resource)]
struct SquareMesh(Handle<Mesh>);

fn planet_xva(t: f32) -> XVA {
    let w = 2.0 * 3.14159 / WAGGLER_PERIOD_SEC;
    // x = (vmax / w) sin(w t)
    // v = vmax cos(w t)
    // a = -w vmax sin(w t)
    XVA {
        x: WAGGLER_MAX_SPEED / w * Vec3::new(t.cos(), t.sin(), 0.0),
        v: WAGGLER_MAX_SPEED * 1.0 * Vec3::new(-t.sin(), t.cos(), 0.0),
        a: WAGGLER_MAX_SPEED * w * Vec3::new(-t.cos(), -t.sin(), 0.0),
    }
}

#[derive(Resource)]
struct PlanetTimer(Timer);

fn planet_system(
    time: Res<Time>,
    mut timer: ResMut<PlanetTimer>,
    mut planet_query: Query<&mut Trajectory, With<Planet>>,
) {
    let dt = time.delta();
    if !timer.0.tick(dt).just_finished() {
        return;
    }
    for mut traj in planet_query.iter_mut() {
        let xva = planet_xva(time.elapsed().as_secs_f32());
        traj.0.push_back((time.startup() + time.elapsed(), xva));
    }
}

#[derive(Component)]
struct Planet;

#[derive(Component)]
struct NoPhysics;

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

fn camera_follows_player_system(
    mut camera_query: Query<&mut Transform, With<PlayerCamera>>,
    player_query: Query<&Trajectory, (With<Player>, Without<Camera2d>)>,
) {
    for mut camera_transform in camera_query.iter_mut() {
        let cx = &mut camera_transform.translation;
        for player_traj in player_query.iter() {
            let px = player_traj.0.back().unwrap().1.x;
            cx.x = px.x;
            cx.y = px.y;
        }
    }
}

fn get_world_cursor_posn(window: &Window, camera: (&Camera, &GlobalTransform)) -> Vec3 {
    let (camera, camera_transform) = camera;

    if let Some(world_position) = window
        .cursor_position()
        .and_then(|cursor| camera.viewport_to_world(camera_transform, cursor))
        .map(|ray| ray.origin.truncate())
    {
        return Vec3::new(world_position.x, world_position.y, 0.0);
    }
    panic!("No world coords");
}

#[derive(Component)]
struct Bullet;

#[derive(Component)]
struct Rocket;

/// This system prints 'A' key state
fn controls_system(
    time: Res<Time>,
    keyboard_input: Res<Input<KeyCode>>,
    mut scroll_evr: EventReader<MouseWheel>,
    mut click_evr: EventReader<MouseButtonInput>,

    mut player_physics_query: Query<&mut Physics, With<Player>>,
    player_traj_query: Query<&Trajectory, With<Player>>,

    windows_q: Query<&Window>,
    mut camera_transform_q: Query<&mut Transform, With<PlayerCamera>>,
    camera_q: Query<(&Camera, &GlobalTransform), With<PlayerCamera>>,

    mut commands: Commands,
    circle_mesh: Res<CircleMesh>,
    bullet_material: Res<BulletMaterial>,
    rocket_material: Res<RocketMaterial>,
) {
    for ev in scroll_evr.iter() {
        use bevy::input::mouse::MouseScrollUnit;
        let mut camera_transform = camera_transform_q.single_mut();
        let movement = match ev.unit {
            MouseScrollUnit::Line => 0.1,
            MouseScrollUnit::Pixel => 1.0,
        };
        let scale = camera_transform.scale;
        camera_transform.scale = Vec3::new(
            (scale.x.ln() + movement * 0.01 * ev.y).exp(),
            (scale.y.ln() + movement * 0.01 * ev.y).exp(),
            scale.z,
        );
    }
    for ev in click_evr.iter() {
        let click_posn = get_world_cursor_posn(windows_q.single(), camera_q.single());
        let player_xva = player_traj_query.single().0.back().unwrap().1;
        let click_dir = (click_posn - player_xva.x).normalize();

        if ev.button == MouseButton::Left && ev.state.is_pressed() {
            commands.spawn((
                Bullet,
                Physics {
                    acceleration: Vec3::ZERO,
                },
                Appearance(MaterialMesh2dBundle {
                    mesh: circle_mesh.0.clone().into(),
                    material: bullet_material.0.clone().into(),
                    transform: Transform {
                        scale: Vec3::new(2.0 * BULLET_RADIUS, 2.0 * BULLET_RADIUS, 0.0),
                        ..Default::default()
                    },
                    ..Default::default()
                }),
                Trajectory(VecDeque::from([(
                    time.last_update().unwrap_or(time.startup()),
                    XVA {
                        x: player_xva.x + click_dir * (PLAYER_RADIUS + BULLET_RADIUS + 10.0),
                        v: SPEED_OF_LIGHT * click_dir,
                        a: Vec3::ZERO,
                    },
                )])),
            ));
        } else if ev.button == MouseButton::Right && ev.state.is_pressed() {
            commands.spawn((
                Rocket,
                Physics {
                    acceleration: Vec3::ZERO,
                },
                Appearance(MaterialMesh2dBundle {
                    mesh: circle_mesh.0.clone().into(),
                    material: rocket_material.0.clone().into(),
                    transform: Transform {
                        scale: Vec3::new(2.0 * ROCKET_RADIUS, 2.0 * ROCKET_RADIUS, 0.0),
                        ..Default::default()
                    },
                    ..Default::default()
                }),
                Trajectory(VecDeque::from([(
                    time.last_update().unwrap_or(time.startup()),
                    XVA {
                        x: player_xva.x + click_dir * (PLAYER_RADIUS + ROCKET_RADIUS + 10.0),
                        v: player_xva.v + click_dir * 0.8 * SPEED_OF_LIGHT,
                        a: Vec3::ZERO,
                    },
                )])),
            ));
        }
    }
    for mut physics in player_physics_query.iter_mut() {
        physics.acceleration = PLAYER_ACCELERATION
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

fn physics_system(time: Res<Time>, mut trajectory_query: Query<(&Physics, &mut Trajectory)>) {
    let dt = time.delta_seconds();
    for (physics, mut traj) in trajectory_query.iter_mut() {
        let (_, last_xva) = traj.0.back().unwrap().to_owned();
        let dv = physics.acceleration * dt;
        let dx = last_xva.v * dt + 0.5 * physics.acceleration * dt * dt;
        let a = physics.acceleration;
        traj.0.push_back((
            time.startup() + time.elapsed(),
            XVA {
                x: last_xva.x + dx,
                v: last_xva.v + dv,
                a: a,
            },
        ));
    }
}

#[derive(Component)]
struct Image;

#[derive(Resource)]
struct VelocityDotMaterial(Handle<ColorMaterial>);

#[derive(Resource)]
struct AccelerationDotMaterial(Handle<ColorMaterial>);

fn vision_system(
    player_position_query: Query<(&Trajectory, &Appearance), With<Player>>,
    square_mesh: Res<SquareMesh>,
    velocity_dot_material: Res<VelocityDotMaterial>,
    acceleration_dot_material: Res<AccelerationDotMaterial>,
    mut object_query: Query<(&Trajectory, &Appearance)>,
    mut image_entity_query: Query<Entity, With<Image>>,
    mut commands: Commands,
    keyboard_input: Res<Input<KeyCode>>,
) {
    for entity in image_entity_query.iter_mut() {
        commands.entity(entity).despawn();
    }

    if keyboard_input.pressed(KeyCode::ShiftLeft) {
        println!("\n------------------");
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
        for (traj, appearance) in object_query.iter_mut() {
            if keyboard_input.pressed(KeyCode::ShiftLeft) {
                println!("");
            }
            // find when, if ever, the object was visible, i.e. when (its distance from player_position) = SPEED_OF_LIGHT*(time ago)
            for (i, ((t0, x0), (t1, x1))) in traj.0.iter().zip(traj.0.iter().skip(1)).enumerate() {
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
                    if keyboard_input.pressed(KeyCode::ShiftLeft) {
                        println!(
                            "step {}: {}-{} visible from {} after {}-{}",
                            i,
                            x0.x,
                            x1.x,
                            player_position.x,
                            (*now - *t1).as_secs_f32(),
                            (*now - *t0).as_secs_f32(),
                        );
                    }

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
    object_query: Query<(&Trajectory, &Appearance)>,
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
                    translation: position.0.back().unwrap().1.x + Vec3::Z,
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
