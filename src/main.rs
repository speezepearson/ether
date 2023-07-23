mod core;
mod vision;

use std::{collections::VecDeque, f64::consts::PI};

use bevy::{
    input::mouse::{MouseButtonInput, MouseWheel},
    math::DVec3,
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
use rand::Rng;
use rand::SeedableRng;
use rand_distr::{Distribution, Poisson};

use crate::core::{extrapolate_xv, Timestamp, SPEED_OF_LIGHT, XV};
use crate::vision::find_images;

const PLAYER_ACCELERATION: f64 = 200.0;
const PLAYER_RADIUS: f64 = 50.0;
const PLANET_RADIUS: f64 = 10.0;
const BULLET_RADIUS: f64 = 5.0;
const ROCKET_RADIUS: f64 = 5.0;
const WAGGLER_PERIOD_SEC: f64 = 3.0;
const WAGGLER_MAX_SPEED: f64 = SPEED_OF_LIGHT * 5.0;

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .insert_resource(PlanetTimer(Timer::from_seconds(0.01, TimerMode::Repeating)))
        .insert_resource(dust::DustTimer(Timer::from_seconds(
            0.1,
            TimerMode::Repeating,
        )))
        .add_systems(Startup, setup)
        .add_systems(Update, quit_system)
        .add_systems(Update, dust::dust_system)
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
pub struct Player;

#[derive(Component)]
pub struct Trajectory(VecDeque<(Timestamp, XV)>);

impl Trajectory {
    fn current_xv(&self, time: &Time) -> XV {
        let now = time.elapsed_seconds_f64();
        let (last_t, last_xv) = self.0.back().unwrap();
        extrapolate_xv(last_t, last_xv, &now)
    }
}

#[derive(Component)]
struct Physics {
    acceleration: DVec3,
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
    commands.insert_resource(SquareMesh(square_mesh));
    let circle_mesh = CircleMesh(meshes.add(shape::Circle::default().into()));
    commands.insert_resource(circle_mesh.clone());
    let velocity_dot_material = materials.add(ColorMaterial::from(Color::rgb(1.0, 0.5, 0.0)));
    commands.insert_resource(VelocityDotMaterial(velocity_dot_material));
    let bullet_material = materials.add(ColorMaterial::from(Color::rgb(1.0, 0.0, 1.0)));
    commands.insert_resource(BulletMaterial(bullet_material));
    let rocket_material = materials.add(ColorMaterial::from(Color::rgb(0.0, 1.0, 1.0)));
    commands.insert_resource(RocketMaterial(rocket_material));
    let dust_material = materials.add(ColorMaterial::from(Color::rgb(1.0, 1.0, 1.0)));
    commands.insert_resource(dust::DustMaterial(dust_material));

    commands.spawn((Camera2dBundle::default(), PlayerCamera));

    commands.spawn((
        Player,
        Physics {
            acceleration: DVec3::ZERO,
        },
        Trajectory(VecDeque::from([(
            time.elapsed_seconds_f64(),
            XV {
                x: DVec3::X * -100.0,
                v: DVec3::ZERO,
            },
        )])),
        Appearance(MaterialMesh2dBundle {
            mesh: circle_mesh.0.clone().into(),
            material: materials.add(ColorMaterial::from(Color::rgb(0.0, 0.0, 1.0))),
            transform: Transform {
                translation: Vec3::ZERO,
                scale: Vec3::new(2.0 * PLAYER_RADIUS as f32, 2.0 * PLAYER_RADIUS as f32, 0.0),
                ..Default::default()
            },
            ..Default::default()
        }),
    ));

    commands.spawn((
        Planet,
        NoPhysics,
        Trajectory(VecDeque::from([(
            time.elapsed_seconds_f64(),
            XV {
                x: planet_xv(0.0).x,
                v: DVec3::ZERO,
            },
        )])),
        Appearance(MaterialMesh2dBundle {
            mesh: circle_mesh.0.into(),
            material: materials.add(ColorMaterial::from(Color::rgb(1.0, 0.0, 0.0))),
            transform: Transform {
                scale: Vec3::new(2.0 * PLANET_RADIUS as f32, 2.0 * PLANET_RADIUS as f32, 0.0),
                ..Default::default()
            },
            ..Default::default()
        }),
    ));
}

#[derive(Resource, Clone)]
pub struct CircleMesh(Handle<Mesh>);

#[derive(Resource, Clone)]
struct BulletMaterial(Handle<ColorMaterial>);

#[derive(Resource, Clone)]
struct RocketMaterial(Handle<ColorMaterial>);

#[derive(Resource)]
struct SquareMesh(Handle<Mesh>);

fn planet_xv(t: f64) -> XV {
    let w = 2.0 * PI / WAGGLER_PERIOD_SEC;
    // x = (vmax / w) sin(w t)
    // v = vmax cos(w t)
    // a = -w vmax sin(w t)
    XV {
        x: WAGGLER_MAX_SPEED / w * DVec3::new(t.cos(), t.sin(), 0.0),
        v: WAGGLER_MAX_SPEED * 1.0 * DVec3::new(-t.sin(), t.cos(), 0.0),
    }
}

#[derive(Resource)]
struct PlanetTimer(Timer);

fn planet_system(
    time: Res<Time>,
    mut timer: ResMut<PlanetTimer>,
    mut planet_q: Query<&mut Trajectory, With<Planet>>,
) {
    let dt = time.delta();
    if !timer.0.tick(dt).just_finished() {
        return;
    }
    let mut traj = planet_q.single_mut();
    let xv = planet_xv(time.elapsed().as_secs_f64());
    traj.0.push_back((time.elapsed_seconds_f64(), xv));
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
    time: Res<Time>,
    mut camera_q: Query<&mut Transform, With<PlayerCamera>>,
    player_q: Query<&Trajectory, (With<Player>, Without<Camera2d>)>,
) {
    let mut camera_transform = camera_q.single_mut();
    let player_traj = player_q.single();
    let cx = &mut camera_transform.translation;
    let px = player_traj.current_xv(&time).x;
    cx.x = px.x as f32;
    cx.y = px.y as f32;
}

fn get_world_cursor_posn(window: &Window, camera: (&Camera, &GlobalTransform)) -> DVec3 {
    let (camera, camera_transform) = camera;

    if let Some(world_position) = window
        .cursor_position()
        .and_then(|cursor| camera.viewport_to_world(camera_transform, cursor))
        .map(|ray| ray.origin.truncate())
    {
        return DVec3::new(world_position.x as f64, world_position.y as f64, 0.0);
    }
    panic!("No world coords");
}

mod dust {
    use std::collections::HashSet;

    use bevy::prelude::{SpatialBundle, Vec3};

    use super::*;

    #[derive(Component)]
    pub struct DustSpeck(DVec3);

    #[derive(Component)]
    pub struct DustBlock(BlockId);

    #[derive(Resource)]
    pub struct DustMaterial(pub Handle<ColorMaterial>);

    #[derive(Resource)]
    pub struct DustTimer(pub Timer);

    #[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone, Debug)]
    struct BlockId(i32, i32);

    pub fn dust_system(
        time: Res<Time>,
        mut timer: ResMut<DustTimer>,
        mut commands: Commands,
        player_traj_q: Query<&Trajectory, With<Player>>,
        dust_q: Query<(Entity, &DustBlock)>,
        circle_mesh: Res<CircleMesh>,
        dust_material: Res<DustMaterial>,
    ) {
        let dt = time.delta();
        if !timer.0.tick(dt).just_finished() {
            return;
        }
        let player_posn = player_traj_q.single().current_xv(&time).x;

        let desired_blocks = blocks(
            player_posn.x - 1000.0,
            player_posn.x + 1000.0,
            player_posn.y - 1000.0,
            player_posn.y + 1000.0,
        );
        let actual_blocks = dust_q.iter().map(|(_, d)| d.0).collect::<HashSet<_>>();
        dust_q.iter().for_each(|(e, block)| {
            if !desired_blocks.contains(&block.0) {
                commands.entity(e).despawn();
            }
        });
        desired_blocks.difference(&actual_blocks).for_each(|block| {
            commands
                .spawn((
                    DustBlock(*block),
                    SpatialBundle {
                        transform: Transform {
                            translation: BLOCK_SIZE as f32
                                * Vec3::new(block.0 as f32, block.1 as f32, 0.0),
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                ))
                .with_children(|e| {
                    dust_in_block(-10.0, block).iter().for_each(|(x, y)| {
                        e.spawn(MaterialMesh2dBundle {
                            mesh: circle_mesh.0.clone().into(),
                            material: dust_material.0.clone(),
                            transform: Transform {
                                translation: BLOCK_SIZE as f32
                                    * Vec3::new(*x as f32, *y as f32, 0.5),
                                scale: Vec3::new(2.0, 2.0, 0.0),
                                ..Default::default()
                            },
                            ..Default::default()
                        });
                    });
                });
        });
    }

    const BLOCK_SIZE: usize = 1000;

    fn blocks(xmin: f64, xmax: f64, ymin: f64, ymax: f64) -> HashSet<BlockId> {
        let left = (xmin / BLOCK_SIZE as f64).floor() as i32;
        let right = (xmax / BLOCK_SIZE as f64).ceil() as i32;
        let bottom = (ymin / BLOCK_SIZE as f64).floor() as i32;
        let top = (ymax / BLOCK_SIZE as f64).ceil() as i32;
        (left..right)
            .flat_map(|x| (bottom..top).map(move |y| BlockId(x, y)))
            .collect()
    }

    fn dust_in_block(ln_density: f64, block: &BlockId) -> Vec<(f64, f64)> {
        let mut rng = rand::rngs::StdRng::seed_from_u64((6379 * block.0 + block.1) as u64);
        let num_points = Poisson::new(BLOCK_SIZE.pow(2) as f64 * ln_density.exp())
            .unwrap()
            .sample(&mut rng) as usize;
        (0..num_points)
            .map(|_| (rng.gen_range(0.0..1.0), rng.gen_range(0.0..1.0)))
            .collect()
    }
}

#[derive(Component)]
struct Bullet;

#[derive(Component)]
struct Rocket;

#[allow(clippy::too_many_arguments)]
fn controls_system(
    time: Res<Time>,
    keyboard_input: Res<Input<KeyCode>>,
    mut scroll_evr: EventReader<MouseWheel>,
    mut click_evr: EventReader<MouseButtonInput>,

    mut player_physics_q: Query<&mut Physics, With<Player>>,
    player_traj_q: Query<&Trajectory, With<Player>>,

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
        let player_xv = player_traj_q.single().current_xv(&time);
        let click_dir = (click_posn - player_xv.x).normalize();

        if ev.button == MouseButton::Left && ev.state.is_pressed() {
            commands.spawn((
                Bullet,
                Physics {
                    acceleration: DVec3::ZERO,
                },
                Appearance(MaterialMesh2dBundle {
                    mesh: circle_mesh.0.clone().into(),
                    material: bullet_material.0.clone(),
                    transform: Transform {
                        scale: Vec3::new(
                            2.0 * BULLET_RADIUS as f32,
                            2.0 * BULLET_RADIUS as f32,
                            0.0,
                        ),
                        ..Default::default()
                    },
                    ..Default::default()
                }),
                Trajectory(VecDeque::from([(
                    time.elapsed_seconds_f64(),
                    XV {
                        x: player_xv.x + click_dir * (PLAYER_RADIUS + BULLET_RADIUS + 10.0),
                        v: SPEED_OF_LIGHT * click_dir,
                    },
                )])),
            ));
        } else if ev.button == MouseButton::Right && ev.state.is_pressed() {
            commands.spawn((
                Rocket,
                Physics {
                    acceleration: DVec3::ZERO,
                },
                Appearance(MaterialMesh2dBundle {
                    mesh: circle_mesh.0.clone().into(),
                    material: rocket_material.0.clone(),
                    transform: Transform {
                        scale: Vec3::new(
                            2.0 * ROCKET_RADIUS as f32,
                            2.0 * ROCKET_RADIUS as f32,
                            0.0,
                        ),
                        ..Default::default()
                    },
                    ..Default::default()
                }),
                Trajectory(VecDeque::from([(
                    time.elapsed_seconds_f64(),
                    XV {
                        x: player_xv.x + click_dir * (PLAYER_RADIUS + ROCKET_RADIUS + 10.0),
                        v: player_xv.v + click_dir * 0.8 * SPEED_OF_LIGHT,
                    },
                )])),
            ));
        }
    }

    let mut physics = player_physics_q.single_mut();
    physics.acceleration = PLAYER_ACCELERATION
        * DVec3::new(
            if keyboard_input.pressed(KeyCode::D) || keyboard_input.pressed(KeyCode::Right) {
                1.0
            } else if keyboard_input.pressed(KeyCode::A) || keyboard_input.pressed(KeyCode::Left) {
                -1.0
            } else {
                0.0
            },
            if keyboard_input.pressed(KeyCode::W) || keyboard_input.pressed(KeyCode::Up) {
                1.0
            } else if keyboard_input.pressed(KeyCode::S) || keyboard_input.pressed(KeyCode::Down) {
                -1.0
            } else {
                0.0
            },
            0.0,
        )
        .try_normalize()
        .unwrap_or(DVec3::ZERO);
}

fn physics_system(time: Res<Time>, mut trajectory_q: Query<(&Physics, &mut Trajectory)>) {
    for (physics, mut traj) in trajectory_q.iter_mut() {
        let (last_t, last_xv) = traj.0.back().unwrap();
        let now = time.elapsed_seconds_f64();
        if physics.acceleration != DVec3::ZERO {
            let new_xv = extrapolate_xv(last_t, last_xv, &now);
            traj.0.push_back((
                now,
                XV {
                    x: new_xv.x,
                    v: new_xv.v + time.delta_seconds_f64() * physics.acceleration,
                },
            ));
        }
    }
}

#[derive(Component)]
struct Image;

#[derive(Resource)]
struct VelocityDotMaterial(Handle<ColorMaterial>);

#[derive(Resource)]
struct AccelerationDotMaterial(Handle<ColorMaterial>);

#[allow(clippy::too_many_arguments)]
fn vision_system(
    time: Res<Time>,
    player_position_q: Query<(&Trajectory, &Appearance), With<Player>>,
    square_mesh: Res<SquareMesh>,
    velocity_dot_material: Res<VelocityDotMaterial>,
    mut object_q: Query<(&Trajectory, &Appearance), Without<Player>>,
    mut image_entity_q: Query<Entity, With<Image>>,
    mut commands: Commands,
    keyboard_input: Res<Input<KeyCode>>,
) {
    for entity in image_entity_q.iter_mut() {
        commands.entity(entity).despawn();
    }

    if keyboard_input.pressed(KeyCode::ShiftLeft) {
        println!("\n------------------");
    }

    let (player_traj, player_appearance) = player_position_q.single();

    let now = time.elapsed_seconds_f64();
    let player_xv = player_traj.current_xv(&time);
    let player_bundle = player_appearance.0.clone();
    commands.spawn((
        Image,
        MaterialMesh2dBundle {
            transform: Transform {
                translation: player_xv.x.as_vec3(),
                ..player_bundle.transform
            },
            ..player_bundle
        },
    ));
    for (traj, appearance) in object_q.iter_mut() {
        if keyboard_input.pressed(KeyCode::ShiftLeft) {
            println!();
        }

        for image in find_images(player_xv.x, now, &traj.0) {
            let object_bundle = appearance.0.clone();
            commands
                .spawn(SpriteBundle {
                    transform: Transform {
                        translation: image.1.x.as_vec3(),
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
                    if image.1.v.length() > 1.0 {
                        parent.spawn((
                            Image,
                            make_line(image.1.v, &square_mesh, &velocity_dot_material.0),
                        ));
                    }
                });
        }
    }
}

fn make_line(
    vec: DVec3,
    square_mesh: &SquareMesh,
    material: &Handle<ColorMaterial>,
) -> MaterialMesh2dBundle<ColorMaterial> {
    let theta = vec.y.atan2(vec.x) as f32;
    let xscale = 0.2 * vec.length() as f32;
    MaterialMesh2dBundle {
        mesh: square_mesh.0.clone().into(),
        material: material.clone(),
        transform: Transform {
            scale: Vec3::new(xscale, 1.0, 0.0),
            rotation: Quat::from_rotation_z(theta),
            translation: Vec3::new(xscale / 2.0 * theta.cos(), xscale / 2.0 * theta.sin(), 0.0),
        },
        ..Default::default()
    }
}

#[derive(Component)]
struct DebugImage;

fn debug_vision_system(
    time: Res<Time>,
    object_q: Query<(&Trajectory, &Appearance)>,
    mut image_entity_q: Query<Entity, With<DebugImage>>,
    mut commands: Commands,
    mut materials: ResMut<Assets<ColorMaterial>>,
) {
    for entity in image_entity_q.iter_mut() {
        commands.entity(entity).despawn();
    }

    for (position, appearance) in object_q.iter() {
        let bundle = appearance.0.clone();
        commands.spawn((
            DebugImage,
            MaterialMesh2dBundle {
                material: materials.add(ColorMaterial::from(Color::rgba(0.5, 0.5, 0.5, 0.5))),
                transform: Transform {
                    translation: position.current_xv(&time).x.as_vec3() + Vec3::Z,
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
