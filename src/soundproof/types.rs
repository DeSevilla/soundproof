// use std::rc::Rc;
use std::sync::Arc;
use std::time::Instant;

use fundsp::prelude32::*;
use piet_common::Color;

// #[cfg(feature = "bevy")]
// use bevy::prelude::*;

use crate::DivisionMethod;
use crate::lambdapi::ast::Tag;
use crate::sound_generators::SoundGenerator;

pub struct SetOnce<T: Clone> {
    body: Option<T>,
}

impl<T: Clone> SetOnce<T> {
    pub fn new() -> Self {
        Self { body: None }
    }

    pub fn get(&mut self, default: T) -> T {
        match &self.body {
            Some(val) => val.clone(),
            None => {
                self.body = Some(default.clone());
                default
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum SoundproofError {
    Mismatch,
}

pub struct ConfigSequencer {
    pub seq: Sequencer,
    pub start_time: Option<Instant>,
    pub live: bool,
}

impl ConfigSequencer {
    pub fn new(seq: Sequencer, live: bool) -> Self {
        // start time is set on push
        Self {
            seq,
            start_time: None,
            live,
        }
    }

    pub fn push_relative(
        &mut self,
        start_time: f64,
        end_time: f64,
        fade_ease: Fade,
        fade_in_time: f64,
        fade_out_time: f64,
        unit: Box<dyn AudioUnit>,
    ) -> EventId {
        if self.live {
            let now = Instant::now();
            let start_time = match self.start_time {
                None => {
                    self.start_time = Some(now);
                    start_time
                }
                Some(_) => start_time,
            };
            self.seq.push_relative(
                start_time,
                end_time,
                fade_ease,
                fade_in_time,
                fade_out_time,
                unit,
            )
        } else {
            self.seq.push_relative(
                start_time,
                end_time,
                fade_ease,
                fade_in_time,
                fade_out_time,
                unit,
            )
        }
    }

    pub fn push_duration(
        &mut self,
        start_time: f64,
        duration: f64,
        fade_ease: Fade,
        fade_in_time: f64,
        fade_out_time: f64,
        unit: Box<dyn AudioUnit>,
    ) -> EventId {
        if self.live {
            let now = Instant::now();
            let start_time = match self.start_time {
                None => {
                    self.start_time = Some(now);
                    start_time
                }
                Some(st) => start_time - (st - now).as_secs_f64(),
            };
            self.seq.push_relative(
                start_time,
                start_time + duration,
                fade_ease,
                fade_in_time,
                fade_out_time,
                unit,
            )
        } else {
            // note: relative isn't *bad* even for rendering to files but if we can do absolute let's do absolute
            // self.seq.push_relative(start_time, start_time + duration, fade_ease, fade_in_time, fade_out_time, unit)
            // self.seq.push(start_time, start_time + duration, fade_ease, fade_in_time, fade_out_time, unit)
            self.seq.push_duration(
                start_time,
                duration,
                fade_ease,
                fade_in_time,
                fade_out_time,
                unit,
            )
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
// #[cfg_attr(feature = "bevy", derive(Component))]
pub struct Timings {
    pub start: f64,
    pub duration: f64,
    pub lean: f32,
}

/// Tree of simultaneous and/or sequential sounds. Lambda calculus terms are translated into this structure.
/// The purpose of the design is to allow layers of sounds at different paces which each progress in time
/// as fits their place in the structure.
#[derive(Clone)]
pub enum SoundTree {
    /// Subtrees will play simultaneously.
    Simul(Vec<SoundTree>, TreeMetadata),
    /// Subtrees will play sequentially.
    Seq(Vec<SoundTree>, TreeMetadata),
    /// Plays a predefined sound-pattern.
    Sound(Arc<dyn SoundGenerator + Send + Sync>, TreeMetadata),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Highlight {
    One,
    Two,
    Three
}

// #[derive(Clone, Debug, PartialEq, Component)]
// #[cfg_attr(feature = "bevy", derive(Component))]
#[derive(Clone, Debug, PartialEq)]
pub struct TreeMetadata {
    pub name: String,
    pub tag: Tag,
    // pub parent: String,
    pub base_color: Color,
    pub alt_color: Color,
    pub max_depth: usize,
    pub will_step: Option<Highlight>,
    // pub dspthing: Option<Uuid>,
    // pub lean: f32,
}

impl SoundTree {
    /// Constructs a SoundTree containing a single sound-pattern.
    pub fn sound(sound: impl SoundGenerator + Send + Sync + 'static, meta: TreeMetadata) -> Self {
        Self::Sound(Arc::new(sound), meta)
    }

    /// Constructs a SoundTree which plays its subtrees one after another.
    pub fn seq(subtrees: impl IntoIterator<Item = SoundTree>) -> Self {
        // avoids nested Seqs; this can affect duration assignments.
        // subject to change but I think I like it
        let mut result = Vec::new();
        // let mut names = "".to_owned();
        // let mut names = "[".to_owned();
        let mut max_depth = 0;
        let will_step = None;
        for val in subtrees {
            match val.clone() {
                SoundTree::Seq(mut trees, meta) => {
                    // names += &meta.name;
                    // names += ";";
                    if meta.max_depth > max_depth {
                        max_depth = meta.max_depth
                    }
                    // will_step = will_step.and(meta.will_step);
                    result.append(&mut trees);
                }
                other => {
                    let meta = other.metadata();
                    // names += &other.metadata().name;
                    // names += ";";
                    if meta.max_depth > max_depth {
                        max_depth = meta.max_depth
                    }
                    // will_step = will_step.and(meta.will_step);
                    result.push(other);
                }
            }
        }
        if result.len() == 1 {
            result.pop().unwrap()
        } else {
            Self::Seq(
                result,
                TreeMetadata {
                    name: "".to_owned(),
                    tag: Tag::Free,
                    // parent: "".to_owned(),
                    base_color: Color::MAROON,
                    alt_color: Color::MAROON,
                    max_depth,
                    will_step: will_step,
                    // dspthing: None,
                },
            )
        }
        // names += "]";
    }

    /// Constructs a SoundTree which plays its subtrees simultaneously.
    pub fn simul(subtrees: impl IntoIterator<Item = SoundTree>) -> Self {
        // avoids redundant nested Simuls; this cannot affect the resulting audio
        let mut result = Vec::new();
        // let mut names = "{".to_owned();
        let mut max_depth = 0;
        let will_step = None;
        for val in subtrees {
            match val.clone() {
                SoundTree::Simul(mut trees, meta) => {
                    // names += &meta.name;
                    // names += "||";
                    if meta.max_depth > max_depth {
                        max_depth = meta.max_depth
                    }
                    // will_step &= meta.will_step;
                    result.append(&mut trees);
                }
                other => {
                    // names += &other.metadata().name;
                    // names += "||";
                    if other.metadata().max_depth > max_depth {
                        max_depth = other.metadata().max_depth
                    }
                    // will_step &= other.metadata().will_step;
                    result.push(other)
                }
            }
        }
        // names += "}";
        if result.len() == 1 {
            result.pop().unwrap()
        } else {
            Self::Simul(
                result,
                TreeMetadata {
                    name: "".to_owned(),
                    tag: Tag::Free,
                    // parent: "".to_owned(),
                    base_color: Color::MAROON,
                    alt_color: Color::MAROON,
                    max_depth,
                    will_step: will_step,
                    // dspthing: None,
                },
            )
        }
    }

    pub fn metadata(&self) -> &TreeMetadata {
        match self {
            SoundTree::Simul(_, tree_metadata) => tree_metadata,
            SoundTree::Seq(_, tree_metadata) => tree_metadata,
            SoundTree::Sound(_, tree_metadata) => tree_metadata,
        }
    }

    pub fn metadata_mut(&mut self) -> &mut TreeMetadata {
        match self {
            SoundTree::Simul(_, tree_metadata) => tree_metadata,
            SoundTree::Seq(_, tree_metadata) => tree_metadata,
            SoundTree::Sound(_, tree_metadata) => tree_metadata,
        }
    }

    /// The total number of nodes in the tree.
    pub fn size(&self) -> usize {
        match self {
            SoundTree::Simul(vec, _) => 1 + vec.iter().map(|x| x.size()).max().unwrap_or(0),
            SoundTree::Seq(vec, _) => vec.iter().map(|x| x.size()).sum::<usize>(),
            SoundTree::Sound(_, _) => 1,
        }
    }

    // pub fn max_depth(&self) -> usize {
    //     match self {
    //         SoundTree::Simul(vec, _) => {
    //             let result = 1 + vec.iter().map(|x| x.max_depth()).max().unwrap_or(0);
    //             println!("{}{result} ({})", vec!["\t"; 5 - result].join(""), vec.len());
    //             result
    //         },
    //         SoundTree::Seq(vec, _) => vec.iter().map(|x| x.max_depth()).max().unwrap_or(0),
    //         SoundTree::Sound(_, _) => 1,
    //     }
    // }

    /// The weight of the tree for duration scaling.
    pub fn weight(&self, exp: f64) -> f64 {
        match self {
            SoundTree::Simul(vec, _) => {
                1.0 + vec
                    .iter()
                    .map(|x| x.subtree_weight(exp))
                    .reduce(f64::max)
                    .unwrap_or(0.0)
            }
            SoundTree::Seq(vec, _) => vec.iter().map(|x| x.subtree_weight(exp)).sum::<f64>(),
            SoundTree::Sound(_, _) => 1.0,
        }
    }

    /// The weight of the tree, considered as a subtree, for duration scaling.
    pub fn subtree_weight(&self, exp: f64) -> f64 {
        self.weight(exp).powf(exp)
    }

    /// A general "induction" principle for the "horizontal" positioning and stereo of nodes in the tree.
    /// Horizontal positioning is originally time but can be other things, like pitch.
    /// Provide the starting info for positioning and range of the root node, plus handlers for the three SoundTree constructors.
    /// Handlers have access to the node's metadata, position, range, and stereo lean, plus info on the children.
    /// This function takes care of applying handlers and supplying that information to them.
    /// Generally should be wrapped up in another function rather than used directly.
    pub fn distribute<T>(
        &self,
        start: f64,
        range: f64,
        scaling: DivisionMethod,
        lean: f32,
        base: &mut impl FnMut(&Arc<dyn SoundGenerator + Send + Sync>, &TreeMetadata, f64, f64, f32) -> T,
        seq: &mut impl FnMut(Vec<T>, &TreeMetadata, f64, f64, f32) -> T,
        simul: &mut impl FnMut(Vec<T>, &TreeMetadata, f64, f64, f32) -> T,
    ) -> T {
        match self {
            SoundTree::Simul(sound_trees, meta) => {
                let dir = 0.0; // if we care abt lean we should have this be either 1 or -1, distributed somehow
                // let scale = vec.len();
                let scale = (self.size() - 1) as f32;
                let base_lean = dir * scale / 2.0;
                let mut output = Vec::new();
                for child in sound_trees {
                    let local_lean = (base_lean - dir * child.size() as f32) * 0.8 / scale; // can't div by 0; if scale is 0, sound_trees is empty
                    // println!("local lean: {local_lean}, {base_lean}, {scale}: {}, {}", self.size(), vec.len());
                    // output.push((start_time, duration, scaling, lean + local_lean));
                    output.push(child.distribute(
                        start,
                        range,
                        scaling,
                        lean + local_lean,
                        base,
                        seq,
                        simul,
                    ));
                }
                seq(output, meta, start, range, lean)
            }
            SoundTree::Seq(sound_trees, meta) => {
                let mut output = Vec::new();
                let mut elapsed = 0.0;
                for child in sound_trees {
                    let ratio = scaling.child_scale(child) / scaling.parent_scale(self);
                    let scaled = range * ratio;
                    output.push(child.distribute(
                        start + elapsed,
                        scaled,
                        scaling,
                        lean,
                        base,
                        seq,
                        simul,
                    ));
                    elapsed += scaled;
                }
                simul(output, meta, start, range, lean)
            }
            SoundTree::Sound(sound, meta) => base(sound, meta, start, range, lean),
        }
    }

    pub fn generate_with(&self, seq: &mut ConfigSequencer, range: f64, scaling: DivisionMethod) {
        self.distribute(
            0.0,
            range,
            scaling,
            0.0,
            &mut (|sound, _, s, r, l| sound.sequence(seq, s, r, l)),
            &mut |_, _, _, _, _| {},
            &mut |_, _, _, _, _| {},
        )
    }

    pub fn sound_times(
        &self,
        duration: f64,
        scaling: DivisionMethod,
    ) -> Vec<(Arc<dyn SoundGenerator + Send + Sync>, Timings, TreeMetadata)> {
        self.distribute(
            0.0,
            duration,
            scaling,
            0.0,
            &mut |sound, meta, s, d, l| {
                let timings = Timings {
                    start: s,
                    duration: d,
                    lean: l,
                };
                vec![(Arc::clone(sound), timings, meta.clone())]
            },
            &mut |children, _, _, _, _| children.concat(),
            &mut |children, _, _, _, _| children.concat(),
        )
    }

    pub fn pervade_metadata(&mut self, f: &impl Fn(&mut TreeMetadata)) {
        match self {
            SoundTree::Simul(children, meta) => {
                f(meta);
                for child in children {
                    child.pervade_metadata(f);
                }
            },
            SoundTree::Seq(children, meta) => {
                f(meta);
                for child in children {
                    child.pervade_metadata(f);
                }
            },
            SoundTree::Sound(_, meta) => f(meta),
        }
    }
}
