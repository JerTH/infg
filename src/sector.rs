
#[derive(Debug, Copy, Clone, Default, PartialEq, PartialOrd)]
struct Position {
    x: f64,
    y: f64,
    z: f64,
}

/// An ID tag referencing a single object
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct GenerationalKey {
    index: usize,
    generation: isize,
    species: usize,
}

struct GenerationalKeyAllocatorEntry {
    alive: bool,
    gen: usize,
}

struct GenerationalKeyAllocator {
    allocated: Vec<GenerationalKeyAllocatorEntry>,
    free: Vec<usize>,
}

impl GenerationalKeyAllocator {
    fn allocate(&mut self) -> GenerationalKey {
        todo!()
    }
}

/// Each aspect consititutes a simulation property, like a more generalized component
struct WorldAspect {

}

/// The central repository for all game-world related data and simulation
/// 
/// Not everything needs to be loaded at once, nor does everything need to exist on the same machine at the same time
/// 
/// Data is primarily stored in column oriented fashion
/// 
/// For data that might be aliased across machines, or cores, or through time, a replication rule is associated with it
/// 
/// 
/// 
/// Extra:
/// Cross cutting logic insertion at command-processing points
struct WorldDatabase {
    key_alloc: Option<GenerationalKeyAllocator>,
    
}

/**
 *                      [world]
 *              [shard]         [shard]
 *        [phs][ai][player]    [phs][ai]
*/
struct _0;
