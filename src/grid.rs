/* ALLOCATOR */

use std::alloc::{GlobalAlloc, Layout};
use std::collections::VecDeque;
use std::sync::atomic::{AtomicU64, Ordering};
use std::collections::HashMap;
use hadron::extent::Extent3;
use hadron::unique::UniqueId;

pub struct TrackingAllocator<A: GlobalAlloc>(pub A, AtomicU64);

unsafe impl<A: GlobalAlloc> GlobalAlloc for TrackingAllocator<A> {
    unsafe fn alloc(&self, l: Layout) -> *mut u8 {
        self.1.fetch_add(l.size() as u64, Ordering::SeqCst);
        self.0.alloc(l)
    }
    unsafe fn dealloc(&self, ptr: *mut u8, l: Layout) {
        self.0.dealloc(ptr, l);
        self.1.fetch_sub(l.size() as u64, Ordering::SeqCst);
    }
}

impl<A: GlobalAlloc> TrackingAllocator<A> {
    pub const fn new(a: A) -> Self {
        TrackingAllocator(a, AtomicU64::new(0))
    }

    pub fn reset_tracking(&self) {
        self.1.store(0, Ordering::SeqCst);
    }
    pub fn get_stats(&self) -> u64 {
        self.1.load(Ordering::SeqCst)
    }
}



/* GRID */

#[derive(Default, Debug, Clone)]
struct GridPropertyUniformIntegerStorage {
    data: Vec<i32>
}

impl GridPropertyUniformIntegerStorage {
    fn initialize(&mut self, grid_extent: &Extent3) {
        let (cx, cy, cz) = grid_extent.as_abs_integer_tuple();
        let cells = cx * cy;
        self.data = vec![0; cells];
    }
}

#[derive(Default, Debug, Clone)]
struct SparseIntegerStorage {
    data: HashMap<UniqueId, i32>
}

#[derive(Default, Debug, Clone)]
enum GridPropertyStorage {
    UniformInteger(GridPropertyUniformIntegerStorage),
    SparseInterger(SparseIntegerStorage),
    
    #[default]
    Null,
}

#[derive(Default, Debug, Clone)]
struct GridProperty {
    name: Option<String>,
    storage: GridPropertyStorage,
}

impl GridProperty {
    fn new() -> GridPropertyBuilder {
        GridPropertyBuilder(GridProperty::default())
    }
}

struct GridPropertyBuilder(GridProperty);

impl GridPropertyBuilder {
    fn with_name(mut self, name: &str) -> Self {
        self.0.name = Some(String::from(name)); self
    }

    fn with_storage_type(mut self, storage: GridPropertyStorage) -> Self {
        self.0.storage = storage; self
    }

    fn build(mut self) -> GridProperty {
        self.0
    }
}

// Goal: Create room with three deposits of gases, allow them to mix into air
#[derive(Debug, Clone, Default)]
struct Grid {
    extent: Extent3,

    /// A list of cells which require an update
    updates: VecDeque<usize>,

    /// Cell property data
    properties: HashMap<UniqueId, GridProperty>,

    property_map: HashMap<String, UniqueId>,
}

// Fluids are room/flow rate based, with grid based artifacts. Concentration gradients are possible.
// Model a concentration/amount of a fluid in a particular part of a room with a limited number of moving "source points" which
// pathfind towards each other/the center of the room. The source points disperse into the volume of the room
// Perhaps consider "fluid meshes"
//
// High pressure fluids in a room will test the boundaries of the room, potentially causing a blow-out
impl Grid {
    fn new() -> GridBuilder {
        GridBuilder(Grid::default())
    }

    fn register_new_property(&mut self, property: GridProperty) -> UniqueId {
        todo!()
    } 

    fn initialize(&mut self) {
        for (_uid, property) in &mut self.properties {
            match &mut property.storage {
                GridPropertyStorage::UniformInteger(storage) => storage.initialize(&self.extent),
                GridPropertyStorage::SparseInterger(_) => todo!(),
                GridPropertyStorage::Null => todo!(),
            }
        }
    }
}

struct GridBuilder(Grid);
impl GridBuilder {
    fn with_extent(mut self, extent: Extent3) -> Self {
        self.0.extent = extent.abs();
        self
    }

    fn build(mut self) -> Grid {
        self.0
    }
}

/* Test Grid Properties */

#[cfg(test)]
mod test {   
}
