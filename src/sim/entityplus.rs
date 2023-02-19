use std::collections::{BinaryHeap, VecDeque, btree_set};
use std::collections::btree_map::{VacantEntry, OccupiedEntry};
/// A relational entity database which supports fast cacheable queries and quick parallel iteration

use std::sync::{LazyLock, Mutex, Arc, MutexGuard};
use std::ops::{Deref, DerefMut};
use std::collections::{HashMap, BTreeSet, hash_map::Entry};
use std::any::{Any, TypeId};
use std::fmt::Debug;
use std::hash::Hash;

//use crate::sim::bytes::HomogeneousMap;

// Macros to make certain operations interfacing with the `EntityDatabase` more ergonomic
#[macro_use]
mod macros {
    macro_rules! component {
        ($a:ty) => {
            ComponentType::from(StableTypeId::of::<$a>())
        };
    }

    macro_rules! component_type_set {
        ($($ct:ty),+) => {
            {
                let mut __set = BTreeSet::new();
                $(
                    __set.insert(component!($ct));
                )*
                ComponentTypeSet(Arc::new(__set))
            }
        };
    }

    macro_rules! family_id_set {
        ($($fi:expr),+) => {
            {
                let mut __set = BTreeSet::<FamilyId>::new();
                $(
                    __set.insert($fi);
                )*
                FamilyIdSet(__set)
            }
        };
    }

    macro_rules! select {
        ($($a:ty),+) => {
            ()
        };
    }
}

/// Stores and controls all entity data, governs how entities are processed
#[derive(Debug)]
pub struct EntityDatabase {
    data: EntityData,
    records: EntityRecords,
    alloc: EntityAllocator,
}

/// Dumb storage which owns the raw data that entities are made of
#[derive(Debug)]
struct EntityData {
    tables: HashMap<FamilyId, DataTable>,
    families: HashMap<FamilyId, Family>,
}

/// Stores and tracks of all bookkeeping data used to manage and query entities
#[derive(Debug)]
struct EntityRecords {
    component_sets: ComponentSetFamilyMap,
    containing: ContainingFamiliesMap,
    entities: EntityFamilyMap,
}

/// Provides `EntityId`'s and manages their re-use
#[derive(Debug)]
struct EntityAllocator {
    count: u32,
    free: Vec<EntityId>,
}

/// A stable `TypeId` clone which (should) be common across builds. This isn't a guarantee, especially
/// across different rust versions
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)] struct StableTypeId(u64);

/// A `ComponentType` uniquely identifies a single kind of component. Regular components
/// are synonymous with a single Type, but relational components are unique to a run-time instance
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)] struct ComponentType(StableTypeId);

#[derive(Copy, Clone)]
pub union IdUnion {
    /// Interprets the id as raw bytes
    bytes: (u8, u8, u8, u8, u8, u8, u8, u8),

    /// Interprets the id as (id, generation, _, flags)
    generational: (u32, u16, u8, u8),

    /// Interprets the id as (id, id) for relational links
    relational: (u32, u32),

    /// Interprets the id as a raw u64, also forces the union into 8 byte alignment
    aligned: u64,
}

/// An `EntityId` uniquely identifies a single entity
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)] pub struct EntityId(IdUnion);

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
enum ComponentDelta {
    Add(ComponentType),
    Remove(ComponentType),
}

/// A `ComponentId` uniquely identifies a single component within a column of components
/// 
/// These are functionally the same as `EntityId`'s
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)] struct ComponentId(IdUnion);

/// A `FamilyId` uniquely identifies a single family
/// 
/// These are functionally the same as `EntityId`'s
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)] struct FamilyId(IdUnion);

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
enum FamilyDelta {
    Add(FamilyId),
    Remove(FamilyId),
}

/// A `FamilyGraphEdge` describes how to transfer an entity between families given a new or removed component
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct FamilyGraphEdge {
    component: ComponentType,
    delta: FamilyDelta,
}

/// Stores raw component data of a single type, describes how to interact with that data
struct ComponentColumnEntry {
    data: Box<dyn Any>, // ComponentColumn<T>
    mvfn: fn(&EntityId, &mut Box<dyn Any>, &mut Box<dyn Any>),
    ctor: fn() -> Box<dyn Any>,
}

type TableEntry = HashMap<ComponentType, ComponentColumnEntry>;

#[derive(Debug)]
struct DataTableGuard<'a>(MutexGuard<'a, TableEntry>);

/// A `DataTable` represents a single table of components unique to a family. This is where actual user
/// application data is stored as rows and columns of components, and so its contents are dynamically typed
/// 
/// `DataTable`'s exist on a one-to-one basis with `Family`'s. Each `FamilyId` points to a single `DataTable` 
/// 
/// The `DataTable` is indexed column-first, where each column represents a component type and rows are entities
///     Name   Health
/// E30 [&'1]  [ 5 ] 
/// E12 [&'2]  [ 8 ] 
/// E77 [&'2]  [ 2 ] 
/// E31 [&'1]  [ 0 ]
///  
#[derive(Default, Debug)] struct DataTable(Arc<Mutex<TableEntry>>);

//trait MoveableAny: Any + Debug {
//    fn as_mut_any<'a>(&'a mut self) -> &'a mut dyn Any;
//    fn as_any<'a>(&'a self) -> &'a dyn Any;
//    fn move_to(&mut self, to: &mut Box<dyn Any>, index: EntityId);
//}

pub trait Component: Any + Debug + 'static {}

/// A `ComponentColumn<T>` is the raw typed storage for a single component type in a `DataTable`
/// 
/// Columns are indexed by `ComponentType`'s, and rows are indexed by `EntityId`'s
#[derive(Default)]
struct ComponentColumn<T: Component>(HashMap<EntityId, T>);

/// A `Family` is a collection of entities with the same components, when components are
/// added or removed from an entity, the entity is moved between families
#[derive(Clone, Debug)]
struct Family {
    /// A reference to the unique set of component id's which comprise this family
    components: ComponentTypeSet,

    /// When an entity gains or loses a component, it must be moved to a different family.
    /// Here we lazily construct a graph of transfers to make subsequent transfers faster
    transfer_graph: HashMap<ComponentType, FamilyGraphEdge>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum FamilyTransform {
    Transfer {
        from: FamilyId,
        dest: FamilyId,
    },
    InitTransfer {
        from: FamilyId,
        set: ComponentTypeSet,
    },
    InitNew(ComponentTypeSet),
    NewEntity(FamilyId),
    NoChange(FamilyId),
}

/// A set of unique component types
#[derive(Clone, Default, PartialEq, Eq, Hash, Debug)] struct ComponentTypeSet(Arc<BTreeSet<ComponentType>>);

/// Maps a set of component types to the single associated family
#[derive(Clone, Default, Debug)] struct ComponentSetFamilyMap(HashMap<ComponentTypeSet, FamilyId>);

/// A set of unique family id's
#[derive(Clone, Default, Debug)] struct FamilyIdSet(BTreeSet<FamilyId>);

/// Maps a single `ComponentType` to the set of families which contain it by their `FamilyId`'s
#[derive(Clone, Default, Debug)] struct ContainingFamiliesMap(HashMap<ComponentType, FamilyIdSet>);

/// Maps a single `EntityId` to its corresponding `FamilyId`
#[derive(Clone, Default, Debug)] struct EntityFamilyMap(HashMap<EntityId, FamilyId>);

// Impl's

// `EntityDatabase`
impl EntityDatabase {
    /// Creates a new empty `EntityDatabase`.
    /// 
    /// Automatically registers a special unit component of type `()`
    pub fn new() -> Self {
        let mut db = EntityDatabase {
            data: EntityData::new(),
            records: EntityRecords::new(),
            alloc: EntityAllocator::new()
        };

        // Register the unit/null family, this component is automatically added to empty
        db.register_component::<()>();
        db
    }
    
    /// Creates a new entity with no components, returning its unique `EntityId`
    pub fn create(&mut self) -> EntityId {
        let id: EntityId = self.alloc.alloc();

        self.add_component(id, ());
        //self.add_unit_component(id);

        id
    }
    
    fn new_family(&mut self, new_component_type: ComponentType, old_family_id: &FamilyId) -> FamilyId {
        let family_components = &self.data.families.get(&old_family_id).expect("expected existing family").components;
        let target_set = family_components.iter().cloned().chain([new_component_type]).collect();
        let target_id = self.get_or_register_family_id_for(target_set);
        let transfer_graph = &mut self.data.families.get_mut(&old_family_id).expect("expected existing family").transfer_graph;
        
        let transfer_edge = transfer_graph
            .entry(new_component_type)
            .or_insert_with(|| {
                FamilyGraphEdge {
                    component: new_component_type,
                    delta: FamilyDelta::Add(target_id)
                }
            }
        );
        
        let new_family_id = match transfer_edge.delta {
            FamilyDelta::Add(add) => add,
            FamilyDelta::Remove(_) => panic!("family delta is remove"),
        };

        new_family_id
    }
    
    fn component_set_of(&self, family: &FamilyId) -> Option<ComponentTypeSet> {
        self.data.families.get(family).and_then(|f| Some(f.components.clone()))
    }

    fn family_of_component_set(&self, set: &ComponentTypeSet) -> Option<FamilyId> {
        self.records.component_sets.get(set).cloned()
    }
    
    /// Attempts to find the `FamilyId` for the family the entity would belong to after the component delta is applied
    /// 
    /// Returns `None` if no suitable family exists
    fn get_family_transform(&self, entity: &EntityId, delta: ComponentDelta) -> FamilyTransform {
        match delta {
            ComponentDelta::Add(added) => {
                // first things first, does the entity exist as far as the database is concerned?
                match self.records.entities.get(entity) {
                    Some(current_family_id) => {
                        // the entity exists, check its transfer graph for a link
                        match self.data.families.get(&current_family_id) {
                            Some(family) => {
                                match family.transfer_graph.get(&added) {
                                    // if we have a valid graph edge for adding this component, use it
                                    Some(FamilyGraphEdge{delta: FamilyDelta::Add(graph_target), ..}) => {
                                        return FamilyTransform::Transfer { from: *current_family_id, dest: *graph_target }
                                    },
                                    // otherwise, try to find a suitable family using the component sets
                                    // todo: interior mutability to update the transfer graph here
                                    _ => {
                                        match self.component_set_of(current_family_id) {
                                            Some(mut set) => {
                                                if set.contains(&added) {
                                                    return FamilyTransform::NoChange(*current_family_id)
                                                } else {
                                                    // we're transfering this entity into a new family, which one?
                                                    let set = set.iter().cloned().chain([added].iter().cloned()).collect();
                                                    match self.family_of_component_set(&set) {
                                                        Some(new_family_id) => {
                                                            return FamilyTransform::Transfer { from: *current_family_id, dest: new_family_id }
                                                        },
                                                        None => {
                                                            return FamilyTransform::InitTransfer { from: *current_family_id, set: set }
                                                        },
                                                    }
                                                }
                                            },
                                            None => {
                                                panic!("family data doesn't exist or is invalid")
                                            },
                                        }
                                    }
                                }
                            },
                            None => {
                                panic!("family data doesn't exist or is invalid")
                            },
                        }
                    },
                    None => {
                        // the entity doesn't currently exist, if we create it, where does it belong?
                        let set = ComponentTypeSet::from(added);
                        match self.family_of_component_set(&set) {
                            Some(family_id) => {
                                return FamilyTransform::NewEntity(family_id)
                            },
                            None => {
                                return FamilyTransform::InitNew(set)
                            },
                        }
                    },
                }
            },
            ComponentDelta::Remove(removed) => {
                todo!()
            },
        }
    }

    /// Inserts or replaces component data for a given entity in the appropriate `DataTable`, associated
    /// by the entities `Family`. Lazily constructs data tables component columns
    fn insert_real_component<T: Component>(&mut self, entity: &EntityId, family: &FamilyId, component: T) -> Result<(), ()> {
        let table = self.data.tables.get_mut(&family).ok_or(())?;
        let mut guard = table.lock();

        match guard.entry(component!(T)) {
            Entry::Occupied(mut occupied) => {
                let column_entry = occupied.get_mut();
                match column_entry.data.downcast_mut::<ComponentColumn<T>>() {
                    Some(column) => {
                        column.insert(*entity, component);
                    },
                    None => {
                        panic!("mismatched column type");
                    },
                }
            },
            Entry::Vacant(vacant) => {
                let column = ComponentColumn::<T>::from((*entity, component));
                let column_entry = ComponentColumnEntry {
                    data: Box::new(column),
                    mvfn: ComponentColumn::<T>::fn_virtual_move,
                    ctor: ComponentColumn::<T>::fn_virtual_ctor,
                };

                vacant.insert(column_entry);
            },
        }
        Ok(())
    }

    fn set_family_record(&mut self, entity: &EntityId, family: &FamilyId) {
        self.records.entities.insert(*entity, *family);
    }

    /// Acquires locks on two `DataTable`'s at once. Attempts to avoid deadlocks by first acquiring `a`
    /// and then trying to acquire `b`, giving up the lock on `a` and starting over if `b` can not be
    /// immediately acquired. Locking two tables in an unknown order is necessary during data copies
    fn lock_tables_yielding<'a, 'b>(&self, a: &'a DataTable, b: &'b DataTable) -> (DataTableGuard<'a>, DataTableGuard<'b>) {
        loop {
            let ga = a.lock();
            match b.try_lock() {
                Ok(gb) => {
                    return (ga, DataTableGuard(gb))
                },
                Err(err) => {
                    std::mem::drop(ga)
                },
            }
        }
    }

    fn resolve_transform<T: Component>(&mut self, entity: EntityId, component: T, transform: FamilyTransform) {
        match transform {
            // the entity is being transfered from one family to another
            FamilyTransform::Transfer { from, dest } => {
                println!("FamilyTransform::Transfer");
                let dest_family = dest;

                {
                    let from = self.data.tables.get(&from).unwrap();
                    let dest = self.data.tables.get(&dest).unwrap();

                    let (mut from, mut dest) = self.lock_tables_yielding(from, dest);
                    from.move_row(&entity, &mut dest);
                }
                
                self.insert_real_component::<T>(&entity, &dest_family, component);
                self.set_family_record(&entity, &dest_family);
            },
            
            // the entity is new, it doesn't have a family yet
            FamilyTransform::NewEntity(family_id) => {
                println!("FamilyTransform::NewEntity");

                self.set_family_record(&entity, &family_id);
                self.insert_real_component::<T>(&entity, &family_id, component);
            },

            // the entity already owns one of these components, we're just replacing it
            FamilyTransform::NoChange(current_family) => {
                println!("FamilyTransform::NoChange");

                self.insert_real_component::<T>(&entity, &current_family, component);
            },

            // the entity is being transfered from one family to another, but we need to create the new family first
            FamilyTransform::InitTransfer { from, set } => {
                println!("FamilyTransform::InitTransfer");

                let dest = self.register_family(set);
                self.resolve_transform(entity, component, FamilyTransform::Transfer { from, dest })
            }

            // the entity doesn't have a family yet and the family it should belong to doesn't exist
            FamilyTransform::InitNew(set) => {
                println!("FamilyTransform::InitNew");

                let family_id = self.register_family(set);
                self.set_family_record(&entity, &family_id);
                self.insert_real_component::<T>(&entity, &family_id, component);
            },
        }
    }


    /// Adds a component `T` to an entity by its `EntityId`
    /// 
    /// 
    pub fn add_component<T: Component>(&mut self, entity: EntityId, component: T) -> Result<(), ()> {
        let component_type = component!(T);
        let transform = self.get_family_transform(&entity, ComponentDelta::Add(component_type));
        self.resolve_transform(entity, component, transform);

        Ok(())
    }
    
    pub fn register_component<T: 'static>(&mut self) {
        StableTypeId::register_debug_info::<T>();
        self.register_family(component_type_set!(T));
    }

    /// Registers a set of components as a `Family`
    fn register_family(&mut self, set: ComponentTypeSet) -> FamilyId {
        let id: FamilyId = self.alloc.alloc().into();

        self.data.families
            .entry(id)
            .or_insert(Family { components: set.clone(), transfer_graph: HashMap::new() });

        self.data.tables
            .entry(id)
            .or_insert(DataTable::default());

        for comp in set.iter().cloned() {
            self.records.containing
                .entry(comp)
                .and_modify(|f| { f.insert(id); } )
                .or_insert(family_id_set!(id));
        }

        self.records.component_sets.insert(set.clone(), id);

        id
    }

    fn get_or_register_family_id_for(&mut self, set: ComponentTypeSet) -> FamilyId {
        let id = if let Some(id) = self.records.family_id_for(set.clone()) {
            id
        } else {
            self.register_family(set.clone())
        };
        id
    }
    
    fn try_get_family_id(&self, set: ComponentTypeSet) -> Option<FamilyId> {
        self.records.family_id_for(set)
    }
}

// `EntityData`
impl EntityData {
    fn new() -> Self {
        EntityData { tables: HashMap::default(), families: HashMap::default() }
    }
}

// `EntityRecords`
impl EntityRecords {
    fn new() -> Self {
        EntityRecords {
            component_sets: ComponentSetFamilyMap::default(),
            containing: ContainingFamiliesMap::default(),
            entities: EntityFamilyMap::default(),
        }
    }

    fn family_id_for(&self, set: ComponentTypeSet) -> Option<FamilyId> {
        self.component_sets.get(&set).cloned()
    }
}

impl EntityAllocator {
    fn new() -> Self {
        EntityAllocator { count: 0u32, free: Default::default() }
    }

    fn alloc(&mut self) -> EntityId {
        let (idx, gen, m1, m2) = if let Some(id) = self.free.pop() {
            unsafe { id.generational }
        } else {
            self.count = self.count.checked_add(1u32).expect("too many entities");
            (self.count, 0u16, 0u8, 0u8)
        };
        EntityId::generational(idx, gen, m1, m2)
    }

    fn free(&mut self, id: EntityId) {
        let (idx, gen, m1, m2) = unsafe { id.generational };
        self.free.push(EntityId::generational(idx, gen.wrapping_add(1u16), m1, m2));
    }
}

static STABLE_TYPE_ID_NAME_MAP: LazyLock<Mutex<HashMap<StableTypeId, &'static str>>>
    = LazyLock::new(|| Mutex::new(HashMap::new()));

// `StableTypeId`
impl StableTypeId {
    fn name(&self) -> Option<&'static str> {
        match STABLE_TYPE_ID_NAME_MAP.lock() {
            Ok(guard) => guard.get(self).copied(),
            Err(err) => panic!("poisoned mutex accessing stable type name: {}", err),
        }
    }
    
    const fn type_name<T>() -> &'static str where T: ?Sized + 'static {
        std::any::type_name::<T>()
    }

    const fn hash<T>() -> u64 where T: ?Sized + 'static {
        let name = Self::type_name::<T>();
        const_fnv1a_hash::fnv1a_hash_str_64(name)
    }

    const fn of<T>() -> Self where T: ?Sized + 'static {
        let hash = Self::hash::<T>();
        debug_assert!(hash & 0xFFFF != 0);
        StableTypeId(hash)
    }

    /// Registers name information for this type at runtime for richer debug messages
    /// 
    /// This function associates a `StableTypeId` with an intrinsic type name at runtime, it is necessary to
    /// call `register_debug_info` *before* any calls to `name` in order to get the types name
    fn register_debug_info<T>() where T: ?Sized + 'static {
        let name = Self::type_name::<T>();
        let tyid = Self::of::<T>();

        match STABLE_TYPE_ID_NAME_MAP.lock() {
            Ok(mut guard) => guard.insert(tyid, name),
            Err(err) => panic!("poisoned mutex registering stable type debug info: {}", err),
        };
    }
}

impl Debug for StableTypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match Self::name(&self) {
            Some(name) => write!(f, "[{}]", name),
            None => write!(f, "[unknown type]"),
        }
    }
}

// `ComponentType`
impl Debug for ComponentType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ComponentType({:X?}:{:?})", self.0.0, self.0)
        //f.debug_tuple("ComponentType").field(&self.0).finish()
    }
}

impl From<StableTypeId> for ComponentType {
    fn from(value: StableTypeId) -> Self {
        ComponentType(value)
    }
}

// `EntityId`
impl EntityId {
    fn generational(idx: u32, gen: u16, m1: u8, m2: u8) -> Self {
        EntityId(IdUnion{generational: (idx, gen, m1, m2)})
    }
}

impl Deref for EntityId {
    type Target = IdUnion;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// `ComponentId`
impl From<EntityId> for ComponentId {
    fn from(value: EntityId) -> Self {
        ComponentId(value.0)
    }
}

impl From<EntityId> for FamilyId {
    fn from(value: EntityId) -> Self {
        FamilyId(value.0)
    }
}

// `ComponentColumnEntry`
impl ComponentColumnEntry {
    fn insert<T: Component>(&mut self, key: EntityId, val: T) -> Option<T> {
        let column = self.data.downcast_mut::<ComponentColumn<T>>().unwrap();
        column.insert(key, val)
    }

    fn move_row_val(&mut self, row: &EntityId, dest: &mut Self) {
        (self.mvfn)(row, &mut self.data, &mut dest.data)
    }

    fn shallow_clone(&self) -> Self {
        let data = (self.ctor)();
        ComponentColumnEntry {
            data: data,
            mvfn: self.mvfn,
            ctor: self.ctor
        }
    }
}

impl Debug for ComponentColumnEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ComponentColumnEntry")
        .field("data", &self.data)
        .field("mvfn", &"fn(&EntityId, &mut dyn Box<Any>, &mut dyn Box<Any>)")
        .finish()
    }
}

// `DataTableGuard`
impl<'a> DataTableGuard<'a> {
    fn move_row(&mut self, row: &EntityId, to: &mut DataTableGuard) {
        for (component, from) in self.iter_mut() {
            match to.entry(*component) {
                Entry::Occupied(mut occupied) => {
                    let to = occupied.get_mut();
                    from.move_row_val(row, to);
                },
                Entry::Vacant(vacant) => {
                    let new_column = vacant.insert(ComponentColumnEntry {
                        data: (from.ctor)(),
                        mvfn: from.mvfn,
                        ctor: from.ctor,
                    });
                    from.move_row_val(row, new_column);
                },
            }
            if let Some(to) = to.get_mut(component) {
                from.move_row_val(row, to);
            }
        }
    }
}

impl<'a> Deref for DataTableGuard<'a> {
    type Target = TableEntry;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

impl<'a> DerefMut for DataTableGuard<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0.deref_mut()
    }
}

// `DataTable`
impl DataTable {
    fn lock(&self) -> DataTableGuard {
        match self.0.lock() {
            Ok(guard) => DataTableGuard(guard),
            Err(err) => panic!("poisoned, unable to lock data table for reading: {}", err),
        }
    }
}

impl Deref for DataTable {
    type Target = Arc<Mutex<TableEntry>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for DataTable {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

// `Component` trait
impl Component for () {}

// `ComponentColumn`
impl<T: Component> ComponentColumn<T> {
    fn new() -> Self {
        ComponentColumn(Default::default())
    }

    fn fn_virtual_move(row: &EntityId, from: &mut Box<dyn Any>, to: &mut Box<dyn Any>) {
        let from = from.downcast_mut::<ComponentColumn<T>>().expect("expect from");
        let dest = to.downcast_mut::<ComponentColumn<T>>().expect("expect dest");
        match from.remove(row) {
            Some(item) => { dest.insert(*row, item); },
            None => { #[cfg(Debug)] println!("no row value to move"); },
        }
    }

    fn fn_virtual_ctor() -> Box<dyn Any> {
        Box::new(ComponentColumn::<T>::new())
    }
}

impl<T: Component> From<(EntityId, T)> for ComponentColumn<T> {
    fn from(value: (EntityId, T)) -> Self {
        let mut column = ComponentColumn::new();
        column.insert(value.0, value.1);
        column
    }
}

impl<T: Component> Debug for ComponentColumn<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ComponentColumn").field("data", &self).finish()
    }
}

impl<T: Component> Deref for ComponentColumn<T> {
    type Target = HashMap<EntityId, T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Component> DerefMut for ComponentColumn<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

// `IdUnion` unsafe is required to access the unions fields
impl PartialEq for IdUnion {
    fn eq(&self, other: &Self) -> bool {
        unsafe { PartialEq::eq(&self.aligned, &other.aligned) }
    }
}

impl Eq for IdUnion {}

impl PartialOrd for IdUnion {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        unsafe { PartialOrd::partial_cmp(&self.aligned, &other.aligned) }
    }
}

impl Ord for IdUnion {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        unsafe { Ord::cmp(&self.aligned, &other.aligned) }
    }
}

impl Hash for IdUnion {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        unsafe { Hash::hash(&self.aligned, state) }
    }
}

impl Debug for IdUnion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe { write!(f, "aligned [{}]", self.aligned) }
    }
}

// `ComponentTypeSet`
impl ComponentTypeSet {
    fn empty() -> Self {
        ComponentTypeSet(Arc::new(BTreeSet::new()))
    }
}

impl FromIterator<ComponentType> for ComponentTypeSet {
    fn from_iter<T: IntoIterator<Item = ComponentType>>(iter: T) -> Self {
        ComponentTypeSet(Arc::new(BTreeSet::from_iter(iter)))
    }
}

impl From<ComponentType> for ComponentTypeSet {
    fn from(value: ComponentType) -> Self {
        ComponentTypeSet(Arc::new([value].iter().cloned().collect()))
    }
}

impl Deref for ComponentTypeSet {
    type Target = BTreeSet<ComponentType>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// `ComponentSetFamilyMap`
impl Deref for ComponentSetFamilyMap {
    type Target = HashMap<ComponentTypeSet, FamilyId>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for ComponentSetFamilyMap {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

// `FamilyIdSet`
impl Deref for FamilyIdSet {
    type Target = BTreeSet<FamilyId>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for FamilyIdSet {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

// `ContainingFamiliesMap`
impl Deref for ContainingFamiliesMap {
    type Target = HashMap<ComponentType, FamilyIdSet>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for ContainingFamiliesMap {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

// `EntityFamilyMap`
impl Deref for EntityFamilyMap {
    type Target = HashMap<EntityId, FamilyId>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for EntityFamilyMap {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

// Tests
#[cfg(test)]
mod test {
    use std::collections::HashSet;

    use super::*;

    // Convenience macros
    #[macro_use]
    mod macros {
        macro_rules! sty {
            ($a:ty) => {
                StableTypeId::of::<$a>()
            };
        }
    }

    #[derive(Debug)]
    struct Name(&'static str);
    impl Component for Name {}
    
    #[derive(Debug)]
    struct Health(u32);
    impl Component for Health {}

    #[allow(unused_variables)]
    #[test]
    fn test_macros() {
        // expected behavior: returns a ComponentType unique to the type it recieves
        let id = component!(u64);

        // expected behavior: returns an iterator over all health and velocity components which belong to the same entity
        let select = select!(Vel, Health);
    }

    #[test]
    fn test_stable_ids() {
        let stable_type_ids = vec![
            sty!(i8), sty!(i16), sty!(i32), sty!(i64), sty!(i128),
            sty!(u8), sty!(u16), sty!(u32), sty!(u64), sty!(u128),
            sty!(str), sty!(String)
        ];

        let mut unique = HashSet::new();
        assert!(stable_type_ids.into_iter().all(|e| unique.insert(e)))
    }

    #[test]
    fn test_create_database() {
        let mut database = EntityDatabase::new();
        database.register_component::<Name>();
        database.register_component::<Health>();

        let one = database.create();
        database.add_component(one, Health(100));
        database.add_component(one, Name("thing one"));

        let two = database.create();
        database.add_component(two, Health(200));
        database.add_component(two, Name("thing two"));
        

        dbg!(&database);
        ////dbg!(&database.data);
    }
}
