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
                ComponentTypeSet(__set)
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

/// A `ComponentId` uniquely identifies a single component within a column of components
/// 
/// These are functionally the same as `EntityId`'s
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)] struct ComponentId(IdUnion);

/// A `FamilyId` uniquely identifies a single family
/// 
/// These are functionally the same as `EntityId`'s
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)] struct FamilyId(IdUnion);

/// A `FamilyGraphEdge` describes how to transfer an entity between families given a new or removed component
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct FamilyGraphEdge { component: ComponentType, add: Option<FamilyId>, remove: Option<FamilyId> }

type TableColumnEntry = HashMap<ComponentType, ComponentColumnEntry>;

/// Stores raw component data of a single type, describes how to interact with that data
struct ComponentColumnEntry {
    data: Box<dyn Any>, // ComponentColumn<T>
    mvfn: fn(&EntityId, &mut Box<dyn Any>, &mut Box<dyn Any>),
}

#[derive(Debug)]
struct DataTableGuard<'a>(MutexGuard<'a, TableColumnEntry>);

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
#[derive(Default, Debug)] struct DataTable(Arc<Mutex<TableColumnEntry>>);

//trait MoveableAny: Any + Debug {
//    fn as_mut_any<'a>(&'a mut self) -> &'a mut dyn Any;
//    fn as_any<'a>(&'a self) -> &'a dyn Any;
//    fn move_to(&mut self, to: &mut Box<dyn Any>, index: EntityId);
//}

pub trait Component: Debug {}

/// A `ComponentColumn<T>` is the raw typed storage for a single component type in a `DataTable`
/// 
/// Columns are indexed by `ComponentType`'s, and rows are indexed by `EntityId`'s
struct ComponentColumn<T: Any + Debug + 'static>(HashMap<EntityId, T>);

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

/// A set of unique component types
#[derive(Clone, Default, PartialEq, Eq, Hash, Debug)] struct ComponentTypeSet(BTreeSet<ComponentType>);

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

    pub fn add_component<T: Component + 'static>(&mut self, entity: EntityId, component: T) {
        let component_type = component!(T);

        // check if this entity has a family already (e.g., does it have any components?)
        let (new_family_id, old_family_id) = if let Some(family_id) = self.records.entities.get(&entity).cloned() {
            // it does have a family, meaning it has existing components
            let family_components = &self.data.families.get(&family_id).expect("expected existing family").components;
            
            // check if the component we are trying to add is already part of the entity, if so, we can just replace the component it
            let (new_family_id, old_family_id) = if family_components.contains(&component_type) {
                (family_id, family_id)
            } else {
                // the component is not already part of the entity, we need to find its new family
                // we will also need to remove it from its old family
                let target_set = family_components.iter().cloned().chain([component_type]).collect();
                let target_id = self.get_or_register_family_id(target_set);
                let transfer_graph = &mut self.data.families.get_mut(&family_id).expect("expected existing family").transfer_graph;
                
                let transfer_edge = transfer_graph
                    .entry(component_type)
                    .or_insert_with(|| {
                        FamilyGraphEdge { component: component_type, add: Some(target_id), remove: None }
                    }
                );
                
                let new_family_id = transfer_edge.add.expect("expected transfer graph edge");
                
                // we have the new target family, remove it from the old family
                let _removed = self.records.entities.remove(&entity);
                self.records.entities.insert(entity, new_family_id);

                (new_family_id, family_id)
            };
            
            (new_family_id, old_family_id)
        } else {
            let set = component_type_set!(T);

            // it doesn't exist yet, this means the entity has no components, lets get or
            // create the family which corresponds to a single component of type `T` and add the entity to it
            let family_id = self.get_or_register_family_id(set.clone());
            self.records.entities.insert(entity, family_id);

            // create the actual family data if necessary, including an empty transfer graph
            // do nothing if it already exists - we do not want to destroy an existing transfer graph
            self.data.families
                .entry(family_id)
                .or_insert(Family { components: set, transfer_graph: Default::default() });

            (family_id, family_id)
        };
        
        {
            // get or create the data table associated with the entity family
            let mut table = self.data.tables
                .entry(new_family_id)
                .or_insert(Default::default())
                .lock();

            // get the component column for the component type from the data table or create it
            let column = table
                .entry(component_type)
                .or_insert(ComponentColumnEntry {
                    data: Box::new(ComponentColumn::<T>::new()),
                    mvfn: |row, a, b| {
                        dbg!((*a).type_id(), (*b).type_id());
                        dbg!((*a).is::<ComponentColumn<T>>(), (*b).is::<ComponentColumn<T>>());
                        dbg!(TypeId::of::<ComponentColumn<T>>());
                        dbg!(TypeId::of::<Box<dyn Any>>());

                        let from = a.downcast_mut::<ComponentColumn<T>>().expect("expect from");
                        let dest = b.downcast_mut::<ComponentColumn<T>>().expect("expect dest");
                        match from.remove(row) {
                            Some(item) => { dest.insert(*row, item); },
                            None => { #[cfg(Debug)] println!("no row value to move"); },
                        }
                    },
                });

            // insert the new component for the entity in the table column
            column.insert(entity, component);
        }
        
        {
            // the entity has moved, we need to copy all of its other components from its old data table to its new one
            if new_family_id != old_family_id {
                let mut to = self.data.tables.get(&new_family_id).expect("expected new data table").lock();
                let mut from = self.data.tables.get(&old_family_id).expect("expected old data table").lock();
                
                from.move_row(&mut to, &entity);
            }
        }
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

        self.records.component_sets.insert(set, id);

        id
    }

    fn add_unit_component(&mut self, id: EntityId) {
        let family = self.get_or_register_family_id(component_type_set!( () ));
        self.records.entities.insert(id, family);
    }

    fn get_or_register_family_id(&mut self, set: ComponentTypeSet) -> FamilyId {
        let id = if let Some(id) = self.records.family_id_for(set.clone()) {
            id
        } else {
            self.register_family(set.clone())
        };
        id
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
    fn insert<T: Debug + Any + 'static>(&mut self, key: EntityId, val: T) -> Option<T> {
        let column = self.data.downcast_mut::<ComponentColumn<T>>().unwrap();
        column.insert(key, val)
    }

    fn move_row_val(&mut self, row: &EntityId, dest: &mut Self) {
        (self.mvfn)(row, &mut self.data, &mut dest.data)
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
    fn move_row(&mut self, to: &mut DataTableGuard, row: &EntityId) {
        for (from, dest) 
        in self.0.iter_mut().zip(to.0.iter_mut()) {
            dbg!(&from, &dest);
            from.1.move_row_val(row, dest.1);
        }
    }
}

impl<'a> Deref for DataTableGuard<'a> {
    type Target = TableColumnEntry;

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
            Err(err) => panic!("unable to lock data table for reading: {}", err),
        }
    }
}

impl Deref for DataTable {
    type Target = Arc<Mutex<TableColumnEntry>>;

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
impl<T: Debug + 'static> ComponentColumn<T> {
    fn new() -> Self {
        ComponentColumn(Default::default())
    }
}

//impl<T: Debug + 'static> MoveableAny for ComponentColumn<T> {
//    fn as_any<'a>(&'a self) -> &'a dyn Any {
//        self
//    }
//
//    fn as_mut_any(&mut self) -> &mut dyn Any {
//        self
//    }
//
//    fn move_to(&mut self, to: &mut Box<dyn Any>, index: EntityId) {
//        let column_from = self
//            .as_mut_any()
//            .downcast_mut::<ComponentColumn<T>>()
//            .expect("expected matching component columns");
//        
//        // unsafe transmute
//        // need a lot more guard rails around this
//        let column_to = unsafe {
//            dbg!(std::mem::transmute::<&mut Box<dyn Any>, &mut Box<ComponentColumn<T>>>(to))
//        };
//
//        #[cfg(Debug)] println!("moving component {:?} from column {:?} to column {:?}", index, from, to);
//
//        let item = match column_from.entry(index) {
//            Entry::Occupied(occupied) => occupied.remove_entry(),
//            Entry::Vacant(_) => panic!("expected valid entry, where did it go?"),
//        };
//        
//        match column_to.entry(index) {
//            Entry::Occupied(_) => panic!("component slot was already occupied"),
//            Entry::Vacant(vacant) => {
//                vacant.insert(item.1);
//            },
//        }
//    }
//}

impl<T: Debug + Any + 'static> Debug for ComponentColumn<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ComponentColumn").field("data", &self).finish()
    }
}

impl<T: Debug + Any + 'static> Deref for ComponentColumn<T> {
    type Target = HashMap<EntityId, T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Debug + Any + 'static> DerefMut for ComponentColumn<T> {
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
impl FromIterator<ComponentType> for ComponentTypeSet {
    fn from_iter<T: IntoIterator<Item = ComponentType>>(iter: T) -> Self {
        ComponentTypeSet(BTreeSet::from_iter(iter))
    }
}

impl Deref for ComponentTypeSet {
    type Target = BTreeSet<ComponentType>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for ComponentTypeSet {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
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

        let entity = database.create();
        database.add_component(entity, Health(100));
        database.add_component(entity, Name("thing one"));

        dbg!(&database);
    }
}
