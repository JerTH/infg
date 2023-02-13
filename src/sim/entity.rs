use std::{fmt::Debug, collections::HashMap, any::{Any, TypeId}, rc::Rc, cell::RefCell};

/// A common id type used to identify entities, their components, and relations
#[derive(Clone, Copy, Eq)]
union CommonId {
    entity: (u32, u16, u16),
    relation: (u32, u32),
    _raw_u64: u64,
}
pub type EntityId = CommonId;
pub type ComponentId = CommonId;
pub type ComponentTypeId = TypeId;
pub type ArchetypeId = usize;

trait Component: Any + Clone {}

//trait CheckedStorage {
//    type Component;
//
//    //fn get(&self) -> Option<&Self::Component>;
//    //fn get_mut(&mut self) -> Option<&mut Self::Component>;
//    //fn insert(&mut self, id: EntityId, item: Self::Component) -> Option<Self::Component>;
//    //fn remove(&mut self, id: EntityId) -> Option<Self::Component>;
//    fn move_component(id: &ComponentId, from: &mut Self, to: &mut Self);
//}

//trait ComponentStorage: Any {
//    fn move_to(&mut self, id: &ComponentId, other: &mut dyn Any);
//}
//
//struct FixedArrayStorage<T> {
//    data: [T; 128]
//}
//
//impl<T> ComponentStorage for FixedArrayStorage<T> {
//    fn move_to<'a>(&mut self, id: &ComponentId, other: &'a mut dyn Any) {
//        let index = id.entity.0 as usize;
//        match other.downcast_mut::<FixedArrayStorage<T>>() {
//            Some(other) => other.data[index] = self.data[index],
//            None => panic!("attempted to move data between storage of different types"),
//        }
//    }
//}
//
//impl<T> CheckedStorage<T> for FixedArrayStorage<T> {
//    fn get(&self) -> Option<&T> {
//        todo!()
//    }
//
//    fn get_mut(&mut self) -> Option<&mut T> {
//        todo!()
//    }
//
//    fn insert(&mut self, id: EntityId, item: T) -> Option<T> {
//        todo!()
//    }
//
//    fn remove(&mut self, id: EntityId) -> Option<T> {
//        todo!()
//    }
//}


#[derive(Debug, Clone)]
struct Storage<T: Component> {
    data: Vec<T>,
    life: Vec<usize>,
}

struct EntityAllocator {}

/// INVARIANT MUST BE SORTED
type ComponentList = Vec<ComponentTypeId>;

type ArchetypeMap = HashMap<ArchetypeId, ArchetypeRecord>;

#[derive(Debug)]
struct EntityRecord {
    archetype: ArchetypeId,
    index: usize,
}

/*
 *  Entities:
 *      Archetypes: Maps lists of components to archetype ids
 *      Entities: Maps entity id's to entity records
 *      Components: Maps component typeid's to an archetype map
 *      ArchetypeData: A list of archetypes indexed by archetype ids
 * 
 *  Archetype id's:
 *      usize: A pure index into the ArchetypeData vector
 * 
 *  Entity record:
 *      Archetype: An archetype id which indexes the archetype corresponding to the given entity
 *      Index: The index of each of the entities components in its corresponding archetype data
 * 
 *  Archetype map:
 *      Hashmap: maps an archetype id to an archetype record
 * 
 *  Archetype record:
 *      usize: Column index which directs to a component type in this archetype
 * 
 *  Archetype:
 *      id: usize: The archetype id of this archetype
 *      kind: A list of components which describe this archetype
 *      data: A vector where each element represents a column of components of arbitrary type
 *      graph: A map of component type ids to archetype edges
 * 
 *  Archetype edge:
 *      add: Archetype id: which archetype to move to when we add a given component
 *      rem: Archetype id: which archetype to move to when we remove a given component
 * 
 */

#[derive(Default)]
struct Entities {
    archetypes: HashMap<ComponentList, ArchetypeId>,
    entities: HashMap<EntityId, EntityRecord>,
    components: HashMap<ComponentTypeId, ArchetypeMap>,

    /// The primary owner of data driving the ECS
    archetype_data: Vec<Archetype>,
}

#[derive(Debug)]
struct Archetype {
    id: ArchetypeId,
    kind: ComponentList,

    /// The component storage for this archetype
    data: Vec<Box<dyn Any>>,

    /// Lazily constructed graph which produces a target archetype given this archetype
    /// and a component. By traversing this graph one can facilitate the fast addition
    /// of components to an entity
    graph: HashMap<ComponentTypeId, ArchetypeEdge>,
}

#[derive(Debug)]
struct ArchetypeEdge {
    add: ArchetypeId,
    rem: ArchetypeId,
}

#[derive(Debug)]
struct ArchetypeRecord {
    component_column: usize,
}

// Impl's

impl Entities {
    fn new() -> Self {
        Default::default()
    }

    fn archetype(&self, id: ArchetypeId) -> Option<&Archetype> {
        self.archetype_data.get(id)
    }

    /// Returns a reference to the archetype associated with a given entity
    /// 
    /// Returns `None` if the entity doesn't exist
    fn archetype_of(&self, entity: &EntityId) -> Option<ArchetypeId> {
        self.entities.get(&entity).map(|entity_record| entity_record.archetype)
    }

    /// Returns a map of archetypes which contain the component
    /// 
    /// Returns `None` if the component doesn't exist or if no archetypes have been
    /// created with the component
    pub fn archetypes_with(&self, component: &ComponentTypeId) -> Option<&ArchetypeMap> {
        self.components.get(component)
    }

    /// Returns a reference to the archetype associated with the given list of components
    pub fn archetype_from(&self, components: &ComponentList) -> ArchetypeId {
        todo!()
    }

    fn component_id_of<T: Component>(&self) -> ComponentTypeId {
        std::any::TypeId::of::<T>()
    }
    
    pub fn move_entity(&mut self, entity_index: &EntityId, from_archetype: ArchetypeId, to_archetype: ArchetypeId) {

    }
    
    pub fn add_component<T: Component>(&mut self, entity: &EntityId, component: T) {
        let log = hadron::debug::log::get();

        let component_id = self.component_id_of::<T>();
        if let Some(record) = self.entities.get(entity) {
            let archetype = self.archetype(record.archetype).expect("missing archetype");
            if let Some(next_archetype) = archetype.graph.get(&component_id) {

                self.move_entity(entity, archetype.id, next_archetype.add);

            } else {
                log.warn(format!("updating archetype graph for {:?} with component {:?}", archetype, component_id));
            }
        } else {
            log.warn(format!("attempted to add component {:?} but entity {:?} doesn't exist", component_id, entity));
        }
    }

    /// Tests if an entity has a component
    pub fn has_component<T: Component>(&self, entity: &EntityId, component: &ComponentTypeId) -> bool {
        if let Some(arch) = self.archetype_of(entity){
            if let Some(arch_set) = self.components.get(component) {
                return arch_set.get(&arch).is_some()
            }
        }
        return false;
    }

    /// Gets a reference to a component for a specific entity
    /// 
    /// Returns `None` if the entity doesn't exist, or the component doesn't exist,
    /// or the entity doesn't have an instance of the component
    pub fn get_component_for<T: Component>(&self, entity: &EntityId) -> Option<&T> {
        let component_id = self.component_id_of::<T>();
        if let Some(component_record) = self.entities.get(entity) {
            let archetype = self.archetype(component_record.archetype).expect("missing archetype");
            
            if let Some(k) = self.components.get(&component_id) {
                if let Some(archetype_record) = k.get(&archetype.id) {
                    let index = archetype_record.component_column;
                    
                    if let Some(dyn_components) = archetype.data.get(index) {
                        if let Some(components) = dyn_components.downcast_ref::<Storage<T>>() {
                            todo!() //return components.get().unwrap()
                        }
                    }
                }
            }
        }
        return None;
    }
}

mod tests {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn test_has_component() {
        let ents = Entities::new();
    }
}



// Std Impl's

impl std::hash::Hash for CommonId {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        unsafe { self._raw_u64.hash(state) }
    }
}

impl std::cmp::PartialEq for CommonId {
    fn eq(&self, other: &Self) -> bool {
        unsafe { std::cmp::PartialEq::eq(&self._raw_u64, &other._raw_u64) }
    }
}

impl std::cmp::PartialOrd for CommonId {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        unsafe { std::cmp::PartialOrd::partial_cmp(&self._raw_u64, &other._raw_u64) }
    }
}

impl std::cmp::Ord for CommonId {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        unsafe { std::cmp::Ord::cmp(&self._raw_u64, &other._raw_u64) }
    }
}

impl std::fmt::Debug for CommonId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe { write!(f, "ent({}, {}, {}) rel({}, {}) raw({})", self.entity.0, self.entity.1, self.entity.2, self.relation.0, self.relation.1, self._raw_u64) }
    }
}
