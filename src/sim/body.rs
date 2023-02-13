use std::{collections::HashMap, any::{Any, TypeId}, ops::{Index, IndexMut}};

use hadron::unique::UniqueId;
/*  begin comment


/// Describes the physical and physiological forms of units
/// 
/// - Every part of a unit is connected to another. Each part gives some capability to the entire body
///  - Total body capabilities are recalculated whenever the parts of the body are changed, these are used for simulation
///  - Body parts can have requirements, and those requirements can be met by other body parts
///  - Need to have a slow, traversable, model representation of each unit, and a fast, queryable, action representation

struct BodyPart {
    name: &'static str,
}

impl BodyPart {
    fn named(name: &'static str) -> Self {
        BodyPart { name: name, }
    }
}

struct BodyPartInternal {
    attached_to: Option<usize>,
    inside_of: Option<usize>,
    data: BodyPart,
}

struct Body {
    parts: Vec<BodyPartInternal>,
    map: HashMap<&'static str, usize>,
}

impl Body {
    fn from_part(part: BodyPart) -> Self {
        let mut body = Body {
            parts: Vec::new(),
            map: HashMap::new(),
        };
        body.map.insert(part.name, body.parts.len());
        body.parts.push(BodyPartInternal {
            attached_to: None,
            inside_of: None,
            data: part,
        });
        body
    }

    fn add_body_part_to(&mut self, to: &'static str, part: BodyPart) -> Result<(), ()> {
        Err(())
    }

    fn add_body_part_inside_of(&mut self, inside: &'static str, part: BodyPart) -> Result<(), ()> {
        Err(())
    }
}

// Blood: Average adult 150-180lbs = 4500 - 5700mL blood
// A body part can be inside another body part - heart inside chest, everything inside shell, etc
// All body parts are tied into the armor system. When they receive damage, there is a chance they change state based on the damage
// A body can be attached to another body
// perhaps bodies can be efficiently packed into a compact data structure to eliminate allocations/mem access

enum Material {
    Bone,
    SoftTissue,

}

#[test]
fn assemble_body() {
    let torso = BodyPart {
        name: "torso",
    };

    let mut human = Body::from_part(torso);
    // torso
    //human.add_body_part_inside_of("torso", BodyPart {
    //    name: "spine",
    //    material: Material::Bone,
    //});
    
    // head
    human.add_body_part_to("torso", BodyPart { name: "neck" }).unwrap();
    human.add_body_part_to("neck", BodyPart { name: "head" }).unwrap();
    human.add_body_part_to("head", BodyPart { name: "left ear" }).unwrap();
    human.add_body_part_to("head", BodyPart { name: "right ear" }).unwrap();
    human.add_body_part_to("head", BodyPart { name: "left eye" }).unwrap();
    human.add_body_part_to("head", BodyPart { name: "right eye" }).unwrap();
    human.add_body_part_to("head", BodyPart { name: "nose" }).unwrap();
    human.add_body_part_to("head", BodyPart { name: "mouth" }).unwrap();

    // left arm
    human.add_body_part_to("torso", BodyPart { name: "left upper arm" }).unwrap();
    human.add_body_part_to("left upper arm", BodyPart { name: "left lower arm" }).unwrap();
    human.add_body_part_to("left lower arm", BodyPart { name: "left wrist" }).unwrap();
    human.add_body_part_to("left wrist", BodyPart { name: "left hand" }).unwrap();
    human.add_body_part_to("left hand", BodyPart { name: "left little finger" }).unwrap();
    human.add_body_part_to("left hand", BodyPart { name: "left ring finger" }).unwrap();
    human.add_body_part_to("left hand", BodyPart { name: "left middle finger" }).unwrap();
    human.add_body_part_to("left hand", BodyPart { name: "left index finger" }).unwrap();
    human.add_body_part_to("left hand", BodyPart { name: "left thumb" }).unwrap();
    
    // right arm
    human.add_body_part_to("torso", BodyPart { name: "right upper arm" }).unwrap();
    human.add_body_part_to("right upper arm", BodyPart { name: "right lower arm" }).unwrap();
}

trait WorldComponentStorage<'a> {
    type Component;

    fn empty() -> Self;
    fn get(&'a self, id: UniqueId) -> Option<&'a Self::Component>;
    fn get_mut(&'a mut self, id: UniqueId) -> Option<&'a mut Self::Component>;
    unsafe fn get_unchecked(&'a self, id: UniqueId) -> &'a Self::Component { unimplemented!() }

    fn insert(&mut self, id: UniqueId, component: Self::Component) -> Result<(), Self::Component>;
}

#[derive(Debug)]
struct PackedComponentStorage<T>(Vec<UniqueId>, Vec<T>);

impl<T> PackedComponentStorage<T> {
    fn resize(&mut self) {
        let (keys, data) = (self.0, self.1);
        
        debug_assert!(keys.len() == data.len());
        let current_size = keys.len();
        let new_size = std::cmp::max(1usize, current_size * 2);
        let difference = new_size - current_size;

        keys.append(&mut (current_size-1..new_size-1).collect::<Vec<_>>());
        
        data.reserve(new_size);
        for _ in 0..difference {
            data.push(None)
        }
    }
}

impl<'a, T: Clone + Default> WorldComponentStorage<'a> for PackedComponentStorage<T> {
    type Component = T;

    fn empty() -> Self {
        PackedComponentStorage(Vec::new(), Vec::new())
    }

    fn get(&'a self, id: UniqueId) -> Option<&'a T> {
        let keys = self.0;
        let data = self.1;

        match id.index() {
            Some(index) => {
                match data.get(index) {
                    Some(item) => return Some(item),
                    None => return None,
                }
            },
            None => return None,
        }
    }

    fn get_mut(&'a mut self, id: UniqueId) -> Option<&'a mut T> {
        match id.index() {
            Some(index) => {
                match self.1.get_mut(index) {
                    Some(item) => return Some(item),
                    None => return None,
                }
            },
            None => return None,
        }
    }

    unsafe fn get_unchecked(&'a self, id: UniqueId) -> &'a T {
        unsafe {
            self.1.get_unchecked(id.index_unchecked() as usize)
        }
    }

    fn insert(&mut self, id: UniqueId, component: Self::Component) -> Result<(), Self::Component> {
        let keys = self.0;
        let data = self.1;
        
        let pub_index = match id.index() {
            Some(index) => index,
            None => return Err(component),
        };

        let priv_index = data.len();
        *keys.get_mut(pub_index).unwrap() = priv_index;



        match id.index() {
            Some(id) => {
                if id > keys.len() {
                    self.resize()
                }
        
                match keys.get(id) {
                    Some(index) => {
                        
                    },
                    None => todo!(),
                }
            },
            None => todo!(),
        }
        
    }
}

#[derive(Debug)]
struct SparseComponentStorage<T>(HashMap<UniqueId, T>);

impl<'a, T> WorldComponentStorage<'a> for SparseComponentStorage<T> {
    type Component = T;

    fn empty() -> Self {
        SparseComponentStorage(HashMap::new())
    }

    fn get(&'a self, id: UniqueId) -> Option<&'a T> {
        self.0.get(&id)
    }

    fn get_mut(&'a mut self, id: UniqueId) -> Option<&'a mut T> {
        self.0.get_mut(&id)
    }
}

#[derive(Debug)]
struct Components {
    components: HashMap<TypeId, Box<dyn Any>>,
}

impl<'a> Components {
    fn new() -> Self {
        Components {
            components: HashMap::new()
        }
    }
    
    fn register<T, S: WorldComponentStorage<'a> + 'static>(&mut self) {
        let key = TypeId::of::<S>();
        let val = S::empty();
        self.components.insert(key, Box::new(val));
    }

    fn get<S: WorldComponentStorage<'a> + 'static>(&self) -> Result<&S, ()> {
        let typeid = TypeId::of::<S>();
        let dynamic_storage = self.components.get(&typeid).ok_or(())?;
        let component = dynamic_storage.downcast_ref::<S>().ok_or(())?;
        Ok(component)
    }

    fn get_mut<T, S: WorldComponentStorage<'a> + 'static>(&mut self) -> Result<&mut S, ()> {
        let typeid = TypeId::of::<S>();
        let dynamic_storage = self.components.get_mut(&typeid).ok_or(())?;
        let component = dynamic_storage.downcast_mut::<S>().ok_or(())?;
        Ok(component)
    }
}

mod tests {
    use super::*;

    #[derive(Debug)]
    struct TestComponent {
        string_field: &'static str,
        int_field: i32,
        option_field: Option<f32>,
    }
    
    #[test]
    fn test_element_storage() {
        //let mut world = World::default();
        //let mut chunk = Chunk::default();
        let mut components = Components::new();
        components.register::<f64, PackedComponentStorage<f64>>();
        components.register::<TestComponent, PackedComponentStorage<TestComponent>>();
        
        {
            let float_elems = components.get_mut::<f64, PackedComponentStorage<f64>>().unwrap();
            float_elems.0.append(&mut vec![0.0, 1.0, 1.0, 2.0, 3.0, 5.0]);
            let test_elems = components.get_mut::<TestComponent, PackedComponentStorage<TestComponent>>().unwrap();
            test_elems.0.append(&mut vec![
                TestComponent {
                    string_field: "first thing",
                    int_field: 42,
                    option_field: None,
                },
                TestComponent {
                    string_field: "and then",
                    int_field: 9999,
                    option_field: Some(3.1415),
                },
            ]);
        }

        {
            let float_elem = components.get::<PackedComponentStorage<f64>>().unwrap();
            let test_elem = components.get::<PackedComponentStorage<TestComponent>>().unwrap();
            println!("{:?}", float_elem);
            println!("{:#?}", test_elem);
        }
    }

    #[derive(Debug)]
    enum TestMaterial {
        SoftTissue,
        Bone,
    }

    #[derive(Debug)]
    enum TestBodyPartTrait {
        PumpsBlood(i16),
        RequiresBlood(i16),
        ProvidesConciousness(i16),
    }

    #[derive(Debug)]
    enum TestAiMood {
        Calm,
        Panicked,
    }

    #[derive(Debug)]
    struct TestAiComponent {
        mood: TestAiMood,
        task_progress: f32,
    }
    
    #[test]
    fn test_elements_create_body() {
        let mut components = Components::new();
        components.register::<TestMaterial, PackedComponentStorage<TestMaterial>>();
        components.register::<TestBodyPartTrait, PackedComponentStorage<TestBodyPartTrait>>();
        components.register::<TestAiComponent, SparseComponentStorage<TestAiComponent>>();

        println!("{:#?}", components);

        {
            let a = components.get::<PackedComponentStorage<TestMaterial>>().unwrap();
            let b = components.get::<PackedComponentStorage<TestBodyPartTrait>>().unwrap();
            let c = components.get::<SparseComponentStorage<TestAiComponent>>().unwrap();

            a.insert()
        {
            let a = components.get::<PackedComponentStorage<TestMaterial>>().unwrap();
            let b = components.get::<PackedComponentStorage<TestBodyPartTrait>>().unwrap();
            let c = components.get::<SparseComponentStorage<TestAiComponent>>().unwrap();
            
            println!("{:#?}", a);
            println!("{:#?}", b);
            println!("{:#?}", c);
        }
    }
}



#[derive(Default)]
struct World {
    chunks: GridVec<Chunk>,
}

#[derive(Default)]
struct Chunk(Option<Components>);

/// A compact vector indexed by x/y/z coordinates, useful for loading/unloading data on a grid like pattern
#[derive(Default)]
struct GridVec<T>(Vec<T>);

impl<T> Index<(isize, isize, isize)> for GridVec<T> {
    type Output = T;

    fn index(&self, index: (isize, isize, isize)) -> &Self::Output {
        todo!()
    }
}

impl<T> IndexMut<(isize, isize, isize)> for GridVec<T> {
    fn index_mut(&mut self, index: (isize, isize, isize)) -> &mut Self::Output {
        todo!()
    }
}


end comment */ 