use std::{collections::HashMap};
use serde::{Serialize, Deserialize};
use hadron::unique::UniqueId;


/// A database of all fragment kinds that exist in the world
/// 
/// A fragment represents an instance of a loosely defined concept, a fleeting image of an idea, tied to a thing
/// On their own, fragments do nothing but attach themselves to things in the world. Their importance comes
/// from how the systems of the world interpret the presence or absence of any given fragment or combination thereof
struct Fragmnets {
    // name <-> uid
    names: HashMap<String, UniqueId>,
}

impl Fragmnets {
    fn get(&mut self, name: String) -> Fragment {
        let name = name.trim().to_lowercase();

        if let Some(uid) = self.names.get(name.as_str()) {
            Fragment(*uid)
        } else {
            let uid = UniqueId::get();
            self.names.insert(name, uid);
            Fragment(uid)
        }
    }
}

/// An opaque fragment
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Fragment(UniqueId);

