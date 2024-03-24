use std::fmt::{Debug, Display};

use uuid::Uuid;

#[derive(Clone)]
pub struct Reference {
    pub uuid: Uuid,
}

impl Display for Reference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "&{}",
            self.uuid
                .hyphenated()
                .encode_lower(&mut Uuid::encode_buffer())
        )
    }
}

impl PartialEq for Reference {
    fn eq(&self, other: &Self) -> bool {
        self.uuid == other.uuid
    }
}

impl Debug for Reference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "&{}", self.uuid)
    }
}
