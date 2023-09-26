use std::{
    fmt::{self, Debug, Display, Formatter},
    rc::Rc,
};

use super::objects::Object;

pub struct IteratorObject {
    pub iterator: Box<dyn Iterator<Item = Object>>,
}

impl Clone for IteratorObject {
    fn clone(&self) -> Self {
        // TODO: implement this
        unimplemented!()
    }
}

impl Iterator for IteratorObject {
    type Item = Object;
    fn next(&mut self) -> Option<Self::Item> {
        self.iterator.next()
    }
}

impl PartialEq for IteratorObject {
    fn eq(&self, other: &Self) -> bool {
        // TODO: possibly implement this
        false
    }
}

impl Debug for IteratorObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Iterator")
    }
}

impl TryFrom<Object> for IteratorObject {
    type Error = String;

    fn try_from(obj: Object) -> Result<Self, Self::Error> {
        match obj {
            Object::Iterator(iterator) => Ok(iterator),
            Object::Array(array) => Ok(IteratorObject {
                iterator: Box::new(array.into_iter()),
            }),
            Object::String(string) => {
                let string = string.to_string();
                Ok(IteratorObject {
                    iterator: Box::new(
                        string
                            .chars()
                            .map(|c| c.to_string().into())
                            .collect::<Vec<_>>()
                            .into_iter(),
                    ),
                })
            }

            Object::Range(start, end) => {
                if let (Object::Integer(start), Object::Integer(end)) =
                    (start.as_ref(), end.as_ref())
                {
                    Ok(IteratorObject {
                        iterator: Box::new((*start..*end).map(|i| i.into())),
                    })
                } else {
                    Err(format!("Expected integer range, got {}..{}", start, end))
                }
            }
            Object::RangeInclusive(start, end) => {
                if let (Object::Integer(start), Object::Integer(end)) =
                    (start.as_ref(), end.as_ref())
                {
                    Ok(IteratorObject {
                        iterator: Box::new((*start..=*end).map(|i| i.into())),
                    })
                } else {
                    Err(format!(
                        "Expected integer inclusive range, got {}..={}",
                        start, end
                    ))
                }
            }

            _ => Err(format!("Expected Iterator, got {}", obj)),
        }
    }
}

impl Display for IteratorObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Iterator")
    }
}
