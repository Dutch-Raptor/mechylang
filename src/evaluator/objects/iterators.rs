use crate::Object;
use std::{
    fmt::{self, Debug, Display, Formatter},
    iter::Sum,
    ops::{Deref, DerefMut},
};

// We have to provide a special trait for our clonable iterator,
// since Clone requires a Sized type (so we can't call it on a trait object).
pub trait CloneIterator: Iterator {
    fn clone_box(&self) -> Box<dyn CloneIterator<Item = Self::Item>>;
}

// Implement our special trait for all Cloneable Iterators
impl<T> CloneIterator for T
where
    T: 'static + Iterator + Clone,
{
    fn clone_box(&self) -> Box<dyn CloneIterator<Item = Self::Item>> {
        Box::new(self.clone())
    }
}

pub struct IteratorObject {
    pub iterator: Box<dyn CloneIterator<Item = <IteratorObject as Iterator>::Item>>,
}

impl Clone for IteratorObject {
    fn clone(&self) -> Self {
        // TODO: implement this
        IteratorObject {
            iterator: self.iterator.clone_box(),
        }
    }
}

impl Iterator for IteratorObject {
    type Item = Object;
    fn next(&mut self) -> Option<Self::Item> {
        self.iterator.next()
    }
}

impl PartialEq for IteratorObject {
    fn eq(&self, _other: &Self) -> bool {
        // TODO: possibly implement this
        false
    }
}

impl Debug for IteratorObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Iterator")
    }
}

impl Deref for IteratorObject {
    type Target = dyn CloneIterator<Item = Object> + 'static;

    fn deref(&self) -> &Self::Target {
        self.iterator.as_ref()
    }
}

impl DerefMut for IteratorObject {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.iterator.as_mut()
    }
}

#[derive(Debug, PartialEq)]
pub enum IntoIteratorError {
    InvalidRange,
    NotAnIterable,
}

impl TryFrom<Object> for IteratorObject {
    type Error = IntoIteratorError;

    fn try_from(obj: Object) -> Result<Self, Self::Error> {
        match &obj {
            Object::Iterator(_) => Ok(obj.to_iterator().expect("obj to be matched as iterator")),
            Object::Array(_) => Ok(IteratorObject {
                iterator: Box::new(obj
                    .to_array()
                    .expect("obj to be matched as array")
                    .into_iter()
                ),
            }),
            Object::String(string) => {
                Ok(IteratorObject {
                    iterator: Box::new(
                        string.to_string()
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
                    Err(IntoIteratorError::InvalidRange)
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
                    Err(IntoIteratorError::InvalidRange)
                }
            }
            Object::RangeFrom(start) => {
                if let Object::Integer(start) = start.as_ref() {
                    Ok(IteratorObject {
                        iterator: Box::new((*start..).map(|i| i.into())),
                    })
                } else {
                    Err(IntoIteratorError::InvalidRange)
                }
            }

            _ => Err(IntoIteratorError::NotAnIterable),
        }
    }
}

impl Display for IteratorObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let size_hint = self.size_hint();
        match size_hint {
            (0, None) => write!(f, "Iterator of unknown size"),
            (lower, None) => write!(f, "Iterator ({} remaining)", lower),
            (lower, Some(upper)) => write!(f, "Iterator ({}..{} remaining)", lower, upper),
        }
    }
}

impl Sum for Object {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        // figure out the type of the iterator
        let mut iterator = iter.peekable();
        let first = iterator.peek().unwrap_or(&Object::Unit);

        // if the iterator is empty, return 0
        if first == &Object::Unit {
            return Object::Integer(0);
        }

        match first {
            Object::Integer(_) => iterator
                .fold(0, |acc, item| {
                    if let Object::Integer(int) = item {
                        acc + int
                    } else {
                        acc
                    }
                })
                .into(),
            Object::String(_) => iterator
                .fold(String::new(), |mut acc, item| {
                    if let Object::String(string) = item {
                        acc.push_str(&string);
                    }
                    acc
                })
                .into(),
            Object::Array(_) => iterator
                .fold(Vec::new(), |mut acc, item| {
                    if let Object::Array(array) = item {
                        acc.extend(array);
                    }
                    acc
                })
                .into(),
            _ => Object::Unit,
        }
    }
}
