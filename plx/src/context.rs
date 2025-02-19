use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    UnknownIdent(String),
    DuplicateIdent(String)
}

#[derive(Clone, Debug, PartialEq)]
pub struct Context<T> {
    pub bindings: HashMap<String, T>
}

impl<T> Context<T> {
    pub fn new() -> Self {
        Self { bindings: HashMap::new() }
    }

    pub fn bind(&mut self, k: String, v: T) -> Option<T> {
        self.bindings.insert(k, v)
    }

    pub fn bind_unique(&mut self, k: String, v: T) -> Result<(), Error> {
        match self.bindings.get(&k) {
            Some(_) => Err(Error::DuplicateIdent(k)),
            None => {
                self.bind(k, v);
                Ok(())
            }
        }
    }

    pub fn lookup(&self, k: &String) -> Result<&T, Error> {
        self.bindings.get(k)
            .ok_or(Error::DuplicateIdent(k.clone()))
    }
}