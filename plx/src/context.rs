use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub struct Context<T> {
    pub bindings: HashMap<String, T>
}

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    DuplicateIdent(String),
    UnknownIdent(String)
}

impl<T> Context<T> {
    pub fn new() -> Self {
        Self { bindings: HashMap::new() }
    }

    pub fn lookup(&self, ident: &String) -> Result<&T, Error> {
        self.bindings.get(ident)
            .ok_or(Error::UnknownIdent(ident.clone()))
    }

    pub fn remove(&mut self, ident: &String) -> Option<T> {
        self.bindings.remove(ident)
    }

    pub fn bind(&mut self, ident: String, value: T) -> Option<T> {
        self.bindings.insert(ident, value)
    }

    pub fn bind_unique(&mut self, ident: String, value: T) -> Result<(), Error> {
        match self.bindings.get(&ident) {
            Some(_) => Err(Error::DuplicateIdent(ident)),
            None => {
                self.bind(ident, value);
                Ok(())
            }
        }
    }
}