pub mod generic;
pub mod hkt;

pub enum Void {}

#[derive(Eq, PartialEq, Debug)]
pub struct Identifier(pub String);
impl Identifier {
    pub fn to_string(self) -> String {
        let Identifier(s) = self;
        return s;
    }
}
