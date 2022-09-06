pub mod generic;
pub mod hkt;

pub mod string_with_vars;
pub use string_with_vars::S as StringWithVars;

mod linked_list;
pub use linked_list::List as LinkedList;
