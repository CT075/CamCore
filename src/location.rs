#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FilePosAnnot<T> {
    pub value: T,
    pub row: usize,
    pub col: usize,
}

impl<T> FilePosAnnot<T> {
    pub fn annot(value: T, row: usize, col: usize) -> Self {
        FilePosAnnot { value, row, col }
    }

    pub fn borrow_value(&self) -> &T {
        let FilePosAnnot {
            value,
            row: _,
            col: _,
        } = self;
        value
    }

    pub fn extract_value(self) -> T {
        let FilePosAnnot {
            value,
            row: _,
            col: _,
        } = self;
        value
    }

    pub fn map<F, T2>(self, f: F) -> FilePosAnnot<T2>
    where
        F: Fn(T) -> T2,
    {
        let FilePosAnnot { value, row, col } = self;
        FilePosAnnot {
            value: f(value),
            row,
            col,
        }
    }

    pub fn substitute<T2>(self, value: T2) -> FilePosAnnot<T2> {
        FilePosAnnot {
            value,
            row: self.row,
            col: self.col,
        }
    }

    pub fn at_location<T2>(&self, value: T2) -> FilePosAnnot<T2> {
        FilePosAnnot {
            value,
            row: self.row,
            col: self.col,
        }
    }
}

impl<T> FilePosAnnot<T>
where
    T: Copy,
{
    pub fn copy_value(&self) -> T {
        self.value
    }
}

pub enum Source<'a> {
    File(&'a str),
    Expansion(Box<Source<'a>>, &'a str),
    Included(Box<Source<'a>>, &'a str),
}

pub struct SourceAnnot<'a, T> {
    value: T,
    source: Source<'a>,
    row: usize,
    col: usize,
}
