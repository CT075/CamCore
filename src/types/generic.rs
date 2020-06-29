use std::collections::binary_heap::BinaryHeap;

pub struct PriorityStack<T> {
    prev: u32,
    heap: BinaryHeap<(u32, T)>,
}

impl<T: Ord> PriorityStack<T> {
    pub fn new() -> Self {
        PriorityStack {
            heap: BinaryHeap::new(),
            prev: 0,
        }
    }

    pub fn push(&mut self, item: T) {
        self.heap.push((self.prev, item));
        self.prev = self.prev + 1;
    }

    pub fn peek(&self) -> Option<&T> {
        self.heap.peek().map(|t| &t.1)
    }

    pub fn pop(&mut self) -> Option<T> {
        self.heap.pop().map(|t| t.1)
    }

    pub fn into_sorted_vec(self) -> Vec<T> {
        self.heap
            .into_sorted_vec()
            .into_iter()
            .map(|t| t.1)
            .collect()
    }
}
