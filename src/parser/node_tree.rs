pub trait Node {
    fn left(&self) -> &Box<dyn Node>;
    fn right(&self) -> &Box<dyn Node>;
}

type BoxedNodeRef = Box<dyn Node>;

pub struct Add {
    left: BoxedNodeRef,
    right: BoxedNodeRef,
}

impl Add {
    pub fn new(left: BoxedNodeRef, right: BoxedNodeRef) -> Self {
        Self { left, right }
    }
}

impl Node for Add {
    fn left(&self) -> &BoxedNodeRef {
        &self.left
    }

    fn right(&self) -> &BoxedNodeRef {
        &self.right
    }
}

