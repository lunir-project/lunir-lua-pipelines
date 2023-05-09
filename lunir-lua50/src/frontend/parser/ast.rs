use tinyvec::TinyVec;

pub type Block = Vec<Node>;

#[derive(Clone, Default, Debug)]
pub struct Node {
    pub(crate) children: Vec<Node>,
    pub(crate) data: Option<NodeData>,
    pub(crate) kind: NodeKind,
}

#[derive(Clone, Debug)]
pub enum NodeKind {
    Boolean,
    Function,
    Identifier,
    KeyValuePair,
    Number,
    Root,
    String,
    VariableDeclaration,
    Table,
    UnaryOperation,
    BinaryOperation,
}

impl Default for NodeKind {
    fn default() -> Self {
        Self::Root
    }
}

#[derive(Clone, Debug)]
pub enum UnaryOperator {
    Negate,
    Not,
}

#[derive(Clone, Debug)]
pub enum BinaryOperator {
    Add,
    Concat,
    Divide,
    Exponentiate,
    Multiply,
    Subtract,
}

#[derive(Clone, Debug)]
pub enum NodeData {
    Boolean(bool),
    Function(Box<FunctionData>),
    Identifier(Box<String>),
    KeyValuePair(Box<KeyValuePair>),
    Locality(Locality),
    Number(f64),
    String(Box<String>),
    UnaryOperation(UnaryOperator),
    BinaryOperation(BinaryOperator),
}

#[derive(Clone, Debug)]
pub struct KeyValuePair(Node, Node);

#[derive(Clone, Debug)]
pub enum Locality {
    Global,
    Local,
}

#[derive(Clone, Debug)]
pub struct VariableDeclarationData {
    initialized_to: Node,
    locality: Locality,
    name: String,
}

#[derive(Clone, Debug)]
pub struct FunctionData {
    pub(crate) name: Option<String>,
    pub(crate) args: TinyVec<[String; 3]>,
}
