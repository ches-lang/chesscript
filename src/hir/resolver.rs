use super::{Hir, HirItem, HirExpression};

#[derive(Clone, Debug, PartialEq)]
pub enum CollectedChild {
    Block(CollectedBlock),
    Identifier(CollectedIdentifier),
}

impl From<CollectedBlock> for CollectedChild {
    fn from(v: CollectedBlock) -> Self {
        CollectedChild::Block(v)
    }
}

impl From<CollectedIdentifier> for CollectedChild {
    fn from(v: CollectedIdentifier) -> Self {
        CollectedChild::Identifier(v)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct CollectedBlock {
    pub id: CollectedIdentifier,
    pub children: Vec<CollectedChild>,
}

impl CollectedBlock {
    pub fn new(id: CollectedIdentifier) -> Self {
        CollectedBlock {
            id: id,
            children: Vec::new(),
        }
    }

    pub fn add_child(&mut self, child: CollectedChild) {
        self.children.push(child);
    }

    pub fn append_children(&mut self, mut children: Vec<CollectedChild>) {
        self.children.append(&mut children);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum IdentifierKind {
    Package,
    Module,
    Function,
    Variable,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CollectedIdentifier {
    pub kind: IdentifierKind,
    pub id: String,
}

impl CollectedIdentifier {
    pub fn new(kind: IdentifierKind, id: &str) -> Self {
        CollectedIdentifier {
            kind: kind,
            id: id.to_string(),
        }
    }
}

pub struct IdentifierCollector;

impl IdentifierCollector {
    pub fn new() -> Self {
        IdentifierCollector {}
    }

    pub fn collect(mut self, parent_id: CollectedIdentifier, hir: &Hir) -> CollectedBlock {
        let mut root = CollectedBlock::new(parent_id);

        for each_child in &hir.items {
            self.item(&mut root, each_child);
        }

        root
    }

    pub(crate) fn item(&mut self, parent: &mut CollectedBlock, item: &HirItem) {
        match item {
            HirItem::Module(module) => {
                let id = CollectedIdentifier::new(IdentifierKind::Module, &module.id);
                let mut new_block = CollectedBlock::new(id);

                for each_item in &module.items {
                    self.item(&mut new_block, each_item);
                }

                parent.add_child(new_block.into());
            },
            HirItem::Function(function) => {
                let id = CollectedIdentifier::new(IdentifierKind::Function, &function.id);
                let mut new_block = CollectedBlock::new(id);

                for each_arg in &function.args {
                    let new_id = CollectedIdentifier::new(IdentifierKind::Variable, &each_arg.id);
                    new_block.add_child(new_id.into());
                }

                for each_expr in &function.exprs {
                    self.expr(&mut new_block, each_expr);
                }

                parent.add_child(new_block.into());
            }
        }
    }

    pub(crate) fn expr(&mut self, parent: &mut CollectedBlock, expr: &HirExpression) {
        match expr {
            HirExpression::Chain(exprs) => {
                for each_expr in exprs {
                    if let Some(each_expr) = each_expr {
                        self.expr(parent, each_expr);
                    }
                }
            },
            _ => (),
        }
    }
}
