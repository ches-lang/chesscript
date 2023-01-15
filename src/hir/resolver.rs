use super::*;

#[derive(Clone, Debug, PartialEq)]
pub enum IdentifierKind {
    Package,
    Module,
    Function,
    Variable,
}

#[derive(Clone, Debug, PartialEq)]
pub enum IdentifierResolution {
    Unresolved,
    Resolved(IdentifierIndex),
}

#[derive(Clone, Debug, PartialEq)]
pub enum NestIdentifier {
    Named(Box<IdentifierDefinition>),
    // fix: add Unnamed(SourceRange)
}

/* Identifier Collection */

#[derive(Clone, Debug, PartialEq)]
pub struct IdentifierDefinition {
    pub index: IdentifierIndex,
    pub kind: IdentifierKind,
    pub parent: Option<IdentifierIndex>,
    pub id: String,
}

#[derive(Debug, PartialEq)]
pub struct IdentifierMap {
    // fix: IdentifierDefinition to NestIdentifier?
    pub(crate) ids: Vec<Box<IdentifierDefinition>>,
}

impl IdentifierMap {
    pub fn new() -> Self {
        Self {
            ids: Vec::new(),
        }
    }

    pub fn add(&mut self, id: Box<IdentifierDefinition>) {
        self.ids.push(id);
    }
}

pub type IdentifierIndex = usize;

#[derive(Clone, Debug, PartialEq)]
pub struct IdentifierIndexCounter {
    index: IdentifierIndex,
}

impl IdentifierIndexCounter {
    pub fn new() -> Self {
        Self {
            index: 0,
        }
    }

    pub fn last(&self) -> Option<IdentifierIndex> {
        if self.index == 0 {
            None
        } else {
            Some(self.index)
        }
    }

    pub fn count_up(&mut self) -> IdentifierIndex {
        self.index += 1;
        self.index
    }
}

#[derive(Debug, PartialEq)]
pub struct IdentifierCollector {
    index_counter: IdentifierIndexCounter,
    pub map: IdentifierMap,
    pub tree: IdentifierTree,
    hierarchy: Vec<IdentifierIndex>,
}

impl IdentifierCollector {
    pub fn new() -> Self {
        Self {
            index_counter: IdentifierIndexCounter::new(),
            map: IdentifierMap::new(),
            tree: IdentifierTree::new(),
            hierarchy: Vec::new(),
        }
    }

    fn generate_identifier(&mut self, kind: IdentifierKind, id: &str) -> IdentifierDefinition {
        let parent = self.hierarchy.last().copied();
        let index = self.index_counter.count_up();
        self.hierarchy.push(index);

        IdentifierDefinition {
            index: index,
            kind: kind,
            parent: parent,
            id: id.to_string(),
        }
    }

    fn generate_nest(&mut self, kind: IdentifierKind, id: &str) -> Nest {
        let parent = self.hierarchy.last().copied();
        let index = self.index_counter.count_up();
        self.hierarchy.push(index);

        let id = IdentifierDefinition {
            index: index,
            kind: kind,
            parent: parent,
            id: id.to_string(),
        };

        Nest {
            id: NestIdentifier::Named(Box::new(id)),
            children: Vec::new(),
        }
    }

    fn add_nest_child(&mut self, id: &mut HirIdentifier, parent: &mut Nest, target: NestChild) {
        match target {
            NestChild::Nest(_) => if let Some(last_index) = self.index_counter.last() {
                self.hierarchy.push(last_index);
            },
            _ => (),
        }

        let target_id = match &target {
            NestChild::Nest(nest) => match &nest.id {
                NestIdentifier::Named(id) => Some(id.clone()),
            },
            NestChild::Identifier(id) => Some(id.clone()),
        };

        if let Some(target_id) = target_id {
            id.resolve(target_id.index);
            self.map.add(target_id);
        }

        parent.children.push(target);
    }

    fn exit_nest(&mut self) {
        self.hierarchy.pop();
    }

    pub fn collect(&mut self, package_name: &str, hir: &mut Hir) {
        let mut root = self.generate_nest(IdentifierKind::Package, &package_name);

        for each_item in &mut hir.items {
            self.item(&mut root, each_item);
        }

        self.exit_nest();

        match &root.id {
            NestIdentifier::Named(id) => self.map.add(id.clone()),
        }

        self.tree.packages.push(root);
    }

    pub(crate) fn item(&mut self, parent: &mut Nest, item: &mut HirItem) {
        match item {
            HirItem::Module(module) => {
                let mut new_nest = self.generate_nest(IdentifierKind::Module, &module.id.id);

                for each_item in &mut module.items {
                    self.item(&mut new_nest, each_item);
                }

                self.exit_nest();
                self.add_nest_child(&mut module.id, parent, new_nest.into());
            },
            HirItem::Function(function) => {
                let mut new_nest = self.generate_nest(IdentifierKind::Function, &function.id.id);

                for each_arg in &mut function.args {
                    let new_id = self.generate_identifier(IdentifierKind::Variable, &each_arg.id.id);
                    self.add_nest_child(&mut each_arg.id, &mut new_nest, new_id.into());
                }

                for each_expr in &mut function.exprs {
                    self.expression(&mut new_nest, each_expr);
                }

                self.exit_nest();
                self.add_nest_child(&mut function.id, parent, new_nest.into());
            }
        }
    }

    pub(crate) fn expression(&mut self, parent: &mut Nest, expr: &mut HirExpression) {
        match expr {
            HirExpression::Chain(exprs) => {
                for each_expr in exprs {
                    if let Some(each_expr) = each_expr {
                        self.expression(parent, each_expr);
                    }
                }
            },
            _ => (),
        }
    }
}

/* Identifier Indexing */

#[derive(Clone, Debug, PartialEq)]
pub enum NestChild {
    Identifier(Box<IdentifierDefinition>),
    Nest(Nest),
}

impl From<IdentifierDefinition> for NestChild {
    fn from(value: IdentifierDefinition) -> Self {
        Self::Identifier(Box::new(value))
    }
}

impl From<Nest> for NestChild {
    fn from(value: Nest) -> Self {
        Self::Nest(value)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Nest {
    pub id: NestIdentifier,
    pub children: Vec<NestChild>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IdentifierTree {
    pub packages: Vec<Nest>,
}

impl IdentifierTree {
    pub fn new() -> Self {
        Self {
            packages: Vec::new(),
        }
    }
}
