use crate::{hir::{*, resolver::*}};
use speculate::speculate;

speculate!{
    before {
        let assert_ids = |mut hir: Hir, expected_root: Vec<NestChild>, mut expected_id_map: Vec<IdentifierDefinition>| {
            let package_name = "test";
            let mut collector = IdentifierCollector::new();
            collector.collect(package_name, &mut hir);

            let package_id = Box::new(
                IdentifierDefinition {
                    index: 1,
                    kind: IdentifierKind::Package,
                    parent: None,
                    id: package_name.to_string(),
                },
            );

            let tree = IdentifierTree {
                packages: vec![
                    Nest {
                        id: NestIdentifier::Named(package_id.clone()),
                        children: expected_root,
                    },
                ],
            };

            let mut expected_id_map = IdentifierMap {
                ids: expected_id_map.drain(..).map(|v| Box::new(v)).collect(),
            };

            expected_id_map.ids.push(package_id);

            assert_eq!(collector.tree, tree);
            assert_eq!(collector.map, expected_id_map);
        };

        let generate_named_id = |index: IdentifierIndex, kind: IdentifierKind, parent: IdentifierIndex, id: &str| {
            NestIdentifier::Named(
                Box::new(
                    IdentifierDefinition {
                        index: index,
                        kind: kind,
                        parent: Some(parent),
                        id: id.to_string(),
                    }
                ),
            )
        };
    }

    describe "collection" {
        describe "collection > item" {
            describe "collection > item > module" {
                test "matches identifiers and subitem" {
                    assert_ids(
                        Hir {
                            items: vec![
                                HirItem::Module(
                                    HirModule {
                                        id: HirIdentifier::unresolved_from(HirIdentifierKind::PascalCase, "Module"),
                                        visibility: HirVisibility::Private,
                                        items: vec![
                                            HirItem::Module(
                                                HirModule {
                                                    id: HirIdentifier::unresolved_from(HirIdentifierKind::PascalCase, "SubModule"),
                                                    visibility: HirVisibility::Private,
                                                    items: vec![],
                                                },
                                            ),
                                        ],
                                    },
                                ),
                            ],
                        },
                        vec![
                            Nest {
                                id: generate_named_id(2, IdentifierKind::Module, 1, "Module"),
                                children: vec![
                                    Nest {
                                        id: generate_named_id(3, IdentifierKind::Module, 2, "SubModule"),
                                        children: vec![],
                                    }.into(),
                                ],
                            }.into(),
                        ],
                        vec![
                            IdentifierDefinition {
                                index: 3,
                                kind: IdentifierKind::Module,
                                parent: Some(2),
                                id: "SubModule".to_string(),
                            },
                            IdentifierDefinition {
                                index: 2,
                                kind: IdentifierKind::Module,
                                parent: Some(1),
                                id: "Module".to_string(),
                            },
                        ].into(),
                    );
                }
            }

            describe "collection > item > function" {
                test "matches identifiers, arguments and expression" {
                    assert_ids(
                        Hir {
                            items: vec![
                                HirItem::Function(
                                    HirFunction {
                                        id: HirIdentifier::unresolved_from(HirIdentifierKind::SnakeCase, "f"),
                                        visibility: HirVisibility::Private,
                                        args: vec![
                                            HirFormalArgument {
                                                id: HirIdentifier::unresolved_from(HirIdentifierKind::SnakeCase, "arg"),
                                                data_type: HirDataType::Primitive(
                                                    HirPrimitiveDataType::S32,
                                                ),
                                            },
                                        ],
                                        return_type: None,
                                        exprs: vec![
                                            // fix: add expression
                                        ],
                                    },
                                ),
                            ],
                        },
                        vec![
                            Nest {
                                id: generate_named_id(2, IdentifierKind::Function, 1, "f"),
                                children: vec![
                                    IdentifierDefinition {
                                        index: 3,
                                        kind: IdentifierKind::Variable,
                                        parent: Some(2),
                                        id: "arg".to_string(),
                                    }.into(),
                                ],
                            }.into(),
                        ],
                        vec![
                            IdentifierDefinition {
                                index: 3,
                                kind: IdentifierKind::Variable,
                                parent: Some(2),
                                id: "arg".to_string(),
                            },
                            IdentifierDefinition {
                                index: 2,
                                kind: IdentifierKind::Function,
                                parent: Some(1),
                                id: "f".to_string(),
                            },
                        ].into(),
                    );
                }
            }
        }
    }
}
