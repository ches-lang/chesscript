use crate::{hir::{*, resolver::*}};
use speculate::speculate;

speculate!{
    before {
        let assert_ids = |hir: Hir, expected: Vec<CollectedChild>| {
            let collector = IdentifierCollector::new();
            let root_id = CollectedIdentifier::new(IdentifierKind::Package, "test");
            let root = collector.collect(root_id, &hir);

            let expected = CollectedBlock {
                id: CollectedIdentifier {
                    kind: IdentifierKind::Package,
                    id: "test".to_string(),
                },
                children: expected,
            };
            assert_eq!(root, expected);
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
                            CollectedBlock {
                                id: CollectedIdentifier {
                                    kind: IdentifierKind::Module,
                                    id: "Module".to_string(),
                                },
                                children: vec![
                                    CollectedBlock {
                                        id: CollectedIdentifier {
                                            kind: IdentifierKind::Module,
                                            id: "SubModule".to_string(),
                                        },
                                        children: vec![],
                                    }.into(),
                                ],
                            }.into(),
                        ],
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
                            CollectedBlock {
                                id: CollectedIdentifier {
                                    kind: IdentifierKind::Function,
                                    id: "f".to_string(),
                                },
                                children: vec![
                                    CollectedIdentifier {
                                        kind: IdentifierKind::Variable,
                                        id: "arg".to_string(),
                                    }.into(),
                                ],
                            }.into(),
                        ],
                    );
                }
            }
        }
    }
}
