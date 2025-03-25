import pytest

from humbug.m6rc import (
    MetaphorASTNode,
    MetaphorASTNodeType,
)


@pytest.fixture
def sample_node():
    return MetaphorASTNode(MetaphorASTNodeType.TEXT, "test input")

@pytest.fixture
def complex_tree():
    root = MetaphorASTNode(MetaphorASTNodeType.ROOT, "document")
    text1 = MetaphorASTNode(MetaphorASTNodeType.TEXT, "Hello")
    role = MetaphorASTNode(MetaphorASTNodeType.ROLE, "user")
    text2 = MetaphorASTNode(MetaphorASTNodeType.TEXT, "World")
    context = MetaphorASTNode(MetaphorASTNodeType.CONTEXT, "global")

    root.attach_child(text1)
    root.attach_child(role)
    role.attach_child(text2)
    role.attach_child(context)

    return root


def test_metaphor_ast_node_creation(sample_node):
    """Test basic node creation"""
    node = MetaphorASTNode(MetaphorASTNodeType.TEXT, "hello")
    assert node.node_type == MetaphorASTNodeType.TEXT
    assert node.value == "hello"
    assert node.parent is None
    assert len(node.children) == 0


def test_metaphor_ast_node_attach_child(sample_node):
    """Test attaching child nodes"""
    child_node = MetaphorASTNode(MetaphorASTNodeType.TEXT, "child input")

    sample_node.attach_child(child_node)
    assert len(sample_node.children) == 1
    assert child_node.parent == sample_node
    assert sample_node.children[0] == child_node


def test_metaphor_ast_node_detach_child(sample_node):
    """Test detaching a child node"""
    child1_node = MetaphorASTNode(MetaphorASTNodeType.TEXT, "child1")
    child2_node = MetaphorASTNode(MetaphorASTNodeType.TEXT, "child2")

    sample_node.attach_child(child1_node)
    sample_node.attach_child(child2_node)
    assert len(sample_node.children) == 2

    sample_node.detach_child(child1_node)
    assert len(sample_node.children) == 1
    assert sample_node.children[0].value == "child2"


def test_metaphor_ast_node_detach_unattached_child(sample_node):
    """Test detaching a child node"""
    child1_node = MetaphorASTNode(MetaphorASTNodeType.TEXT, "child1")
    child2_node = MetaphorASTNode(MetaphorASTNodeType.TEXT, "child2")

    sample_node.attach_child(child1_node)
    assert len(sample_node.children) == 1

    with pytest.raises(ValueError) as exc_info:
        sample_node.detach_child(child2_node)

    assert "Node is not a child of this node" in str(exc_info)


def test_str_single_node(sample_node):
    """Test string representation of a single node without children"""
    assert str(sample_node) == "TEXT: test input"


def test_str_with_child():
    """Test string representation of a parent node with one child"""
    parent = MetaphorASTNode(MetaphorASTNodeType.ROOT, "parent")
    child = MetaphorASTNode(MetaphorASTNodeType.TEXT, "child")
    parent.attach_child(child)

    expected = (
        "ROOT: parent\n"
        "    TEXT: child"
    )
    assert str(parent) == expected


def test_str_complex_tree(complex_tree):
    """Test string representation of a complex tree structure"""
    expected = (
        "ROOT: document\n"
        "    TEXT: Hello\n"
        "    ROLE: user\n"
        "        TEXT: World\n"
        "        CONTEXT: global"
    )
    assert str(complex_tree) == expected


def test_str_empty_value():
    """Test string representation of a node with empty value"""
    node = MetaphorASTNode(MetaphorASTNodeType.TEXT, "")
    assert str(node) == "TEXT: "


def test_repr_single_node(sample_node):
    """Test repr of a single node without children"""
    assert repr(sample_node) == "TEXT(test input)[0]"


def test_repr_with_children(complex_tree):
    """Test repr of nodes with different numbers of children"""
    assert repr(complex_tree) == "ROOT(document)[2]"
    assert repr(complex_tree.children[1]) == "ROLE(user)[2]"  # The ROLE node has 2 children


def test_str_special_characters():
    """Test string representation with special characters"""
    node = MetaphorASTNode(MetaphorASTNodeType.TEXT, "Hello\nWorld")
    assert str(node) == "TEXT: Hello\nWorld"
    assert repr(node) == "TEXT(Hello\nWorld)[0]"


def test_str_unicode_characters():
    """Test string representation with Unicode characters"""
    node = MetaphorASTNode(MetaphorASTNodeType.TEXT, "Hello 🌍")
    assert str(node) == "TEXT: Hello 🌍"
    assert repr(node) == "TEXT(Hello 🌍)[0]"
