import pytest

from metaphor import (
    MetaphorASTNode,
    MetaphorRootNode, MetaphorTextNode, MetaphorRoleNode,
    MetaphorContextNode, MetaphorActionNode, MetaphorCodeNode
)


@pytest.fixture
def sample_node():
    return MetaphorTextNode("test input")

@pytest.fixture
def complex_tree():
    root = MetaphorRootNode()
    text1 = MetaphorTextNode("Hello")
    role = MetaphorRoleNode("user")
    text2 = MetaphorTextNode("World")
    context = MetaphorContextNode("global")

    root.add_child(text1)
    root.add_child(role)
    role.add_child(text2)
    role.add_child(context)

    return root


def test_metaphor_ast_node_creation(sample_node):
    """Test basic node creation"""
    node = MetaphorTextNode("hello")
    assert node.value() == "hello"
    assert node.parent is None
    assert len(node.children) == 0


def test_metaphor_ast_node_add_child(sample_node):
    """Test attaching child nodes"""
    child_node = MetaphorTextNode("child input")

    sample_node.add_child(child_node)
    assert len(sample_node.children) == 1
    assert child_node.parent == sample_node
    assert sample_node.children[0] == child_node


def test_metaphor_ast_node_remove_child(sample_node):
    """Test detaching a child node"""
    child1_node = MetaphorTextNode("child1")
    child2_node = MetaphorTextNode("child2")

    sample_node.add_child(child1_node)
    sample_node.add_child(child2_node)
    assert len(sample_node.children) == 2

    sample_node.remove_child(child1_node)
    assert len(sample_node.children) == 1
    assert sample_node.children[0].value() == "child2"


def test_metaphor_ast_node_detach_unattached_child(sample_node):
    """Test detaching a child node"""
    child1_node = MetaphorTextNode("child1")
    child2_node = MetaphorTextNode("child2")

    sample_node.add_child(child1_node)
    assert len(sample_node.children) == 1

    with pytest.raises(ValueError) as exc_info:
        sample_node.remove_child(child2_node)

    assert "Node is not a child of this node" in str(exc_info)


def test_str_single_node(sample_node):
    """Test string representation of a single node without children"""
    assert str(sample_node) == "MetaphorTextNode: test input"


def test_str_with_child():
    """Test string representation of a parent node with one child"""
    parent = MetaphorRootNode()
    child = MetaphorTextNode("child")
    parent.add_child(child)

    expected = (
        "MetaphorRootNode: \n"
        "    MetaphorTextNode: child"
    )
    assert str(parent) == expected


def test_str_complex_tree(complex_tree):
    """Test string representation of a complex tree structure"""
    expected = (
        "MetaphorRootNode: \n"
        "    MetaphorTextNode: Hello\n"
        "    MetaphorRoleNode: user\n"
        "        MetaphorTextNode: World\n"
        "        MetaphorContextNode: global"
    )
    assert str(complex_tree) == expected


def test_str_empty_value():
    """Test string representation of a node with empty value"""
    node = MetaphorTextNode("")
    assert str(node) == "MetaphorTextNode: "


def test_repr_single_node(sample_node):
    """Test repr of a single node without children"""
    assert repr(sample_node) == "MetaphorTextNode(test input)[0]"


def test_repr_with_children(complex_tree):
    """Test repr of nodes with different numbers of children"""
    assert repr(complex_tree) == "MetaphorRootNode()[2]"
    assert repr(complex_tree.children[1]) == "MetaphorRoleNode(user)[2]"  # The ROLE node has 2 children


def test_str_special_characters():
    """Test string representation with special characters"""
    node = MetaphorTextNode("Hello\nWorld")
    assert str(node) == "MetaphorTextNode: Hello\nWorld"
    assert repr(node) == "MetaphorTextNode(Hello\nWorld)[0]"


def test_str_unicode_characters():
    """Test string representation with Unicode characters"""
    node = MetaphorTextNode("Hello 🌍")
    assert str(node) == "MetaphorTextNode: Hello 🌍"
    assert repr(node) == "MetaphorTextNode(Hello 🌍)[0]"


def test_get_children_of_type():
    """Test getting children of specific type"""
    parent = MetaphorRootNode()
    text1 = MetaphorTextNode("Text 1")
    text2 = MetaphorTextNode("Text 2")
    role = MetaphorRoleNode("user")

    parent.add_child(text1)
    parent.add_child(role)
    parent.add_child(text2)

    text_nodes = parent.get_children_of_type(MetaphorTextNode)
    assert len(text_nodes) == 2
    assert all(isinstance(node, MetaphorTextNode) for node in text_nodes)

    role_nodes = parent.get_children_of_type(MetaphorRoleNode)
    assert len(role_nodes) == 1
    assert isinstance(role_nodes[0], MetaphorRoleNode)
