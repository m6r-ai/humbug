import pytest

from metaphor import (
    MetaphorASTNode,
    MetaphorASTRootNode, MetaphorASTTextNode, MetaphorASTRoleNode,
    MetaphorASTContextNode, MetaphorASTActionNode, MetaphorASTCodeNode
)


@pytest.fixture
def sample_node():
    return MetaphorASTTextNode("test input")

@pytest.fixture
def complex_tree():
    root = MetaphorASTRootNode()
    text1 = MetaphorASTTextNode("Hello")
    role = MetaphorASTRoleNode("user")
    text2 = MetaphorASTTextNode("World")
    context = MetaphorASTContextNode("global")

    root.add_child(text1)
    root.add_child(role)
    role.add_child(text2)
    role.add_child(context)

    return root


def test_metaphor_ast_node_creation(sample_node):
    """Test basic node creation"""
    node = MetaphorASTTextNode("hello")
    assert node.value() == "hello"
    assert node.parent is None
    assert len(node.children) == 0


def test_metaphor_ast_node_add_child(sample_node):
    """Test attaching child nodes"""
    child_node = MetaphorASTTextNode("child input")

    sample_node.add_child(child_node)
    assert len(sample_node.children) == 1
    assert child_node.parent == sample_node
    assert sample_node.children[0] == child_node


def test_metaphor_ast_node_remove_child(sample_node):
    """Test detaching a child node"""
    child1_node = MetaphorASTTextNode("child1")
    child2_node = MetaphorASTTextNode("child2")

    sample_node.add_child(child1_node)
    sample_node.add_child(child2_node)
    assert len(sample_node.children) == 2

    sample_node.remove_child(child1_node)
    assert len(sample_node.children) == 1
    assert sample_node.children[0].value() == "child2"


def test_metaphor_ast_node_detach_unattached_child(sample_node):
    """Test detaching a child node"""
    child1_node = MetaphorASTTextNode("child1")
    child2_node = MetaphorASTTextNode("child2")

    sample_node.add_child(child1_node)
    assert len(sample_node.children) == 1

    with pytest.raises(ValueError) as exc_info:
        sample_node.remove_child(child2_node)

    assert "Node is not a child of this node" in str(exc_info)


def test_str_single_node(sample_node):
    """Test string representation of a single node without children"""
    assert str(sample_node) == "MetaphorASTTextNode: test input"


def test_str_with_child():
    """Test string representation of a parent node with one child"""
    parent = MetaphorASTRootNode()
    child = MetaphorASTTextNode("child")
    parent.add_child(child)

    expected = (
        "MetaphorASTRootNode: \n"
        "    MetaphorASTTextNode: child"
    )
    assert str(parent) == expected


def test_str_complex_tree(complex_tree):
    """Test string representation of a complex tree structure"""
    expected = (
        "MetaphorASTRootNode: \n"
        "    MetaphorASTTextNode: Hello\n"
        "    MetaphorASTRoleNode: user\n"
        "        MetaphorASTTextNode: World\n"
        "        MetaphorASTContextNode: global"
    )
    assert str(complex_tree) == expected


def test_str_empty_value():
    """Test string representation of a node with empty value"""
    node = MetaphorASTTextNode("")
    assert str(node) == "MetaphorASTTextNode: "


def test_repr_single_node(sample_node):
    """Test repr of a single node without children"""
    assert repr(sample_node) == "MetaphorASTTextNode(test input)[0]"


def test_repr_with_children(complex_tree):
    """Test repr of nodes with different numbers of children"""
    assert repr(complex_tree) == "MetaphorASTRootNode()[2]"
    assert repr(complex_tree.children[1]) == "MetaphorASTRoleNode(user)[2]"  # The ROLE node has 2 children


def test_str_special_characters():
    """Test string representation with special characters"""
    node = MetaphorASTTextNode("Hello\nWorld")
    assert str(node) == "MetaphorASTTextNode: Hello\nWorld"
    assert repr(node) == "MetaphorASTTextNode(Hello\nWorld)[0]"


def test_str_unicode_characters():
    """Test string representation with Unicode characters"""
    node = MetaphorASTTextNode("Hello 🌍")
    assert str(node) == "MetaphorASTTextNode: Hello 🌍"
    assert repr(node) == "MetaphorASTTextNode(Hello 🌍)[0]"


def test_get_children_of_type():
    """Test getting children of specific type"""
    parent = MetaphorASTRootNode()
    text1 = MetaphorASTTextNode("Text 1")
    text2 = MetaphorASTTextNode("Text 2")
    role = MetaphorASTRoleNode("user")

    parent.add_child(text1)
    parent.add_child(role)
    parent.add_child(text2)

    text_nodes = parent.get_children_of_type(MetaphorASTTextNode)
    assert len(text_nodes) == 2
    assert all(isinstance(node, MetaphorASTTextNode) for node in text_nodes)

    role_nodes = parent.get_children_of_type(MetaphorASTRoleNode)
    assert len(role_nodes) == 1
    assert isinstance(role_nodes[0], MetaphorASTRoleNode)
