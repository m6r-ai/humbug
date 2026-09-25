"""Tests for the frontend-agnostic conversation DAG model."""

import json
import os

from conversation_dag import ConversationDag


def _write_conv(path: str, message_ids: list[str], parent: dict | None = None) -> None:
    """Write a minimal .conv file with the given message IDs and parent metadata."""
    data = {
        "metadata": {"version": "0.1", "parent": parent},
        "conversation": [{"id": mid, "source": "user"} for mid in message_ids],
    }
    with open(path, "w", encoding="utf-8") as f:
        json.dump(data, f)


def _parent(message_id: str, tool_call_id: str) -> dict:
    """Build a parent metadata dict."""
    return {"message_id": message_id, "tool_call_id": tool_call_id}


class TestScanning:
    """Tests for scanning the conversations directory."""

    def test_missing_directory_yields_empty_dag(self, tmp_path):
        """A non-existent conversations directory produces an empty DAG."""
        dag = ConversationDag(os.path.join(str(tmp_path), "does_not_exist"))

        assert dag.get_all_paths() == []
        assert dag.get_roots() == []
        assert dag.get_fork_edges() == []

    def test_scan_indexes_all_conv_files(self, tmp_path):
        """All .conv files in the directory tree are indexed."""
        root = str(tmp_path)
        sub = os.path.join(root, "nested")
        os.makedirs(sub)
        _write_conv(os.path.join(root, "a.conv"), ["m1"])
        _write_conv(os.path.join(sub, "b.conv"), ["m2"])
        _write_conv(os.path.join(root, "not_a_conv.txt"), ["m3"])

        dag = ConversationDag(root)

        assert sorted(os.path.basename(p) for p in dag.get_all_paths()) == ["a.conv", "b.conv"]

    def test_malformed_file_is_skipped(self, tmp_path):
        """A malformed .conv file is skipped without aborting the scan."""
        root = str(tmp_path)
        _write_conv(os.path.join(root, "good.conv"), ["m1"])
        with open(os.path.join(root, "bad.conv"), "w", encoding="utf-8") as f:
            f.write("{ not valid json")

        dag = ConversationDag(root)

        assert sorted(os.path.basename(p) for p in dag.get_all_paths()) == ["good.conv"]

    def test_refresh_picks_up_new_files(self, tmp_path):
        """refresh() rescans the directory and picks up newly added files."""
        root = str(tmp_path)
        _write_conv(os.path.join(root, "a.conv"), ["m1"])
        dag = ConversationDag(root)
        assert len(dag.get_all_paths()) == 1

        _write_conv(os.path.join(root, "b.conv"), ["m2"])
        dag.refresh()

        assert sorted(os.path.basename(p) for p in dag.get_all_paths()) == ["a.conv", "b.conv"]


class TestDelegation:
    """Tests for delegation parent/child resolution."""

    def test_child_resolves_to_parent(self, tmp_path):
        """A delegated child resolves to the conversation containing its parent message."""
        root = str(tmp_path)
        parent = os.path.join(root, "parent.conv")
        child = os.path.join(root, "child.conv")
        _write_conv(parent, ["m1", "m2"])
        _write_conv(child, ["m3"], parent=_parent("m2", "tc1"))

        dag = ConversationDag(root)

        assert dag.get_parent_paths(child) == [os.path.normpath(parent)]
        assert dag.get_children(parent) == [os.path.normpath(child)]

    def test_root_conversation_has_no_parent(self, tmp_path):
        """A conversation with no parent metadata is a root."""
        root = str(tmp_path)
        parent = os.path.join(root, "parent.conv")
        _write_conv(parent, ["m1"])

        dag = ConversationDag(root)

        assert dag.get_parent_paths(parent) == []
        assert dag.get_roots() == [os.path.normpath(parent)]

    def test_child_with_unresolvable_parent_is_a_root(self, tmp_path):
        """A child whose parent message exists nowhere is treated as a root."""
        root = str(tmp_path)
        child = os.path.join(root, "child.conv")
        _write_conv(child, ["m3"], parent=_parent("missing", "tc1"))

        dag = ConversationDag(root)

        assert dag.get_parent_paths(child) == []
        assert dag.get_roots() == [os.path.normpath(child)]

    def test_child_with_forked_parent_has_multiple_parents(self, tmp_path):
        """A child spawned from a message present in two files resolves to both parents."""
        root = str(tmp_path)
        parent = os.path.join(root, "parent.conv")
        parent_fork = os.path.join(root, "parent_fork.conv")
        child = os.path.join(root, "child.conv")
        _write_conv(parent, ["m1", "m2"])
        _write_conv(parent_fork, ["m1", "m2", "m6"])
        _write_conv(child, ["m3"], parent=_parent("m2", "tc1"))

        dag = ConversationDag(root)

        parents = sorted(os.path.basename(p) for p in dag.get_parent_paths(child))
        assert parents == ["parent.conv", "parent_fork.conv"]

    def test_nested_delegation(self, tmp_path):
        """A grandchild resolves to its immediate (child) parent, not the root."""
        root = str(tmp_path)
        parent = os.path.join(root, "parent.conv")
        child = os.path.join(root, "child.conv")
        grandchild = os.path.join(root, "grandchild.conv")
        _write_conv(parent, ["m1", "m2"])
        _write_conv(child, ["m3", "m4"], parent=_parent("m2", "tc1"))
        _write_conv(grandchild, ["m5"], parent=_parent("m3", "tc2"))

        dag = ConversationDag(root)

        assert dag.get_parent_paths(grandchild) == [os.path.normpath(child)]
        assert sorted(os.path.basename(p) for p in dag.get_children(child)) == ["grandchild.conv"]


class TestForkEdges:
    """Tests for inferred fork edges."""

    def test_fork_edge_between_shared_files(self, tmp_path):
        """Two files sharing a message ID produce a fork edge at the last shared ID."""
        root = str(tmp_path)
        a = os.path.join(root, "a.conv")
        b = os.path.join(root, "b.conv")
        _write_conv(a, ["m1", "m2", "m3"])
        _write_conv(b, ["m1", "m2", "m4"])

        dag = ConversationDag(root)

        edges = dag.get_fork_edges()
        assert len(edges) == 1
        assert edges[0].fork_message_id == "m2"

    def test_no_fork_edges_without_shared_messages(self, tmp_path):
        """Files with no shared message IDs produce no fork edges."""
        root = str(tmp_path)
        _write_conv(os.path.join(root, "a.conv"), ["m1"])
        _write_conv(os.path.join(root, "b.conv"), ["m2"])

        dag = ConversationDag(root)

        assert dag.get_fork_edges() == []


class TestOperationScope:
    """Tests for compute_operation_scope()."""

    def test_exclusive_descendants_are_included(self, tmp_path):
        """A child whose only parent is in scope is included in the operation."""
        root = str(tmp_path)
        parent = os.path.join(root, "parent.conv")
        child = os.path.join(root, "child.conv")
        _write_conv(parent, ["m1", "m2"])
        _write_conv(child, ["m3"], parent=_parent("m2", "tc1"))

        dag = ConversationDag(root)
        included, excluded = dag.compute_operation_scope({os.path.normpath(parent)})

        assert included == {os.path.normpath(parent), os.path.normpath(child)}
        assert excluded == set()

    def test_shared_child_is_excluded(self, tmp_path):
        """A child with a parent outside the scope is left behind."""
        root = str(tmp_path)
        parent = os.path.join(root, "parent.conv")
        parent_fork = os.path.join(root, "parent_fork.conv")
        child = os.path.join(root, "child.conv")
        _write_conv(parent, ["m1", "m2"])
        _write_conv(parent_fork, ["m1", "m2", "m6"])
        _write_conv(child, ["m3"], parent=_parent("m2", "tc1"))

        dag = ConversationDag(root)
        included, excluded = dag.compute_operation_scope({os.path.normpath(parent)})

        assert included == {os.path.normpath(parent)}
        assert excluded == {os.path.normpath(child)}


class TestIncrementalUpdates:
    """Tests for add_file/remove_file/refresh_file_if_changed."""

    def test_refresh_file_if_changed_reports_none_when_unchanged(self, tmp_path):
        """An unchanged file reports no change."""
        root = str(tmp_path)
        path = os.path.join(root, "a.conv")
        _write_conv(path, ["m1"])
        dag = ConversationDag(root)

        assert dag.refresh_file_if_changed(path) is None

    def test_refresh_file_if_changed_reports_content_for_appended_messages(self, tmp_path):
        """Appending messages with unchanged parentage reports a content change."""
        root = str(tmp_path)
        path = os.path.join(root, "a.conv")
        _write_conv(path, ["m1"])
        dag = ConversationDag(root)

        _write_conv(path, ["m1", "m2"])

        assert dag.refresh_file_if_changed(path) == "content"

    def test_refresh_file_if_changed_reports_structure_for_new_parent(self, tmp_path):
        """Changing the parent linkage reports a structural change."""
        root = str(tmp_path)
        path = os.path.join(root, "a.conv")
        _write_conv(path, ["m1"])
        dag = ConversationDag(root)

        _write_conv(path, ["m1"], parent=_parent("m9", "tc1"))

        assert dag.refresh_file_if_changed(path) == "structure"

    def test_remove_file_drops_it_from_the_dag(self, tmp_path):
        """Removing a file drops it from the DAG and its message IDs."""
        root = str(tmp_path)
        path = os.path.join(root, "a.conv")
        _write_conv(path, ["m1"])
        dag = ConversationDag(root)

        dag.remove_file(path)

        assert dag.get_all_paths() == []
        assert dag.get_node(path) is None
