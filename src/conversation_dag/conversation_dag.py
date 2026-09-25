"""Conversation DAG model for a mindspace conversations directory."""

import json
import logging
import os
from dataclasses import dataclass


@dataclass
class ConversationNode:
    """All index data for a single conversation file."""
    path: str
    message_ids: list[str]
    parent_message_id: str | None
    parent_tool_call_id: str | None


@dataclass
class ForkEdge:
    """A fork relationship between two conversation files."""
    path_a: str
    path_b: str
    fork_message_id: str


class ConversationDag:
    """
    A DAG of conversation files in a conversations directory.

    Pure, synchronous and frontend-agnostic.  Construct one with the absolute
    path to a conversations directory and it scans the directory immediately.

    Two kinds of edges are tracked:

    - Delegation edges: recorded explicitly in each child file's metadata
      (parent_message_id + parent_tool_call_id).
    - Fork edges: inferred by finding message IDs shared across multiple files.
      The last shared message ID between any two files is the fork point.

    This class holds no live state: it reflects the filesystem as of the last
    refresh() call.  Callers that need to react to filesystem changes (for
    example a UI panel) are responsible for watching the directory and calling
    refresh() as appropriate.
    """

    def __init__(self, conversations_dir: str) -> None:
        """
        Initialise the DAG for a conversations directory.

        Args:
            conversations_dir: Absolute path to the conversations directory.
                If the directory does not exist the DAG is empty.
        """
        self._logger = logging.getLogger("ConversationDag")
        self._conversations_dir = conversations_dir

        # path -> ConversationNode
        self._nodes: dict[str, ConversationNode] = {}

        # message_id -> set of paths that contain it
        self._message_id_index: dict[str, set[str]] = {}

        # Computed fork edges
        self._fork_edges: list[ForkEdge] = []

        self.refresh()

    def conversations_dir(self) -> str:
        """
        Get the conversations directory this DAG was built from.

        Returns:
            Absolute path to the conversations directory.
        """
        return self._conversations_dir

    def refresh(self) -> None:
        """Rescan the conversations directory and rebuild the DAG."""
        self._nodes.clear()
        self._message_id_index.clear()
        self._fork_edges.clear()

        if not os.path.isdir(self._conversations_dir):
            return

        for dirpath, _dirnames, filenames in os.walk(self._conversations_dir):
            for filename in filenames:
                if filename.lower().endswith('.conv'):
                    self.add_file(os.path.join(dirpath, filename))

        self.recompute_fork_edges()

    def add_file(self, path: str) -> None:
        """
        Add a single conversation file to the DAG.

        Used by callers that maintain the DAG incrementally rather than
        rebuilding it with refresh().

        Args:
            path: Absolute path to the .conv file.
        """
        norm_path = os.path.normpath(path)
        result = self._read_conv_file(norm_path)
        if result is None:
            return

        message_ids, parent_message_id, parent_tool_call_id = result

        node = ConversationNode(
            path=norm_path,
            message_ids=message_ids,
            parent_message_id=parent_message_id,
            parent_tool_call_id=parent_tool_call_id
        )
        self._nodes[norm_path] = node

        for msg_id in message_ids:
            if msg_id not in self._message_id_index:
                self._message_id_index[msg_id] = set()

            self._message_id_index[msg_id].add(norm_path)

    def remove_file(self, path: str) -> None:
        """
        Remove a conversation file from the DAG.

        Args:
            path: Absolute path to the .conv file.
        """
        norm_path = os.path.normpath(path)
        node = self._nodes.pop(norm_path, None)
        if node is None:
            return

        for msg_id in node.message_ids:
            paths = self._message_id_index.get(msg_id)
            if paths:
                paths.discard(norm_path)
                if not paths:
                    del self._message_id_index[msg_id]

    def refresh_file_if_changed(self, path: str) -> str | None:
        """
        Re-index a file if its content has changed.

        Args:
            path: Absolute path to the .conv file.

        Returns:
            ``"structure"`` if the parent linkage changed (the DAG topology is
            different), ``"content"`` if only message IDs changed (new messages
            were appended but the parentage is the same), or ``None`` if the
            file is unchanged.
        """
        norm_path = os.path.normpath(path)
        result = self._read_conv_file(norm_path)
        if result is None:
            return None

        message_ids, parent_message_id, parent_tool_call_id = result
        existing = self._nodes.get(norm_path)
        if existing is not None:
            if (existing.parent_message_id == parent_message_id
                    and existing.parent_tool_call_id == parent_tool_call_id):
                if existing.message_ids == message_ids:
                    return None

                # Parent linkage unchanged — only message content grew.  Update
                # the message ID index without signalling a structural change.
                self.remove_file(norm_path)
                self.add_file(norm_path)
                return "content"

        self.remove_file(norm_path)
        self.add_file(norm_path)
        return "structure"

    def get_node(self, path: str) -> ConversationNode | None:
        """
        Get the node for a conversation file.

        Args:
            path: Absolute path to the conversation file.

        Returns:
            ConversationNode if indexed, None otherwise.
        """
        return self._nodes.get(os.path.normpath(path))

    def get_all_paths(self) -> list[str]:
        """
        Get all indexed conversation file paths.

        Returns:
            List of absolute paths.
        """
        return list(self._nodes.keys())

    def get_children(self, path: str) -> list[str]:
        """
        Get all direct delegation children of a conversation.

        A child is a conversation whose parent_message_id appears in the
        given conversation's message_ids.

        Args:
            path: Absolute path to the parent conversation file.

        Returns:
            List of absolute paths of child conversations.
        """
        norm_path = os.path.normpath(path)
        node = self._nodes.get(norm_path)
        if node is None:
            return []

        message_id_set = set(node.message_ids)
        children = []
        for candidate_path, candidate_node in self._nodes.items():
            if candidate_path != norm_path and candidate_node.parent_message_id in message_id_set:
                children.append(candidate_path)

        return children

    def get_parent_paths(self, path: str) -> list[str]:
        """
        Resolve all parent file paths for a delegated child conversation.

        A child may have multiple parents when it was spawned from a message
        that exists in more than one conversation (i.e. the parent was forked
        after the child was created).

        Args:
            path: Absolute path to the child conversation file.

        Returns:
            List of absolute paths of parent conversation files.  Empty if
            this is a root conversation or the parent cannot be resolved.
        """
        node = self._nodes.get(os.path.normpath(path))
        if node is None or node.parent_message_id is None:
            return []

        paths = self._message_id_index.get(node.parent_message_id, set())
        norm_path = os.path.normpath(path)
        return [p for p in paths if p != norm_path]

    def get_fork_edges(self) -> list[ForkEdge]:
        """
        Get all detected fork relationships.

        Returns:
            List of ForkEdge objects describing fork relationships.
        """
        return list(self._fork_edges)

    def get_roots(self) -> list[str]:
        """
        Get all root conversations (those with no delegation parent).

        Returns:
            List of absolute paths of root conversations.
        """
        return [
            path for path, node in self._nodes.items()
            if not self.get_parent_paths(path)
        ]

    def compute_operation_scope(self, paths: set[str]) -> tuple[set[str], set[str]]:
        """
        Compute which conversations can safely be moved or deleted as part of an operation.

        Given a set of root paths being operated on, walks the DAG to find all
        descendants.  A descendant is included in the operation if every one of
        its parents is also included.  A descendant is excluded (left behind) if
        any of its parents falls outside the included set — typically because it
        is a shared child referenced by a conversation not being operated on.

        Args:
            paths: Absolute paths of the conversations being directly operated on.

        Returns:
            Tuple of (included, excluded) where:
            - included: paths that will be moved/deleted, including the originals
              and any exclusively-owned descendants.
            - excluded: descendant paths that must be left behind because they
              are shared with conversations outside the operation scope.
        """
        included: set[str] = set()
        excluded: set[str] = set()

        # Normalise the seed paths
        queue: list[str] = [os.path.normpath(p) for p in paths]
        for p in queue:
            included.add(p)

        # BFS over descendants
        visited: set[str] = set(included)
        while queue:
            current = queue.pop(0)
            for child in self.get_children(current):
                child = os.path.normpath(child)
                if child in visited:
                    continue

                visited.add(child)

                # Check whether all parents of this child are within included
                parent_paths = self.get_parent_paths(child)
                all_parents_included = all(
                    os.path.normpath(p) in included for p in parent_paths
                )

                if all_parents_included:
                    included.add(child)
                    queue.append(child)

                else:
                    excluded.add(child)

        return included, excluded

    def _read_conv_file(self, path: str) -> tuple[list[str], str | None, str | None] | None:
        """
        Read only the fields needed for indexing from a .conv file.

        Extracts message IDs and parent metadata without constructing full
        AIMessage objects.

        Args:
            path: Absolute path to the .conv file.

        Returns:
            Tuple of (message_ids, parent_message_id, parent_tool_call_id),
            or None if the file cannot be read or is not a valid conversation.
        """
        try:
            with open(path, 'r', encoding='utf-8') as f:
                data = json.load(f)

            if not isinstance(data, dict):
                return None

            conversation = data.get('conversation')
            if not isinstance(conversation, list):
                return None

            message_ids = []
            for msg in conversation:
                if isinstance(msg, dict):
                    msg_id = msg.get('id')
                    if isinstance(msg_id, str):
                        message_ids.append(msg_id)

            parent_message_id = None
            parent_tool_call_id = None
            metadata = data.get('metadata')
            if isinstance(metadata, dict):
                parent = metadata.get('parent')
                if isinstance(parent, dict):
                    mid = parent.get('message_id')
                    tid = parent.get('tool_call_id')
                    if isinstance(mid, str):
                        parent_message_id = mid

                    if isinstance(tid, str):
                        parent_tool_call_id = tid

            return message_ids, parent_message_id, parent_tool_call_id

        except (OSError, json.JSONDecodeError, ValueError) as e:
            self._logger.warning("Could not index conversation file %s: %s", path, str(e))
            return None

    def recompute_fork_edges(self) -> None:
        """
        Recompute all fork edges from the current message ID index.

        A fork edge exists between two files A and B when they share at least
        one message ID.  The fork point is the last message ID (by position in
        A's message list) that both files share.
        """
        self._fork_edges = []

        # Find all message IDs shared by more than one file
        shared: dict[str, set[str]] = {
            msg_id: paths
            for msg_id, paths in self._message_id_index.items()
            if len(paths) > 1
        }

        if not shared:
            return

        # For each pair of files that share at least one message ID, find the
        # last shared message ID (the fork point).
        processed_pairs: set[frozenset] = set()

        for paths in shared.values():
            path_list = sorted(paths)  # Deterministic ordering
            for i, path_a in enumerate(path_list):
                for j in range(i + 1, len(path_list)):
                    pair = frozenset([path_a, path_list[j]])
                    if pair in processed_pairs:
                        continue

                    processed_pairs.add(pair)
                    fork_msg_id = self._find_fork_point(path_a, path_list[j])
                    if fork_msg_id:
                        self._fork_edges.append(ForkEdge(
                            path_a=path_a,
                            path_b=path_list[j],
                            fork_message_id=fork_msg_id
                        ))

    def _find_fork_point(self, path_a: str, path_b: str) -> str | None:
        """
        Find the last shared message ID between two conversation files.

        Args:
            path_a: Normalised path to first conversation file.
            path_b: Normalised path to second conversation file.

        Returns:
            The last shared message ID, or None if no shared messages.
        """
        node_a = self._nodes.get(path_a)
        node_b = self._nodes.get(path_b)
        if node_a is None or node_b is None:
            return None

        ids_b = set(node_b.message_ids)
        last_shared = None
        for msg_id in node_a.message_ids:
            if msg_id in ids_b:
                last_shared = msg_id

        return last_shared
