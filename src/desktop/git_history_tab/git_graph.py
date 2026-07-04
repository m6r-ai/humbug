"""Commit-graph lane layout."""

from dataclasses import dataclass, field

from git import CommitInfo


_NUM_COLORS = 8  # size of the lane colour palette


@dataclass
class GraphRow:
    """Lane geometry for a single commit row."""
    node_col: int
    node_color: int
    top_lanes: list[tuple[int, int]] = field(default_factory=list)     # (col, color)
    bottom_lanes: list[tuple[int, int]] = field(default_factory=list)  # (col, color)
    merge_cols: list[int] = field(default_factory=list)   # top cols merging into the node
    branch_cols: list[int] = field(default_factory=list)  # bottom cols branching from the node
    max_col: int = 0


def compute_graph(commits: list[CommitInfo]) -> list[GraphRow]:
    """
    Compute lane geometry for commits ordered newest-first.

    Args:
        commits: Sequence of objects with ``commit_hash`` and ``parents``.

    Returns:
        A list of GraphRow, one per commit, in the same order.
    """
    lanes: list[str | None] = []   # per-column: the commit hash that column awaits
    color_of: dict[str, int] = {}
    counter = [0]

    def color_for(h: str) -> int:
        if h not in color_of:
            color_of[h] = counter[0] % _NUM_COLORS
            counter[0] += 1

        return color_of[h]

    def free_column() -> int | None:
        return next((i for i, e in enumerate(lanes) if e is None), None)

    rows: list[GraphRow] = []

    for commit in commits:
        h = commit.commit_hash
        parents = list(commit.parents)

        cols = [i for i, e in enumerate(lanes) if e == h]
        if cols:
            node_col = cols[0]

        else:
            free = free_column()
            if free is None:
                node_col = len(lanes)
                lanes.append(h)

            else:
                node_col = free
                lanes[free] = h

            cols = [node_col]

        node_color = color_for(h)
        top_lanes = [(i, color_for(e)) for i, e in enumerate(lanes) if e is not None]
        merge_cols = cols[1:]

        # Extra columns awaiting this same commit merge into the node.
        for extra in merge_cols:
            lanes[extra] = None

        branch_cols: list[int] = []
        if parents:
            lanes[node_col] = parents[0]
            color_of[parents[0]] = node_color  # first parent continues the node's colour
            for parent in parents[1:]:
                color_for(parent)
                free = free_column()
                if free is None:
                    lanes.append(parent)
                    branch_cols.append(len(lanes) - 1)

                else:
                    lanes[free] = parent
                    branch_cols.append(free)

        else:
            lanes[node_col] = None

        bottom_lanes = [(i, color_for(e)) for i, e in enumerate(lanes) if e is not None]
        max_col = max(
            [node_col]
            + [c for c, _ in top_lanes]
            + [c for c, _ in bottom_lanes]
        )
        rows.append(GraphRow(
            node_col=node_col,
            node_color=node_color,
            top_lanes=top_lanes,
            bottom_lanes=bottom_lanes,
            merge_cols=merge_cols,
            branch_cols=branch_cols,
            max_col=max_col,
        ))

    return rows
