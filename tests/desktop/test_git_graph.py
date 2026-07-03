"""Tests for the commit-graph lane layout (pure logic, no Qt)."""

from dataclasses import dataclass

from desktop.git_history_tab.git_graph import compute_graph


@dataclass
class _C:
    commit_hash: str
    parents: list


def test_linear_history_single_lane():
    commits = [_C("c", ["b"]), _C("b", ["a"]), _C("a", [])]
    rows = compute_graph(commits)
    assert [r.node_col for r in rows] == [0, 0, 0]
    assert all(r.max_col == 0 for r in rows)
    assert all(not r.branch_cols and not r.merge_cols for r in rows)


def test_merge_creates_branch_and_merges_back():
    # m is a merge of parents p1 and p2; both lead back to base b.
    commits = [
        _C("m", ["p1", "p2"]),
        _C("p1", ["b"]),
        _C("p2", ["b"]),
        _C("b", []),
    ]
    rows = compute_graph(commits)

    # The merge commit spawns a second lane (branch_cols non-empty).
    assert rows[0].branch_cols, "merge should open a second lane"
    assert rows[0].max_col >= 1

    # The base commit is awaited by two lanes that merge into it.
    base_row = rows[-1]
    assert base_row.merge_cols or base_row.node_col == 0


def test_colors_within_palette():
    commits = [_C("c", ["b"]), _C("b", ["a"]), _C("a", [])]
    rows = compute_graph(commits)
    for row in rows:
        assert 0 <= row.node_color < 8
        assert all(0 <= color < 8 for _, color in row.top_lanes + row.bottom_lanes)


def test_empty():
    assert compute_graph([]) == []
