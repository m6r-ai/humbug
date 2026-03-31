from __future__ import annotations

from pathlib import Path
from typing import Any
from typing import cast

from benchmark import BenchmarkCase, BenchmarkSuite, Implementation
from menai import Menai

_SUITE_DIR = Path(__file__).resolve().parent

PUZZLES: list[tuple[str, str, list[int]]] = [
    ("Easy (36 givens)", "easy", [
        5,3,0, 0,7,0, 0,0,0,
        6,0,0, 1,9,5, 0,0,0,
        0,9,8, 0,0,0, 0,6,0,
        8,0,0, 0,6,0, 0,0,3,
        4,0,0, 8,0,3, 0,0,1,
        7,0,0, 0,2,0, 0,0,6,
        0,6,0, 0,0,0, 2,8,0,
        0,0,0, 4,1,9, 0,0,5,
        0,0,0, 0,8,0, 0,7,9,
    ]),
    ("Medium (30 givens)", "medium", [
        0,0,0, 2,6,0, 7,0,1,
        6,8,0, 0,7,0, 0,9,0,
        1,9,0, 0,0,4, 5,0,0,
        8,2,0, 1,0,0, 0,4,0,
        0,0,4, 6,0,2, 9,0,0,
        0,5,0, 0,0,3, 0,2,8,
        0,0,9, 3,0,0, 0,7,4,
        0,4,0, 0,5,0, 0,3,6,
        7,0,3, 0,1,8, 0,0,0,
    ]),
    ("Hard (25 givens)", "hard", [
        0,0,0, 6,0,0, 4,0,0,
        7,0,0, 0,0,3, 6,0,0,
        0,0,0, 0,9,1, 0,8,0,
        0,0,0, 0,0,0, 0,0,0,
        0,5,0, 1,8,0, 0,0,3,
        0,0,0, 3,0,6, 0,4,5,
        0,4,0, 2,0,0, 0,6,0,
        9,0,3, 0,0,0, 0,0,0,
        0,2,0, 0,0,0, 1,0,0,
    ]),
    ("Expert (23 givens)", "expert", [
        8,0,0, 0,0,0, 0,0,0,
        0,0,3, 6,0,0, 0,0,0,
        0,7,0, 0,9,0, 2,0,0,
        0,5,0, 0,0,7, 0,0,0,
        0,0,0, 0,4,5, 7,0,0,
        0,0,0, 1,0,0, 0,3,0,
        0,0,1, 0,0,0, 0,6,8,
        0,0,8, 5,0,0, 0,1,0,
        0,9,0, 0,0,0, 4,0,0,
    ]),
]

_ITERATIONS: dict[str, int] = {
    "easy": 3,
    "medium": 3,
    "hard": 1,
    "expert": 1,
}


def _board_to_menai(flat: list[int]) -> str:
    """Convert a flat 81-element board into a Menai nested-list literal."""
    rows = []
    for r in range(9):
        cells = " ".join(str(flat[r * 9 + c]) for c in range(9))
        rows.append(f"(list {cells})")
    return "(list\n  " + "\n  ".join(rows) + ")"


def _is_valid_board(board: Any) -> bool:
    """Return True if *board* is a fully solved, valid 9×9 sudoku grid.

    Checks that every row, column, and 3×3 box contains exactly the digits 1–9.
    """
    digits = set(range(1, 10))

    if not isinstance(board, (list, tuple)) or len(board) != 9:
        return False
    for row in board:
        if not isinstance(row, (list, tuple)) or len(row) != 9:
            return False

    for r in range(9):
        if set(board[r]) != digits:
            return False

    for c in range(9):
        if {board[r][c] for r in range(9)} != digits:
            return False

    for br in range(3):
        for bc in range(3):
            box = {
                board[br * 3 + dr][bc * 3 + dc]
                for dr in range(3)
                for dc in range(3)
            }
            if box != digits:
                return False

    return True


def _solve_python_idiomatic(flat: list[int]) -> list[list[int]]:
    """Solve a sudoku puzzle using mutable backtracking on a flat list.

    Returns the solved board as a 9×9 list-of-lists.
    """
    board = list(flat)

    rows: list[set[int]] = [set() for _ in range(9)]
    cols: list[set[int]] = [set() for _ in range(9)]
    boxes: list[set[int]] = [set() for _ in range(9)]

    for i, val in enumerate(board):
        if val != 0:
            r, c = divmod(i, 9)
            b = (r // 3) * 3 + (c // 3)
            rows[r].add(val)
            cols[c].add(val)
            boxes[b].add(val)

    def backtrack() -> bool:
        """Recursively fill the next empty cell; return True on success."""
        try:
            idx = board.index(0)
        except ValueError:
            return True
        r, c = divmod(idx, 9)
        b = (r // 3) * 3 + (c // 3)
        for digit in range(1, 10):
            if digit not in rows[r] and digit not in cols[c] and digit not in boxes[b]:
                board[idx] = digit
                rows[r].add(digit)
                cols[c].add(digit)
                boxes[b].add(digit)
                if backtrack():
                    return True
                board[idx] = 0
                rows[r].discard(digit)
                cols[c].discard(digit)
                boxes[b].discard(digit)
        return False

    backtrack()
    return [board[r * 9: r * 9 + 9] for r in range(9)]


def _solve_python_functional(flat: list[int]) -> list[list[int]]:
    """Solve a sudoku puzzle using pure-functional backtracking on immutable tuples.

    Returns the solved board as a 9×9 list-of-lists.
    """
    initial: tuple[tuple[int, ...], ...] = tuple(
        tuple(flat[r * 9 + c] for c in range(9)) for r in range(9)
    )

    def candidates(board: tuple[tuple[int, ...], ...], r: int, c: int) -> frozenset[int]:
        """Return the set of digits that may legally be placed at (r, c)."""
        used_row: frozenset[int] = frozenset(board[r])
        used_col: frozenset[int] = frozenset(board[rr][c] for rr in range(9))
        br, bc = (r // 3) * 3, (c // 3) * 3
        used_box: frozenset[int] = frozenset(
            board[br + dr][bc + dc] for dr in range(3) for dc in range(3)
        )
        return frozenset(range(1, 10)) - used_row - used_col - used_box

    def next_empty(
        board: tuple[tuple[int, ...], ...]
    ) -> tuple[int, int] | None:
        """Return the (row, col) of the first empty cell, or None if the board is full."""
        for r in range(9):
            for c in range(9):
                if board[r][c] == 0:
                    return r, c
        return None

    def place(
        board: tuple[tuple[int, ...], ...], r: int, c: int, digit: int
    ) -> tuple[tuple[int, ...], ...]:
        """Return a new board with *digit* placed at (r, c)."""
        new_row = board[r][:c] + (digit,) + board[r][c + 1:]
        return board[:r] + (new_row,) + board[r + 1:]

    def search(
        board: tuple[tuple[int, ...], ...]
    ) -> tuple[tuple[int, ...], ...] | None:
        """Return a solved board, or None if no solution exists from this state."""
        cell = next_empty(board)
        if cell is None:
            return board
        r, c = cell
        for digit in candidates(board, r, c):
            result = search(place(board, r, c, digit))
            if result is not None:
                return result
        return None

    solved = search(initial)
    if solved is None:
        raise ValueError("No solution found")
    return [list(row) for row in solved]


class Suite(BenchmarkSuite):
    """Benchmark suite comparing Menai, idiomatic Python, and functional Python sudoku solvers."""

    name = "Sudoku"
    description = "Solve sudoku puzzles of varying difficulty."

    def cases(self) -> list[BenchmarkCase]:
        """Return one case per puzzle, with iteration counts scaled by difficulty."""
        return [
            BenchmarkCase(
                name=label,
                input=flat,
                iterations=_ITERATIONS[difficulty],
            )
            for label, difficulty, flat in PUZZLES
        ]

    def implementations(self, menai: Menai) -> list[Implementation]:
        """Return Menai, idiomatic Python, and functional Python solver implementations."""
        def run_menai(flat: list[int]) -> list[list[int]]:
            """Solve the puzzle by calling the Menai sudoku-solver module."""
            board_expr = _board_to_menai(flat)
            expr = (
                '(let ((sudoku (import "sudoku-solver")))'
                ' (let ((solve-fn (dict-get sudoku "solve")))'
                f" (solve-fn {board_expr})))"
            )
            result = cast(list, menai.evaluate(expr))
            return [list(cast(list, row)) for row in result]

        def run_python_idiomatic(flat: list[int]) -> list[list[int]]:
            """Solve the puzzle using mutable backtracking."""
            return _solve_python_idiomatic(flat)

        def run_python_functional(flat: list[int]) -> list[list[int]]:
            """Solve the puzzle using pure-functional backtracking."""
            return _solve_python_functional(flat)

        return [
            Implementation(name="Menai", run=run_menai),
            Implementation(name="Python (idiomatic)", run=run_python_idiomatic),
            Implementation(name="Python (functional)", run=run_python_functional),
        ]

    def results_equal(self, a: Any, b: Any) -> bool:
        """Return True if both *a* and *b* are independently valid solved sudoku boards."""
        return _is_valid_board(a) and _is_valid_board(b)
