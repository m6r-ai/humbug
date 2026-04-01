from __future__ import annotations

from pathlib import Path
from typing import Any

from benchmark import BenchmarkCase, BenchmarkSuite, Implementation
from menai import Menai

_SUITE_DIR = Path(__file__).resolve().parent

_SCRAMBLES: list[tuple[str, list[str]]] = [
    ("1-move", ["R"]),
    ("2-move", ["R", "U"]),
    ("3-move", ["R", "U", "F"]),
    ("4-move", ["R", "U", "R'", "D"]),
    ("5-move", ["R", "U", "R'", "D", "F"]),
    ("6-move", ["R", "U", "R'", "D", "F", "R"]),
]

_FACE_ORDER = ("U", "D", "F", "B", "L", "R")

_ALL_MOVES = [
    f"{base}{suffix}"
    for base in ("U", "D", "F", "B", "L", "R")
    for suffix in ("", "'", "2")
]

_INVERSE: dict[str, str] = {}
for _base in ("U", "D", "F", "B", "L", "R"):
    _INVERSE[_base] = f"{_base}'"
    _INVERSE[f"{_base}'"] = _base
    _INVERSE[f"{_base}2"] = f"{_base}2"


def _face_of(move: str) -> str:
    """Return the face letter for a move string."""
    return move[0]


def _solved_cube_idiomatic() -> dict[str, list[int]]:
    """Return a solved cube in the idiomatic dict representation.

    Each face is a 9-element list of sticker colour indices:
    U=0, D=1, F=2, B=3, L=4, R=5.
    """
    return {face: [color] * 9 for color, face in enumerate(_FACE_ORDER)}


def _rotate_face_cw(face: list[int]) -> list[int]:
    """Return a new face list rotated 90° clockwise."""
    return [
        face[6], face[3], face[0],
        face[7], face[4], face[1],
        face[8], face[5], face[2],
    ]


def _move_U(cube: dict[str, list[int]]) -> dict[str, list[int]]:
    """Apply a U move (clockwise) and return the new cube."""
    U, D, F, B, L, R = (list(cube[f]) for f in _FACE_ORDER)
    return {
        "U": _rotate_face_cw(U), "D": D,
        "F": [R[0], R[1], R[2], F[3], F[4], F[5], F[6], F[7], F[8]],
        "B": [L[0], L[1], L[2], B[3], B[4], B[5], B[6], B[7], B[8]],
        "L": [F[0], F[1], F[2], L[3], L[4], L[5], L[6], L[7], L[8]],
        "R": [B[0], B[1], B[2], R[3], R[4], R[5], R[6], R[7], R[8]],
    }


def _move_D(cube: dict[str, list[int]]) -> dict[str, list[int]]:
    """Apply a D move (clockwise) and return the new cube."""
    U, D, F, B, L, R = (list(cube[f]) for f in _FACE_ORDER)
    return {
        "U": U, "D": _rotate_face_cw(D),
        "F": [F[0], F[1], F[2], F[3], F[4], F[5], L[6], L[7], L[8]],
        "B": [B[0], B[1], B[2], B[3], B[4], B[5], R[6], R[7], R[8]],
        "L": [L[0], L[1], L[2], L[3], L[4], L[5], B[6], B[7], B[8]],
        "R": [R[0], R[1], R[2], R[3], R[4], R[5], F[6], F[7], F[8]],
    }


def _move_F(cube: dict[str, list[int]]) -> dict[str, list[int]]:
    """Apply an F move (clockwise) and return the new cube."""
    U, D, F, B, L, R = (list(cube[f]) for f in _FACE_ORDER)
    return {
        "U": [U[0], U[1], U[2], U[3], U[4], U[5], L[8], L[5], L[2]],
        "D": [R[0], R[3], R[6], D[3], D[4], D[5], D[6], D[7], D[8]],
        "F": _rotate_face_cw(F), "B": B,
        "L": [L[0], L[1], D[2], L[3], L[4], D[1], L[6], L[7], D[0]],
        "R": [U[6], R[1], R[2], U[7], R[4], R[5], U[8], R[7], R[8]],
    }


def _move_B(cube: dict[str, list[int]]) -> dict[str, list[int]]:
    """Apply a B move (clockwise) and return the new cube."""
    U, D, F, B, L, R = (list(cube[f]) for f in _FACE_ORDER)
    return {
        "U": [R[8], R[5], R[2], U[3], U[4], U[5], U[6], U[7], U[8]],
        "D": [D[0], D[1], D[2], D[3], D[4], D[5], L[6], L[3], L[0]],
        "F": F, "B": _rotate_face_cw(B),
        "L": [U[2], L[1], L[2], U[1], L[4], L[5], U[0], L[7], L[8]],
        "R": [R[0], R[1], D[6], R[3], R[4], D[7], R[6], R[7], D[8]],
    }


def _move_L(cube: dict[str, list[int]]) -> dict[str, list[int]]:
    """Apply an L move (clockwise) and return the new cube."""
    U, D, F, B, L, R = (list(cube[f]) for f in _FACE_ORDER)
    return {
        "U": [B[8], U[1], U[2], B[5], U[4], U[5], B[2], U[7], U[8]],
        "D": [F[0], D[1], D[2], F[3], D[4], D[5], F[6], D[7], D[8]],
        "F": [U[0], F[1], F[2], U[3], F[4], F[5], U[6], F[7], F[8]],
        "B": [B[0], B[1], D[6], B[3], B[4], D[3], B[6], B[7], D[0]],
        "L": _rotate_face_cw(L), "R": R,
    }


def _move_R(cube: dict[str, list[int]]) -> dict[str, list[int]]:
    """Apply an R move (clockwise) and return the new cube."""
    U, D, F, B, L, R = (list(cube[f]) for f in _FACE_ORDER)
    return {
        "U": [U[0], U[1], F[2], U[3], U[4], F[5], U[6], U[7], F[8]],
        "D": [D[0], D[1], B[6], D[3], D[4], B[3], D[6], D[7], B[0]],
        "F": [F[0], F[1], D[2], F[3], F[4], D[5], F[6], F[7], D[8]],
        "B": [U[8], B[1], B[2], U[5], B[4], B[5], U[2], B[7], B[8]],
        "L": L, "R": _rotate_face_cw(R),
    }


_BASE_MOVES: dict[str, Any] = {
    "U": _move_U, "D": _move_D, "F": _move_F,
    "B": _move_B, "L": _move_L, "R": _move_R,
}


def _apply_move_idiomatic(
    cube: dict[str, list[int]], move: str
) -> dict[str, list[int]]:
    """Apply a single move (including prime and double variants) to a cube."""
    if move.endswith("2"):
        fn = _BASE_MOVES[move[:-1]]
        return fn(fn(cube))
    elif move.endswith("'"):
        fn = _BASE_MOVES[move[:-1]]
        return fn(fn(fn(cube)))
    else:
        return _BASE_MOVES[move](cube)


def _apply_moves_idiomatic(
    cube: dict[str, list[int]], moves: list[str]
) -> dict[str, list[int]]:
    """Apply a sequence of moves to a cube and return the resulting cube."""
    for move in moves:
        cube = _apply_move_idiomatic(cube, move)
    return cube


def _cube_solved_idiomatic(cube: dict[str, list[int]]) -> bool:
    """Return True if every sticker on the cube matches its face centre."""
    return all(cube[face][i] == cube[face][4] for face in _FACE_ORDER for i in range(9))


def _heuristic_idiomatic(cube: dict[str, list[int]]) -> int:
    """Return a lower-bound estimate of moves needed to solve the cube."""
    return sum(
        1 for face in _FACE_ORDER for i in range(9) if cube[face][i] != cube[face][4]
    ) // 8


def _ida_star_idiomatic(
    cube: dict[str, list[int]], max_depth: int
) -> list[str] | None:
    """Run IDA* on the cube and return a solution move list, or None."""

    def search(
        state: dict[str, list[int]],
        g: int,
        bound: int,
        path: list[str],
        last_move: str | None,
    ) -> int | list[str]:
        """Recursively search for a solution within the current bound."""
        h = _heuristic_idiomatic(state)
        f = g + h
        if f > bound:
            return f
        if _cube_solved_idiomatic(state):
            return path[:]
        minimum = 10 ** 9
        for move in _ALL_MOVES:
            if last_move is not None:
                if _face_of(move) == _face_of(last_move):
                    continue
                if move == _INVERSE.get(last_move):
                    continue
            new_state = _apply_move_idiomatic(state, move)
            path.append(move)
            result = search(new_state, g + 1, bound, path, move)
            path.pop()
            if isinstance(result, list):
                return result
            if result < minimum:
                minimum = result
        return minimum

    bound = _heuristic_idiomatic(cube)
    path: list[str] = []
    while bound <= max_depth:
        result = search(cube, 0, bound, path, None)
        if isinstance(result, list):
            return result
        if result == 10 ** 9:
            return None
        bound = result
    return None


def _solved_cube_functional() -> tuple[tuple[int, ...], ...]:
    """Return a solved cube in the functional tuple representation.

    Six face-tuples in order (U, D, F, B, L, R), each a 9-tuple of colour indices.
    """
    return tuple(tuple([color] * 9) for color in range(6))


def _rotate_face_cw_functional(face: tuple[int, ...]) -> tuple[int, ...]:
    """Return a new face tuple rotated 90° clockwise."""
    return (
        face[6], face[3], face[0],
        face[7], face[4], face[1],
        face[8], face[5], face[2],
    )


def _move_U_f(cube: tuple[tuple[int, ...], ...]) -> tuple[tuple[int, ...], ...]:
    """Apply a U move (clockwise) and return the new cube tuple."""
    U, D, F, B, L, R = cube
    return (
        _rotate_face_cw_functional(U), D,
        (R[0], R[1], R[2], F[3], F[4], F[5], F[6], F[7], F[8]),
        (L[0], L[1], L[2], B[3], B[4], B[5], B[6], B[7], B[8]),
        (F[0], F[1], F[2], L[3], L[4], L[5], L[6], L[7], L[8]),
        (B[0], B[1], B[2], R[3], R[4], R[5], R[6], R[7], R[8]),
    )


def _move_D_f(cube: tuple[tuple[int, ...], ...]) -> tuple[tuple[int, ...], ...]:
    """Apply a D move (clockwise) and return the new cube tuple."""
    U, D, F, B, L, R = cube
    return (
        U, _rotate_face_cw_functional(D),
        (F[0], F[1], F[2], F[3], F[4], F[5], L[6], L[7], L[8]),
        (B[0], B[1], B[2], B[3], B[4], B[5], R[6], R[7], R[8]),
        (L[0], L[1], L[2], L[3], L[4], L[5], B[6], B[7], B[8]),
        (R[0], R[1], R[2], R[3], R[4], R[5], F[6], F[7], F[8]),
    )


def _move_F_f(cube: tuple[tuple[int, ...], ...]) -> tuple[tuple[int, ...], ...]:
    """Apply an F move (clockwise) and return the new cube tuple."""
    U, D, F, B, L, R = cube
    return (
        (U[0], U[1], U[2], U[3], U[4], U[5], L[8], L[5], L[2]),
        (R[0], R[3], R[6], D[3], D[4], D[5], D[6], D[7], D[8]),
        _rotate_face_cw_functional(F), B,
        (L[0], L[1], D[2], L[3], L[4], D[1], L[6], L[7], D[0]),
        (U[6], R[1], R[2], U[7], R[4], R[5], U[8], R[7], R[8]),
    )


def _move_B_f(cube: tuple[tuple[int, ...], ...]) -> tuple[tuple[int, ...], ...]:
    """Apply a B move (clockwise) and return the new cube tuple."""
    U, D, F, B, L, R = cube
    return (
        (R[8], R[5], R[2], U[3], U[4], U[5], U[6], U[7], U[8]),
        (D[0], D[1], D[2], D[3], D[4], D[5], L[6], L[3], L[0]),
        F, _rotate_face_cw_functional(B),
        (U[2], L[1], L[2], U[1], L[4], L[5], U[0], L[7], L[8]),
        (R[0], R[1], D[6], R[3], R[4], D[7], R[6], R[7], D[8]),
    )


def _move_L_f(cube: tuple[tuple[int, ...], ...]) -> tuple[tuple[int, ...], ...]:
    """Apply an L move (clockwise) and return the new cube tuple."""
    U, D, F, B, L, R = cube
    return (
        (B[8], U[1], U[2], B[5], U[4], U[5], B[2], U[7], U[8]),
        (F[0], D[1], D[2], F[3], D[4], D[5], F[6], D[7], D[8]),
        (U[0], F[1], F[2], U[3], F[4], F[5], U[6], F[7], F[8]),
        (B[0], B[1], D[6], B[3], B[4], D[3], B[6], B[7], D[0]),
        _rotate_face_cw_functional(L), R,
    )


def _move_R_f(cube: tuple[tuple[int, ...], ...]) -> tuple[tuple[int, ...], ...]:
    """Apply an R move (clockwise) and return the new cube tuple."""
    U, D, F, B, L, R = cube
    return (
        (U[0], U[1], F[2], U[3], U[4], F[5], U[6], U[7], F[8]),
        (D[0], D[1], B[6], D[3], D[4], B[3], D[6], D[7], B[0]),
        (F[0], F[1], D[2], F[3], F[4], D[5], F[6], F[7], D[8]),
        (U[8], B[1], B[2], U[5], B[4], B[5], U[2], B[7], B[8]),
        L, _rotate_face_cw_functional(R),
    )


_BASE_MOVES_F: dict[str, Any] = {
    "U": _move_U_f, "D": _move_D_f, "F": _move_F_f,
    "B": _move_B_f, "L": _move_L_f, "R": _move_R_f,
}


def _apply_move_functional(
    cube: tuple[tuple[int, ...], ...], move: str
) -> tuple[tuple[int, ...], ...]:
    """Apply a single move (including prime and double variants) to a cube tuple."""
    if move.endswith("2"):
        fn = _BASE_MOVES_F[move[:-1]]
        return fn(fn(cube))
    elif move.endswith("'"):
        fn = _BASE_MOVES_F[move[:-1]]
        return fn(fn(fn(cube)))
    else:
        return _BASE_MOVES_F[move](cube)


def _apply_moves_functional(
    cube: tuple[tuple[int, ...], ...], moves: list[str]
) -> tuple[tuple[int, ...], ...]:
    """Apply a sequence of moves to a functional cube and return the result."""
    for move in moves:
        cube = _apply_move_functional(cube, move)
    return cube


def _cube_solved_functional(cube: tuple[tuple[int, ...], ...]) -> bool:
    """Return True if every sticker on the functional cube matches its face centre."""
    return all(cube[fi][i] == cube[fi][4] for fi in range(6) for i in range(9))


def _heuristic_functional(cube: tuple[tuple[int, ...], ...]) -> int:
    """Return a lower-bound estimate of moves needed to solve the functional cube."""
    return sum(
        1 for fi in range(6) for i in range(9) if cube[fi][i] != cube[fi][4]
    ) // 8


def _ida_star_functional(
    cube: tuple[tuple[int, ...], ...], max_depth: int
) -> list[str] | None:
    """Run IDA* on a functional cube and return a solution move list, or None."""

    def search(
        state: tuple[tuple[int, ...], ...],
        g: int,
        bound: int,
        path: list[str],
        last_move: str | None,
    ) -> int | list[str]:
        """Recursively search for a solution within the current bound."""
        h = _heuristic_functional(state)
        f = g + h
        if f > bound:
            return f
        if _cube_solved_functional(state):
            return path[:]
        minimum = 10 ** 9
        for move in _ALL_MOVES:
            if last_move is not None:
                if _face_of(move) == _face_of(last_move):
                    continue
                if move == _INVERSE.get(last_move):
                    continue
            new_state = _apply_move_functional(state, move)
            path.append(move)
            result = search(new_state, g + 1, bound, path, move)
            path.pop()
            if isinstance(result, list):
                return result
            if result < minimum:
                minimum = result
        return minimum

    bound = _heuristic_functional(cube)
    path: list[str] = []
    while bound <= max_depth:
        result = search(cube, 0, bound, path, None)
        if isinstance(result, list):
            return result
        if result == 10 ** 9:
            return None
        bound = result
    return None


class Suite(BenchmarkSuite):
    """Benchmark suite comparing Menai, idiomatic Python, and functional Python Rubik's cube IDA* solvers."""

    name = "rubiks_cube"
    description = "Solve scrambled Rubik's cubes of increasing depth using IDA*."

    def cases(self) -> list[BenchmarkCase]:
        """Return one case per scramble sequence."""
        return [
            BenchmarkCase(name=name, input=moves, iterations=1)
            for name, moves in _SCRAMBLES
        ]

    def implementations(self, menai: Menai) -> list[Implementation]:
        """Return Menai, idiomatic Python, and functional Python IDA* solver implementations."""
        def run_menai(scramble_moves: list[str]) -> tuple[list[str], list[str]]:
            """Scramble the cube via Menai and solve it with the Menai IDA* solver."""
            moves_literal = "(list " + " ".join(f'"{m}"' for m in scramble_moves) + ")"
            expr = (
                '(let ((rubiks (import "rubiks_cube")))'
                '  (let ((solved-cube-fn  (dict-get rubiks "solved-cube"))'
                '        (apply-moves-fn  (dict-get rubiks "apply-moves"))'
                '        (ida-star-fn     (dict-get rubiks "ida-star")))'
                f'    (let ((scrambled (apply-moves-fn (solved-cube-fn) {moves_literal})))'
                '      (ida-star-fn scrambled 20))))'
            )
            raw = menai.evaluate(expr)
            if not isinstance(raw, dict):
                raise ValueError(f"Unexpected Menai result type: {type(raw)}")
            found = raw.get("found", False)
            value = raw.get("value")
            if not found or not isinstance(value, (list, tuple)):
                raise ValueError(f"Menai solver found no solution for {scramble_moves}")
            return (scramble_moves, list(value))

        def run_python_idiomatic(scramble_moves: list[str]) -> tuple[list[str], list[str]]:
            """Scramble the cube and solve it using the idiomatic Python IDA* solver."""
            scrambled = _apply_moves_idiomatic(_solved_cube_idiomatic(), scramble_moves)
            solution = _ida_star_idiomatic(scrambled, 20)
            if solution is None:
                raise ValueError(f"Idiomatic solver found no solution for {scramble_moves}")
            return (scramble_moves, solution)

        def run_python_functional(scramble_moves: list[str]) -> tuple[list[str], list[str]]:
            """Scramble the cube and solve it using the functional Python IDA* solver."""
            scrambled = _apply_moves_functional(_solved_cube_functional(), scramble_moves)
            solution = _ida_star_functional(scrambled, 20)
            if solution is None:
                raise ValueError(f"Functional solver found no solution for {scramble_moves}")
            return (scramble_moves, solution)

        return [
            Implementation(name="Menai", run=run_menai),
            Implementation(name="Python (idiomatic)", run=run_python_idiomatic),
            Implementation(name="Python (functional)", run=run_python_functional),
        ]

    def results_equal(self, a: Any, b: Any) -> bool:
        """Return True if both results solve the same scramble.

        Each result is a ``(scramble_moves, solution_moves)`` tuple.  The
        scramble is applied to a solved cube and then the solution from each
        result is applied independently; both must yield a solved cube.
        """
        scramble_a, solution_a = a
        scramble_b, solution_b = b

        cube_a = _apply_moves_idiomatic(_solved_cube_idiomatic(), scramble_a)
        cube_a = _apply_moves_idiomatic(cube_a, solution_a)
        if not _cube_solved_idiomatic(cube_a):
            return False

        cube_b = _apply_moves_idiomatic(_solved_cube_idiomatic(), scramble_b)
        cube_b = _apply_moves_idiomatic(cube_b, solution_b)
        return _cube_solved_idiomatic(cube_b)
