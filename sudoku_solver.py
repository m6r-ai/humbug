"""
Sudoku Solver using Backtracking Algorithm with MRV Heuristic

This module provides functions to solve Sudoku puzzles using a backtracking approach
with the Minimum Remaining Values (MRV) heuristic for improved performance.
Empty cells are represented by 0.
"""

from typing import List, Tuple, Optional


def print_board(board: List[List[int]]) -> None:
    """
    Print the Sudoku board in a readable format.
    
    Args:
        board: 9x9 list representing the Sudoku board
    """
    for i in range(9):
        if i % 3 == 0 and i != 0:
            print("-" * 21)
        
        for j in range(9):
            if j % 3 == 0 and j != 0:
                print("|", end=" ")
            
            if j == 8:
                print(board[i][j])
            else:
                print(str(board[i][j]) + " ", end="")


def is_valid(board: List[List[int]], num: int, pos: Tuple[int, int]) -> bool:
    """
    Check if placing a number at a position is valid according to Sudoku rules.
    
    Args:
        board: 9x9 list representing the Sudoku board
        num: Number to place (1-9)
        pos: Tuple (row, col) representing the position
    
    Returns:
        bool: True if the placement is valid, False otherwise
    """
    row, col = pos
    
    # Check row
    for j in range(9):
        if board[row][j] == num and col != j:
            return False
    
    # Check column
    for i in range(9):
        if board[i][col] == num and row != i:
            return False
    
    # Check 3x3 box
    box_row = (row // 3) * 3
    box_col = (col // 3) * 3
    
    for i in range(box_row, box_row + 3):
        for j in range(box_col, box_col + 3):
            if board[i][j] == num and (i, j) != pos:
                return False
    
    return True


def find_empty(board: List[List[int]]) -> Optional[Tuple[int, int]]:
    """
    Find an empty cell in the board (represented by 0).
    
    Args:
        board: 9x9 list representing the Sudoku board
    
    Returns:
        tuple: (row, col) of empty cell, or None if no empty cells
    """
    for i in range(9):
        for j in range(9):
            if board[i][j] == 0:
                return (i, j)
    return None


def find_empty_mrv(board: List[List[int]]) -> Optional[Tuple[int, int]]:
    """
    Find an empty cell with the minimum remaining values (MRV heuristic).
    This significantly improves performance by choosing the most constrained cell first.
    
    Args:
        board: 9x9 list representing the Sudoku board
    
    Returns:
        tuple: (row, col) of empty cell with fewest valid options, or None if no empty cells
    """
    min_options = 10  # Maximum possible is 9
    best_cell = None
    
    for i in range(9):
        for j in range(9):
            if board[i][j] == 0:
                # Count how many valid numbers can go in this cell
                valid_count = 0
                for num in range(1, 10):
                    if is_valid(board, num, (i, j)):
                        valid_count += 1
                
                # If this cell has fewer options, it's a better choice
                if valid_count < min_options:
                    min_options = valid_count
                    best_cell = (i, j)
                    
                    # If we find a cell with only one option, we can't do better
                    if min_options == 1:
                        return best_cell
    
    return best_cell


def solve(board: List[List[int]], use_mrv: bool = True) -> bool:
    """
    Solve the Sudoku puzzle using backtracking algorithm.
    
    Args:
        board: 9x9 list representing the Sudoku board (modified in place)
        use_mrv: If True, use MRV heuristic (default); if False, use simple search
    
    Returns:
        bool: True if solution found, False otherwise
    """
    # Choose which strategy to use for finding empty cells
    if use_mrv:
        empty = find_empty_mrv(board)
    else:
        empty = find_empty(board)
    
    # Base case: no empty cells means puzzle is solved
    if not empty:
        return True
    
    row, col = empty
    
    # Try numbers 1-9
    for num in range(1, 10):
        if is_valid(board, num, (row, col)):
            board[row][col] = num
            
            # Recursively try to solve the rest
            if solve(board, use_mrv):
                return True
            
            # Backtrack if solution not found
            board[row][col] = 0
    
    return False


def is_valid_board(board: List[List[int]]) -> bool:
    """
    Check if the initial board configuration is valid.
    
    Args:
        board: 9x9 list representing the Sudoku board
    
    Returns:
        bool: True if valid, False otherwise
    """
    if len(board) != 9:
        return False
    
    for row in board:
        if len(row) != 9:
            return False
        for num in row:
            if not isinstance(num, int) or num < 0 or num > 9:
                return False
    
    # Check for duplicate numbers in initial configuration
    for i in range(9):
        for j in range(9):
            if board[i][j] != 0:
                num = board[i][j]
                board[i][j] = 0  # Temporarily set to 0 to check
                if not is_valid(board, num, (i, j)):
                    board[i][j] = num  # Restore
                    return False
                board[i][j] = num  # Restore
    
    return True


def main() -> None:
    """
    Example usage of the Sudoku solver.
    """
    # Example Sudoku puzzle (0 represents empty cells)
    board = [
        [5, 3, 0, 0, 7, 0, 0, 0, 0],
        [6, 0, 0, 1, 9, 5, 0, 0, 0],
        [0, 9, 8, 0, 0, 0, 0, 6, 0],
        [8, 0, 0, 0, 6, 0, 0, 0, 3],
        [4, 0, 0, 8, 0, 3, 0, 0, 1],
        [7, 0, 0, 0, 2, 0, 0, 0, 6],
        [0, 6, 0, 0, 0, 0, 2, 8, 0],
        [0, 0, 0, 4, 1, 9, 0, 0, 5],
        [0, 0, 0, 0, 8, 0, 0, 7, 9]
    ]
    
    print("Original Sudoku Puzzle:")
    print("=" * 21)
    print_board(board)
    print()
    
    if not is_valid_board(board):
        print("Error: Invalid board configuration!")
        return
    
    print("Solving with MRV heuristic...")
    print()
    
    if solve(board, use_mrv=True):
        print("Solution:")
        print("=" * 21)
        print_board(board)
    else:
        print("No solution exists for this puzzle.")


if __name__ == "__main__":
    main()
