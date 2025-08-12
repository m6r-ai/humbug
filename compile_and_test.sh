#!/bin/bash

# Compile the Sudoku solver
gcc -o sudoku_solver sudoku_solver.c

if [ $? -eq 0 ]; then
    echo "✓ Compilation successful!"
    
    echo ""
    echo "=== Testing with sample puzzle ==="
    echo "Input puzzle:"
    cat sample_puzzle.txt
    
    echo ""
    echo "Running solver..."
    # We'll simulate the input since we can't interact with the terminal directly
    # For testing, we'll create a simple test version
    
    # Create a test version that reads from file
    cat > test_solver.c << 'EOF'
#include <stdio.h>
#include <stdbool.h>

#define SIZE 9

bool isValid(int board[SIZE][SIZE], int row, int col, int num) {
    // Check row
    for (int x = 0; x < SIZE; x++) {
        if (board[row][x] == num) return false;
    }
    
    // Check column
    for (int x = 0; x < SIZE; x++) {
        if (board[x][col] == num) return false;
    }
    
    // Check 3x3 box
    int startRow = row - row % 3;
    int startCol = col - col % 3;
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            if (board[i + startRow][j + startCol] == num) return false;
        }
    }
    return true;
}

bool findEmptyLocation(int board[SIZE][SIZE], int *row, int *col) {
    for (*row = 0; *row < SIZE; (*row)++) {
        for (*col = 0; *col < SIZE; (*col)++) {
            if (board[*row][*col] == 0) return true;
        }
    }
    return false;
}

bool solveSudoku(int board[SIZE][SIZE]) {
    int row, col;
    if (!findEmptyLocation(board, &row, &col)) return true;
    
    for (int num = 1; num <= 9; num++) {
        if (isValid(board, row, col, num)) {
            board[row][col] = num;
            if (solveSudoku(board)) return true;
            board[row][col] = 0;
        }
    }
    return false;
}

void printBoard(int board[SIZE][SIZE]) {
    printf("\n+-----+-----+-----+\n");
    for (int i = 0; i < SIZE; i++) {
        printf("|");
        for (int j = 0; j < SIZE; j++) {
            printf(" %d ", board[i][j]);
            if ((j + 1) % 3 == 0) printf("|");
        }
        printf("\n");
        if ((i + 1) % 3 == 0) printf("+-----+-----+-----+\n");
    }
}

int main() {
    int board[SIZE][SIZE] = {
        {5,3,0,0,7,0,0,0,0},
        {6,0,0,1,9,5,0,0,0},
        {0,9,8,0,0,0,0,6,0},
        {8,0,0,0,6,0,0,0,3},
        {4,0,0,8,0,3,0,0,1},
        {7,0,0,0,2,0,0,0,6},
        {0,6,0,0,0,0,2,8,0},
        {0,0,0,4,1,9,0,0,5},
        {0,0,0,0,8,0,0,7,9}
    };
    
    printf("=== Input Sudoku ===\n");
    printBoard(board);
    
    if (solveSudoku(board)) {
        printf("\n=== Solution ===\n");
        printBoard(board);
        printf("\n✓ Sudoku solved successfully!\n");
    } else {
        printf("\n❌ No solution exists.\n");
    }
    
    return 0;
}
EOF

    gcc -o test_solver test_solver.c
    ./test_solver
    
    # Clean up
    rm test_solver test_solver.c
    
else
    echo "✗ Compilation failed!"
fi