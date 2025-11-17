# Editor Operations Refactoring Proposal

## Problem Statement

The current `editor_replace_lines` operation is ambiguous and leads to confusion when the AI tries to edit files. The operation tries to do too much:

1. **Delete lines**: `replace_lines(3, 5, "")` - delete lines 3-5
2. **Replace lines**: `replace_lines(3, 5, "new content\n")` - replace lines 3-5 with new content
3. **Insert lines**: Unclear semantics - should it insert before or after?

This ambiguity causes problems:
- When deleting the Kotlin code, we wanted to remove lines 9-12 but keep spacing
- When inserting, it's unclear if we're inserting before line N, after line N, or replacing line N
- The operation mixes concerns: deletion, insertion, and replacement

## Proposed Solution: Two Orthogonal Operations

Replace `editor_replace_lines` with two clear, orthogonal operations:

### 1. `editor_delete_lines`

**Purpose**: Delete one or more complete lines from the document.

**Parameters**:
- `tab_id` (required): Editor tab identifier
- `start_line` (required): First line to delete (1-indexed)
- `end_line` (required): Last line to delete (1-indexed, inclusive)
- `move_cursor_after` (optional, default true): Whether to position cursor after deletion

**Behavior**:
- Deletes lines `start_line` through `end_line` inclusive
- Removes the newline characters of deleted lines
- If deleting the last line, doesn't create a trailing newline
- Cursor positioned at start of first line after deletion (or end of document)

**Examples**:
```python
# Delete line 5
editor_delete_lines(tab_id="...", start_line=5, end_line=5)

# Delete lines 10-15
editor_delete_lines(tab_id="...", start_line=10, end_line=15)
```

### 2. `editor_insert_lines`

**Purpose**: Insert new lines at a specific position in the document.

**Parameters**:
- `tab_id` (required): Editor tab identifier
- `line` (required): Line number where to insert (1-indexed)
- `position` (required): Either "before" or "after"
- `content` (required): Content to insert (should end with \n for complete lines)
- `move_cursor_after` (optional, default true): Whether to position cursor after insertion

**Behavior**:
- Inserts `content` before or after the specified `line`
- If `position="before"`, content is inserted at the start of line `line`
- If `position="after"`, content is inserted after line `line` (after its newline)
- **Special case**: If inserting after a line that doesn't have a trailing newline (typically the last line), a newline is automatically added before the inserted content
- Cursor positioned after inserted content

**Examples**:
```python
# Insert a new line before line 5
editor_insert_lines(tab_id="...", line=5, position="before", content="new line\n")

# Insert multiple lines after line 10
editor_insert_lines(tab_id="...", line=10, position="after", 
                   content="line 1\nline 2\nline 3\n")

# Append to end of file (after last line)
# If last line has no newline, one is added automatically
editor_insert_lines(tab_id="...", line=<last_line>, position="after", 
                   content="new content\n")
```

## Why Two Operations Are Sufficient

With just `delete_lines` and `insert_lines`, we can accomplish everything:

1. **Delete lines**: Use `editor_delete_lines`
2. **Insert lines**: Use `editor_insert_lines` 
3. **Replace lines**: Combine operations:
   ```python
   # Replace lines 5-7 with new content
   editor_delete_lines(start_line=5, end_line=7)
   editor_insert_lines(line=5, position="before", content="new content\n")
   ```

This composition approach is clearer and more explicit about what's happening.

## Benefits of This Approach

1. **Clear Intent**: Each operation has a single, well-defined purpose
2. **No Ambiguity**: 
   - Want to delete? Use `delete_lines`
   - Want to insert? Use `insert_lines` with clear before/after semantics
   - Want to replace? Delete then insert - explicit about the steps
3. **Easier for AI**: The AI can reason about which operation to use without confusion
4. **Composable**: Operations can be combined to achieve complex edits
5. **Predictable**: Each operation does exactly what its name suggests
6. **Simpler Implementation**: Fewer operations to implement and maintain

## Implementation Notes

### Current `replace_lines` Implementation

The current implementation (in `editor_widget.py` lines 1575-1627) handles:
- Validation of line numbers
- Finding the text blocks for start and end lines
- Selecting the range (including newlines)
- Replacing with new content

### Implementation Strategy

1. **Remove `editor_replace_lines`** completely (no existing uses to worry about)

2. **Implement `editor_delete_lines`** in `editor_widget.py`:
   ```python
   def delete_lines(self, start_line: int, end_line: int, move_cursor_after: bool = True) -> None:
       """Delete one or more complete lines from the document."""
       # Similar to replace_lines but removes content entirely
       # Handle newline removal carefully
   ```

3. **Implement `editor_insert_lines`** in `editor_widget.py`:
   ```python
   def insert_lines(self, line: int, position: str, content: str, move_cursor_after: bool = True) -> None:
       """Insert new lines at a specific position."""
       # For "before": position cursor at start of line, insert content
       # For "after": position cursor at end of line
       #   - Check if line has trailing newline
       #   - If not (typically last line), add one first
       #   - Then insert content
   ```

4. **Update `editor_tab.py`**:
   - Remove `replace_lines` method
   - Add `delete_lines` method (delegates to editor_widget)
   - Add `insert_lines` method (delegates to editor_widget)

5. **Update `system_ai_tool.py`**:
   - Remove `editor_replace_lines` operation definition
   - Add `editor_delete_lines` operation definition
   - Add `editor_insert_lines` operation definition
   - Update tool description and parameters

### Edge Cases to Handle

1. **Empty file**: 
   - Delete: Error if trying to delete from empty file
   - Insert: Should work with line=1, position="before" or "after"

2. **Last line without newline**: 
   - Delete: Remove without adding newline
   - Insert after: Add newline before inserting content

3. **Invalid line numbers**: 
   - Clear error messages
   - Delete: start_line and end_line must be valid
   - Insert: line must be valid (1 to line_count)

4. **Inserting after last line**: 
   - Check if last line has newline
   - Add one if missing (since we're inserting a complete line)

5. **Deleting all lines**: 
   - Should result in an empty file (no lines)

6. **Position parameter validation**:
   - Must be exactly "before" or "after"
   - Clear error if invalid

### Newline Handling for Insert

When inserting after a line that doesn't have a trailing newline:
```python
# Example: file ends with "last line" (no \n)
# User wants to insert after it

# Current state:
# Line 1: "first line\n"
# Line 2: "last line"  (no newline)

# Insert after line 2:
editor_insert_lines(line=2, position="after", content="new line\n")

# Implementation should:
# 1. Detect that line 2 has no trailing newline
# 2. Add a newline to line 2: "last line\n"
# 3. Insert the new content: "new line\n"

# Result:
# Line 1: "first line\n"
# Line 2: "last line\n"
# Line 3: "new line\n"
```

## Migration Example

Current approach (ambiguous):
```python
# Delete line 3 - unclear if this is right
editor_replace_lines(start_line=3, end_line=3, new_lines="")

# Insert before line 5 - have to replace it and put it back!
editor_replace_lines(start_line=5, end_line=5, new_lines="new line\nold line 5 content\n")

# Replace lines 10-12 with new content
editor_replace_lines(start_line=10, end_line=12, new_lines="new content\n")
```

New approach (clear):
```python
# Delete line 3 - obvious
editor_delete_lines(start_line=3, end_line=3)

# Insert before line 5 - obvious
editor_insert_lines(line=5, position="before", content="new line\n")

# Replace lines 10-12 with new content - explicit two-step process
editor_delete_lines(start_line=10, end_line=12)
editor_insert_lines(line=10, position="before", content="new content\n")
```

## Summary

Replace the ambiguous `editor_replace_lines` with two orthogonal operations:
- `editor_delete_lines`: Remove lines
- `editor_insert_lines`: Add lines at a specific position

The key insight is that **replacement is just deletion + insertion**, and making this explicit is clearer than having a combined operation. The special handling for inserting after lines without trailing newlines ensures that line-based operations work correctly even at the end of files.
