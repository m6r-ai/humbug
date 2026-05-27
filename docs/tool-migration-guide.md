# Tool Migration Guide

## Overview

This guide describes the migration of Humbug's tool and tab architecture from its
current form to a new `src/tools/` structure.  The goal is to make tools genuinely
pluggable: adding a new tool should require no changes to any central file.

The migration is designed to be done one tool at a time, with the application fully
functional after each step.

---

## Target Architecture

### Principles

- A **tool** is a capability available within the mindspace, usable by the AI, the
  human, or both.  The distinction between "AI tool" and "UI component" is dropped.
- Each tool owns all of its layers: AI interface, context model, and tab widget.
- The `src/mindspace/` package knows nothing about specific tools.  It owns only
  lifecycle infrastructure (`ContextRegistry`, `ContextInfo`).
- `ColumnManager` knows nothing about specific tab types.  It dispatches to a
  **tab factory registry** keyed by tool name string.
- `ContextType` and `TabType` enums are eliminated.  Tool identity is a plain string
  (the tool's registered name).

### Directory Structure

Each tool lives under `src/tools/<toolname>/` with up to three subdirectories:

```text
src/tools/
└── <toolname>/
    ├── tool/                    # AI tool implementation (no GUI dependency)
    │   └── <toolname>_tool.py
    ├── context/                 # Context model (no GUI dependency)
    │   └── <toolname>_context.py
    └── tab/                     # PySide6 tab widget and supporting widgets
        ├── <toolname>_tab.py
        └── <toolname>_widget.py (etc.)
```

Not all subdirectories are required:

| Tool        | tool/ | context/ | tab/ |
|-------------|-------|----------|------|
| clock       | ✓     |          |      |
| filesystem  | ✓     |          |      |
| menai       | ✓     |          |      |
| help        | ✓     |          |      |
| delegate    | ✓     |          |      |
| system      | ✓     |          |      |
| editor      | ✓     | ✓        | ✓    |
| terminal    | ✓     | ✓        | ✓    |
| conversation| ✓     | ✓        | ✓    |
| preview     | ✓     | ✓        | ✓    |
| diff        |       |          | ✓    |
| log         |       |          | ✓    |
| shell       |       |          | ✓    |

### Dependency Rules

Within a tool:
- `tool/` must not import from `context/` or `tab/`
- `context/` must not import from `tab/`
- `tab/` may import from `tool/` and `context/`

Across tools:
- Tools must not import from each other's `tab/` subdirectories
- Tools may import from each other's `tool/` or `context/` subdirectories only
  where there is a genuine semantic dependency (e.g. `system` tool knowing about
  `conversation` context type string)

---

## Infrastructure Changes

This work is complete.  Before starting the per-tool migrations, three housekeeping
tasks remain:

1. **Update `dependency-rules.yaml`** to add entries for `src/tools/` (or individual
   tool packages as they are created).  Without this the dependency checker will
   reject imports from the new location.  This must be done before the first tool
   moves.

2. **Delete dead files** — `src/humbug/tabs/tab_type.py` and
   `src/mindspace/context/context_type.py` are no longer imported anywhere.
   They can be deleted at any time.

3. **Flatten `src/mindspace/context/`** — now that `ContextType` is gone the
   package contains only `ContextInfo`, `ContextRegistry`, and three context models
   that will move to `src/tools/` during their respective tool migrations.  Once
   those models have moved, the remaining two files can be moved directly into
   `src/mindspace/` and the `context/` subdirectory deleted.

---

## Per-Tool Migration Steps

Once the infrastructure changes are in place, each tool can be migrated
independently.  The application must be fully functional after each tool migration.

### Migration procedure for a tool with tool/ + context/ + tab/

Use `editor` as the example.

#### Step 1 — Create the new directory structure

```text
src/tools/editor/
src/tools/editor/tool/
src/tools/editor/context/
src/tools/editor/tab/
```

Add `__init__.py` to each.

#### Step 2 — Move context files

Move (do not copy) context-layer files:

- `src/humbug/tabs/editor/editor_context.py`
  → `src/tools/editor/context/editor_context.py`
- `src/humbug/tabs/editor/editor_diff_applier.py`
  → `src/tools/editor/context/editor_diff_applier.py`

Update all imports.  Verify no `tab/` imports have crept in.

#### Step 3 — Move tab files

Move all tab-layer files:

- `src/humbug/tabs/editor/editor_tab.py`
  → `src/tools/editor/tab/editor_tab.py`
- `src/humbug/tabs/editor/editor_widget.py`
  → `src/tools/editor/tab/editor_widget.py`
- `src/humbug/tabs/editor/editor_goto_line_dialog.py`
  → `src/tools/editor/tab/editor_goto_line_dialog.py`

Update all imports.

#### Step 5 — Move tool file

Move:

- `src/humbug/tabs/editor/editor_ai_tool.py`
  → `src/tools/editor/tool/editor_tool.py`

Rename the class from `EditorAITool` to `EditorTool`.  Update all imports and
registration in `main_window.py`.

#### Step 5 — Update factory registration in `main_window.py`

Update the `register_tab_factory` call for `"editor"` in `main_window.py` to import
from the new path.

#### Step 6 — Verify

- Open and edit a file in an editor tab
- Move the editor tab between columns (exercises `register_context_models`)
- Apply a diff via the AI tool
- Save via the AI tool
- Close and reopen the mindspace (exercises session restore via factory)
- Confirm no imports of old paths remain

#### Step 7 — Clean up

- Delete `src/humbug/tabs/editor/` if empty
- Remove the `EditorTab` branch from `ColumnManager._on_context_opened`
- Remove `_find_editor_tab_by_path` and any remaining `isinstance(tab, EditorTab)`
  checks from `ColumnManager`

---

### Migration procedure for a tool with tab/ only

Use `log` as the example.

Steps 2 and 4 are skipped (no context, no AI tool).  Steps 1, 3, 5, 6, 7 apply
as above.

If the tool acquires an AI interface or context model later, steps 2 and 4 can be
added at that point without touching anything outside the tool's directory.

---

### Migration procedure for a tool with tool/ only

Use `clock` as the example.

Steps 2, 3, and 5 are skipped (no context, no tab).  Only steps 1, 4, 6, and 7
apply.

---

## GUI Integration Points

Three areas of the existing GUI couple `main_window.py` and `MindspaceView` to
specific tab types.  Each needs to be addressed as part of the migration.

### 1. Named tab-creation methods on `ColumnManager`

`main_window.py` currently calls type-specific methods such as `new_conversation()`,
`open_file()`, `open_diff()`, `open_preview_page()`, and `new_terminal()`.
`ColumnManager` also has `show_system_log()` and `show_system_shell()` which find
or create singleton tabs by type.

After migration, `ColumnManager` exposes a single generic entry point:

```python
def open(self, context_type: str, path: str = "", ephemeral: bool = False) -> str:
    """Open or focus a tab for the given tool name, returning the context_id."""
```

`main_window.py` passes the tool name string directly (`"conversation"`, `"editor"`,
`"terminal"` etc.).  The named methods on `ColumnManager` are kept as thin wrappers
during migration and deleted once all callers have been updated.

The singleton pattern for `log` and `shell` (find-existing-or-create) moves into
each tool's tab factory, which can check the `ContextRegistry` for an existing
instance before creating a new one.

### 2. Mapping active tabs to the mindspace tree view

`main_window.py._map_tab_to_mindspace_view` currently switches on `isinstance` to
map a tab to a `MindspaceViewType`, so the mindspace tree can reveal and select the
corresponding file when the active tab changes.

After migration, `TabBase` gains an optional method:

```python
def mindspace_view_type(self) -> str | None:
    """
    Return the mindspace view type name this tab belongs to, or None if it
    should not be revealed in the mindspace tree.
    """
    return None
```

Each tab overrides this to return the appropriate string (`"conversations"`,
`"files"`, `"preview"`, `"vcs"`).  `main_window.py` calls `tab.mindspace_view_type()`
directly and the `isinstance` switch is deleted.  `MindspaceViewType` itself is
unchanged — it remains a GUI-layer enum owned by `src/humbug/`.

### 3. Mindspace tree opening files in tabs

`MindspaceView` emits signals when the user clicks a file, and `main_window.py`
connects these to `ColumnManager.open_file_by_mindspace_view_type()`, which
contains a `MindspaceViewType` → tab-creation switch.

After migration, each tool's tab factory registers a mapping from its
`mindspace_view_type` string to itself.  `ColumnManager` exposes:

```python
def open_from_mindspace_view(
    self, view_type: MindspaceViewType, path: str, ephemeral: bool
) -> str | None:
    """Open a tab for the given mindspace view type and path."""
```

This method looks up the tool name for the given `MindspaceViewType`, then
delegates to the generic `open()` entry point.  The mapping from `MindspaceViewType`
to tool name is registered by each tool's tab factory alongside the tab factory
itself, so `ColumnManager` never contains a hardcoded switch.

`open_file_by_mindspace_view_type` is kept as a wrapper during migration and deleted
at the end.

### Migration order for GUI integration points

These changes can be introduced incrementally alongside the per-tool migrations:
- Add `mindspace_view_type()` to `TabBase` (returning `None`) before any tool
  migrations, so each tab can override it as it moves
- Replace `_map_tab_to_mindspace_view` in `main_window.py` once all tabs have been
  migrated and each overrides `mindspace_view_type()`
- Replace `open_file_by_mindspace_view_type` in `ColumnManager` once all tab
  factories have registered their `MindspaceViewType` mappings

---

## Migration Order

The recommended order minimises risk and keeps the application working at each step:

1. **Housekeeping** — update `dependency-rules.yaml`; delete `tab_type.py` and
   `context_type.py`; optionally flatten `src/mindspace/context/`
2. **clock** — simplest tool, no context, no tab; validates the tool/ migration path
3. **menai** — similar to clock
4. **filesystem** — similar, but has mindspace dependency
5. **help** — depends on `AIToolManager`, migrate after other tools are settled
6. **log** — simplest tab-only tool; validates the tab/ migration path and factory
7. **diff** — tab-only, slightly more complex
8. **shell** — tab-only, most complex of the tab-only tools
9. **editor** — first full tool/ + context/ + tab/ migration; highest value for
   validating the complete pattern
10. **terminal** — similar to editor
11. **preview** — similar to editor
12. **conversation** — most complex, leave until the pattern is proven
13. **delegate** — has GUI callback dependency; migrate after conversation
14. **system** — has GUI callback dependency; migrate last

---

## What Remains in `src/humbug/`

After all migrations:

- `main_window.py` — application entry point; registers tools and factories at
  startup; should shrink significantly
- `column_manager.py` — manages columns and tab lifecycle; no longer imports any
  specific tab or context type
- `mindspace/` — mindspace UI components (tree views, search, VCS panel etc.);
  unchanged
- `tabs/` — `TabBase`, `TabState`, `TabBar`, `ColumnWidget`, `FindWidget`, and other
  shared tab infrastructure; specific tab implementations have all moved to
  `src/tools/`
- Everything else (style, language, settings, user, status) — unchanged

---

## What is Deleted

- `src/mindspace/context/context_type.py`
- `src/humbug/tabs/tab_type.py`
- `src/humbug/tabs/editor/` (after editor migration)
- `src/humbug/tabs/terminal/` (after terminal migration)
- `src/humbug/tabs/conversation/` (after conversation migration)
- `src/humbug/tabs/preview/` (after preview migration)
- `src/humbug/tabs/diff/` (after diff migration)
- `src/humbug/tabs/log/` (after log migration)
- `src/humbug/tabs/shell/` (after shell migration)
- `src/clock_ai_tool/`, `src/filesystem_ai_tool/`, `src/menai_ai_tool/`,
  `src/help_ai_tool/`, `src/delegate_ai_tool/` (after respective migrations)
- `src/mindspace/context/conversation_context.py`,
  `src/mindspace/context/preview_context.py`,
  `src/mindspace/context/terminal_context.py` (after respective migrations)
