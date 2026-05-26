# Context Model Handoff

## What This Is

This document describes work in progress on the Humbug codebase to introduce a
clean model layer — the **context model** — that separates the AI tool layer from
the Qt GUI layer.  A previous AI session completed the foundation and two of the
four context types.  This document tells the next session what has been done, what
remains, and exactly how to complete it.

All tools must pass before and after every change:

```bash
source venv/bin/activate
mypy src
pylint src
python -m tools.dependency_checker check
python -m pytest tests/ -q
```

The current state is: all four tools pass cleanly.

---

## Background: The Architecture

### The Problem Being Solved

The AI tools previously reached directly into Qt widgets to do their work —
calling methods on `ColumnManager`, casting to `TabBase` subclasses, and driving
Qt viewport scrolling.  This made the tools untestable without a `QApplication`
and impossible to reuse in a non-Qt frontend.

### The Correct Causal Direction

The central principle is: **the model drives, the UI reacts**.

When an AI tool wants to open an editor, it should express that intent to the
model layer.  The Qt frontend observes the model event and creates the tab.  The
tool never calls Qt methods directly.

```
AI tool
  → mindspace.contexts().open(EDITOR, path=...) 
  → ContextRegistry emits OPENED
  → ColumnManager (subscribed) creates EditorTab
  → EditorTab creates EditorContext, registers it via register_model()
  → AI tool retrieves context_id from registry response
```

Not:

```
AI tool
  → column_manager.open_file(path)        ← WRONG: tool drives Qt
  → ColumnManager creates EditorTab
  → ColumnManager registers context as side effect
```

### The Solution

A new `src/mindspace/` package (Qt-free, enforced by the dependency checker)
contains:

- **`Mindspace`** — the core model: settings, interaction log, path resolution,
  session persistence.
- **`ContextRegistry`** — tracks all open contexts with a callback-based event
  system.  AI tools call `registry.open(...)` to express intent; the Qt
  `ColumnManager` subscribes to `OPENED`/`CLOSED` events and creates/destroys
  tabs in response.  Each context has a stable `context_id` (currently identical
  to the Qt `tab_id`).
- **Context models** — pure Python objects that own the data each AI tool needs.
  Registered via `registry.register_model(context_id, model)` and retrieved via
  `registry.get_model(context_id, ModelType)`.

The AI tools receive a `Mindspace` instance by constructor injection from
`MainWindow`.  They call `mindspace.contexts().open(...)` to open contexts and
`mindspace.contexts().get_model(...)` to operate on them.

### Key Files

```
src/mindspace/
    __init__.py
    mindspace.py                    ← core model, Qt-free
    mindspace_settings.py
    mindspace_error.py
    mindspace_log_level.py
    mindspace_message.py
    mindspace_interactions.py
    mindspace_search_engine.py
    mindspace_content_type.py
    context/
        __init__.py
        context_type.py             ← ContextType enum
        context_info.py             ← immutable ContextInfo snapshot
        context_registry.py         ← ContextRegistry with register_model/get_model
        conversation_context.py     ← DONE: ConversationContext
        terminal_context.py         ← DONE: TerminalContext
```

---

## What Is Already Done

### Steps 1-3: Foundation

- `src/mindspace/` package created, all files Qt-free, enforced by dependency
  checker.
- `Mindspace` core model extracted from `MindspaceManager`.
- `MindspaceManager` (Qt) is now a thin wrapper that delegates to `Mindspace`
  and adds Qt signals and directory tracking.
- All AI tools receive `Mindspace` by constructor injection — no singleton
  grabbing inside tool constructors.

### Step 4: ContextRegistry

- `ContextRegistry` with `ContextEvent` (OPENED, CLOSED, UPDATED, FOCUSED).
- `ContextInfo` frozen dataclass.
- `ContextType` enum.
- `register_model` / `get_model` generic model storage on the registry.
- **Current (transitional) wiring:** `ColumnManager._add_tab_to_column` calls
  `registry.open()` and `_remove_tab_from_column` calls `registry.close()`.
  This is the wrong causal direction (Qt drives model) and must be inverted as
  part of the remaining work — see Step 6 below.

### Step 5a: ConversationContext (DONE)

**File:** `src/mindspace/context/conversation_context.py`

Wraps `AITranscriptConversation`.  Implements:
- `get_conversation_info()` → metadata dict
- `read_messages(...)` → paginated/filtered message list
- `get_message_by_id_or_index(...)` → single message dict
- `search_messages(...)` → search results
- `scroll_to_message(...)` → fires `on_scroll_to_message` callback (Qt supplies
  this; CLI passes `None`)

**Current wiring:** `ColumnManager._add_tab_to_column` creates a
`ConversationContext` and registers it when a `ConversationTab` is added.  This
is transitional — it will be inverted in Step 6.

**`ConversationAITool`** (`src/humbug/tabs/conversation/conversation_ai_tool.py`):
Fully rewritten.  Constructor takes only `mindspace: Mindspace`.  All operations
retrieve `ConversationContext` via
`mindspace.contexts().get_model(tab_id, ConversationContext)`.

### Step 5b: TerminalContext (DONE)

**File:** `src/mindspace/context/terminal_context.py`

Owns a `TerminalBase` (PTY process) and `TerminalState` (buffer).  Implements:
- `get_buffer_content(max_lines)` → plain text
- `get_status()` → status dict
- `send_keystrokes(keystrokes)` → async, writes to PTY

**Structural change:** `TerminalState` is now created by `TerminalTab` (not
`TerminalWidget`).  `TerminalWidget` accepts an existing `TerminalState` via its
constructor and calls `state.set_response_callback(...)` to wire itself up.
`TerminalState.set_response_callback()` was added to
`src/terminal/terminal_state.py`.

**Current wiring:** `TerminalTab.__init__` creates `TerminalContext` and
registers it.  This is transitional — it will be inverted in Step 6.

**`TerminalAITool`** (`src/humbug/tabs/terminal/terminal_ai_tool.py`):
Fully rewritten.  Constructor takes only `mindspace: Mindspace`.  All operations
retrieve `TerminalContext` via
`mindspace.contexts().get_model(tab_id, TerminalContext)`.

---

## What Remains

### Step 5c: PreviewContext

**Goal:** A Qt-free `PreviewContext` that owns the raw content strings from
`PreviewContent` and implements search against them directly (rather than through
Qt widgets).

**Key insight:** `PreviewContent`
(`src/humbug/tabs/preview/preview_content.py`) is already almost pure Python —
it reads files and generates content as a list of `(PreviewContentType, str)`
tuples.  Its only Qt dependency is a single call to
`MindspaceManager().get_relative_path(...)` in `_generate_directory_content`,
which should be replaced with `mindspace.get_relative_path(...)` passed in at
construction.

**What to build:**

```python
# src/mindspace/context/preview_context.py
class PreviewContext:
    def __init__(
        self,
        context_id: str,
        path: str,
        content_blocks: list,           # list of (PreviewContentType, str)
        on_scroll_to_position=None,     # optional Qt callback
    ): ...
    def context_id(self) -> str: ...
    def get_info(self) -> dict: ...     # path, content_type, block_count
    def search(self, text, ...) -> dict: ...   # searches raw strings
    def scroll_to(self, block_index, section_index, position, viewport_position):
        # fires on_scroll_to_position callback; no-op if None
```

`content_blocks` is a list of `(PreviewContentType, str)` tuples from
`PreviewContent.get_preview_content()`.  Search operates on the raw strings —
not through Qt widgets.

**Wiring (transitional — to be inverted in Step 6):**
- `PreviewContent.__init__` should accept `mindspace: Mindspace` instead of
  grabbing `MindspaceManager()`.
- `ColumnManager._add_tab_to_column`: when a `PreviewTab` is added, call
  `PreviewContent.get_preview_content()`, create a `PreviewContext`, and register
  it via `register_model`.  Wire the `on_scroll_to_position` callback to the
  tab's scroll method.
- `PreviewAITool`: rewrite to use `PreviewContext` from registry, remove
  `column_manager` dependency.  Update `MainWindow` wiring accordingly.

---

### Step 5d: EditorContext

**The hard one.** The editor's data is fused with `QPlainTextEdit` — there is no
separate Python buffer underneath.  `get_text_range`, `get_cursor_info`,
`find_all_occurrences` etc. all use `self.document()` and `QTextCursor`.

**The approach:** Use `QTextDocument` as a shared model object.

`QTextDocument` can exist independently of any view widget.  `EditorWidget`
(which is a `QPlainTextEdit`) can be given an external `QTextDocument` via
`setDocument()`.  The `EditorContext` holds the `QTextDocument` and implements
the data operations against it.

**Consequence:** `EditorContext` still lives in `src/humbug/` (it depends on
Qt), but it provides the clean interface `EditorAITool` depends on — the tool no
longer needs `ColumnManager` or `EditorTab`.

**What to build:**

```python
# src/humbug/tabs/editor/editor_context.py
class EditorContext:
    def __init__(
        self,
        context_id: str,
        document: QTextDocument,
        path: str,
        on_goto_line=None,      # optional Qt callback
    ): ...
    def context_id(self) -> str: ...
    def get_text_range(self, start_line, end_line) -> str: ...
    def get_cursor_info(self) -> dict: ...
    def get_editor_info(self) -> dict: ...
    def find_all_occurrences(self, text, ...) -> list: ...
    def get_selected_text(self) -> str: ...
    def get_diff(self, context_lines) -> str: ...
    def apply_diff(self, diff_text) -> dict: ...
    def save(self) -> bool: ...
    def goto_line(self, line, col):
        # fires on_goto_line callback; no-op if None
```

`goto_line` fires `on_goto_line` — the Qt `EditorWidget` supplies this to scroll
its viewport; a CLI would pass `None`.

**Wiring (transitional — to be inverted in Step 6):**
- `EditorWidget` creates its `QTextDocument` first, passes it to `EditorContext`,
  then calls `self.setDocument(document)`.
- `ColumnManager._add_tab_to_column`: when an `EditorTab` is added, create an
  `EditorContext` and register it.
- `EditorAITool`: rewrite to use `EditorContext` from registry, remove
  `column_manager` dependency.

**Note:** Because `EditorContext` depends on Qt (`QTextDocument`, `QTextCursor`),
it lives in `src/humbug/tabs/editor/editor_context.py`, not in
`src/mindspace/context/`.

---

### Step 6: Invert the Causal Direction

This is the architecturally significant step.  Currently, the Qt `ColumnManager`
drives the `ContextRegistry` — it opens and closes contexts as a side effect of
creating and destroying tabs.  The correct direction is the reverse: AI tools and
the model layer open contexts, and the Qt `ColumnManager` reacts.

**The target flow for every context-opening operation:**

```
AI tool / delegate_ai_tool
  → mindspace.contexts().open(context_type, path=..., metadata=...)
  → ContextRegistry emits OPENED(ContextInfo)
  → ColumnManager._on_context_opened(info) creates the appropriate Qt tab
  → Qt tab creates its context model and calls register_model()
```

**What this requires:**

**A. `ContextRegistry.open()` returns the `context_id` synchronously** (it
already does).  The AI tool uses this as the handle.

**B. `ColumnManager` subscribes to registry events** when a mindspace is opened,
and unsubscribes when it closes.  It needs an `_on_context_opened(info:
ContextInfo)` handler that maps `ContextType` to the appropriate tab creation
call.  The `protect_tab` / `unprotect_tab` pattern (currently used in
`SystemAITool`) moves here.

**C. `SystemAITool`** stops calling `ColumnManager` methods for opening
contexts.  Instead:
- `open_editor_tab` → `mindspace.contexts().open(EDITOR, path=...)`
- `new_terminal_tab` → `mindspace.contexts().open(TERMINAL)`
- `open_conversation_tab` → `mindspace.contexts().open(CONVERSATION, path=...)`
- `new_conversation_tab` → `mindspace.contexts().open(CONVERSATION)`
- `open_preview_tab` → `mindspace.contexts().open(PREVIEW, path=...)`
- `open_diff_tab` → `mindspace.contexts().open(DIFF, path=...)`

The tool returns the `context_id` from `registry.open()` as the result.

**D. `delegate_ai_tool.py`** — the `on_conversation_created` callback currently
calls `column_manager.new_conversation(...)` directly.  After the inversion it
should call `mindspace.contexts().open(CONVERSATION, ...)` and let
`ColumnManager` react.  The `tab_ids` tracking dict becomes a `context_ids` dict
keyed by session path.

**E. The transitional wiring in `ColumnManager._add_tab_to_column`** (which
currently calls `registry.open()` as a side effect) must be removed once
`ColumnManager` is the *reactor* rather than the *driver*.  There must be no
double-registration.

**F. `close_tab` in `SystemAITool`** — closing a tab should call
`mindspace.contexts().close(context_id)`, which emits `CLOSED`, which
`ColumnManager` observes and closes the Qt tab.  The current
`column_manager.close_tab_by_id()` call moves to the `ColumnManager`'s
`_on_context_closed` handler.

**G. `list_tabs` / `get_tab_info` / `move_tab`** in `SystemAITool` — these
query and mutate layout state.  `list_tabs` becomes
`mindspace.contexts().list_all()`.  `get_tab_info` becomes
`mindspace.contexts().get(context_id)`.  `move_tab` becomes
`mindspace.contexts().update(context_id, column_index=n)` — `ColumnManager`
observes `UPDATED` with a changed `column_index` and moves the Qt tab.

**Important:** `ContextRegistry.open()` must be able to carry enough metadata
for `ColumnManager` to create the right tab.  The `ContextInfo` already has
`context_type`, `path`, `title`, `is_ephemeral`, and `column_index`.  For
conversations, the `AIConversation` object needs to be passed somehow — this can
be done via a `metadata: dict` field on `ContextInfo`, or by having the AI tool
register the conversation object before emitting `OPENED`.  The exact mechanism
is a design decision for this step.

**Scope note:** Step 6 is the largest single step.  It touches `SystemAITool`,
`delegate_ai_tool.py`, `ColumnManager`, and the session restore path.  It should
be done as a single coherent change with all tools passing at the end.  Do not
attempt to do it partially.

---

## Testing Notes

After completing each context type (Steps 5c, 5d), test the corresponding AI
tool manually inside Humbug before proceeding:

- **PreviewContext**: open a preview tab, use the AI `preview` tool to call
  `get_info`, `search`, and `scroll_to`.
- **EditorContext**: open an editor tab, use the AI `editor` tool to call
  `read_lines`, `search`, `goto_line`, `apply_diff`, and `save_file`.

After Step 6 (causal inversion):

- Open a conversation, editor, terminal, and preview tab via the AI `system`
  tool and verify all four open correctly.
- Close each via the AI `system` tool and verify the registry reports them gone.
- Test `delegate_ai_tool` delegation — verify the child conversation tab opens
  and closes correctly.
- Test session restore — open a mindspace with saved tabs and verify all contexts
  are registered correctly on restore.

---

## Invariants to Preserve

1. **All four tools must pass after every change** — mypy, pylint, dependency
   checker, pytest.
2. **`src/mindspace/` must never import Qt** — the dependency checker enforces
   this.  `EditorContext` is the one exception: it lives in `src/humbug/`.
3. **`ContextRegistry.close(context_id)`** removes both the `ContextInfo` and
   the registered model in one call.  Do not add type-specific removal methods
   to `Mindspace`.
4. **Visualisation callbacks** follow the `ConversationContext` pattern: the
   context accepts an optional callable at construction; the Qt layer supplies
   it; a CLI passes `None`.  The context never imports Qt.
5. **`context_id` equals `tab_id`** throughout the transition.  The rename is
   deferred until all context models are complete and the causal inversion is
   done.
6. **The model drives, the UI reacts.** After Step 6, no AI tool or model-layer
   code calls `ColumnManager` methods to open or close tabs.  `ColumnManager`
   is a pure subscriber to `ContextRegistry` events.
