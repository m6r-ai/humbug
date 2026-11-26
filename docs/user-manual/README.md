# User manual — maintenance notes

This directory is the user-facing manual. `index.md` is the entry point and the reading order;
every other file is a chapter. Contributors: `AGENTS.md` in the repository root states the rule —
a change to what the user sees or can do updates the matching chapter in the same change.

This file is for maintainers and is not part of the reading chain. It maps each chapter to the code
that decides the behaviour it describes, so you can find the chapter a change affects.

## The chain

Chapters are linked in reading order: each ends with `Previous` / `Next` footers, and `index.md`
lists them by number. When inserting a chapter, three edits are needed — the new file, the index,
and the footer links of the two chapters it now sits between. Deleting a chapter needs the same
three, in reverse.

## Chapter map

| Chapter | Documents |
|---|---|
| `what-is-humbug.md` | The idea, not the code: `blueprint.md` for purpose and intent |
| `installation.md` | Release builds, `pyproject.toml`, `src/desktop/__main__.py`, the onboarding tour in `src/desktop/tour/` |
| `first-conversation.md` | `src/desktop/conversation_tab/` (`conversation_input.py`, `conversation_message.py`), tool approval widgets |
| `mindspaces.md` | `src/mindspace/`, `src/desktop/mindspace/`, `src/desktop/sidebar_manager/`, `src/desktop/conversation_sidebar/`, `src/desktop/file_sidebar/` |
| `tabs-and-columns.md` | `src/desktop/tab_manager/`, `src/desktop/tab/`, `src/context/` (ADR-0008) |
| `conversations.md` | `src/ai/`, `src/desktop/conversation_tab/` (`prompt_minimap.py`), `src/conversation_context/`, `src/desktop/language/` (status bar strings) |
| `ai-models-and-backends.md` | `src/ai/ai_model.py`, `src/ai/ai_conversation_settings.py`, `src/ai/<backend>/`, the AI Backends settings page |
| `managing-files.md` | `src/desktop/file_sidebar/`, `src/desktop/file_watcher/` |
| `trash.md` | `src/desktop/trash_sidebar/`, `Mindspace.trash_dir()`, `restore_trashed()`, `purge_trashed()`, `src/mindspace/trash_manifest.py` |
| `journal.md` | `src/desktop/journal_sidebar/`, `src/mindspace/file_journal.py`, `journal_reversal.py`, `mindspace_change_watcher.py`, `mindspace_scan_pruned_paths.py`, `src/path_matching/` (ADR-0018, ADR-0020) |
| `editing-files.md` | `src/desktop/editor_tab/`, `src/editor_context/`, `src/syntax/` |
| `terminal.md` | `src/desktop/terminal_tab/`, `src/terminal/` |
| `preview.md` | `src/desktop/preview_tab/`, `src/markdown_/`, `src/html_/`, `src/pdf/`, `src/docx/` |
| `git-diffs.md` | `src/desktop/diff_tab/`, `src/desktop/vcs_sidebar/`, `src/diff/`, `src/git/` |
| `ai-tools.md` | `src/ai_tool/` (the registry and `main_window.py`'s tool registrations); per-tool approval behaviour in each `src/*_ai_tool/` module |
| `searching.md` | `src/desktop/search_sidebar/`, `src/mindspace/mindspace_search_engine.py`, `mindspace_ignored_dirs.py`, `src/desktop/widgets/find_widget.py` |
| `quick-switcher.md` | `src/desktop/quick_switcher/`, `src/desktop/palette.py` |
| `attachments.md` | `src/desktop/conversation_tab/conversation_input.py`, attachment extraction for PDF/DOCX |
| `forking-conversations.md` | `src/desktop/conversation_sidebar/` (folders, pinning, undo/redo), `src/conversation_dag/`, ADR-0019 |
| `delegation.md` | `src/delegate_ai_tool/`, `src/conversation_dag/`, ADR-0011 |
| `token-usage.md` | `src/desktop/usage_tab/`, `src/mindspace/mindspace_usage.py` |
| `mission-control.md` | `src/desktop/mission_tab/`, `src/mission/`, ADR-0026 (autonomous mode) |
| `user-settings.md` | `src/desktop/user/`, `src/desktop/settings_dialog.py` (`_build_display_page`, `_build_file_access_page`, `_build_network_access_page`, `_build_ai_backends_page`), `src/filesystem_ai_tool/filesystem_access_settings.py`, `src/http_ai_tool/http_access_settings.py` |
| `mindspace-settings.md` | `src/mindspace/mindspace_settings.py`, `src/desktop/settings_dialog.py` (the `This Mindspace` pages) |
| `conversation-settings.md` | `src/desktop/conversation_tab/conversation_settings_dialog.py`, `src/ai/ai_conversation_settings.py` |
| `keyboard-shortcuts.md` | every `setShortcut` call in `src/desktop/main_window.py`, plus per-tab key handling |
| `humbug-shell.md` | `src/desktop/shell_tab/`, the `ShellCommand*` classes registered in `main_window.py` |
| `mindspace-log.md` | `src/desktop/log_tab/`, `src/mindspace/mindspace_interactions.py` (ADR-0017) |

## Things that go stale fastest

- **Menu labels, button labels, and shortcut text.** These live in
  `src/desktop/language/en/en_strings.py`; when a string changes, search the manual for the old
  wording.
- **Settings pages and their contents.** `src/desktop/settings_dialog.py` builds them; a new
  `_build_*_page` method means a new section in `user-settings.md` or `mindspace-settings.md`.
- **The tool table in `ai-tools.md` and `mindspace-settings.md`.** Both list the registered tools;
  `main_window.py` registers them.
- **Defaults.** Values such as the checkpoint interval, scrollback limit, and tab size are quoted in
  the manual; they come from `src/mindspace/mindspace_settings.py` and the `user_settings.py`
  dataclass.
- **Storage locations.** Anything described as living in `.humbug/` is named in
  `src/mindspace/mindspace.py`'s constants at the top of the `Mindspace` class.
