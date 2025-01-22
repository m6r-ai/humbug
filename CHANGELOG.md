# Changelog for Humbug

## v0.3.1 (2025-01-22)

This is a bug fix release:

- The "Embed:" keyword was not using the correct mindspace to find files.
- The Kotlin syntax highlighter was not handling types correctly.

## v0.3 (2025-01-20)

This release adds the following new features:

- Drag and drop of files and conversations into the tab column view.
- Added new icons for the file view and made them scalable if you zoom in and out.
- Added the ability to delete files in the file view by right clicking and selecting "Delete File".
- Added a Kotlin lexer/parser for syntax highlighting.
- Remember the last directory used for file operations and for opening conversations.
- Renamed "workspace" to "mindspace".
- Conversation text editing now supports soft tabs and tab/backtab.
- Added a JSON lexer/parser for syntax highlighting.

The release fixes the following bugs:

- End of file was not correctly detected so block tab operations could crash.
- Don't indent empty lines when we do a block indent.
- Hard tabs were being stripped from prompts to the AI backends.
- Drag and drop labels could end up illegible in light mode.
- Closing and opening a new mindspace could result in the wrong directory tree being shown in the mindspace tree view.

## v0.2 (2025-01-14)

This release adds the following new features:

- The tabbed interface now supports up to 6 columns of tabs.  Tabs can be moved to adjacent columns via
  drag and drop operations, or can be used to create new columns.  Columns can be merged together.

The release fixes the following bugs:

- Default AI backends are now chosen based on the AI providers configured.
- OpenAI backend defaults to o1-mini instead of gpt-4o-mini.
- Backup files were not always handled correctly.
- Files and directories are now sorted in ascending order.

## v0.1 (2025-01-09)

This is the initial release
