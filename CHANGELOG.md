# Changelog for Humbug

## v0.6 (2025-02-xx)

New features:

- Added support for Claude Sonnet 3.7 (chat and extended thinking).
- Added support for Deepseek Chat and Deepseek Reasoner models.
- For AI models that expose reasoning steps, the conversation tabs now capture the
  reasoning in a separate message box, after the user's message and before the AI's
  full response.
- Small improvements to the status messages for conversations and terminals.

Internal structure changes:

- Reworked the ConversationSettings class to make it easier to add new models.  These are
  now handled via a single new dictionary entry.
- Separated language translations into separate files/directories.
- The ConversationTab class was much more complex than the equivalent EditorTab and
  TerminalTab classes so refactored this to break out a new ConversationWidget class.

Bug fixes:

- Resolved some problems with failure handling in the AI networking code.
- Corrected the missing language translations for terminal status messages.

## v0.5 (2025-02-24)

New features:

- Added a local command line/shell tab type.  This will let you open a Unix shell
  on MacOS and Linux, or a command prompt on Windows.  From these you can use your standard
  command line tools.
- Added Go, Java, Rust, and Swift lexers/parsers for syntax highlighting.
- Columns can now be merged by dragging one on top of another.
- Columns can now be swapped with ones either side.
- Messages can be bookmarked to make it quick to jump backwards and forwards.
- A default AI model and temperature can now be set in the mindspace settings.

Internal structure changes:

- The conversation, editor, and mindspace UI functionality has been moved into separate
  subdirectories to make it easier to find related functionality.
- Lexers and parsers have been moved into separate per-language subdirectories to ease
  navigation of the code.

Bug fixes:

- Resolved a problem with the conversation settings dialog.

## v0.4 (2025-02-03)

This release adds the following new features:

- Added a Scheme lexer/parser for syntax highlighting.
- Added a "Find" function to search for text in files.
- Added support for i18n - initial 3 languages are English, French, and Arabic.
- When highlighting text, the syntax highlights are now preserved.
- Added the ability to rename existing conversations.
- The mindspace name is shown above the mindspace file tree.
- Added a pop-up context menu on conversation tabs that provides access to conversation
  settings and forking for that conversation.
- When creating a new mindspace, the user is asked if they wish to create "conversations",
  "metaphor", and "src" folders.

The release fixes the following bugs:

- The expand/collapse icons in the mindspace file tree did not scale with the zoom level.
- Vertical scrolling in the conversation window only worked for pure vertical scrolling, not
  where horizontal scrolling occurred at the same time.
- Editing a file no longer jumps the cursor into the middle of the input tab when it's first
  modified.  This also eliminates a glitch that made displays momentarily shrink and then
  expand back.
- Embedded code highlighting now avoids problems with 3 backticks in a comment or string
  dropping the syntax highlighting back to the AI markdown highlighter.
- Syntax highlighting of module/package names in Kotlin and Python are now handled so they do
  not appear as object fields/properties.
- Close buttons on the tab view used to have the wrong background and cross colour for the
  active tab in an inactive column.
- If text was highlighted using the keyboard in a conversation view then moving the mouse
  outside of the conversation tab would cause the tab to scroll.

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
- Added a Sui Move lexer/parser for syntax highlighting.
- Remember the last directory used for file operations and for opening conversations.
- Renamed "workspace" to "mindspace".
- Conversation text editing now supports soft tabs and tab/backtab.
- Added a JSON lexer/parser for syntax highlighting.
- Added default support for Ollama (Phi 4 and Llama3.2 for now) so you can work with a local LLM.

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
