# Changelog for Humbug

## v0.3 (2025-01-xx)

This release adds the following new features:

- Drag and drop of files and conversations into the tab column view.
- Added new icons for the file view and made them scalable if you zoom in and out.
- Added the ability to delete files in the file view by right clicking and selecting "Delete File".

Te release fixes the following bugs:

- Detect the end of file correctly so block tab operations don't crash.
- Don't indent empty lines when we do a block indent.

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
