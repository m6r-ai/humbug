# Change log for Humbug

## v0.30 (2025-11-xx)

New features:

- Added support for Google Gemini 3 Pro, and removed support for Gemini 1.5 models.

Bug fixes:

- Remove "patch" tests as patch has now been removed from the project.
- The new code to modify text in the editor could sometimes get confused about deletion behaviour.  Split this into
  distinct "delete", "insert", and "append" operations to avoid this.
- Fixed a problem when editing the last line of a file.
- Some of the system AI tool descriptions were not clear about the need to use tab IDs.
- Fixed a problem where the user queues a message during an AI streaming response but where no tool call occurs.
- Fixed a problem with cancelling messages after an API error.

## v0.29 (2025-11-16)

New features:

- The conversation input now has a settings button to make it easy to change the conversation settings (without having to
  look at the menu).
- The mindspace header now has a settings button to make it easy to change the mindspace settings (without having to look
  at the menu).
- The mindspace settings now have a configuration section relating to terminal scroll-back.  This can now be capped to
  improve performance and reduce memory usage.
- Added GPT 5.1 support, and removed GPT O1 support.
- Added more support to allow an AI to interact with editor tabs.  This includes reading content, finding things, moving the
  view, selecting text regions, understanding the editor metadata, modifying text, and saving a modified file.

Bug fixes:

- Many visual elements are now better spaced and scale better with the zoom factor.
- Resolve an intermittent problem that can be caused when changing the styling of a markdown document that has horizontal
  rules in it.
- Buttons in headers now have "hover" and "pressed" effects.

## v0.28 (2025-11-09)

New features:

- Improved some visual styling elements to improve readability.

Bug fixes:

- Resolved a problem with restyling after a tool approval widget has been removed.
- Resolved a problem with the mindspace view expand/shrink logic.

## v0.27 (2025-11-08)

New features:

- Added Claude Haiku 4.5 support.
- AIFPL now has dramatically improved error responses to allow LLMs to do better debugging of problems.
- When an AI is streaming responses, and especially useful where it is making a series of tool calls, you can now type
  a message and queue it for when the next tool call completes.  This message will be passed to the LLM and allows you
  to provide feedback about what it is currently doing.  This includes, for example, saying "stop" and asking it to stop
  what it is doing.  You could also use this to indicate an approach it is trying will not work.
- The wiki feature that was previously in the software has been renamed as a "preview" capability, and the design has been
  simplified.

Bug fixes:

- Resolved problems with some AIFPL tail recursions not working correctly.
- Resolved an exception triggered when a highlighted code section in a message was deleted (by deleting that message).
- Resolved a bug in the deferred update processing of streamed messages.
- Added clarification that AIFPL cons operations are not the same as classic Lisp/Scheme.

Internal structure changes:

- Moved AIFPL test coverage to 100% of statements and conditionals.
- Started to add AIFPL tools to the codebase.  These are not yet integrated but start to demonstrate how AIFPL can be
  used for complex algorithmic processing.  The first tool supports patching files with unified diffs.

## v0.26 (2025-10-07)

New features:

- Humbug now features a simple new functional programming language designed for LLMs to use (AIFPL, short for "AI
  functional programming language").  This is scheme-like, but doesn't attempt to implement all of scheme.  It's deliberately
  restricted to a pure functional language and has simplifications around things like string processing so there is no
  character atom type.  The functionality is a significant superset of the previous calculator AI tool which has now been
  removed as a consequence.
- As we have a new language, added an AIFPL syntax highlighter (this makes the docs render correctly).
- Removed Claude Sonnet 3.5 support as that is now deprecated by Anthropic.
- Removed the calculator tool.  The LLMs now have AIFPL so there's no need to carry two tools that can do calculations.
- Added Claude Sonnet 4.5 support.
- Added GLM 4.6 support.

Bug fixes:

- The system AI tool was not escaping control characters in a way the terminal could handle so things like backspace,
  escape, or Ctrl+C didn't work correctly.
- The `terminal_read` operation did not account for lines at the bottom of the terminal potentially never having had
  content.
- Correctly clear down empty line tracking when the terminal display is cleared.
- The Scheme lexer had a subtle bug that could cause the system to block.
- Resolve a problem when terminal tabs were moved between columns.

## v0.25 (2025-09-10)

New features:

- Updated the system AI tool to allow the AI to send commands to a terminal window (subject to user approval) and to be
  able to query the status of terminal.  This allows the AI to have full control of the terminal!
- When the AI backend gets a 200 OK connection response from the AI a new temporary "AI connected" message appears so the
  user can tell the AI is responding but not showing any messages (e.g. setting up a tool call).  This also means
  conversation transcripts now enable between network analytics.
- Removed the "bookmark" functionality in the conversation tabs.  While this was nice idea early in development it appears
  to be something almost no-one uses now because the AI can write files via the filesystem tool.  Removing this significantly
  simplifies the code (over 300 lines removed).
- Added `grok-code-fast-1` support using the xAI backend.

Bug fixes:

- Delegating a task could trigger a previously-silent exception.
- If we edit text that matches an active search in an editor the editor will re-trigger the search so the search highlights
  and match counts are correct.
- If the user hits cancel on a conversation that's waiting on a tool approval this would leave the tool approval widget
  on the screen.  Now this is cleaned up.
- File watching would not reliably stop when tabs were closed.  This is now handled precisely, not lazily.
- Duplicating a file in the mindspace conversation view would not set the correct file extension.

Internal structure changes:

- Added a new `tools` directory and a `dependency_checker` tool.  This checks that internal modules are not using
  dependencies that they shouldn't be using.

## v0.24 (2025-08-31)

New features:

- Added bit operations and base conversions to the calculator tool.
- Added new sleep and alarm operations to the clock tool.  These let the LLM sleep for a duration or until a specific
  date/time.
- Wiki, conversation, and editor tabs now watch the primary file on which they're based.  If the file is deleted then they
  will now strike out the tab label and change it to red text, until the file is restored.
- When an unmodified editor tab's file is changed externally to the system then update the tab view to reflect the new
  content.
- When moving a tab that has an open find bar, preserve the find bar and the search text.
- Added an exception "canary".  If an unhandled exception occurs the background of the status bar will now turn red to
  alert you.
- Conversations in the conversations mindspace view now hide their `.conv` or `.json` file extensions.
- "New mindspace" and "open mindspace" operations now default to the user's home directory.

Bug fixes:

- If you try to open a directory that is not a valid mindspace then the previous one will no longer be closed.
- The calculator tool now has a warning that ^ is for xor, and not for exponentiation.
- Further tightened the requirement for a delegating parent LLM to tell the child LLM that it has been delegated its
  task by a parent LLM.
- If you deleted a conversation in the mindspace view this could result in an error if closing the conversation tab would
  automatically delete the file.
- Resolved a problem with label sizing on file renames.
- Resolved a visual issue with the active tab highlighting.
- Attempting to open a folder as a conversation used to trigger a silent exception.

## v0.23 (2025-08-22)

New features:

- Added a wiki view to the mindspace view.
- Added the "." directory into all the mindspace views as a target for actions.
- Added a user settings option to change the sort orders for folders/files.
- Updated the DeepSeek context window sizes to match the DeepSeek 3.1 capabilities.

Bug fixes:

- When focusing a delegated AI conversation, don't assume we can focus the input box as there isn't one.

Internal structure changes:

- Separated out the different types of mindspace views into separate directories to simplify maintenance.

## v0.22 (2025-08-20)

New features:

- Added support for GPT-5.  Removed support for the deprecated GPT-4.5.
- Added support for gpt-oss:20b and gpt-oss:120b via the Ollama driver.
- Added support for Z.ai's GLM 4.5 models via a new Zai driver.
- Removed the unused M6R backend.
- Split out the conversations into a new view and simplified the files view so it no longer deals with complexities
  associated with conversations.  This enabled a UI simplification that gives more screen real estate.
- When we delete the end of a conversation from a specific user message, reinsert that prompt into the input box.
- Revised click and double-click behaviour for all mindspace tree actions.  These just create default ephemeral and
  permanent tabs.
- Improved the delegate_ai tool description to clarify what child AIs can do, and to ensure child AIs are made aware that
  they are child AIs.

Bug fixes:

- The filesystem tool was setting `0o600` permissions instead of `0o666 & ~umask`.
- On attempting to close the application, tab close checks would be run twice.  Now they're only run once.
- Handle many more types of 500 series HTTP error with retries.
- In looking at the new conversations view, it became clear that sorting by creation time was very slow.  Added a caching
  solution and this has reduced application startup times by > 200 ms when lots of conversations are present.
- Fixed a problem with tab bar zooming.
- Fixed a problem when scrolling an file/folder edit in the mindspace tree views.
- When delegating tasks to another AI the requester user name was mis-set.

Internal structure changes:

- Split out the AI conversation transcript handling and moved it to a top-level package within the Humbug repo.  This is
  useful for stand-alone applications.

## v0.21 (2025-08-06)

New features:

- The current active message that's being processed in a conversation is now the one that is highlighted when the
  conversation is active.  This now uses a rotating colour border to make it more visually obvious.  This means that
  it's also obvious if a delegated AI is actively communicating too.
- If the user tries to close a conversation that is currently streaming then give them a warning dialog and ask them
  to confirm they want to cancel it.
- Added support for Claude Opus 4.1.

Bug fixes:

- Navigating between messsages in a conversation could be very slow, and the recent changes involving tool use could
  have the UI attempt to navigate messages that were not visible.
- Loading large and complex conversations could be very slow.  Optimized this by adding lazy updates to the UI.
- The tab bar layout was difficult to see properly at times, the scroll buttons didn't have hover effects, and the
  type icon did not activate the tab.  All these are now resolved.
- In cleaning up the widget hierarchy, various small problems with layouts were discovered and fixed.

Internal structure changes:

- Various tab types (conversation, log, shell, wiki) use a message structure inside a container widget.  Unified the
  Qt style sheet (QSS) handling for each message type to improve performance and unify styling in a single style sheet.
- Dramatically simplified all tab types so their style sheets are fully defined in their top-level widget classes.
- The structure of tab objects is now much more consistent across all 6 different types of tabs.  This makes it easier
  to work out where the business logic for each type of tab should be found.

## v0.20 (2025-07-26)

New features:

- Added support for an LLM conversation to spawn a child conversation to undertake tasks for it.
- AI tool processing was heavily updated to support true parallel tool calling.
- Support production gemini 2.5 pro and gemini 2.5 flash models.
- When navigating the mindspace tree, the delete key will now trigger file/folder deletion.

Bug fixes:

- Single clicks on the mindspace tree should not cause the tree to lose focus.
- Added missing type hints to the Windows terminal code.
- Resolved some linter and pytest issues on Windows and MacOS.
- Added missing exception handling in the Wiki link handling.

Internal structure changes:

- `markdown` was renamed to `dmarkdown` to avoid namespace issues with `pyinstaller`.
- The conversation tab design was reworked so all transcript operations happen within the `ConversationWidget` class.
- Removed the AI rate limiter.  It never had a full view of all API uses and did not have any way to adapt to different
  AIs having different rates.
- Refined a number of callbacks to use more consistent naming.
- Revised the transcript handling to be synchronous rather than asynchronous as the underlying filesystem calls were
  synchronous anyway.
- Reworked naming of all signals to be snake_case, and of all signal handlers to use `_on` as a naming prefix rather than
  `_handle`.
- Internal package files now all now have useful `__init__.py` files.

## v0.19 (2025-07-15)

New features:

- Added a new system "log" tab type.  Removed logging of Metaphor conversation errors from the Humbug shell.  Started adding
  system audit logging for many types of system events.
- Added support for Grok4 and updated all Grok3 instances to production versions, rather than beta versions.
- Added a running total of both input and output tokens for conversation status.
- Improved the "system" AI tools so the AI can now get details about a specific tab, get information about all open tabs, and
  move tabs between columns.
- Added a section about AI tools to the mindspace settings dialog, where users can enable and disable individual AI tools.
- The Anthropic backend now uses prompt caching to dramatically reduce token costs (the other backends already did this
  automatically).

Bug fixes:

- Improved the system tool description so the LLM has a clearer idea of what the tool can do.
- The terminal tab did not gain the keyboard focus correctly when the tab was opened.
- When opening the Humbug shell or conversation window, the previously-active message was not correctly restored if the
  user clicked the tab, but didn't click the message.
- A couple of OpenAI LLM models had the wrong name.
- Increased the timeout on HTTP requests to LLMs to 5 minutes to allow for them planning to use tools.
- The Humbug shell message labels did not scale correctly when the display was zoomed.

Internal structure changes:

- Massively reworked the source code structure.  Instead of all components sitting under src/humbug, many generic components
  have been moved to the src directory.  At the same time, any dependendencies that might make reuse of those components
  has been reworked so they can stand alone.  README.md files have been added to each directory to summarise their use.
- The structure of shell commands has been simplified so they no longer call-back into the MainWindow class.

## v0.18 (2025-07-08)

New features:

- Added a "system" tool that gives the AI the ability to start terminal, wiki, editor, Humbug shell, and conversation tabs.
- Added support for collapsing and expanding individual messages in a conversation.  By defaul tool use messages are
  auto-hidden, but an authorization request message is now provided.
- Added support for Qwen3:8b model.

Bug fixes:

- Resolve problem with responses back to parallel tool calls.
- Resolve problem with trailing whitespace after Metaphor embedded filenames.
- When writing files to the filesystem, use a "destructive" button colour for overwrites, and "recommended" for new files.
- When copying files to the filesystem, use a "destructive" button colour for overwrites, and "recommended" for new files.
- The filesystem tool did not fully validate all arguments.  While this wouldn't have failed, it wouldn't have given
  useful exception responses.
- The filesystem tool sometime reported successes for no-op operations that are really failures.  The tool now reports the
  failures so the LLM is aware it didn't cause the operation to succeed.
- The filesystem tool did not fully validate all arguments passed to it.  It now carefully checks all args are present and
  in a valid form.
- MessageBox messages will now correctly size to fit the window.

Internal structure changes:

- The internal mindspace logging and shell command interaction logic has been moved to the mindspace and the shell command
  tab namespaces to reflect how they're actually used.
- Shell command logic has moved the the shell command area of the GUI code.
- Generic AI tools have moved to the ai/tools directory.

## v0.17 (2025-07-02)

New features:

- All models that support tool/function calling now implement tool/function calling.  Safe tools do not require user
  authorization but any tools that might do damage require an explicit authorization from the user.
- Added clock, calculator, and filesystem tools.  These allow the LLM to read the data/time, perform calcuations,
  and interact with the current mindspace's filesystem.
- Added support for Gemma3 and Qwen3 models through the Ollama AI backend.  Qwen3 supports both thinking and non-thinking
  modes.
- Forking conversations now uses the original name and a suffix of `- fork`, possibly followed by a number, indicating
  the conversation that was forked.
- When a conversation is actively communicating with an LLM the input message box border pulses.

Bug fixes:

- When invoking new conversations or `m6rc` from the system shell, Humbug was not correctly setting reasoning capabilities
  for thinking/reasoning AI models.
- When recording conversation transcripts, Humbug was not recording the reasoning capabilities that were used in coming up
  with an answer.  This meant it was also not restoring these correctly after reloading a conversation.
- The conversation input box could lose focus when an AI message finished streaming.
- Markdown headings may now be preceeded by 0, 1, 2, or 3 spaces.
- File renames did not reliably update wiki tabs that were based on the old filename.
- Under some circumstances, the column manager could crash when removing an ephemeral tab.
- `m6rc` now uses mindspace-relative file paths to ensure consistency with the filesystem tool.
- The System Shell is now known as the Humbug Shell.
- The terminal now starts in the mindspace folder.
- When streaming conversations that reference local files we could end up blocked.

## v0.16 (2025-06-16)

New features:

- When you click any non-conversation file in the mindspace tree it now opens it in the dynamic wiki view (just as drag
  and drop does).  Conversations open in a conversation tab.  Tabs opened by a single click are ephemeral and will have
  a different label colour and an italics tab label.  Ephemeral tabs will be replaced by the next new tab that is
  opened, unless the user interacts with the tab (e.g. adding to a conversation).  Such interactions make the tab
  become permanent.
- If you double-click a file it opens in an editor.
- All "hidden" files except for ".humbug" are now shown in the mindspace tree.
- There are new icons on message boxes.
- The conversation input box now has a submit icon to submit new messages, and a stop icon to stop any current
  message that is being sent by the AI
- If the content of a non-active wiki tab changes, the tab label will now turn purple to notify you that things have
  been updated while you weren't looking.
- If a conversation turn in any non-active conversation tab completes, the tab label will now turn purple to notify you
  that things have been updated while you weren't looking.
- Zooming now has finer-grained steps.
- JSON syntax highlighting now differentiates keys from values.
- The conversations folder now defaults to sorting by the conversation start date.  You can select sorting alphabetically
  by right clicking the changing the "Sort By".
- Wiki views will render any "README.md" file for folder pages that contain one, and "file.ext.md" for any file "file.ext"

Bug fixes:

- When renaming or duplicating files, the file extension is no longer automatically selected, so the user does not
  accidently remove the file extension when typing a new file name.
- When creating a new file or folder, ensure the mindspace tree is scrolled to the new file/folder before
  attempting to edit the label.
- Changing a tab label could cause a temporary glitch in the display.

Internal structure changes:

- Start to add syntax parser tests.

## v0.15 (2025-06-10)

New features:

- You can now create a new file by right clicking in the mindspace tree.
- You can now create a new folder by right clicking in the mindspace tree.
- You can now delete an empty folder by right clicking in the mindspace tree.
- You can now duplicate a file by right clicking in the mindspace tree.
- You can now edit the `.conv` conversation files by right clicking in the mindspace tree.
- You can now move and non-protected (e.g. not "conversations", "metaphor", and ".humbug") files or folders by
  dragging and dropping within the mindspace tree.

Bug fixes:

- The `m6rc` command would search the current working directory before the mindspace when including files.  It should not
  take any notice of the CWD.
- Fixed a problem in the JSON lexer that could result in a crash.

## v0.14 (2025-06-04)

New features:

- When you select a tab, or open a new one, that has a representation in the mindspace tree view then the matching
  entry in the tree will be expanded, selected, and scrolled into view.
- Markdown parsing for code blocks is ambiguous because code fences inside code block could be misinterpreted as
  ending the code block.  Humbug now uses the syntax parsers to handle code block parsing as well as highlighting
  to prevent blocks being closed too soon.
- Removed the Gemini 2.0 Pro model, but added Gemini 2.5 Flash preview.  Updated models to the latest versions.
- The markdown syntax highlighter has been extended to cover tables and inline attributes.
- The wiki pages now provide metadata about file and directory size, modification timestamps, and file permissions.
- Tabs are now tracked on an MRU basis so when the current tab is closed, the most recently active tab becomes active.

Bug fixes:

- If an image in a markdown file did not have a paragraph line separator from the preceding line then the formatting
  was corrupted.
- Under some circumstances markdown lists and paragraphs interacted in odd ways and introduced rendering defects.
- If you closed a tab then the tab label that ended up under the mouse pointer would not set the hover rendering
  correctly.
- When processing Metaphor files that have code fences inside embedded code blocks we could exit the code parsing
  too early, corrupting the output.

Internal structure changes:

- Added comprehensive markdown AST tests.
- Updated the Metaphor AST tests to introduce the smae sort of example-based tests as markdown.

## v0.13 (2025-05-28)

New features:

- Added a new type of "wiki" tab and a matching "wiki" system shell command.  The wiki pages are dynamically generated
  view of all the information relevent to a file or directory, including a preview of any markdown file content, and
  any source code content.  This is now the default for opening files from the mindspace tree view.  Editing source code
  can now be done via the edit button, or by right clicking on the file in the mindspace tree.
- Any dialog box that has a destructive default action now has a red default button instead of blue.
- Added new Anthropic models: Claude Sonnet 4, Claude Opus 4.
- Added new Mistral small model: Devstral Small.
- Tab labels now have tool tips.
- Tab label types are now identified by icons instead of text prefixes.

Bug fixes:

- Markdown rendering of lists that included paragraphs did not work correctly.
- Attempting to quit Humbug with unsaved changes in a file would result in you being asked twice if you wanted
  to discard.
- Path names with `~` prefixes are now handled correctly in the system shell.
- Path names with spaces in them are now handled correctly in the system shell.

Internal structure changes:

- The QTextEdit markdown renderer was moved to allow it to be reused more easily.

## v0.12.2 (2025-05-13)

Bug fixes:

- When starting the terminal from the MacOS application launcher we didn't default to the user's home directory
  or have a LANG environment variable.

## v0.12.1 (2025-05-13)

Bug fixes:

- The MacOS shell could intermittently fail to close correctly when the application was closing, leaving it
  hanging.
- Bold text in a heading could end up the wrong colour.
- Some spacings around horizontal rules could end up slightly too large.
- Headings did not use the correct font sizes.

## v0.12 (2025-05-13)

New features:

- Markdown tables are now rendered as tables, and horizontal rules as horizontal rules, when an AI is streaming
  its responses to the user.
- AI backend settings now have explicit enable/disable checkboxes and also have an optional URL as well as
  an API key, so you can point to custom URLs rather than just the standard ones offered by the AI companies.
- The layout of user, mindspace, and conversation settings dialogs has been updated to make them more compact
  and more consistent with each other.
- Added initial syntax highlighting support for Solidity.

Bug fixes:

- If you deleted a message from a conversation and then forked it, you could end up with a corrupted message
  history in the forked conversation (not in the original).
- Resolved problems with scaling of some dialog boxes when using a zoomed-in display.
- Updated all dialog boxes to have buttons appear in a consistent order, colour the recommended action in
  blue, and use consistent layout/spacing.
- 503 errors are now retried and have a better status message.
- If you changed the API key for an AI backend this would not update an open conversation tab until it was
  closed and re-opened.

Internal structure changes:

- Settings dialogs now use a series of new settings components that make it easier to handle larger amounts
  of configuration data, and to ensure settings are all handled in the same consistent way.

## v0.11 (2025-05-02)

New features:

- The system shell syntax highlighting now recognizes the difference between option values and positional
  arguments, using different colours for each.
- When forking conversations, the new conversation now appears in an adjacent column so the old and new
  conversations appear side-by-side.
- Added a button to the AI response messages allowing a conversation to be forked immediately after that
  message.
- Added a button to the user messages allowing all messages from that message onwards to be deleted
  from the conversation history.
- The preferred action button in all dialog boxes is now blue, matching conventions elsewhere.
- The `m6rc` system shell command now takes a `-j` or `--submit` flag.  Without either of these,
  Metaphor prompts are compiled, but not submitted, matching the behaviour of "New Metaphor Conversation".
  Pass `-j` or `--submit` to have the compiled prompt be automatically submitted.

Bug fixes:

- The system shell now keeps messages adjacent to the command prompt (like conversations do).
- When changing between light and dark mode, the system shell now changes text colours correctly.
- When moving terminal tabs the terminal would become unresponsive due to a mistake introduced in v0.10.

## v0.10 (2025-04-23)

New features:

- There is a new type of tab, called the "system shell".  This is a command line interface that lets you
  interact with Humbug features.  Initial commands let you open conversations, files, and terminals.  You
  can also run the Metaphor compiler and run code reviews.
- The Metaphor compiler now supports command line parameters for use in "Include" and "Embed" blocks.  This
  means it's now possible to create template Metaphor files and fill in the blanks at runtime.  One such
  example is the new "review.m6r" file.
- If you start a Metaphor conversation but there is a problem with the Metaphor file/files you now see the
  details in the system tab rather than in a dialog box.  This makes them much easier to read.
- A number of error messages from the Metaphor compiler have been improved to make them easier to understand.
- Supports xAI's Grok-3 beta models.
- Supports OpenAI o3, o4-mini, gpt-4.1, gpt-4.1-mini, and gpt-4.1-nano models.
- On saving and restoring mindspaces, the active tab in each column is now restored.  Also the column
  that was active when saved is also restored as the active column.  This means you no longer have to
  reset the focus of tabs.
- AI assistant response messages now show which AI model generated the message.
- The "system" responses are now tagged as being from Humbug.
- If you close a conversation and there are no messages from the AI in it then the conversation
  transcript is now automatically deleted.  This avoid cluttering the conversations directory with
  empty files.

Internal structure changes:

- The codebase is now fully type-checkable using mypy (0 errors or warnings).
- Implemented a set of pylint checker rules (0 errors or warnings).
- Removed the use of @property.  The use of @property decorators made it hard to determine if code was
  making use of attributes or methods.  These are now much clearer, making it easier to ensure we don't
  inadvertently access attributes and risking side effects.
- Renamed some files and classes to better reflect how they fit into the overall design.

Bug fixes:

- Several small bugs were resolved when implementing mypy and pylint checking.  In a few instances the
  type hints had been incorrect, although type hints have no impact on the execution of python code.
- If we had a markdown file embedded inside an AI response and that markdown file contained code inside
  a code fenced block then this would exit the code fenced region too soon.
- The TERM environment variable is now set to xterm-256color on MacOS and Linux.
- The system status bar messages could sometimes be overwritten by status from the wrong tab.  This is
  now resolved.

## v0.9 (2025-04-01)

New features:

- The current focus message is now highlighted across all conversations.  You can now step between
  messages in the current conversation using "Alt+Up" and "Alt+Down".
- You can now both copy to the clipboard, or save, whole messages in Markdown format, as well as copying or
  saving code blocks within messages.
- Block-level Markdown syntax highlighting is now supported.

Bug fixes:

- The Metaphor compiler's parser is now more permissive about the use of blank lines to help improve
  the readability of inluded Metaphor.
- The bookmark menu items did not correctly change language when the language was changed at runtime.
- Resolve a rendering problem with lists that contain paragraphs.
- Resolve a problem where editor windows did not get focus correctly when clicked.
- Resolved a duplicate "Humbug" menu item on MacOS.

## v0.8 (2025-03-27)

New features:

- Added a user settings dialog to make it easy to set API keys for AI services.  Note this takes
  over the "Ctrl+," shortcut, and "Conversation Settings" now use "Ctrl+Shift+,".  Any API keys
  configured for earlier versions of Humbug will be carried forwards.
- The font size and language settings have moved from mindspace settings to user settings.
- The default theme can now be set in the user settings.
- Updated the Edit menu so the old "Dark Mode" option is now also replaced with "Display Theme" that
  matches the user setting.
- Messages from the AI are now rendered using the markdown formatting provided.  This makes them
  easier to read and hides the markdown annotations.
- Added support for Google Gemini 2.0 pro (experimental) and Gemini 2.5 pro (experimental).
- Folded an implementation of the Metaphor compiler directly into Humbug.  Updated this so compiled
  Metaphor prompts render more cleanly within the UI.

Internal structure changes:

- AI models are now handled by new UserManager and UserSettings classes.  This lets API keys be
  updated at runtime and avoids passing AI backend lists around in APIs.
- All elements related to AI conversations have moved to the ai directory and the network
  interactions are all moved to a new AIConversation class and out of the GUI.
- GUI elements related to the tab displays have been moved into a new gui/tab sub-directory.
- Non-GUI terminal functionality has been moved into a new directory at the top level of the
  source tree so it's easier to use outside of the GUI.
- Optimized the terminal emulator performance for large terminals.
- Load times for very large conversation histories are now about 50% faster.

Bug fixes:

- Resolved a problem where tab close buttons did not render correctly when hovering between tabs.
- Column swapping did not work correctly with right-to-left layouts.
- When moving an actively streaming conversation between columns we used to cancel the
  network activity and lost the AI responses.
- The backspace key was generating the wrong keyboard code which made it buggy on Windows.  It
  should emit \x7f and not \x08.  Ctrl+Backspace now generates \x08 instead of \x7f.

## v0.7 (2025-03-10)

New features:

- Messages containing embedded source files are now broken into sub-message components and
  they are displayed more cleanly.  This makes it much easier to work with the embedded code.
- Added the ability to "copy" or "save-as" any embedded code in a conversation view.
- Added C# syntax highlighting.
- Added support for Mistral AIs: codestral-latest, mistral-large-latest, and mistral-small-latest.
- Remove OpenAI o1-preview but added o1, o3-mini, and gpt-4.5-preview.

Internal structure changes:

- Reworked AI backends to have a cleaner code structure and follow more systematic naming
  conventions.
- Reworked the conversation, editor, and terminal find functionality to place it within the
  matching widget classes.  This avoids creating complex logic to hide the internals of
  these widget classes and sets a future template.
- Optimized core lexing for syntax highlighting, making Python highlighting about 30% faster.
- Optimized message highlighting, making that an extra 20% faster too.
- Added a new ProgrammingLanguageUtils class to centralize all programming language lookups.

Bug fixes:

- Added elipsis to the Python syntax highlighting engine.
- Go, Rust, and Swift were not correctly identified in editor tab status bars.
- Fixed a problem with the Scheme lexer not handling some error condititions correctly.

## v0.6 (2025-02-27)

New features:

- Added support for Claude Sonnet 3.7 (chat and extended thinking).
- Added support for Deepseek Chat and Deepseek Reasoner models.
- For AI models that expose reasoning steps, the conversation tabs now capture the
  reasoning in a separate message box, after the user's message and before the AI's
  full response.
- Added support for C# syntax highlighting.
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
- Resolved a few problems with the Swift syntax highligher.

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
