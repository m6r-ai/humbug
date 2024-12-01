# Backlog

## Cut/copy/paste support

We need cut/copy/paste support for the system clipboard.

## Resolve problem with histories

The current histories rely on the "AI:" or "User:" prefix on the text displayed to be provide history back to the LLM.  This
is completely broken!  We should have the concept of a history including metadata.  The metadata must be able to be displayed
to the user on-demand.  Timestamp, model, temperature, etc.

## Input buffer limitations

The current input buffer only supports up to 10 lines of text.  It's supposed to handle arbitrary amounds of text and support
scrolling if the display region is too small.

## Signal handlers

Review the current signal handlers.  Almost certainly don't want them apart from SIGWINCH and SIGTERM?

## Support Claude

Need to support interactions with Claude as well as ChatGPT.

## Add unit tests

Add unit tests to improve maintainability.

## Eliminate use of blanket "Exception" handlers

Exception handlers should be more specific.

## Ensure logging captures all exception events

Ensure we do not silently swallow exceptions.

## Improve log file processing

Remove old log files at the start of each new run.

## Improve transcript file processing

Put transcript files in a directory.

## Improve network message validation

Ensure that all messages returned over network connections are fully checked and verified to be ok before we process them.

## Support Gemini

Need to support interactions with Gemini as well as ChatGPT.

## Commands

Need to support multiple commands to manipulate projects and files.

## Ensure Windows 11 works

The current code has not been tested with Windows 11.  This needs to be verified and any bugs fixed.

## Terminal under-size reporting

If the terminal is shrunk too small then change the display to say the app must have a larger display to operate.

## Syntax highlighting

Add syntax highlighting code to be used in editing code and it rendering outputs from an LLM.

Include highlighters for:

- C
- C++
- CSS
- HTML
- Java
- JavaScript
- Kotlin
- Metaphor
- Python
- TypeScript

## Improve navigation for the input window history

The current design is a little clunky as it only triggers up/down behaviour if we're at the start/end of the input buffer.

## Improve transcript records

The transcript log file currently include streaming responses from an LLM but should only include the final versions of any
messages received.

## Highlighting of cancelled history

Use a background colour to indicate that a message was cancelled.  Include the cancelled prompt in this.

## Model support

Need to support multiple AI models and temperatures.

## Estimate the size of prompts before submission

Provide users with a view of the size of any prompts before they're submitted so they have an idea how many tokens will be
used.  Perhaps estimate the cost of the prompt?

## The multi-line input buffer auto-collapse

The multi-line input buffer is supposed to collapse if it's not fully in use.

## Horizontal scrolling

We need the ability to do horizontal scrolling of very wide lines.

## Status information

We need information about the size of the history and the current cursor positions.

## Metaphor support

The tool needs to be able interact with Metaphor files in a more user-friendly way.

## Support deleting elements from the history

Prompt history is expensive, so allow blind alleys to be deleted from the prompt history.

## Highlight a whole prompt/reply pair

Make it easy to see which parts of the history are a request/response pair.

## Support M6R backend services

Support M6R backend server-based services that can perform custom or optimized operations.

## Support folding history entries

All user and AI message folding to make it easier to scroll the history.

## Menus

Look at where drop down menus need to be added.
