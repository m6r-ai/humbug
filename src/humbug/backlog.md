# Backlog

## Signal handlers

Review the current signal handlers.  Almost certainly don't want them apart from SIGWINCH and SIGTERM?

## Support Claude

Need to support interactions with Claude as well as ChatGPT.

## Add unit tests

Add unit tests to improve maintainability.

## Visuals on the conversation tabs

We should not change the colour of the focused history area.  It should remain the same colour as its tab so this
looks like a seamless window including the tab.

The text is a little too close to the left and right edge of the conversation window.

## Eliminate use of blanket "Exception" handlers

Exception handlers should be more specific.

## Ensure logging captures all exception events

Ensure we do not silently swallow exceptions.

## Improve log file processing

Remove old log files at the start of each new run.

## Improve transcript file processing

Put transcript files in a directory.

Each message needs to identify the conversation to which it belongs!

## Page up/down in input area

These are currently going to the top and bottom of the input area rather than paging up/down correctly.

## Keyboard navigation in the history area

We should be able to use cursor keys, page up and page down in the history area of a conversation.

## Cost warnings

We want to be able to estimate the cost of a chat and the cost of the next prompt!

## Persist conversations

Ideally we should be able to persist conversations from their transcripts.

## Light mode

Not everyone likes dark mode, so we should have a light mode too.

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

## Look at estimating token usage for cancelled responses

If the user cancels a response, try to estimate how many tokens were sent in each direction.