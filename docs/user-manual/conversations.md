# Conversations

A conversation is the core unit of work in Humbug. It is a persistent, structured exchange
between you and an AI: your messages, the AI's responses, any reasoning the AI shows, and a
full record of every tool action the AI took along the way. This chapter covers everything about
working with conversations beyond the basics introduced in
[Your First Conversation with an AI](first-conversation.md).

---

## Conversation structure

Every conversation is made up of a sequence of **messages**. Each message has a **banner** — a
header bar that identifies who sent it and provides action buttons — followed by the message
content.

Message types you will see in a conversation:

| Message type | What it contains |
|---|---|
| **Your message** | What you typed, plus any files you attached |
| **Reasoning** | The AI's internal thinking steps, if the model supports visible reasoning |
| **AI response** | The AI's reply, rendered as formatted Markdown |
| **Tool call** | A record of a tool the AI requested to use |
| **Tool result** | The outcome of that tool call |
| **System message** | A status or error message from Humbug itself |

Tool call and tool result messages are **collapsed by default** to keep the conversation readable.
You can expand any message by clicking the expand/collapse button in its banner.

---

## Message actions

Each message type exposes a set of action buttons in its banner.

### Your messages

Every one of your messages has four buttons:

- **Fork** — create a new conversation that branches from this point (see [Forking](#forking)
  below)
- **Edit** — modify the message text in place (see [Editing messages](#editing-messages) below)
- **Delete** — remove this message and everything after it from the conversation
- **Copy** — copy the message content to the clipboard as Markdown

If you attached files to the message, an **attachments** button also appears, letting you expand
or collapse the view of those attached files.

### AI responses and reasoning

AI response and reasoning messages have two buttons:

- **Copy** — copy the content to the clipboard as Markdown
- **Save** — save the content to a file

---

## Editing messages

To edit one of your previous messages, click the **Edit** button in its banner. The message text
becomes an editable area. Make your changes, then confirm with the **Confirm** button, or cancel
with **Cancel**.

When you confirm an edit, everything after that message in the conversation is discarded and the
edited message is re-submitted to the AI. This lets you correct a question or rephrase a prompt
without starting a new conversation.

---

## Forking

Forking creates a new conversation that starts as an exact copy of the current conversation up to
and including a specific message. From that point, the fork is independent — changes in one do not
affect the other.

To fork, click the **Fork** button on any of your messages. A new conversation tab opens, named
after the original with a "— fork" suffix. The new conversation appears in a column alongside the
original so you can compare them.

Forking is useful when you want to explore a different direction without losing the path you are
already on.

---

## Deleting messages

Clicking the **Delete** button on one of your messages removes that message and every message
after it from the conversation. The text of the deleted message is placed back into the input
area, so you can revise and resubmit without retyping.

This is useful when an AI response has gone off track and you want to back up and try again with
a different prompt.

---

## Reasoning

Some AI models can show their reasoning — the step-by-step thinking they do before producing an
answer. When this is available, reasoning appears as a separate message between your prompt and
the AI's response.

Reasoning messages are collapsed by default. Click the expand button to read through the AI's
chain of thought. This can be useful for understanding why the AI reached a particular conclusion,
or for spotting where it went wrong.

You can control whether reasoning is visible, hidden, or disabled entirely in the
[conversation settings](#conversation-settings).

---

## Network errors and retrying

If a network request to the AI fails, Humbug shows a system error message with a **Retry**
button. Clicking it re-sends everything back to the last point where the AI was waiting for
input, without you having to do anything manually.

---

## Conversation settings

Each conversation has its own settings, accessible via the **cog button** in the input area or
from the right-click context menu on the conversation tab.

The settings for a conversation are:

- **Model** — which AI model to use. You can change this at any point, including mid-conversation
- **Temperature** — controls how creative or conservative the AI's responses are (not available
  for all models). Lower values produce more predictable output; higher values produce more varied
  responses
- **Reasoning** — for models that support it, choose between no reasoning, hidden reasoning
  (the model thinks but you do not see it), or visible reasoning (reasoning shown as a separate
  message)
- **Reasoning effort** — for models that support variable reasoning depth, controls how much
  effort the model puts into its reasoning steps

The settings dialog also shows read-only information about the selected model: its context window
size and maximum output token limit.

### Switching models mid-conversation

You can change the model at any point in a conversation. The new model will be used from your
next message onwards. The full conversation history is sent to the new model as context, so it
has the same shared understanding of what has been discussed.

---

## The status bar

While a conversation tab is active, the status bar at the bottom of the window shows:

- The current model name
- The temperature setting
- Input token count for the current turn, and the context window limit
- Output token count for the current turn, and the maximum output token limit
- Running totals of input and output tokens across the whole conversation

This lets you keep an eye on how much context has been used and how close you are to the model's
limits.

For a mindspace-wide breakdown of token usage across all conversations and models, see
[Token Usage](token-usage.md).

---

## Conversation files

Every conversation is automatically saved to a `.conv` file in your mindspace's conversations
folder as it progresses. You do not need to save manually.

Conversations can be renamed by double-clicking their label in the Conversations panel. You can
also duplicate or delete them from the right-click context menu.

If you start a conversation but never receive any AI response, the conversation file is
automatically deleted when you close the tab to avoid cluttering the conversations folder.

---

*[Index](index.md) · Previous: [Tabs & Columns](tabs-and-columns.md) · Next: [AI Models & Backends](ai-models-and-backends.md)*
