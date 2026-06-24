# Your First Conversation with an AI

This chapter walks you through starting a conversation with an AI, understanding what you see on
screen, and getting the most out of the back-and-forth exchange. By the end you will know how to
send messages, read responses, handle tool requests, and keep things on track if something is not
going the way you want.

---

## Starting a new conversation

Conversations are created from the **Conversations** section of the mindspace panel on the left
side of the screen.

- **Right-click** anywhere in the Conversations section and choose **New Conversation** from the
  context menu.

A new conversation tab will open, ready for you to start.

If this is your first time using Humbug, or you have just set up a new mindspace, the Conversations
section may be empty. That is expected — your conversations are stored in your mindspace, so each
mindspace starts fresh.

---

## The conversation tab

A conversation tab has two parts:

- **The message history** occupies most of the tab. As your conversation grows, messages from you
  and the AI accumulate here in chronological order.
- **The input area** sits at the bottom. This is where you type your messages.

### The input area

The input area is a multi-line text box. You can type a short question or a detailed prompt — there
is no length limit. At the bottom of the input area you will find four small buttons:

| Button | What it does |
|---|---|
| **Paperclip** | Attach a file from your mindspace to the message |
| **Submit** | Send your message to the AI |
| **Settings (cog)** | Open the conversation settings for this tab |
| **Stop** | Cancel the AI's current response (only visible while the AI is working) |

The label above the input area shows which AI model is currently active and the keyboard shortcut
to submit: **Cmd+Enter** on macOS, **Ctrl+Enter** on Windows and Linux.

---

## Sending your first message

Type your message in the input area and press **Cmd+Enter** (macOS) or **Ctrl+Enter**
(Windows/Linux), or click the submit button.

The input area label will change to indicate the AI is processing. You will see the AI's response
appear in the message history as it is streamed back — the text arrives progressively rather than
all at once.

### What a good first message looks like

You do not need to follow a special format. Just describe what you want to do, the same way you
would describe it to a knowledgeable colleague. For example:

- *"Can you explain how a hash table works?"*
- *"I have a Python script that reads a CSV file and calculates totals. Can you take a look and
  suggest improvements?"*
- *"Help me plan the structure of a short article about renewable energy."*

If your question involves files in your mindspace, you can either describe them in words or attach
them directly using the paperclip button.

---

## Reading the AI's response

The AI's response appears as a message in the history, formatted using Markdown. Headings, lists,
tables, and code blocks are all rendered so they are easy to read.

If the AI model you are using shows its reasoning steps before answering (some models do), you will
see a separate reasoning section before the main response. This section is collapsed by default to
keep the conversation clean, but you can expand it to see the AI's chain of thought.

### AI model labels

Each AI response shows which model produced it, so if you switch models mid-conversation you can
always tell which message came from where.

---

## Tool use and approvals

When the AI wants to do something that goes beyond generating text — reading a file, running a
command, writing to your mindspace — it will make a **tool request**. Humbug always asks for your
approval before anything that could change your files or run code on your machine.

When a tool request is pending, an approval widget appears in the conversation. It shows you
exactly what the AI wants to do, along with a summary of the specific action it is requesting. You
have three options:

| Button | What happens |
|---|---|
| **Approve** | The tool runs and the AI continues |
| **I'm not sure** | The approval is cancelled, and Humbug tells the AI you want to discuss further. The input area becomes active so you can ask a question or redirect the AI |
| **Reject** | The tool request is refused and the AI is informed |

Tool requests that are read-only — such as reading a file — do not require approval and happen
automatically.

---

## Sending a message while the AI is working

If the AI is in the middle of a long response or a series of tool calls, you do not have to wait
for it to finish before you can communicate. You can type a message in the input area at any time
and submit it. The message will be **queued** and delivered to the AI as soon as it reaches a
suitable pause point, such as after the current tool call completes.

This is useful for:

- Telling the AI to stop what it is doing and try a different approach
- Pointing out something that will affect the work in progress
- Simply saying "stop" if the AI is going in the wrong direction

---

## Stopping a response

To stop the AI mid-response, click the **Stop** button (the square icon that replaces the submit
button while the AI is working), or press **Esc**. Humbug will ask you to confirm before
cancelling, to avoid accidental stops.

---

## Navigating a long conversation

As a conversation grows, you can scroll through the message history. A few shortcuts make
navigation faster:

- **Alt+Up / Alt+Down** — jump between your own messages, skipping AI responses and tool details.
  This is handy for reviewing what you have asked so far or jumping to a specific point in the
  conversation.
- **Cmd+F / Ctrl+F** — open a find bar to search for text anywhere in the conversation.

---

## Conversations are saved automatically

Every conversation is saved to your mindspace as it goes. You do not need to save manually. If you
close a conversation tab and reopen it from the Conversations panel, everything will be exactly
as you left it.

---

*[Index](index.md) · Previous: [Installation & First Launch](installation.md) · Next: [Mindspaces](mindspaces.md)*
