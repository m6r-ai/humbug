# Attaching Files to Conversations

You can attach files to messages in a conversation to give the AI direct access to their
contents — without having to copy and paste text manually. Attachments work for any file type
that Humbug can read, including source code, Markdown, plain text, PDF, and DOCX.

---

## Attaching a file

Click the **paperclip button** in the input area. A file picker opens. Navigate to the file
you want and confirm.

Attached files appear as labelled chips between the text input and the submit button. You can
attach multiple files to a single message. To remove an attachment before sending, click the
**×** button on its chip.

### Dragging a file in

You can also drag a file from the Files or Preview panel and drop it on the conversation's input
box. The input highlights its border while you hover, and the file is attached when you let go.

Where you drop matters. A file dropped anywhere else in the tab area opens in a tab, as it
normally does; only a drop on the input box attaches it. Dropping a folder, or a conversation
from the Conversations panel, does nothing at all, because neither has contents of its own that
could be sent.

### Large attachments

Attachments are sent whole, so a large file consumes a large part of the model's context window
before you have said anything. If a file is over about **100KB**, Humbug warns you before attaching
it and tells you its size, then lets you decide. A source file or a note is rarely a problem; a
generated data file or a bundle usually is.

If what you actually need is for the AI to inspect a large file selectively, attaching it is the
wrong tool — ask it to read the file with its filesystem tool instead, where it can search and read
only the parts that matter.

---

## What the AI receives

When you submit a message with attachments, Humbug sends both your text and the full contents
of each attached file to the AI. For PDF and DOCX files, the text is extracted automatically —
the AI sees the text content, not the raw binary format.

The AI can refer to the attached files freely in its response, as if it had read them itself.

---

## Viewing attachments in the conversation history

Attachments are saved as part of the conversation and remain visible in the history. On any
of your historical messages that had attachments, an **attachments button** appears in the
message banner. Click it to expand the attached files and view their contents inline in the
conversation.

---

## Attachments vs the filesystem tool

Attaching a file explicitly sends its contents as part of one message. If you want the AI
to read, search, or write files throughout a session without you having to attach them each
time, the AI can use its **filesystem tool** instead — reading files directly from your
mindspace as needed. Attachments are best when you want to be specific about exactly what
the AI should focus on in a given turn.

---

*[Index](index.md) · Previous: [Quick Switcher](quick-switcher.md) · Next: [Forking & Managing Conversations](forking-conversations.md)*
