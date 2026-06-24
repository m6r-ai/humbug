# Forking & Managing Conversations

This chapter covers how to organise your conversations — forking them to explore alternatives,
renaming and duplicating them, and cleaning up ones you no longer need.

---

## Forking

Forking creates a new, independent conversation that starts as an exact copy of the current
one up to and including a specific message. From that point, the two conversations diverge —
changes in one do not affect the other.

To fork a conversation, click the **Fork button** on any of your messages. A new conversation
tab opens, named after the original with a "— fork" suffix. It opens alongside the original
so you can compare the two paths.

Forking is useful when:

- An AI response has been useful but you want to explore a different follow-up
- You want to try the same question with a different model or temperature
- You want to keep your current thread clean while experimenting with a divergent idea

---

## Renaming conversations

Double-click a conversation label in the **Conversations** view to rename it inline. Press
**Enter** to confirm or **Esc** to cancel.

Giving conversations descriptive names makes it easier to find them later, especially as the
list grows.

---

## Duplicating conversations

Right-click a conversation in the Conversations view and choose **Duplicate**. This creates
a complete copy of the conversation, including all messages and history. The duplicate is
immediately available for editing and can be renamed inline.

---

## Deleting conversations

Right-click a conversation in the Conversations view and choose **Delete**. You will be asked
to confirm.

If you open a new conversation, send no messages that receive an AI response, and then close
the tab, the conversation file is automatically deleted. This prevents the conversations folder
from filling up with empty stubs.

---

## Conversation relationships

When an AI delegates a task to a child AI, the child conversation appears nested beneath
its parent in the Conversations view. This hierarchy is preserved even after the session —
you can always trace which conversations were spawned from which.

---

## Navigating a conversation

**Alt+Down** and **Alt+Up** jump between your own messages in the active conversation,
skipping AI responses and tool details. This lets you quickly scan what you have asked
across a long session.

Individual messages can be **collapsed and expanded** by clicking the toggle in the message
banner. Tool call and tool result messages are collapsed by default to keep the conversation
readable.

---

## Where conversations are stored

Conversations are saved automatically as `.conv` files in the `conversations` folder inside
your mindspace. You do not need to save them manually. They persist across sessions and are
restored when you reopen a mindspace.

---

*[Index](index.md) · Previous: [Attaching Files to Conversations](attachments.md) · Next: [Delegating Tasks Between AIs](delegation.md)*
