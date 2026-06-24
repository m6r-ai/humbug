# Delegating Tasks Between AIs

Humbug supports **AI task delegation** — the ability for one AI conversation to spawn a
second, focused AI conversation to handle a specific sub-task on its behalf. This lets you
tackle complex, multi-part problems without one conversation accumulating an unmanageable
amount of context.

---

## How delegation works

When the AI decides a sub-task is better handled in isolation, it can request to open a
new child conversation. This appears in Humbug as a tool request requiring your **explicit
approval** — you can see what the child AI will be asked to do before it starts.

Once you approve, a new conversation tab opens for the child AI. The parent AI is paused
while the child works. When the child completes its task, it returns a result to the parent,
which then continues.

---

## What you see

- A **tool approval widget** appears in the parent conversation, describing what the child
  AI will be asked to do and which model it will use
- The **child conversation tab** opens in a new column so you can watch it work alongside
  the parent
- In the **Conversations view**, child conversations appear nested beneath their parent,
  so you can see the relationship clearly
- The parent conversation shows a **rotating colour border** on its active message while
  waiting for the child, indicating it is still alive but paused

---

## Interacting with a child AI

The child conversation's input area remains active while it is working. You can type a
message and submit it to provide feedback, redirect the child AI, or ask it to stop.

---

## When to expect delegation

Delegation is an AI-driven decision — it happens when the AI judges that a task is
best handled separately. You are most likely to see it when:

- You ask the AI to handle a complex, multi-stage task
- The AI wants to research or process something without cluttering the main conversation
- You have explicitly asked the AI to break work into parallel or sequential sub-tasks

You always have the opportunity to approve or reject a delegation request before any child
AI is created.

---

## Child conversations and session history

Child conversations are saved alongside parent conversations in your mindspace. Their
relationship to the parent is preserved, so you can review the full picture of what
happened — both the parent's reasoning and the child's work — even after the session ends.

---

*[Index](index.md) · Previous: [Forking & Managing Conversations](forking-conversations.md) · Next: [Token Usage](token-usage.md)*
