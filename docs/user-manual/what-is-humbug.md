# What is Humbug?

Humbug is a platform for working with AI — not just talking to it.

Most AI tools give you a chat window. You type, the AI replies, and that is more or less all there
is to it. Humbug takes a different approach: it treats both you and the AI as active participants
in the same environment, with access to the same tools. The AI can read and write files, run code,
open tabs, search your project, and hand tasks off to other AI instances — all within the same
interface that you are using.

The goal is genuine collaboration: you focus on what matters and the AI handles the rest, with you
staying in control throughout.

---

## The key ideas

### Mindspaces

Everything in Humbug is organised around a **mindspace** — a persistent workspace for a project.
Each mindspace has its own files, settings, conversations, and history. When you open Humbug, you
open a mindspace. When you switch to a different project, you switch mindspace.

Think of a mindspace the way you would think of a project folder, except that it also holds all
the context of your conversations with AI.

### Conversations

A **conversation** is how you communicate with an AI. You type a message, the AI responds, and
the exchange builds up over time into a shared history that both you and the AI can refer back to.

Conversations are persistent — they are saved to your mindspace and can be reopened at any time.
You can run many conversations simultaneously, each with a different AI model or focused on a
different aspect of your work.

### Tabs and columns

Humbug's interface is built around **tabs**, organised into **columns**. You can have conversations,
file editors, terminal windows, and file previews all open at the same time, side by side. You can
drag tabs between columns, create new columns, and arrange the layout to suit how you work.

The AI can open and interact with tabs too. When you ask the AI to edit a file, it can open that
file in an editor tab, make changes, and show you the result — all without you having to do it
yourself.

### Tools

AIs in Humbug have access to **tools** — capabilities that let them go beyond simply generating
text. These include:

- **Filesystem access** — reading and writing files in your mindspace
- **Terminal access** — running commands in a shell (with your approval)
- **Editor access** — reading, modifying, and saving files open in editor tabs
- **Clock** — working with dates and times
- **Menai** — a built-in programming language designed for AI use, safe to run without any
  approval because it cannot access the outside world
- **Task delegation** — the ability to spin up a second AI to work on a sub-task

Every tool action that could cause harm — writing a file, running a command — requires your
explicit approval before it takes effect. You always know what the AI is doing.

### Multiple AI models

Humbug works with AI models from many different providers. You can switch models mid-conversation,
run different models in different conversations simultaneously, or use a local model with no
internet connection required. You are never locked in to a single provider.

---

## Who is Humbug for?

Humbug is designed for anyone who needs to work with AI on tasks that take more than a single
exchange to complete:

- **Developers** who want an AI that can read the codebase, write and edit files, run tests, and
  work through problems methodically
- **Researchers and writers** who want to organise their thinking, drafts, and references in one
  place alongside an AI collaborator
- **Anyone working on a sustained project** where context and history matter

If you have found yourself copying and pasting between a chat window and your actual work, Humbug
is built to solve exactly that problem.

---

## A note on transparency

Humbug is designed to keep you informed. Every tool call the AI makes is shown to you. Anything
that could change your files or run code on your machine requires your approval first. The AI
cannot do things behind your back.

This is a deliberate design choice. AI makes mistakes. Keeping you in the loop means problems
can be caught early, before they become expensive to fix.

---

*[Index](index.md) · Next: [Installation & First Launch](installation.md)*
