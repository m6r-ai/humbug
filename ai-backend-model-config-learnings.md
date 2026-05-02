# AI Backend and Model Configuration — Design Learnings

## Background

This document captures the design learnings from an initial attempt to implement
user-configurable AI backends and models in Humbug. The implementation was reverted
because the UI/UX was not validated before the backend changes were made. This
document records what we learned so we can approach the work correctly the second
time.

## What We Were Trying to Achieve

The existing system has a fixed 1:1 mapping between a backend type (e.g. `anthropic`,
`ollama`) and a single backend instance. This breaks down for users who have:

- Multiple instances of the same backend type (e.g. two Ollama servers)
- Local inference servers (vLLM, Ollama) alongside cloud providers
- Multiple accounts with the same provider

The goal is to allow users to configure named backend instances and named model
entries, giving them full control over what runs where.

## Core Architectural Decisions (Still Valid)

These decisions were correct and should be carried forward:

### Backend instances

The current fixed dict keyed by provider type becomes a list of named instances.
Each instance has:

- A stable UUID as its id
- A user-visible display name
- A backend type (the protocol/API shape — anthropic, ollama, openai-compatible, etc.)
- Credentials (API key, URL)

Multiple instances of the same type are fully supported.

### Model entries

A user-configured model entry binds together:

- A stable UUID as its id
- A user-visible display name
- A backend instance id
- The API model name to send in requests
- Capability metadata (context window, max output tokens, supports temperature,
  temperature default, reasoning capability, tool capability)

The model entry is the **single source of truth** for all conversation behaviour.
Temperature and reasoning choice are baked into the entry rather than being
per-conversation overrides — if you want the same model at different temperatures,
you create two entries.

### Model catalogue

The existing `MODELS` dict is retained as a **reference catalogue** used only when
creating model entries in the UI to pre-fill capability fields. It is never consulted
at request time. This means:

- Known models get their capabilities pre-filled automatically
- Unknown/custom models work fine — the user fills in the fields manually
- The catalogue can be updated without changing any runtime behaviour

### Conversation settings

`AIConversationSettings` becomes a thin wrapper around `AIModelSettings`. There is
no longer a separate `model`, `temperature`, or `reasoning` field at the conversation
level — everything comes from the model entry.

### Serialisation

- `UserSettings.ai_backends` becomes `List[AIBackendSettings]`
- `UserSettings.ai_models` becomes `List[AIModelSettings]`
- `MindspaceSettings` stores `model_id: str` (the id of the default model entry for
  this mindspace, empty string meaning none selected)
- `AIMessage` gains `model_id` and `backend_id` fields in the transcript so saved
  conversations are fully self-describing

## What Went Wrong: UI First

The implementation made all the backend changes first, then tried to build the UI on
top. This was the wrong order. The UI turned out to be unusable as designed, but by
that point the backend had already been significantly restructured, making it hard to
iterate.

**The lesson: build and validate the UI first, with stub/mock data if necessary.
Only then plan and implement the backend changes to match.**

## The Right Approach

### Phase 1 — UI only, no backend changes

Build the AI Models settings page as a standalone UI exercise against the existing
data model:

1. A list showing configured model entries (with a helpful empty-state message when
   none are configured)
2. **"Add from Catalogue"** button — opens a dialog showing catalogue entries, user
   picks one, picks a backend (can be a simple text field initially), optionally
   renames it. Capability fields are pre-filled and can be edited.
3. **"Add Custom"** button — opens a form with all fields for manual entry
4. Each entry in the list has a **Remove** button

Iterate on this until the UX feels right before touching any backend code.

### Phase 2 — Settings dialog architecture fix

The settings dialog should be accessible even when no mindspace is open. Currently
the "This Mindspace" items are greyed out when no mindspace is active, which makes
the dialog feel broken during first-run setup.

The cleaner split:

- **User settings** (Display, File Access, AI Backends, AI Models) — always
  accessible, always shown in the nav regardless of mindspace state
- **Mindspace settings** (Default Model, Tools, Editor, Terminal) — only shown when
  a mindspace is open; hidden entirely rather than greyed out when not

This is important because a new user needs to configure backends and models before
they can open a mindspace and do anything useful.

### Phase 3 — Backend changes

Once the UI is validated, implement the backend changes to match. The broad strokes
are known (see Core Architectural Decisions above) but the exact shape should be
driven by what the validated UI needs.

## UI Design Decisions

### Model catalogue

- The catalogue should eventually be **browsable** (grouped by provider, scrollable)
  rather than a flat combo with 50+ entries. However, a flat list is acceptable for
  the initial implementation — browsability can be added later without changing the
  data model.
- The catalogue is the **primary discovery mechanism** for users. It should feel like
  "here are the models we know about — pick one and configure it" rather than an
  internal lookup table.

### Add from Catalogue vs Add Custom

These are two distinct paths:

- **Add from Catalogue** — pick a known model, select backend instance, optionally
  rename. Capability fields are pre-filled and hidden unless the user wants to edit
  them. This is the happy path for most users.
- **Add Custom** — fill in everything manually. This is the escape hatch for local
  models, unreleased models, or anything not in the catalogue.

The two options should be clearly labelled as separate buttons, not modes within a
single form.

### Capability fields

When adding from catalogue, the capability fields (context window, max output tokens,
supports temperature, temperature, reasoning capability, tool capability) should be
pre-filled and **collapsed by default** — shown only if the user explicitly wants to
edit them. This keeps the happy path simple.

When adding a custom model, all fields are shown and required.

### Empty state

When no models are configured, the models page should show a clear, friendly message
explaining what to do — not just an empty list. Something like:
"No models configured. Use 'Add from Catalogue' to add a known model, or 'Add Custom'
to configure one manually."

### Model entry display

Each configured model entry in the list should show:
- Display name (prominent)
- API model name and backend instance name (secondary, smaller)
- Remove button

This gives the user enough context to identify entries without overwhelming them.

## Open Questions

- Should the model entry list support reordering? (Probably yes eventually, for
  controlling which model appears first in conversation dropdowns.)
- Should there be an Edit button per entry, or is Remove + Re-add sufficient?
- When a backend instance is removed, should its model entries be automatically
  removed too, or just marked as broken?
