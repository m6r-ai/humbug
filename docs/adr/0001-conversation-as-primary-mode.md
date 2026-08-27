# ADR-0001: Conversation as the primary mode of operation

Date: 2026-08-27  
Status: Accepted

## Context

Humbug had to choose where agentic behaviour lives. The dominant patterns each had drawbacks:

- **Chatbot wrappers** put a thin shell around an AI API. They cannot support multi-step work: no tools, no persistence, no way for the AI to act on the world.
- **IDE-plugin models** embed the AI as an assistant inside a human-driven tool. The AI reacts to human actions but never drives its own workflows; it remains a second-class actor.
- **Agent frameworks** place an orchestration layer — planners, task graphs, execution engines — between the human and the AI. This hides what is happening from the user, duplicates planning the model already does well in context, and creates a second component whose state must be kept in sync with the conversation.

Three principles constrained the choice:

- Transparency over automation: the human must see, and be able to course-correct, everything the AI does.
- Auditability: there must be a persistent record of what happened and why.
- Equal actors: whatever mode of operation is chosen must serve humans and AIs identically.

Humbug is also bootstrapped — each version is built using the previous one — so the collaboration model had to support long-running, multi-session work without a separate system to maintain.

## Decision

The conversation is the primary unit of collaboration and the primary mode of operation.

- Agentic work happens from within a conversation. The AI discovers what it needs, edits content, runs tools, orchestrates the UI, and delegates to other AI instances — driven by the conversation loop itself.
- There is no separate orchestration layer. The model's own planning ability, exercised through tools, is the orchestration.
- Conversations are persisted, first-class artifacts stored in the mindspace. The transcript is simultaneously the working record, the audit trail, and the context from which future sessions resume.

## Alternatives considered

- Separate orchestration/agent framework was rejected because it hides intermediate steps from the user, contradicting transparency, and adds a component that must be kept in sync with conversation state.
  It also duplicates judgement the LLM exercises better in context, where it can see the actual conversation.
- IDE-plugin model was rejected because the AI can only assist inside human-driven workflows.
  Since it can never be a first-class actor, equal access to tools becomes impossible.
- Chatbot wrapper was rejected because we want agentic capabilities and AI model-independence.

## Consequences

### Positive

- One mental model for humans and AIs alike: everything happens in conversations, visible in the same UI.
- The transcript doubles as the forensic record — what was asked, what was done, and why.
- No orchestration layer to build or maintain; the codebase stays small and comprehensible.

### Negative

- The conversation engine is load-bearing infrastructure; changes to it carry high risk and demand strong test coverage.
- Long-running work requires deliberate context management — bounded tool outputs, transcript handling, and session hand-off become core concerns rather than optimisations.
- The frontend must render rich conversation state (tool calls, approvals, reasoning), coupling it to conversation concepts.
- Every capability must be expressible as a tool callable from a conversation; anything that does not fit the tool model is harder to add.
