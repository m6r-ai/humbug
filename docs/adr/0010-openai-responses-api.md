# ADR-0010: OpenAI backend uses the Responses API

Date: 2026-09-25  
Status: Accepted

## Context

Humbug supports a range of AI providers behind a common backend interface (ADR-0004,
ADR-0006). The OpenAI backend was built against the Chat Completions API, which was the
only general-purpose text generation endpoint OpenAI offered when the backend was written.

OpenAI has since introduced the Responses API as its recommended primitive for new
integrations, and the two APIs are no longer equivalent in capability. For the newest
models, the Responses API is the only endpoint that supports the full range of features
Humbug depends on:

- Tool calling on newer models: Some current models only accept function tools on the
  Responses endpoint. On Chat Completions they either reject tools or require reasoning to
  be disabled for tools to be accepted at all, forcing a choice between reasoning and tool
  use. This directly undermines Humbug's core model of agentic, tool-using conversations
  (ADR-0001, ADR-0006).

- Advanced reasoning: The Responses API exposes reasoning capabilities that are not
  available, or are available only in a degraded form, on Chat Completions.

Chat Completions remains supported by OpenAI, so this is not a forced migration. The
question is whether Humbug should continue to target the older endpoint or move the OpenAI
backend to the newer one.

Two constraints shape the decision:

- Statelessness: Humbug treats the conversation as the source of truth and resends the
  full history on each request, which is what allows a conversation to switch providers
  without losing context. The Responses API offers server-side conversation state
  (`store` and `previous_response_id`), but adopting it would make OpenAI conversations
  depend on OpenAI-hosted state and would diverge from how every other backend behaves.

- Vendor independence: The OpenAI backend is one of several. A change here must not
  leak OpenAI-specific concepts into the shared backend interface or the provider-agnostic
  message model.

## Decision

The OpenAI backend targets the Responses API instead of Chat Completions. This is a
replacement, not an additional mode: the backend no longer speaks Chat Completions.

The backend continues to operate statelessly. It resends the full conversation history
on every request and does not use server-side conversation state, preserving the property
that a conversation can move between providers without losing context. Where reasoning
models require their prior reasoning to be carried forward, it is carried in the request
itself rather than by referencing server-side state.

The migration is confined to the OpenAI backend. The shared backend interface and the
provider-agnostic message model are unchanged, so other providers and the conversation
layer are unaffected.

## Alternatives considered

- Stay on Chat Completions: This would keep the backend unchanged, but would leave the
  newest OpenAI models unusable for tool calling (or usable only with reasoning disabled),
  which is the primary motivation for the change. It would also forgo the newer reasoning
  capabilities.

- Support both endpoints, selectable per model: This would allow an incremental
  rollout and preserve Chat Completions for models that work well on it. It was rejected
  because it doubles the surface area of the backend and its tests, and because the
  provider-agnostic message model would need to accommodate the union of both APIs'
  shapes. The benefit is speculative: there is no current need to keep Chat Completions
  for any model Humbug supports.

- Adopt server-side conversation state (`store` / `previous_response_id`): This is the
  path OpenAI recommends for many applications and would reduce request size. It was
  rejected because it makes OpenAI conversations dependent on OpenAI-hosted state, breaks
  the provider-independence that lets a conversation move between backends, and conflicts
  with Humbug's local-first data ownership (ADR-0003).

## Consequences

### Positive

- The newest OpenAI models are usable for tool calling, so they work with Humbug's
  agentic conversation model rather than being limited to tool-free text generation.

- Advanced reasoning capabilities are available on OpenAI models.

- Conversations remain provider-independent: the stateless request model is preserved, so
  a conversation can still move between OpenAI and other backends without losing context.

- The change is contained within the OpenAI backend; the shared interface and message
  model are untouched.

### Negative

- The OpenAI backend no longer works with any OpenAI-compatible endpoint that implements
  only Chat Completions. Providers that expose a Chat Completions-compatible API are served
  by their own backends, so this affects only OpenAI itself, but it does mean the OpenAI
  backend cannot be pointed at a Chat-Completions-only proxy.

- The backend's request and response handling now track the Responses API's item-based
  shapes, which differ from the message-based shapes used by the other backends. This is
  additional provider-specific code to maintain, and it must be updated if OpenAI revises
  the Responses API.

- Stateless reasoning means prior reasoning is resent on each turn, which increases request
  size relative to server-side state. This is the deliberate cost of preserving provider
  independence.
