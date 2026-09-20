# ADR-0009: Logging level policy

Date: 2026-09-20  
Status: Accepted

## Context

Humbug writes structured logs to rotating files under `~/.humbug/logs`. These logs are the
primary diagnostic artefact a user can hand to a developer when something goes wrong, and they
are also a record of significant system activity.

For the logs to be useful, the level assigned to an event must mean something. If everything is
logged, the events that matter are buried under routine tracing and a log that is mostly noise is
worse than useless. If too little is logged, a session cannot be understood without reproducing
it. The project therefore needs a principled rule for choosing a log level, so that both human
and AI contributors classify events consistently and the default logs are worth reading.

Two properties of Humbug shape the rule:

- The human-in-the-loop safety model depends on visibility. Tool approval and denial
  decisions are the point at which the user sanctions an AI action. If those decisions are not
  visible at the default level, the audit trail loses its most important entries.

- Conversation content is high-volume and privacy-sensitive. Logging full request or response
  payloads serves no diagnostic purpose that the conversation store does not already serve, while
  inflating the logs and copying user content into them.

## Decision

The default root log level is `INFO`. `DEBUG` is reserved for high-frequency tracing that is only
useful when actively diagnosing a specific problem.

Log levels are assigned by the following rule:

- **DEBUG** — high-frequency, per-operation tracing whose value is only realised when debugging a
  specific issue. Examples: file-watcher events, per-stream-chunk parsing, tab and terminal
  lifecycle traces, and internal bookkeeping.

- **INFO** — significant, relatively low-frequency events that describe what the system is doing
  at a level a user or developer would want to see in a normal log. Examples: the start and end of
  an AI response, queuing a message, executing a tool call, and tool authorization requests and
  their outcomes.

- **WARNING** — failures and anomalies that do not necessarily abort the operation but that
  indicate something did not go as expected. Examples: API error responses, a failed git status,
  a failed update check, a file that could not be indexed, an unknown backend encountered in
  settings, and an uncaught exception triggering the canary.

- **ERROR** / **CRITICAL** — errors that abort an operation or represent a serious fault.

Two consequences follow directly from this rule:

1. Security-relevant events are never logged at `DEBUG`. Tool approval and denial decisions
   are part of the human-in-the-loop safety model and must be visible at the default level.

2. Full request/response payloads are not logged. The entire conversation history is not a
   useful log line at any level; it is high-volume, privacy-sensitive, and duplicates what is
   already persisted in the conversation store.

## Alternatives considered

- Default to `DEBUG`. This maximises diagnostic detail but makes the level of most events
  meaningless, since everything is emitted regardless. The events that matter are lost among
  routine tracing and the logs grow without bound.

- Default to `WARNING`. This produces very small logs, but hides the INFO-level narrative of
  what the system is doing (starting a response, executing a tool, requesting authorization) that
  makes a log useful for understanding a session without reproducing it.

- Make the level configurable via an environment variable or setting.** This adds a mechanism
  whose only purpose is to re-enable high-frequency tracing. It is speculative until there is a
  concrete need, and the level can be changed in source when that need arises. If a genuine need
  for runtime reconfiguration appears, it can be added later and this ADR superseded.

- Log full payloads at `DEBUG`. At the `INFO` default they would be unreachable code, which
  conflicts with the project's YAGNI principle. They also carry privacy and log-size costs that
  demotion does not address.

## Consequences

### Positive

- Logs are dominated by events that are meaningful at the default level, so a user reporting a
  problem produces a log that is actually useful.

- Security-relevant decisions (tool approval and denial) and failures are visible without any
  configuration.

- The rule is simple and applies uniformly, so contributors classify new log statements without
  case-by-case debate.

- Not logging full payloads avoids copying user conversation content into the logs.

### Negative

- High-frequency tracing is not captured by default. Diagnosing an issue that depends on it
  requires changing the level in source and rebuilding or rerunning.

- Contributors must apply the rule correctly. Misclassifying a significant event as `DEBUG` hides
  it from the default logs; misclassifying routine tracing as `INFO` reintroduces noise. Code
  review is the safeguard.
