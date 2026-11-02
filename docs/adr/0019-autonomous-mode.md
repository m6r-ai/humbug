# ADR-0019: Autonomous Mode — conversation-native missions alongside Classic Mode

Date: 2026-11-02  
Status: Proposed — Pending Maintainer Review

## Context

### Problem

Humbug's interface is built around tabs. The user starts each conversation, watches
it, and answers each approval in the tab that raised it. For multi-step work the user
coordinates by hand: which conversation does what, what depends on what, what is
finished, and what is waiting for them.

The request is for a second, optional experience, called Autonomous Mode:

- The user states an objective.
- Humbug turns it into a structured mission of tasks.
- The tasks run, in parallel where it is safe, with their progress, failures and
  results tracked.
- The user is asked only when they are needed, and can open or take over any
  conversation.

Classic Mode must stay as it is. Both modes share mindspaces, conversations, tools,
persistence and security controls. Interface mode and autonomy permissions are
separate settings.

The full repository assessment is in
[Humbug_Autonomous_Mode_Architecture_Assessment.md](../Humbug_Autonomous_Mode_Architecture_Assessment.md).

### What already exists

Most of the execution machinery is already in place:

- **Several conversations run at once.** Each `AIConversation` is independent, and
  many can stream on the single qasync loop.
- **Delegation already runs in parallel.** `DelegateAITool` starts a child
  conversation and returns a continuation. The continuations from one turn are
  awaited together, so children run concurrently.
- **Delegation is authorised.** Every delegation needs approval. A delegated
  session can be resumed only by its parent, checked against `ConversationDag`
  (ADR-0011).
- **One approval mechanism.** Every approval goes through
  `AIConversation._request_tool_authorization`, which is resolved by
  `approve_pending_tool_calls` or `reject_pending_tool_calls`.
- **The interface is a projection.** `ContextRegistry` holds the workspace state
  independently of any interface, and the desktop draws from it (ADR-0008).
- **An audit log.** The interaction log is append-only and fsync'd, and the AI
  cannot reach it (ADR-0017).

### What is missing

- **No limits.** Delegation has no depth limit, no concurrency limit, and no spend
  limit.
- **Approvals need the conversation's tab.** An approval is answered only by that
  tab. A conversation without a tab waits forever, and the delegate tool ignores a
  failure to open the child's tab.
- **Bookkeeping lives in the widget.** Usage accounting, and the audit record of
  completed AI messages and approval decisions, are done in `ConversationWidget`. A
  conversation driven without its tab is therefore neither counted nor audited.
- **Cancelling stops only one conversation.** Cancelling a conversation does not
  cancel the children it delegated.
- **No record of objectives.** Nothing records what a group of conversations is
  working towards, or how far it has got.
- **No restart detection.** Interrupted turns are neither detected nor reported
  after a restart.

### Constraints from accepted decisions

- **ADR-0001** says agentic work happens within conversations and that "there is
  no separate orchestration layer". It rejected planners, task graphs and execution
  engines, because they hide steps from the user and duplicate planning the model
  already does in context.
- **ADR-0006** rejected a separate AI surface: "a headless agent API alongside a
  human-only GUI".
- **ADR-0007** requires approval for any change to persistent state. The blueprint
  requires "explicit user approval before each action" for tools with side effects.
- **ADR-0014** allows one process per mindspace, with an exclusive lock, and one
  window per process.
- **ADR-0003, ADR-0004, ADR-0005:** local-first, minimal dependencies, and a
  backend free of Qt.

### Goals

- An objective-driven workspace offered as an alternative view, with Classic kept
  as the default.
- Missions that persist and survive a restart.
- Safe parallel execution within enforced limits.
- One place to see and answer every pending decision.
- Pause, resume, cancel and take over at any time.
- No reduction of any existing security guarantee.

### Non-goals

- A planning or scheduling engine that decides or dispatches work without a
  conversation.
- Missions that span mindspaces.
- Approving state-changing actions without a human. That needs a separate decision;
  see "Autonomy levels".
- Remote or mobile access (ADR-0018).
- Any change to the transcript format.

### Decision drivers

1. Preserve ADR-0001, ADR-0006 and ADR-0007, rather than working around them.
2. Keep one source of truth for execution: transcripts and the conversation graph.
3. Behaviour lives in Qt-free backend modules; the desktop only renders and
   triggers.
4. Every autonomous action appears in a transcript and the audit log.
5. Reuse before building. YAGNI.

## Decision

### Missions are conversation-native

Autonomous Mode is built as a thin mission layer over the existing conversation
engine. **Planning and coordination remain inside an AI conversation.** Humbug adds
only what a conversation cannot do for itself.

- **A mission** is a first-class record scoped to one mindspace. It holds the
  objective, the autonomy level and policy, the status, the plan the AI recorded,
  and a link to its root conversation. It is not a second copy of execution state:
  what happened is read from transcripts and `ConversationDag`.
- **The mission lead** is an ordinary conversation that the mission opens. It
  plans the work, records the plan, and runs the tasks by delegating to child
  conversations with the existing delegate tool. Independent tasks run in parallel
  as they already do today.
- **Humbug's backend adds** the following:
  - **Limits**, enforced in the delegate path: depth, children per task, total
    tasks, concurrent turns, and a token budget.
  - **An approval hub** that answers existing approvals from a central inbox.
  - **Stopping and closing children.** Cancelling cascades to children, and
    children's contexts are always closed.
  - **Detection of interrupted missions** after a restart.
  - **Write-scope leases** between parallel tasks.
  - **A projection of mission state** for the interface.

**No component plans, chooses or dispatches tasks on its own.** Humbug enforces
limits and keeps records; the conversation does the orchestrating. The maintainer
is asked to confirm that this stays within ADR-0001 (see "Maintainer approval
conditions").

### Component boundaries

| Component | Kind | Responsibility | Does not |
|---|---|---|---|
| `mission` module | New, Qt-free | Mission lifecycle, store, limits, approval hub, recovery check, activity projection | Plan, choose or dispatch tasks; import `desktop` |
| `mission_ai_tool` | New AI tool | Lets the lead record plans, update task status, report evidence and ask a human | Approve, change policy, or touch files |
| Mission lead | Ordinary conversation | Plans and delegates | Bypass tools or approvals |
| `DelegateAITool` | Existing, extended | Starts children; enforces limits; cascades cancellation | Change approval rules |
| `AIConversation` | Existing, small fixes | Executes; marks a pending approval as `WAITING_FOR_APPROVAL`; claims busy before it yields | — |
| `AITranscriptConversation` | Existing, extended | Persists; also records usage and audit entries (moved out of the widget) | — |
| Desktop mission views | New, Qt | Render backend state; trigger backend operations | Hold mission state or make decisions |

The backend responsibilities currently held by desktop code move first. These moves
keep behaviour the same, and Classic benefits from them too:

- `json_store` moves to a Qt-free module.
- Usage accounting moves out of `ConversationWidget`.
- The audit record of completed messages and approval decisions moves out of
  `ConversationWidget`.

### Interface mode and autonomy are separate settings

- **Interface mode** is Classic or Autonomous. It is a user-wide default with a
  switch for the current window. Because one window is one process and one
  mindspace (ADR-0014), a per-window switch is in effect per mindspace. Switching
  changes only what is drawn: conversations continue and nothing is closed.
- **Autonomy level** is set per mission. A per-mindspace maximum caps it. The
  interface mode does not determine autonomy, and the autonomy level does not
  determine the interface mode.

### Autonomy levels

| Level | Behaviour | Status under this ADR |
|---|---|---|
| Assist | Missions organise and track work; every approval behaves exactly as in Classic | Decided |
| Supervised | As Assist, with approvals also collected in the inbox and notified | Decided |
| Bounded Autopilot | Pre-authorisation at mission start, for a finite and displayed set of non-destructive operation classes | **Not decided here.** It replaces approval before each action with approval in advance, which changes ADR-0007. It requires a separate ADR before implementation. |

### Approvals: one mechanism, two surfaces

- The approval hub subscribes to `TOOL_APPROVAL_REQUIRED` on every live
  conversation in the mindspace.
- It resolves approvals only through the existing approve and reject methods.
- The conversation tab and the inbox show the same pending request. Whichever
  answers first wins, and the other surface clears.
- An inbox decision is bound to the conversation and the tool-call message. The hub
  refuses to resolve a request that is no longer pending.
- No AI tool can reach the hub.

### Security invariants

1. **Never more than Classic.** The effective authority of an autonomous action is
   at most what the same action has in Classic Mode. Mission policy can only narrow
   it. No mechanism in this ADR turns a deny into an allow.
2. **One approval path.** Every state-changing action is approved through the
   existing primitive, by a human, at the time it happens.
3. **Destructive operations always need a fresh human decision,** at every
   autonomy level.
4. **Children cannot gain authority.** A child's tool set is a subset of its
   parent's and of the mission policy's. Depth, child counts and the total task
   count are bounded. The ADR-0011 rule for resuming a session is unchanged.
5. **No silent automatic replay.** After a restart, a tool call that has no result
   is marked as not executed, and the mission waits for a human.
6. **Everything is audited.** Every autonomous turn, tool call and decision reaches
   the interaction log, whether or not a tab is open.
7. **One mindspace.** Missions stay inside one mindspace, and tools stay inside
   `resolve_tool_path`.
8. **Parallel writes are scoped.** Each running task holds a write-scope lease that
   does not overlap another's, and a write fails if the file changed after the task
   read it. Terminal commands cannot be scoped, so in a mission they always need
   approval and are shown as unscoped.
9. **Everything is bounded.** Retries, concurrent turns and token spend each have a
   limit. When a limit is reached, the mission stops and tells the user; it never
   fails silently.
10. **Mission records hold no secrets.**

### Data ownership and persistence

- **Mission records** are stored as `.humbug/missions/<id>.json` inside the
  mindspace and written with an atomic replace. AI tools cannot reach them
  (`.humbug/` exclusion), and the existing `.humbug/.gitignore` covers them.
- **Execution history** stays in `.conv` transcripts and the conversation graph.
  The transcript format does not change.
- **Mission events** are written to the interaction log, tagged with the mission
  and the conversation.
- **The interface mode default** is stored in user settings. **The autonomy
  maximum** is stored in mindspace settings.
- **Only one writer.** A mindspace is written by one process only (ADR-0014).

### Pause, resume, cancel and take over

- **Pause:** no new turn or delegation starts. Running turns finish at their next
  boundary, and long waits are interrupted (ADR-0015).
- **Resume:** sends the lead conversation a message describing the current state.
- **Cancel:** cancels the lead and cascades to every child.
- **Take over:** when the user types into a mission conversation, its task is
  marked as human-controlled and the lead is told. The mission sends that
  conversation no further autonomous messages.

### Recovery after restart

When a mindspace opens, the backend runs these steps:

1. It finds missions that were planning, running or waiting.
2. It inspects their conversations.
3. It marks those missions as interrupted and then paused, and reports what it
   found.

Recovery only detects; it never executes anything. When the user resumes, each
tool call without a result is recorded as "interrupted, not executed", and then
the lead is resumed. Any action with side effects goes through approval again.

### Backwards compatibility

- Classic is the default. Without missions, behaviour is unchanged apart from the
  moves listed under "Component boundaries", each of which is covered by tests.
- An older Humbug ignores `.humbug/missions/`.
- No dependency is added.

### Rollout and rollback

- **Delivery order:** the phases follow the roadmap in the assessment:
  1. The moves out of the desktop, plus the mode setting.
  2. The approval hub and inbox.
  3. The mission model and persistence.
  4. Limits, leases and controls.
  5. Recovery and verification.
  6. Hardening.
- **Visibility:** Autonomous Mode stays off by default until phase 5.
- **Rollback:** switch the interface back to Classic. Mission records are inert
  files, and conversations remain ordinary conversations. Removing the feature
  leaves every transcript readable.

### Acceptance criteria

- **Classic unchanged:** the existing test suite passes, and Classic behaviour is
  unchanged.
- **Approvals without a tab:** an approval raised by a conversation without a tab
  can be answered from the inbox.
- **Stale decisions are refused:** a decision for a request that is no longer
  pending cannot approve anything.
- **Bookkeeping without a tab:** a conversation driven without a tab is audited and
  its usage is counted.
- **Cancel cascades:** cancelling a mission or a parent conversation stops every
  child.
- **Limits are enforced and reported:** depth, children, concurrency and budget
  limits are enforced, and the user is told when one is hit.
- **Write leases hold:** a write outside its lease, or overlapping another task's
  lease, is refused.
- **Interrupted missions recover:** if the process is killed mid-mission, the
  mission is paused after restart, its findings are shown, and no tool call runs
  twice.
- **Invariants are tested:** each security invariant has a test.

### Open questions

1. Should Bounded Autopilot be pursued? If so, which operation classes would be
   eligible?
2. What should the default limits be?
3. Should mission children get a narrower tool set by default (for example, no
   terminal)?
4. Should rejection reasons reach the AI? Today they are only logged.
5. Should mission records travel with conversation move and delete operations?
6. Is the per-window interface switch wanted, or only a user-wide setting?

### Maintainer approval conditions

Implementation does not start until the maintainer:

1. Confirms that mission records and enforced limits fall within ADR-0001. The
   alternative is to direct an explicit amendment of ADR-0001.
2. Accepts the security invariants above.
3. Accepts that the bookkeeping moves out of the desktop come first.
4. Agrees that Bounded Autopilot is out of scope until it has its own ADR.

## Alternatives considered

- **A dashboard only (Option A).** A new view of running conversations,
  approvals and the delegation tree, with no mission model. This is compatible and
  cheap, but it cannot record objectives, enforce limits, recover from a restart,
  or answer approvals for conversations without a tab. It is kept as the
  deliverable for phase 2, built on this decision.

- **An orchestration engine (Option B in engine form).** Planner, scheduler and
  executor services that decide and dispatch tasks. This is the pattern ADR-0001
  rejected. It hides decisions from the transcript, it duplicates planning the
  model already does, and it creates a second store of execution state that must
  be kept in step with the transcripts. The parallel execution it would provide
  already exists through delegation.

- **An independent orchestration runtime (Option C).** A separate process that
  drives Humbug through an interface. This conflicts with the following:
  - ADR-0006, because it is a headless AI surface.
  - ADR-0014, because it adds a second writer to the mindspace.
  - ADR-0003 and ADR-0004.

  It would also need a second approval path and an authenticated communication
  channel between processes.

- **Using Menai to define and run workflows.** Menai is pure and has no I/O, so it
  cannot run, wait for or observe anything. Checking a plan is easy to do in
  Python. Menai has no role in the core.

- **Missions spanning mindspaces.** These would need cooperation between processes
  that ADR-0014 deliberately avoided, and they would weaken the boundaries set by
  `resolve_tool_path` and the per-mindspace audit log.

- **Auto-approving within a mission by default.** Rejected. It would silently grant
  authority that Classic does not have. Pre-authorisation, if it is wanted at all,
  needs its own decision.

- **Replaying interrupted tool calls on restart.** Rejected. Terminal commands,
  HTTP writes and file moves are not safe to repeat.

## Consequences

### Positive

- **Execution truth stays in one place.** The transcripts and the conversation
  graph remain the record, and every step stays visible in a conversation.
- **Little new code.** Parallel execution, delegation, authorisation and the audit
  log are reused, and the new components are small and testable without Qt.
- **Classic benefits too.** Conversations without a tab are audited and counted,
  cancellation cascades, and the hang on approvals without a tab goes away.
- **One approval mechanism serves both modes,** so there is no second policy to
  keep consistent.
- **Ready for other interfaces.** All behaviour is in the backend, so other front
  ends (including remote access) can show the same missions and inbox.

### Negative

- **The work rests on the conversation engine.** The moves out of the desktop
  touch load-bearing code (ADR-0001), so they need tests first.
- **The model decides the plan.** Plan quality depends on it, and a poor plan is
  caught by limits and humans, not by an engine.
- **Without Bounded Autopilot, "autonomous" still means a human approves every
  state-changing action.** Autonomy comes from aggregation and parallelism, not
  from removing approvals. Some users will expect more.
- **Write leases reduce parallelism,** and terminal commands cannot be scoped.
- **New state.** Mission records are a new kind of mindspace state, and file
  operations on conversations must decide how to treat them.

### Risks

- **Regressions in Classic** from the moves out of the desktop. Mitigation: tests
  first, and one move per change.
- **Pressure to ship autopilot early.** Mitigation: this ADR excludes it.
- **Drift towards an engine over time.** Mitigation: any component that plans or
  dispatches on its own needs a new ADR.
