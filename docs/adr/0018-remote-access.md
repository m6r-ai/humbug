# ADR-0018: Remote access to a running Humbug from another device

Date: 2026-11-01  
Status: Proposed

## Context

A user working in a mindspace on one machine (Mac 1) wants to move to another machine
(Mac 2) and carry on with the same work there:

- see the mindspace's conversations;
- read them;
- send messages;
- watch the AI answer.

All the while, the work keeps running on Mac 1.

Two existing decisions rule out the obvious shortcuts:

- Copying or syncing the mindspace to Mac 2 conflicts with local-first data ownership
  (ADR-0003). The mindspace must stay on Mac 1.
- Running the conversation on Mac 2 would split one conversation between two engines.

Humbug on Mac 2 must therefore act as a *remote front end* to Humbug on Mac 1. ADR-0008
already anticipates this: the frontend-agnostic `ContextRegistry` and context models
exist so that a front end other than the Qt desktop can be built on them.

### The network is not Humbug's concern

The network between the two machines is provided by HumbugScale, a separate project.
It is a private, encrypted WireGuard network joining one person's devices. Each device
gets a private address in `100.64.0.0/10` (in practice `100.80.x.x`). HumbugScale's
policy lets only the same person's devices reach each other.

HumbugScale's proposal (`docs/HUMBUG-REMOTE-ACCESS-PROPOSAL.md` in that repository)
assigns ownership as follows:

- **Humbug owns** the remote protocol, pairing, conversation access and audit.
- **HumbugScale owns** the connection only.

Humbug therefore implements no NAT traversal, relays or discovery.

### How the desktop drives a conversation today

- **Sending.** `ConversationWidget.submit` handles widget-only concerns: the input
  box, attachments, editing, auto-scroll, and rejecting a pending approval when the
  user types over it. It then calls `submit_message(requester, text)` on the
  conversation's `AITranscriptConversation`, which forwards to `AIConversation`.
- **Busy conversations.** If the conversation is already streaming or running tools,
  `AIConversation` queues the message itself.
- **Turns started elsewhere.** The widget already copes with turns it did not start.
  Delegated children are driven directly on the model, and the widget picks up the
  turn from `AI_CONNECTED`.
- **Attribution.** `requester` is stored on the user message as `user_name`, so a
  message's origin can be recorded.
- **Observing.** The widget subscribes to `AIConversationEvent` callbacks:
  `MESSAGE_ADDED`, `MESSAGE_UPDATED`, `MESSAGE_COMPLETED`, `STREAMING_UPDATE`,
  `AI_CONNECTED`, `COMPLETED`, `ERROR` and `TOOL_APPROVAL_REQUIRED`.
  - Callbacks are kept in a set, so several observers can subscribe.
  - The conversation awaits each callback in turn. An observer that blocks therefore
    stalls the AI loop.
- **Open conversations.** An open conversation is a `ContextRegistry` context with a
  `ConversationContext` model registered against it. Opening a conversation context
  makes the desktop create its tab, which loads the transcript and registers that
  model.
- **Usage and approvals.** Token usage accounting and the approval controls live in
  the conversation tab. A conversation therefore needs a tab on Mac 1 for its usage
  to be recorded and its approvals to be answered.

### Behaviour that lives in the desktop today

Humbug's direction is that the backend owns behaviour and the desktop only presents
it. In practice, several things a second front end needs still live in `desktop`:

- **Usage accounting.** Token usage is recorded by `ConversationWidget`. A
  conversation without a widget is not accounted for.
- **The approval-rejection rule.** "A new user message rejects the pending approval"
  is implemented in `ConversationWidget.submit`, not in the conversation model.
- **Conversation models.** These are created by the conversation tab when a context
  opens. When a model is supplied up front instead (as delegation does), the tab
  adopts it, but loading its history is left to the widget.
- **Global settings.** `UserSettings` and `UserManager` are desktop classes.
- **Settings and claim files.** The atomic, owner-only, file-locked JSON store that
  protects them (`json_store`) sits in `desktop`. It is plain Python and has no Qt
  dependency.

A remote front end built on top of these would have to re-implement them, or reach
into the desktop for them. Either way it would end up with a second, divergent copy
of behaviour that should exist once.

### Threading

- Humbug runs asyncio on the Qt thread through qasync. Every model is touched from
  that one thread.
- None of the models is thread-safe, and none needs to be.
- Their consistency depends on single-threaded, cooperative execution: state can
  change only across an `await`.

### Instances

- Each Humbug process has exactly one mindspace open. An OS lock guarantees that only
  one process can open a given mindspace (ADR-0014).
- Global settings in `~/.humbug/user-settings.json` are shared by every instance and
  applied live in all of them.

### Standard-library limits

Humbug adds no dependencies (ADR-0004). This decision may use only `asyncio`, `json`,
`hmac`, `hashlib`, `secrets` and `ssl`. The standard library:

- has no public-key signature or key-agreement primitives;
- cannot generate a TLS certificate;
- supports pre-shared-key TLS only from Python 3.13, while Humbug supports 3.10 and
  later.

## Decision

### The backend controls; the desktop renders and triggers

All remote-access behaviour lives in a new Qt-free module, `remote_access`. This
covers state, rules, persistence, validation, lifecycle and models.

- It depends only on frontend-agnostic modules: `ai`, `ai_transcript_conversation`,
  `conversation_context`, `conversation_dag`, `context`, `mindspace` and the relocated
  JSON store (below).
- It is registered in `dependency-rules.yaml`.

The desktop does two things only:

- **It renders state the backend exposes.** It subscribes to backend callbacks, as it
  already does for `ContextRegistry` and `AIConversation`.
- **It triggers backend operations** when the user acts.

The desktop holds no remote-access state of its own, makes no decisions, and writes
no remote-access files. Any other front end, such as a CLI or Humbug Mobile's host
side, gets identical behaviour by driving the same backend objects.

| Responsibility | Backend (`remote_access` and existing models) | Desktop |
|---|---|---|
| Settings: switch, bind address, port | Owns, validates and persists them | Shows the settings page and passes the user's edits to the backend |
| Server lifecycle | Decides whether to serve when told a mindspace has opened or closed, binds, stops, reports errors | Tells the backend that a mindspace opened or closed |
| Sessions | Authenticates, limits, expires and closes them, and keeps the connected-client count | Shows the indicator, and triggers **Disconnect all** |
| Pairing | Generates codes, runs the exchange, stores pairings, revokes them | Shows the code and the paired-devices list, and triggers **Pair** and **Revoke** |
| Sending a message | Validates it, opens the conversation if needed, applies the approval rule, submits, audits | Nothing (Mac 1). On Mac 2, triggers the send from the input box |
| Events and resume | Derives events, assigns sequence numbers, buffers, replays or resyncs | Nothing |
| Remote client (Mac 2) | Connects, re-authenticates, reconnects, resumes, and holds proxy models of the remote mindspace and conversations | Renders the proxy models and triggers their operations |
| Audit | Writes every log entry | Nothing |

The backend exposes two kinds of object:

- **A server-side service**, one per running Humbug. It offers operations (attach or
  detach a mindspace, update settings, create a pairing code, list and revoke
  pairings, disconnect all) and callbacks (state changed, client count changed,
  pairings changed).
- **A client-side connection**, one per remote host. It offers operations (connect,
  pair, disconnect) and proxy models (below), with callbacks for connection-state
  changes.

### Prerequisite moves into the backend

Remote access must not build on behaviour that currently lives in the desktop. As
part of this work, the following move down a layer without changing what Classic
users see:

- **The JSON store.** `json_store` moves from `desktop` to a Qt-free module, so
  backend modules can write atomic, owner-only, locked files. `desktop` continues to
  use it from there.
- **Usage accounting.** This moves from `ConversationWidget` into the conversation
  model layer. A turn is then accounted for whichever front end started it, or
  whether any front end is attached at all.
- **The approval-rejection rule.** This moves from `ConversationWidget.submit` into
  the model layer, as part of submitting a message with a stated origin. Each origin
  then gets its rule in one place:
  - a local message rejects a pending approval, as today;
  - a remote message does not.
- **History loading.** Loading a conversation's history moves into the model layer.
  A model created by the backend and adopted by a tab therefore already holds its
  history, and the widget only renders it.

  Today an adopting widget skips loading. Whether a model supplied up front, as
  delegation's resume path does, ever runs without its history must be checked with a
  test before this move. If it does, the move fixes that too.

The server performs every operation through the same models the desktop uses. There is
no second implementation of conversation logic:

| Concern | Model used |
|---|---|
| Conversation list | `ConversationDag` over the mindspace's conversations directory |
| Open conversations | `ContextRegistry` |
| Sending and observing | `AITranscriptConversation` |
| File paths | `Mindspace.resolve_tool_path` |

The backend persists its own files under `~/.humbug/remote-access/` through the
relocated JSON store. All are owner-only:

- settings;
- paired devices (Mac 1);
- saved hosts (Mac 2).

`UserSettings` is not involved.

### Remote requests run on the UI event loop

- **Event loop.** The server is an `asyncio` stream server started on the running
  qasync loop. Request handlers are coroutines on the Qt thread, exactly like the
  desktop's own handlers. There are no worker threads and no cross-thread
  marshalling.
- **No in-memory locks.** No lock is needed around in-memory models; single-threaded
  execution already serialises access.
- **Ordering per connection.** Each connection processes one request at a time, in
  arrival order.
- **Re-validation after awaits.** A handler that awaits must look up any context,
  model or path again after the await before using it. A conversation can be closed
  or moved by the user in the meantime.
- **Event callbacks never await the network.** A remote client's event callback only
  appends to a bounded per-session queue, and a separate task writes the queue to the
  socket. A slow or stalled client therefore cannot stall the AI loop on Mac 1. If
  the queue overflows, the session is closed, and the client recovers by resuming
  (see "Events and resume").
- **Cross-process files.** The paired-device and saved-host files are shared between
  instances, so they go through the existing file lock.

### Sending a message goes to the conversation on Mac 1

`conversation.send` names a conversation by its mindspace-relative path.

1. **Find or create the model.** If the conversation is open on Mac 1, the server uses
   the model already registered for it. Otherwise the backend creates the
   conversation model itself and loads its history from the transcript.
2. **Ask a front end to show it.** The backend registers the model by opening a
   context, supplying it as the initial model. This is the mechanism delegation
   already uses. The desktop reacts as it does to any opened context: it shows a tab
   that renders the model it is given. It does not create the model.
3. **Submit.** The server submits the message through the model layer with a remote
   origin. `requester` is set to "Remote: <device name>", so the transcript and
   Mac 1's UI show where the message came from.
4. **Busy conversations.** A message sent while the conversation is busy is queued by
   `AIConversation`, as it is for any other caller.

The model layer applies the approval rule for the remote origin: a remote message
never rejects or answers a pending approval. Approvals stay on Mac 1 (see "Approvals
stay on Mac 1").

The conversation still appears on Mac 1, because Mac 1's user must be able to see the
work and answer its approvals. That is the backend directing the front end, not the
front end owning the conversation.

The engine is unchanged:

- Tool use, delegation and child conversations behave exactly as they do for a
  message typed on Mac 1.
- The approvals they raise appear on Mac 1.

### Approvals stay on Mac 1

In this decision, a remote client cannot:

- approve or reject tool calls;
- interrupt waits;
- cancel turns;
- edit or delete messages;
- change settings.

It is told that an approval is pending: the tool name, the reason text and whether the
action is destructive. It shows the approval as "Waiting for approval on <Mac 1 name>".

The approval's detail block (a diff, a request body) is not sent. A pending approval is
cleared on Mac 2 when the tool-call message completes, which is the existing event
fired on approval or rejection.

Remote approval, with signed per-action statements, is a separate future decision.

### Off by default, and one deterministic server per mindspace

**The switch is per mindspace.** Remote access is switched on for one mindspace. The
backend stores the switch, bind address and port in its own settings file in the
user's configuration directory, keyed by the mindspace's real path.

- They are not stored inside the mindspace. Opening a mindspace that someone else
  prepared can therefore never start a listener.
- The default is off. With the default, nothing listens.

**Which instance serves is never ambiguous.**

- **The backend decides.** The desktop's only part is to tell the backend service when
  this instance's mindspace has opened or closed. The service then reads its own
  settings and decides whether to serve.
- **No race.** Only one process can open a mindspace, so the serving instance is the
  one that has that mindspace open and has remote access switched on for it. There is
  no race between instances and no takeover.

**Port clashes are reported, not worked around.** Two mindspaces in two instances can
both be switched on, on different ports. If a second instance tries a port that is
already bound, by Humbug or anything else:

- its bind fails;
- it reports "port in use";
- it serves nothing;
- it never falls back to another port.

The server never sets `SO_REUSEPORT`. On Windows it binds with exclusive address use,
so no other process can share or take over the port.

**State while serving.** The service publishes its state:

- off;
- listening on an address;
- failed, with a reason such as "port in use";
- the number of connected clients.

The desktop renders this state as a visible indicator. Its **Disconnect all** action
calls the service, which closes every session immediately: transports are aborted,
not drained.

Switching remote access off, closing the mindspace or quitting Humbug tells the
service to stop. The service then ends every session.

**Revocations across instances.** Paired devices are shared by every instance, so the
service re-checks the paired-devices file on each request and on a short timer.

- A revocation made in this instance ends its sessions at once.
- A revocation made in another instance ends this instance's sessions within that
  interval.

### Bind addresses

- The default bind address is `127.0.0.1`.
- Accepted addresses are loopback addresses and addresses in HumbugScale's range,
  `100.64.0.0/10`.
- Wildcard addresses (`0.0.0.0`, `::`) are refused, and so is any other address.
- Validation happens when the setting is saved and again immediately before binding.

During development across two Macs, the server binds to Mac 1's HumbugScale address.
Later, HumbugScale's connector will forward its tunnel to `127.0.0.1`, so a machine
whose firewall blocks all incoming connections works without any change to Humbug.

### Security model

| Property | Provided by |
|---|---|
| Confidentiality and integrity in transit | The HumbugScale WireGuard tunnel, or the loopback interface |
| Authentication of both ends | A per-pairing key shared only by the two devices |
| Access to data | Request-scoped operations bounded to one mindspace |

Network reachability grants nothing. A client that can reach the port but holds no
valid pairing key receives only a refusal.

**What happens if the server were bound elsewhere.** Humbug does not encrypt the
stream itself; that is the tunnel's job. Bound to a LAN or public address:

- transcripts and file contents would cross the network in clear text;
- an attacker in a position to intercept could read them;
- such an attacker could also inject requests into an already-authenticated
  connection, because frames are not individually authenticated.

The pairing key itself is never transmitted, so it would not be exposed.

**Why the defaults prevent this.** The default loopback address and the refusal of
every address outside loopback and the HumbugScale range keep the server off any
network where these attacks are possible.

**Residual risks.**

- **Other local users on Mac 1.** Any local process, belonging to any user of Mac 1,
  can connect to the loopback port. It still needs a pairing key, and pairing keys are
  stored in owner-only files.
- **Ordinary carrier-grade NAT.** An interface that is not HumbugScale but uses an
  address in `100.64.0.0/10` would pass the bind check. This is accepted and
  documented.

### Pairing

1. **Pair a device on Mac 1.** Mac 1 shows a one-time code with 128 random bits,
   encoded as 26 base32 characters in groups. Mac 1 also shows its host and port.
   - The code is valid for 5 minutes.
   - It is consumed on first successful use, or after three failed proofs.
   - Only one code is live at a time.
2. **Enter it on Mac 2.** The user types the host, port and code into Mac 2.
3. **Prove knowledge of the code without sending it.** The proofs use HMAC-SHA-256,
   keyed by the code, over:
   - a fixed label;
   - fresh 32-byte nonces from both sides;
   - the client's device name.

   The client proves first. Mac 1 replies with a new pairing identifier and its own
   proof, and the client verifies that proof before storing anything.
4. **Derive the key on both sides.** Mac 1 issues the pairing identifier and its nonce.
   Both sides derive the 32-byte pairing key as an HMAC of those values under the code.
   The key never crosses the wire.
5. **Store it.** Each side stores the pairing in its configuration directory, with
   owner-only permissions:
   - Mac 1 stores the device name, pairing identifier, key, creation time and
     last-used time.
   - Mac 2 stores the host, port, server name, pairing identifier and key.
6. **List and revoke.** Mac 1 lists its pairings and can revoke any of them.
   Revocation deletes the record and immediately ends that pairing's open sessions.

All comparisons use `hmac.compare_digest`, and every MAC uses a distinct label so that
no proof can be replayed as another kind of proof.

**Rate limiting.** Failed pairing proofs are limited across all clients to a small
number per minute. Once the limit is reached, the live code is invalidated, and the
user must generate a new one.

### Sessions

A session is an authenticated connection.

1. **Hello.** The client sends its pairing identifier and a nonce.
2. **Challenge.** The server replies with a challenge nonce.
3. **Response.** The client answers with an HMAC under the pairing key.
4. **Session issued.** The server verifies the response and returns:
   - a random session identifier;
   - an expiry of at most one hour;
   - its own MAC, so the client authenticates Mac 1 too.

A session identifier belongs to its connection and is never accepted on another one.
When it expires, the server closes the connection and the client authenticates again.

**Refusals.**

- An unknown or revoked pairing, or a wrong MAC, receives the same generic refusal, so
  a client cannot tell which one failed.
- A revoked pairing is refused on its next attempt.

**Limits.**

- A handshake must complete within a short timeout.
- Authentication failures are rate-limited per remote address.
- The number of concurrent sessions is capped.
- Each session has a request-rate limit.

### Protocol: `humbug-remote/1`

**Framing.** Newline-delimited JSON over TCP, using asyncio streams.

- The first frame from each side carries the version string `humbug-remote/1`.
- A peer with another version is refused.
- A frame larger than 1 MiB closes the connection.

**Message kinds.**

| Kind | Fields | Purpose |
|---|---|---|
| Request | `id`, `op`, parameters | A call from the client |
| Response | the matching `id`, then a `result` or an `error` with a code and message | The server's reply to one request |
| Event | an increasing `seq` | An unsolicited update from the server |

**Operations.**

| Phase | Operation | Effect |
|---|---|---|
| R0 | `mindspace.info` | The served mindspace's name and the server's machine name |
| R0 | `conversations.list` | Mindspace-relative paths, titles, parent links and whether each is running |
| R0 | `conversation.read` | One page of messages, by offset and count (count capped), read from the open model if the conversation is open, otherwise from its transcript file |
| R0 | `files.list` | One directory's entries (count capped), excluding `.humbug` and ignored directories |
| R0 | `files.read` | One UTF-8 text file up to 256 KiB, resolved through `resolve_tool_path`; anything binary or larger is refused |
| R1 | `conversation.send` | Sends a message as described above (text size capped) |
| R1 | `events.subscribe` / `events.unsubscribe` | For named conversations |

**Data minimisation.**

- There is no operation that returns more than one conversation's messages or one
  file's contents.
- There is no search, no archive and no export.
- Nothing inside `.humbug` is reachable.

### Events and resume

Events are derived from the existing `AIConversationEvent` callbacks. No new engine
event is added. The event types are:

- message added;
- message updated;
- message completed;
- streaming progress (the in-progress message's content, coalesced to a few updates
  per second per message);
- turn completed;
- turn failed;
- approval pending;
- approval cleared.

**Ordering.**

- Each server run has a random `epoch`.
- Every event receives the next `seq` in a single counter for that run.
- The server keeps a bounded buffer of recent events.

**Resuming after a reconnect.** The client subscribes again with the epoch and the last
`seq` it processed.

- **Replay.** If the epoch matches and nothing after that `seq` has left the buffer,
  the server replays the missed events in order.
- **Resync.** Otherwise the server sends a resync snapshot for each subscribed
  conversation: its state and its latest page of messages. Live events follow.
- **Server restarts.** A server restart changes the epoch, so the client always
  resyncs rather than trusting `seq` numbers from an earlier run.

Either way, the client misses nothing it has not been told to fetch again.

### Audit

Mac 1 records remote activity in the mindspace's interaction log (ADR-0017), with the
device name and pairing identifier in the record's context:

- pairing;
- revocation;
- session start and end;
- refusals;
- every remote message sent (attributed to its conversation, like any other
  conversation activity);
- every file read (path only).

The contents of reads are not logged.

### The remote client is backend models; Mac 2's views only render them

On Mac 2, the client side of `remote_access` owns all of the following:

- the connection: authentication, session expiry and re-authentication, reconnecting,
  and resuming from the last `seq` or applying a resync;
- the saved-hosts file;
- a set of proxy models:
  - **The remote mindspace.** Its name, the server's machine name, the connection
    state, the conversation list and directory listings, each fetched on request.
  - **Each opened remote conversation.** Its pages of messages, the in-progress
    message, whether a turn is running, and any pending approval. Each is kept
    current by events.

Proxy models expose operations, for example "load the previous page" or "send this
text", and callbacks when their state changes. They hold only what the user has opened
or fetched, and only in memory. Nothing is written to Mac 2's disk, apart from the
saved host and pairing key.

Remote views are opened through Mac 2's `ContextRegistry`, supplying the proxy model
as the initial model. This is the same way the backend gives any other model to a
front end. Remote contexts are ephemeral and are not saved in Mac 2's session.

### Desktop interface

Each element renders backend state and triggers backend operations. None of them holds
remote-access state.

**On Mac 1.**

- **Settings → Remote access:** shows the service's settings for this mindspace
  (switch, bind address, port). Edits go to the service, which validates and saves
  them, and rejected values come back with the service's reason. A **Pair a device**
  button asks the service for a one-time code and displays it.
- **Paired devices:** renders the service's list. **Revoke** calls the service. The
  page states that pairings apply across mindspaces, because they are per device.
- **Indicator:** renders the service's state and client count. **Disconnect all**
  calls the service.

**On Mac 2.**

- **Connect to remote Humbug…:** a dialog that collects the host, port and, if needed,
  the pairing code, and passes them to the client backend. Pre-filled values come from
  the backend's saved hosts.
- **Remote panel:** a sidebar panel, registered through the existing panel API while
  connected. It renders the remote mindspace proxy's conversations and files.
- **Remote tabs:** opening an item asks the backend to open the matching proxy
  context. The desktop shows it in a tab labelled **Remote: <Mac 1 name>**.
- **Remote conversation view:** a minimal tab type that renders a remote-conversation
  proxy with Humbug's existing message and Markdown rendering.
  - Its input triggers the proxy's send.
  - A pending approval renders as "Waiting for approval on <Mac 1 name>", with no
    controls.
  - It does not reuse `ConversationWidget`. That widget is still bound to a local
    `AIConversation` and handles that conversation's persistence and approval
    controls, which do not exist on Mac 2.

  Making `ConversationWidget` a pure renderer of any conversation model, local or
  remote, would let both share one view. That is a larger change, and it is left as
  a follow-up the prerequisite moves make easier.

### Delivery

| Step | Delivers |
|---|---|
| 1 | This ADR |
| 2 | The prerequisite moves into the backend, with Classic behaviour unchanged. Then the Qt-free core: settings, pairing, protocol, server service, client connection and proxy models, with tests, including the security tests. The core is complete and testable without any desktop code. |
| 3 | R0 in the desktop: views that render and trigger the R0 backend |
| 4 | R1: send, live events and resume in the backend, then their rendering |

Each step ends with the code checker clean, the tests passing, and a stop for review.

**Out of scope:**

- agents and tasks beyond what conversations already do;
- remote approvals;
- Humbug Mobile;
- embedding the HumbugScale connector.

## Alternatives considered

- **Copy or sync the mindspace to Mac 2.** This would conflict with ADR-0003. It would
  also give two engines a claim to one conversation, and two writers to one audit log,
  which ADR-0014 exists to prevent.

- **WebSocket transport**, as HumbugScale's proposal suggests. WebSocket needs either a
  dependency or a hand-written implementation of its handshake and framing. Its
  benefits are browser and proxy compatibility, and neither applies inside a private
  tunnel. Newline-delimited JSON over TCP carries the same messages. A WebSocket
  framing can be added later around the same protocol if a client needs it.

- **TLS on the connection.** The standard library cannot generate a certificate, so
  each machine would need one from an external tool, and pre-shared-key TLS needs
  Python 3.13. Inside the tunnel or on loopback, TLS would add no confidentiality the
  transport lacks.

- **Per-frame MACs without encryption.** These would protect integrity on an untrusted
  network but not confidentiality. Binding to such a network would still be unsafe,
  and the protection would invite that mistake. The bind restriction is the control
  instead.

- **Public-key pairing**, with signatures and stored peer keys, as in HumbugScale's
  proposal. The standard library has no signature or key-agreement primitives, so
  this would need a dependency (ADR-0004). Symmetric pairing keys give mutual
  authentication between two devices. They do not give non-repudiation, which only
  remote approval needs, so public keys are left to that future decision.

- **A PAKE for pairing.** A PAKE protects a low-entropy code against offline guessing.
  The code here has 128 bits of entropy, so an HMAC proof is already resistant, and
  the standard library has no PAKE.

- **A single global switch, with whichever instance starts first serving.** With two
  windows open, the user could not tell which mindspace Mac 2 would reach. If that
  instance quit, another could start serving a different mindspace without anyone
  choosing it. A per-mindspace switch makes the served mindspace the user's explicit
  choice.

- **Keeping remote-access logic in the desktop**, with only the wire protocol in the
  backend. This would be quicker, because the settings dialog, `UserSettings`,
  `json_store` and the conversation widget are already there. But every other front
  end would need its own copy of pairing, session, lifecycle and send rules. That is
  the divergence the frontend-agnostic architecture (ADR-0008) exists to prevent.

- **Building on behaviour that sits in the widget.** This includes having the desktop
  supply file-storage callables, letting a tab create the conversation model, and
  leaving usage accounting and the approval rule in `ConversationWidget`. Each would
  make remote access depend on a front end being present and behaving in a particular
  way. Moving them into the backend costs some changes to existing code, but each one
  then exists once.

- **Storing remote-access settings in `UserSettings`.** `UserSettings` is a desktop
  class. Remote-access settings are owned and validated by the backend that acts on
  them, so they live in the backend's own file.

- **Storing the switch in the mindspace's own settings.** A mindspace prepared by
  someone else could then arrive with remote access switched on. Keeping the switch in
  the user's own settings file means only the user can enable it.

- **A companion process, or a server thread.**
  - A companion process cannot reach the in-memory models without an IPC layer, which
    ADR-0014 rejected. It would also need a second claim on the mindspace.
  - A thread would touch models that are not thread-safe, so every call would have to
    be marshalled back to the Qt thread anyway.
  - Running on the existing event loop gives the same isolation with no new machinery.

- **Reusing `ConversationWidget` on Mac 2.** The widget is bound to a local
  `AIConversation` and its persistence, accounting and approval handling. Driving it
  from remote events would mean reimplementing the model behind it, which is the
  second implementation of conversation logic this decision avoids.

- **Remote approvals now.** Approving actions from another device needs statements
  bound to the exact action, single-use and short-lived, and backed by a hardware key.
  This decision keeps every approval on Mac 1 and leaves that design to its own ADR.

## Consequences

### Positive

- The mindspace never leaves Mac 1. Mac 2 holds only the pages and files it asked for,
  in memory.
- One conversation engine serves both front ends. A message sent from Mac 2 behaves
  exactly like one typed on Mac 1, including tools, delegation and approvals.
- All remote-access behaviour is in the backend, and the desktop only renders and
  triggers. A CLI, a headless host or Humbug Mobile's host side gets the same
  behaviour by driving the same objects.
- The core can be tested completely without Qt.
- The prerequisite moves (the JSON store, usage accounting, the approval rule and
  history loading) also benefit Classic. A conversation's usage and rules no longer
  depend on whether a widget is attached.
- There are no new dependencies, and nothing Qt-specific in the protocol or server.
- Running on the existing event loop needs no locks around models and no thread
  hand-off.
- Nothing listens unless the user switches it on for a mindspace. When on, the server
  listens only on loopback or the private overlay, and the user can always see how
  many clients are connected.
- The served mindspace is always the user's explicit choice, and a port clash is
  reported rather than worked around.
- Every pairing, session and remote action is in Mac 1's audit log.

### Negative

- Transport security depends entirely on the tunnel or loopback. The bind restriction
  is the only protection against misuse, and an ordinary carrier-grade NAT address
  would pass it.
- Pairing keys are stored in clear in owner-only files, like API keys today. Anyone
  who can read the user's files on either machine can act as that pairing.
- Symmetric keys give no non-repudiation. Remote approval will need its own key
  scheme.
- A remote user cannot unblock a conversation waiting for approval. The work pauses
  until someone answers on Mac 1.
- The prerequisite moves change load-bearing code before any remote feature exists:
  the conversation model layer, the conversation widget and the JSON store. They need
  their own tests showing that Classic behaviour is unchanged.
- Sending to a conversation that is not open makes its tab appear on Mac 1, so that
  Mac 1's user can see the work and answer its approvals. Mac 1's layout can
  therefore change while nobody is at it.
- A remote message does not reject a pending approval, while a local message does.
  The rule now lives in one place, but the two origins deliberately behave
  differently.
- The remote conversation view is a new, minimal renderer. Until `ConversationWidget`
  becomes a pure renderer of any conversation model, Mac 2 does not have every
  feature of the local view.
- A slow client is disconnected rather than allowed to apply back-pressure, so it
  depends on resume or resync to catch up.
- Each mindspace served at the same time needs its own port, chosen by the user.
