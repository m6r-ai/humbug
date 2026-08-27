# ADR-0003: Local-first data ownership

Date: 2026-08-27  
Status: Accepted

## Context

Humbug handles sensitive material: source code, research, documents, and complete AI conversation transcripts.
Trust in both cloud-based and AI tooling is low, and users reasonably want to know where their data goes.

Humbug also explicitly targets regulated environments, which often prohibit cloud services outright.
Auditability principles require a full record of human and AI actions to live where the user controls it.

Considerations:

- Local models (Ollama, vLLM) make fully offline operation a realistic mode of use, not a degraded one.
- Vendor independence would be undermined if data lived server-side (lock-in would return through the back door).

## Decision

All data lives on the user's machine, inside the mindspace.

- Conversations, settings, the interaction log, usage data, and the user's own files are stored locally.
  Nothing is sent anywhere except to the AI providers the user has explicitly configured, or unless the user specifically approves it.
- Humbug works fully offline.
  Cloud AI backends are opt-in per provider, and network access happens only when the user selects a cloud model.
- There are no accounts and no telemetry.
  The user owns their data and their backups.
  Humbug's protections (internal versioning of its own state) also operate entirely locally.

## Alternatives considered

- Accounts with cloud sync as the default would require server infrastructure and an operations burden incompatible with the
  project's nature, would create a trust and compliance problem, would exclude air-gapped environments, and would make the
  vendor a custodian of data the user should own outright.
- A hybrid of local files and cloud-stored conversations would split the record of work across a trust boundary, leaving the audit trail incomplete and the user's most sensitive artefact (the transcripts) in someone else's custody.

## Consequences

### Positive

- Works in air-gapped and regulated environments using local models.
- The complete forensic record remains under user control; nothing to request from a third party.
- No server costs, no accounts, no privacy surface beyond the application itself.
- Data outlives the project: if Humbug disappeared, everything remains on disk in readable formats.

### Negative

- No cross-device sync today; users manage their own backup and transfer. Any future sync must be built as Humbug-internal infrastructure, not a hosted service.
- Multi-human collaboration on a single mindspace is out of scope for now.
- Conveniences that assume a cloud (shared catalogues, hosted search) are unavailable or must be built locally.
