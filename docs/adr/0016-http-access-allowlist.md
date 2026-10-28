# ADR-0016: HTTP access allowlist for read-only requests

Date: 2026-10-27  
Status: Accepted

## Context

The HTTP tool originally required user approval for every request.  That is safe, but
it makes routine read-only work noisy: an AI checking documentation, polling a status
endpoint, or fetching a file from a known host interrupts the user every time.

The filesystem tool already solves the analogous problem.  Reads inside the mindspace
run without asking, reads outside it are governed by a user-configured allowlist and
denylist, and a denied path is refused outright rather than offered for approval.  The
HTTP tool had no equivalent, so the user had no way to say "these hosts are fine".

An HTTP allowlist is not a straight port of the filesystem one, for three reasons.

First, the approval policy in ADR-0007 turns on whether an operation changes persistent
state.  A GET or HEAD is read-only; a POST, PUT, PATCH, DELETE or download is not.  Only
the read-only methods may be auto-approved.

Second, the matching unit is different.  Filesystem patterns are globs over paths.  A
glob over a URL string is unsafe: `*.example.com` implemented with `fnmatch` would match
`evilexample.com`, and matching the raw URL would let a crafted path or query string
influence the result.  The host, scheme and port must be parsed and compared as separate
components.

Third, an HTTP client can be pointed at internal infrastructure.  Cloud instance
metadata services answer on link-local addresses, and private networks are reachable
from the user's machine but are not what they had in mind when allowlisting a public
host.  An allowlist that auto-approves without a denylist could be turned into a route
to those services, including via a redirect from an allowlisted host.

## Decision

The HTTP tool gains an allowlist and a denylist, configured per user alongside the
existing filesystem access settings, and exposed through a "Network Access" page in the
settings dialog.

**Only GET and HEAD are eligible for automatic approval.**  Every other operation always
prompts, whatever the allowlist says.

**The denylist is checked first and refuses outright.**  A denied URL raises an
authorisation error without prompting, so a protected address is never offered to the
user for approval.  It ships with defaults covering loopback, link-local, private
networks and the well-known cloud metadata endpoints.

**The master switch governs automatic approval only.**  When it is off every request
prompts, exactly as if no allowlist were configured.  It does not block network access;
disabling the tool is how network access is withdrawn.  This differs from the filesystem
switch, which blocks external reads while leaving mindspace-local reads working — the
same shape, since in both cases the switch scopes the feature rather than the tool.

**Entries may constrain scheme, host and port, and may use subdomain wildcards or CIDR
networks.**  Matching is performed on the parsed URL components, never the raw string.
A subdomain wildcard matches subdomains only: `*.example.com` matches `api.example.com`
but not `example.com`, and never `evilexample.com`.  A CIDR entry matches when the host
is an IP literal inside the network.  Malformed entries are ignored rather than treated
as fatal.

**Every redirect target is checked before it is followed.**  The client is asked not to
follow redirects automatically for GET and HEAD; the tool instead inspects each 3xx
response, re-runs the access check against the `Location` target, and only then
continues.  An allowlisted host that redirects to a denied address is refused, and one
that redirects to an unlisted address prompts.

## Alternatives considered

- **Auto-approve all methods for allowlisted hosts.**  A POST to an allowlisted host can
  still change persistent state, which ADR-0007 says must be gated.  Allowlisting a host
  for reads is not consent to write to it.

- **Glob matching over the URL string, for consistency with the filesystem tool.**  Simple,
  but it fails in both directions: an over-broad pattern silently approves hosts the user
  did not intend, and matching the raw string lets the path or query influence the result.

- **No denylist, relying on the allowlist alone.**  The allowlist is opt-in, so a user who
  never configures one is unaffected.  But once a public host is allowlisted, a redirect
  from it would reach anything, and the user would have no way to carve out internal
  addresses.

- **Let the client follow redirects and check only the initial URL.**  This is what the
  tool did before, and it is precisely the hole the denylist is meant to close.

- **Make the master switch block all external HTTP.**  Rejected because it duplicates the
  tool-enable setting.  Two controls that both disable the tool leave the user unsure
  which one to use.

## Consequences

### Positive

- Routine read-only requests to trusted hosts no longer interrupt the user, while every
  state-changing request still does.

- The denylist defaults protect internal infrastructure without any configuration.

- Checking redirects closes the obvious route from an allowlisted host to a protected
  address.

- Matching on parsed components means the semantics are predictable and testable, and a
  pattern cannot be defeated by URL formatting.

### Negative

- The pattern grammar is richer than the filesystem globs, so the two features are not
  symmetric and the settings UI must explain the syntax.

- CIDR matching only applies to IP literals.  A hostname that resolves to a private
  address is not caught by a CIDR entry, because the tool does not resolve names.  DNS
  rebinding therefore remains out of scope; the denylist reduces exposure rather than
  eliminating it.

- Redirect checking adds a round trip of control between the tool and the client for
  every hop, and the maximum number of redirects is enforced by the tool rather than by
  the client.

- Users who allowlist a host are trusting it.  The allowlist is an explicit decision, and
  the settings UI says so.
