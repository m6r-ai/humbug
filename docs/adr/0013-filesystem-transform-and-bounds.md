# ADR-0013: Filesystem transforms, binary content, context-window bounds, and transform symmetry

Date: 2026-10-01  
Status: Accepted

## Context

The filesystem tool exposes file content to the AI and lets the AI modify files on
disk. It already supports a Menai transform on write: `transform_file` reads a file,
runs a side-effect-free Menai program over its content, and writes the result back to
the same path.

Several limitations have become apparent.

First, there is no way to run a transform on a read. The AI can read a file, or read
it with line numbers, but it cannot compute over the content before it reaches its
context window. This matters because reads are bounded: a large file cannot be read
in full, and the AI has no way to reduce it to the part it actually needs.

Second, the tool only transforms text. Menai has a first-class bytes type with
construction, typed multi-byte integer reads and writes, IEEE-754 float access,
LEB128 encoding, cryptographic hashing, checksums, and higher-order operations over
bytes. Binary file content has a natural representation in Menai, and operations such
as computing a file's SHA-256 digest or parsing a binary header are not expressible
today.

Third, `transform_file` can only write back to the source path. There is no way to
read one file, transform it, and write the result to a different path, leaving the
source untouched.

Fourth, the write transform's return contract is larger than it needs to be. A program
may currently return a string or a list of strings; a list of strings is joined with
newlines. This is a one-off convenience with no coherent counterpart: Menai has no
"list of bytes" type, and a program with a list of lines can already call
`list->string` itself. The join also bakes a newline convention into the tool that is
invisible in the program.

The same applies on the input side. The transform binds `input-lines`, the content split
on `\n` and bound as a list of strings. It carries no information that `input-text` does
not, and no line range; a program that wants lines can split the text itself. Both the
filesystem and the editor transform bind it identically, so any change to it affects
both operations.

A common thread runs through these: the existing 64KB output bound is applied as a
property of the tool, but its actual purpose is to protect the AI's context window.
The bound exists to stop a single tool call from flooding the context with content
the AI cannot use. That purpose only applies when a result enters the context window.

## Decision

A Menai program may be run over file content on both read and write. The result is
either returned to the AI or written to a file. The 64KB bound is applied to the
channel, not the operation: it applies to any result that enters the AI's context
window, and to nothing else.

**Transforms on read.** A new operation, `analyse_file`, runs a Menai program over a
file's content and returns the program's result to the AI. The program may return any
Menai value, which is serialised to text for the AI. An analysis is a pure operation
that writes nothing, so it requires no user approval, consistent with ADR-0007.

**Transforms on write to a different path.** `transform_file` gains an optional
`output_path`. When present, the transformed content is written to that path and the
source is left untouched. When absent, behaviour is unchanged: the source is
overwritten. Both forms modify persistent state and require user approval.

**Explicit content form.** Both operations take a `form` parameter, `"text"` (the
default) or `"binary"`. The form selects both the input binding and the required
return type; it is declared, never inferred. The tool does not sniff file content to
decide the form.

**Input bindings.** The form determines the single set of bindings placed in the
transform's `inputs` dict:

- `form: "text"` binds `input-text` (the decoded string), using the `encoding` parameter
  to decode.
- `form: "binary"` binds `input-bytes` (the raw bytes).

Only the bindings for the selected form are produced. The tool does not bind
representations the program cannot use, and there are no absent or `#none` bindings to
test for.

**`input-lines` is removed from the filesystem and editor transform operations.** The
`input-lines` binding is the content split on `\n` and bound as a list of strings. It
carries no information that `input-text` does not, and no line range: a program that
wants lines writes `(string->list input-text "\n")`, which makes the delimiter explicit
and is a single operation. It is removed from the filesystem transform for the same
reason the list-of-strings return is removed: it is a convenience that hides a newline
convention and contradicts the single-binding rule that `form` establishes.

The editor transform binds `input-lines` in exactly the same way as the filesystem
transform, so it is removed from both in lockstep. The two operations are kept
symmetric: if one offers a binding, both do, and if one does not, neither does. The
editor tool does not otherwise change.

**Return contract.** On a write, the program must return exactly one Menai value of the
type the form requires: a `string` for `form: "text"`, or `bytes` for
`form: "binary"`. A list of strings is no longer accepted; a program with a list of
lines calls `list->string` itself. A list of bytes is not accepted either; Menai has no
such type, and a program with a list of integers calls `list->bytes` itself. On an
analysis, any Menai value is accepted and serialised for the AI.

**Input and output share a single form.** The input form and the return type are both
determined by the same `form` parameter, so a program reads and writes the same kind of
content. A transform that converts between the two is not expressible as a single
operation; it would be two operations, or a program that returns a value of the required
form.

**Bounds.** The 64KB output bound applies to any result that enters the AI's context
window: a plain read, and a read with a transform. For a read transform, the bound is
applied to the serialised form of the result — the text the AI actually receives. For
a binary result this means the bound applies after serialisation, so the underlying
byte count is smaller than the bound by the serialisation factor.

The bound does not apply to a write transform. The content passes from file to file
and never enters the context window. A write transform is therefore unbounded in its
output, for both text and binary.

The 10MB input cap on any single file read is unchanged and applies to all operations,
including write transforms.

## Alternatives considered

- **Apply the 64KB bound to all transforms, including writes.** This treats the bound
  as a property of the operation rather than the channel. It would reject legitimate
  write transforms whose output exceeds 64KB for no benefit, since that output never
  reaches the AI and cannot flood its context. The bound's purpose is not served by
  applying it where there is no context window to protect.

- **Represent binary content as a base64 or hex string in the `inputs` dict.** This
  would avoid depending on Menai's bytes type. It was rejected because Menai already
  has a native bytes type with a documented naming convention consistent with its
  other collection types. Introducing a string encoding would add a convention to
  learn, document, and maintain, and would discard the typed accessors
  (`bytes-read-u32-be` and similar) that make binary transforms useful.

- **Detect binary files heuristically and route them differently.** A content sniffing
  heuristic would decide what the AI meant. This was rejected as opaque: the tool
  would behave differently based on invisible classification, which conflicts with the
  transparency principle. Whether a transform operates on text or bytes is declared by
  the caller through `form`, not guessed from the file.

- **Bind all representations and let the program choose.** The `inputs` dict could
  always carry `input-text`, `input-lines`, and `input-bytes`, with the program
  selecting by which binding it reads. This was rejected: it forces every transform to
  pay for representations it does not use, it requires a rule for bindings that cannot
  be produced (a binary file cannot always be decoded to text), and it makes the
  text/binary choice implicit rather than visible in the tool call. An explicit `form`
  parameter produces one binding set and no ambiguity.

- **Keep the list-of-strings return on write.** A program with a list of lines could
  have the tool join them. This was rejected as a one-off convenience with no coherent
  counterpart: there is no list-of-bytes form, the join imposes an invisible newline
  convention, and Menai already provides `list->string`. Removing it leaves the write
  contract as a single Menai value per form.

- **Keep `input-lines` on the filesystem transform.** The binding could remain as a
  convenience so a program need not split the text itself. This was rejected for the
  same reasons as the list-of-strings return: it is a convenience that hides a newline
  convention, it duplicates information already in `input-text`, and it contradicts the
  single-binding rule. Because the editor transform binds `input-lines` identically, the
  two operations are kept symmetric and the binding is removed from both. Removing it
  from the filesystem tool alone would leave the two operations inconsistent.

- **Allow `transform_file` to return any Menai value on write.** On a write the result
  must be written to disk, so it must be text or bytes. Accepting an arbitrary value
  and serialising it would produce files whose content is a serialisation of a data
  structure, which is not what a file transform means. Read transforms accept any
  value because their result is a message to the AI; write transforms accept content
  because their result is a file.

- **Add the read transform as a parameter on `read_file`.** An optional `program` on
  `read_file` would avoid a new operation. It was rejected because it makes
  `read_file`'s contract conditional on whether `program` is supplied, and because the
  two modes differ in kind: a plain read returns file content, while an analysis
  returns the result of a computation. Naming them separately — `read_file` and
  `analyse_file` — keeps each contract simple and makes the pair symmetric with
  `transform_file`: `analyse_*` returns to the AI, `transform_*` writes to a file.

## Consequences

### Positive

- A read with a transform is the escape hatch from the read bound. The AI can reduce a
  file larger than the bound to the region or summary it needs, rather than being
  unable to read it at all.

- Binary content is supported without a new representation convention, using Menai's
  existing bytes type and its typed accessors. Operations such as hashing, checksum
  computation, and header parsing become expressible.

- Write transforms are unbounded, so a transform may produce an output larger than its
  input, and binary round-trips do not pass through the context window at all.

- `output_path` allows a transform to derive a new file without modifying the source,
  which is both safer and more expressive than in-place overwrite alone.

- Because transforms are deterministic, the audit log's record of a transform — the
  program and the file it ran over — is a complete and verifiable account of the
  operation.

### Negative

- The bound is now a property of the channel rather than the operation, so tool
  authors must apply it by asking whether a result enters the context window. Applying
  it to a write, or failing to apply it to a read, would be a misclassification in
  either direction.

- A binary read transform is bounded on its serialised form, so it can return fewer
  bytes than a text transform can return characters. This is a consequence of the
  serialisation factor and must be stated in the tool documentation so the AI can
  reason about it.

- The write transform's existing list-of-strings return is removed. A program that
  currently returns a list of lines must be changed to return a string, joining the
  lines itself with `list->string`. This is a deliberate removal of behaviour, not an
  accidental regression.

- The `input-lines` binding is removed from both the filesystem and editor transform
  operations. A program that reads `input-lines` must be changed to split the text
  itself with `(string->list input-text "\n")`. The editor tool changes solely to keep
  the two operations symmetric; it gains no other change from this decision.

- A transform cannot read one form and write the other. Converting between text and
  binary content requires two operations, or a program that produces a value of the
  required form. This is a consequence of `form` selecting both the input binding and
  the return type.

- The `form` parameter must be supplied correctly by the caller. A binary file analysed
  as text will fail to decode, and a text file analysed as binary will produce bytes
  the program may not expect. The failure is explicit rather than silent, which is the
  intended trade.
