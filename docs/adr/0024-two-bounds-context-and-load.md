# ADR-0024: Two bounds — context and load

Date: 2026-11-07  
Status: Accepted

## Context

The filesystem tool applies two limits that have been treated as one. The 64KB response
bound has a stated purpose, fixed by ADR-0013: it protects the context window, and it
applies to text entering the conversation and to text a tool derives, and to nothing
else. The 10MB file-size cap has no stated purpose. ADR-0013 recorded it as
"unchanged" while redefining the other bound, so it was inherited rather than argued.

It is applied as one check, in one form, at nine call sites: `read_file`,
`read_file_lines`, `write_file`, `append_to_file`, `copy_file`, `search_file`,
`search`, `analyse_file` and `transform_file`. That makes it look like one rule. It is
not one rule. At each site it stands in for something different, and at several of them
it stands in for nothing:

- `analyse_file` already bounds its result at 64KB. Its source cap refuses a file whose
  entire contribution to the conversation would be a summary. ADR-0013 called the read
  transform the escape hatch from the read bound; the cap puts a 10MB doorway on the
  hatch.
- `read_file_lines` reads the file whole and then slices to the requested lines. The cap
  therefore refuses a range read that would have cost only the range.
- `search` skips files above the cap and says nothing about it, so "no matches" means
  either "not found" or "not looked at". The AI cannot tell the difference, and neither
  can the user reading the transcript.
- `copy_file` copies with `shutil.copy2`, which streams, and records the destination
  pre-image through `FileJournal.retain_file`, which copies in chunks. Neither holds the
  content whole. The cap costs the user a legitimate operation and protects nothing.
  `move` is the same operation with less work still and has never been capped.
- `append_to_file` caps the *resulting* file size, which is a policy about how large a
  file may be, not about memory. The journal already digests the file in chunks.
- `write_file` and `transform_file` are the two sites where the content genuinely is
  held whole, and there the cap does real work.

The journal is not the constraint that motivated the cap and cannot become one by
removing it: `JournalBlobStore.store_file` and `digest_of_file` read and write in 1MB
chunks and are documented as costing a constant amount of memory for a file of any size.
The cap is the only thing that keeps that path from being exercised.

The cost of the cap, then, is borne by the user: content that could be summarised or
read in part is refused outright, and the refusal does not say which of the two concerns
— the conversation or the process — the AI has run into.

## Decision

Two bounds, each with one stated purpose, and content made reachable in proportion to
the bound that actually applies to it.

**The context bound.** 64KB, unchanged, as defined by ADR-0013: it applies to text
entering the conversation and to derived text, and to nothing else. It remains the only
bound whose purpose is the context window.

**The load bound.** A separate, named bound, 1GB by default, applied only where an
operation holds a file's content whole in memory. Its purpose is the process's memory,
not the conversation's, and its refusal says so. Because it is a statement about
transient cost rather than about the tool's contract, it is a constructor parameter
(`max_load_size_mb`) rather than a constant.

The value is chosen against what loading costs, not against what a user may want to
open. Holding content whole costs a multiple of its size: the file's bytes, the Menai
value wrapping them, and in a transform a second copy of the source, so a loaded file
peaks at roughly three times its size. 1GB is the largest value whose peak a desktop
machine can be expected to afford. It is not a judgement that content above some size
is unreasonable; a 10MB ceiling was exactly that judgement, and it refused files that
cost nothing to read.

Operation by operation:

- `read_file` holds the file whole, and for PDF and DOCX the extracted text as well. Load
  bound applies. Its refusal names the route that does work: a line range for text, an
  analysis for binary.
- `read_file_lines` reads forward to the requested line range, keeps only that range, and
  stops at its end. No size bound. The result is bounded by the context bound as before.
  A single line longer than the load bound is refused by name, because a range read cannot
  return it without holding it.
- `search_file` and `search` stream their files line by line. No size bound. A file that
  is skipped — undecodable with the requested encoding, unreadable, or containing a line
  longer than the load bound — is listed in the result with its path and the reason, so
  a search that found nothing states what it did not look at.
- `copy_file` streams and loses its bound entirely, as `move` has none.
- `write_file` holds the content it is given, so the load bound applies to that content.
- `append_to_file` holds only the content it is given, so the load bound applies to that
  content and not to the file it is appended to.
- `transform_file` holds the whole input, so the load bound applies. It gains no ranges:
  a transform's output is a whole file, and applying a result to a byte range is a patch
  rather than a transform.
- `analyse_file` holds the whole input, so the load bound applies by default, and gains
  optional `offset` and `length` byte-range parameters valid only with `form: "binary"`.
  A range is loaded and bound in place of the file, so a file of any size can be analysed
  in pieces. No byte range is offered for `form: "text"`: an offset into an encoded text
  need not fall on a character boundary, and a decode failure part-way through a range
  would muddle the two bounds. Whether text or bytes is being read stays a declared
  choice, as ADR-0013 requires.

**What ranges do not do.** They make a large file addressable; they do not make it
computable in one call. A program that must see every byte — a digest of the whole file,
say — still needs the file to fit the load bound, and the tool says so rather than
implying a route it does not have.

## Alternatives considered

- **Raise the number.** The arbitrariness is not that 10MB is the wrong value; it is
  that the bound is applied where no bound is needed. Raising it leaves every wrong
  refusal in place, just at a higher threshold.

- **One bound for everything, sized generously — 1GB, say.** A single number would have
  to serve both the context window and the process's memory. At 1GB it protects neither:
  far too large to protect a context window, and stated in terms of the conversation when
  its real subject is memory. One number doing two jobs can state only one of the reasons.

- **Keep the cap and expose it as a user setting.** Moves a decision to the user who has
  no way to judge it and no reason to be asked. The tool knows which of its operations
  hold content whole; the user does not.

- **Give `transform_file` byte ranges as well.** A write to a byte range is a patch, not
  a transform: it must leave the file's remainder unchanged, and the operation would no
  longer mean "the program's result is the file". A different operation, not a parameter
  on this one.

- **Bound reads by pages and tell the program which page it has.** Requires a paging
  convention in Menai and in every program that reads a file. The AI can already ask for
  the next range.

- **Report search skips by raising rather than listing them.** A search that matched
  elsewhere is still useful, and refusing it because one file could not be read serves
  nobody. A `skipped` list keeps the answer and states its coverage.

- **Leave `read_file` able to read any size and truncate at the context bound.** ADR-0013
  rejected partial results for good reason: content the AI believes is whole but is not
  is worse than content it must ask for again.

## Consequences

### Positive

- Every bound has one stated reason, and every refusal names that reason and the route
  around it.
- A text file of any size can have any range of its lines read, and a binary file of any
  size can be analysed in ranges.
- A search states what it did not look at instead of reporting silence.
- Copying a file is no longer refused for a size that costs no memory, and behaves
  consistently with moving one.
- The journal's chunked capture path is exercised rather than bypassed.
- A large binary file has a route to the AI at all: the analysis of a range, in place of
  no route.
- The load bound no longer doubles as a judgement about which files are reasonable. A
  300MB PDF, deck or binary is loaded whole, because loading it costs memory and nothing
  else.

### Negative

- Two bounds must be explained, in the tool description, in the user manual, and to
  anyone writing a tool of their own. One number was simpler to state and wrong.
- Loading 1GB whole costs roughly three times that at peak in a transform, which the
  default permits. The bound guards against exhausting the process; it does not promise
  that a load of the permitted size will be quick, and a file above it still needs a
  range rather than a whole-file operation.
- `analyse_file` gains two parameters that are valid with one `form` only, and must
  reject the other combination explicitly.
- A search can return a `skipped` list that the AI is free to ignore, and ignoring it
  reproduces the old silence. The list is a field of the result, not an error.
- `read_file_lines` no longer has the whole file to count, so a `start_line` past the end
  is discovered by reading to the end of the file rather than from a count. The cost of
  establishing it is unchanged; only its source is.
- Removing the copy bound means an approval prompt can name a copy of any size. The
  prompt names the source and the destination, and the approval is the size check.
- For line-oriented reads the load bound is applied in characters rather than bytes,
  because a line is a text unit and one character may occupy up to four bytes. A line
  of the permitted length can therefore consume more memory than the bound names. The
  alternative is to split lines by bytes, which misreads encodings whose terminator is
  not a single `\n` byte.
- The load bound must be applied by hand at each site that holds content whole, and
  failing to apply it — or applying it to a streaming operation — is a misclassification
  in either direction, as ADR-0013 warned of the context bound.
