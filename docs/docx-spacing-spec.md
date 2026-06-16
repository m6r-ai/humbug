# DOCX Spacing Specification

This document defines the spacing policy for the Markdown → DOCX converter.
It is the authoritative reference for how vertical space is produced between
block-level elements in the generated `.docx` output.

---

## Units

All spacing values are in **twips** (twentieths of a point).  Common values:

| Twips | Points |
|-------|--------|
| 200   | 10pt   |
| 240   | 12pt   |
| 280   | 14pt   |
| 360   | 18pt   |
| 480   | 24pt   |

The standard inter-block gap is **200 twips (10pt)**.

---

## How Word computes spacing between two blocks

For two adjacent paragraphs A and B, Word renders:

```
gap = max(A.spacing_after, B.spacing_before)
```

They do not add.  This means only one side needs to own a given gap.

Tables have no `spacing_after` equivalent in OOXML.  Gaps after tables and
blockquotes (which are implemented as single-cell borderless tables) are
produced by a **spacer paragraph** — a zero-height Normal paragraph with
`spacing_before=0` and `spacing_after=0`.  The spacer's line height provides
the visual gap.

---

## Spacing ownership rules

### `spacing_before` — block asserts its own leading gap

Only headings assert `spacing_before`, and the values must exceed the standard
200 twips so they take effect over the preceding block's `spacing_after=200`:

| Block type  | `spacing_before` |
|-------------|-----------------|
| Heading 1   | 480 twips (24pt) |
| Heading 2   | 360 twips (18pt) |
| Heading 3   | 280 twips (14pt) |
| Heading 4   | 240 twips (12pt) |
| Heading 5–6 | 200 twips (10pt) — same as Normal, no visual distinction |

All other block types have no `spacing_before`.

### `spacing_after` — block asserts its own trailing gap

All block types inherit `spacing_after=200` from the Normal style, with the
following exceptions:

| Block type | `spacing_after` |
|---|---|
| Code block — all lines except last | 0 (suppressed by CodeBlock style) |
| Code block — last line | 200 twips (explicit override on paragraph) |
| Tight list — all paragraphs except last of last item | 0 (explicit) |
| Tight list — last paragraph of last item | 200 twips (explicit override) |
| Loose list — all item paragraphs | None (inherits 200 from Normal) |
| Table / blockquote | n/a — spacer paragraph used instead |

No heading has an explicit `spacing_after` — all inherit 200 twips from Normal.

---

## Lists

### Tight lists

- Every paragraph in every item has `spacing_after=0` — no inter-item gap.
- The last paragraph of the last item has `spacing_after=200` explicitly set,
  so the block following the list sees a normal gap.
- If the last item ends with a **code block**, the last line of that code block
  gets `spacing_after=200` explicitly.
- If the last item ends with a **blockquote or table**, a spacer paragraph is
  emitted after the list ends (since there is no paragraph to patch).
- Blockquote/table spacers are **suppressed** inside tight list items — no gap
  between a blockquote and the next item in the same tight list.

### Loose lists

- Every item paragraph has `spacing_after=None`, inheriting 200 twips from the
  Normal style.  This produces the inter-item gap.
- If a loose item ends with a code block, the last line gets `spacing_after=200`
  explicitly (because the CodeBlock style sets `spacing_after=0`, so `None`
  would inherit zero from the style rather than from Normal).
- If a loose item ends with a blockquote or table, a spacer paragraph is emitted
  after that item's content (since there is no paragraph to patch).
- The last item of a loose list follows the same rules as any other loose item —
  the Normal `spacing_after=200` (or explicit override) provides the gap after
  the list.

### Nested lists

- The nesting depth does not change the policy.  Each sub-list follows the same
  tight/loose rules independently.
- When a tight outer item ends with a tight nested sub-list, the last paragraph
  of the nested list gets `spacing_after=200` only if it is also the last
  paragraph of the outer item (i.e. the nested list is the last child of the
  outer item).  Otherwise it gets `spacing_after=0`.

---

## Code blocks

- Every line is a separate paragraph with the `CodeBlock` style.
- The `CodeBlock` style sets `spacing_after=0` on all lines.
- The last line always gets an explicit `spacing_after=200` override, regardless
  of context (top-level, inside a loose list item, inside a blockquote).
- Exception: inside a **tight list item** that is not the last item, the last
  line gets `spacing_after=0` (no gap before the next item).

---

## Blockquotes and tables

- Blockquotes are rendered as single-cell borderless tables.
- A **spacer paragraph** (Normal style, `spacing_before=0`, `spacing_after=0`)
  is emitted after every blockquote or table.
- Exception: inside a **tight list item** that is not the last item, the spacer
  is suppressed (no gap before the next item).
- The spacer's line height provides the visual gap.  The `spacing_before` of
  the following block (e.g. a heading) adds to this, which may produce a
  slightly larger gap before headings after blockquotes/tables — this is
  acceptable.

---

## Summary table

| Transition | Gap source |
|---|---|
| Paragraph → paragraph | `spacing_after=200` on Normal style |
| Paragraph → heading | `spacing_before` on heading (480/360/280/240/200) |
| Code block → paragraph | `spacing_after=200` on last code line |
| Code block → heading | `spacing_before` on heading wins (all > 200) |
| Tight list → paragraph | `spacing_after=200` on last item paragraph |
| Tight list → heading | `spacing_before` on heading wins (all > 200) |
| Loose list → paragraph | `spacing_after=200` inherited by last item |
| Loose list → heading | `spacing_before` on heading wins (all > 200) |
| Blockquote → paragraph | spacer paragraph line height |
| Blockquote → heading | spacer line height + `spacing_before` on heading |
| Table → paragraph | spacer paragraph line height |
| Table → heading | spacer line height + `spacing_before` on heading |
| Tight list item → next item | `spacing_after=0` on all item paragraphs |
| Loose list item → next item | `spacing_after=200` inherited from Normal |
| Code line → next code line | `spacing_after=0` on CodeBlock style |
