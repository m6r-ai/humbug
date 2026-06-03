# Conversation Streaming "Bounce" Investigation — Handover Summary (v2)

## The problem (still unsolved)

In a Humbug conversation tab, while an AI response is **streaming** and the user has
**scrolled up** (so they are NOT at the bottom), on-screen content in the **lower part of
the viewport** visibly **bounces** ~once per reveal tick for the whole duration of
streaming (15+ seconds for a long response).

**CRITICAL CORRECTION vs v1 of this doc:** the bounce is **one to two whole lines
(~18–36px)**, NOT 1–2px. v1 and early v2 work wrongly assumed a sub-pixel/1–2px effect;
several experiments were mis-evaluated because of this. Treat the excursion as
**whole-line-sized**.

## Reliable reproduction

1. Conversation with enough history to scroll.
2. Scroll **up** (away from the bottom). NOTE: the initial scroll gesture itself produces
   a transient `value` drift in the logs — **ignore the first ~0.5s of any capture**; it
   is the scroll settling, not the bug.
3. While scrolled up, a long response streams in. **A code block makes it most obvious,
   but pure markdown bounces by a SIMILAR (full-line) amount** — see "Key new facts".
4. Observe: stable, already-rendered content in the lower region (e.g. the streaming
   message's earlier lines, above the line currently growing) **jumps up by one or two
   lines and snaps back**, every reveal tick. The user described it as "the whole child
   widget bounces upwards and then back to where it should be."

## Architecture (key facts — unchanged from v1, condensed)

- `src/desktop/conversation_tab/conversation_widget.py` — `ConversationWidget`.
  - `QScrollArea`, `setWidgetResizable(True)`, vertical scrollbar `AlwaysOn`, `AlignTop`.
    Its widget is `_messages_container` (`QWidget` + `QVBoxLayout`).
  - Layout: `[message0 … messageN, addStretch(), _input_spacer]`. Messages inserted at
    `count() - 2`.
  - The editable input is a **floating overlay** parented to `ConversationWidget`,
    OUTSIDE the scroll area.
- Each `ConversationMessage` holds `ConversationMessageSection`s, each with a text-edit:
  - Markdown/text: `MarkdownTextEdit` (`MinHeightTextEdit`, a `QTextEdit`).
  - Code blocks: `CodeBlockTextEdit` (`MinHeightPlainTextEdit`, a `QPlainTextEdit`).
- Display text-edits are **height-locked**: `_on_content_resized` (driven by the document
  layout's `documentSizeChanged`, a **synchronous same-thread signal**) calls
  `setFixedHeight(...)` then `updateGeometry()`. `updateGeometry()` POSTS a layout request
  processed on a later event-loop pass (this matters — see hypothesis).
- Streaming flow: `_response_reveal_timer` (24ms) → `_advance_response_reveal` (drips text,
  ≤20 chars/tick via `_response_reveal_chunk_size`, snapped to word boundary) →
  `ConversationMessage.set_content` → section `set_content` → for markdown,
  `MarkdownRenderer.visit` (**clears & rebuilds the ENTIRE QTextDocument every tick**);
  for code, `set_text` (incremental cursor edit).

## What we PROVED this session (high confidence, all by measurement)

A `BounceDebug` logger (INFO level, via existing class loggers) was added at three layers.
All probes are CURRENTLY STILL IN THE CODE (see "Instrumentation currently in code").

1. **Settled scroll value does NOT drift in steady state.** After the scroll gesture
   settles, the outer scrollbar `value` is constant (e.g. 2030) for the entire stream
   while `max` grows. v1 said "perfectly constant" — true *at settle*. (v2 briefly thought
   it drifted; that was the scroll gesture, ruled out.)

2. **Settled widget geometry is constant.** A per-reveal-tick probe (`BounceDebug REVEAL`)
   logging the streaming widget's viewport-relative top, sampled SYNCHRONOUSLY right after
   `set_content`, reads a CONSTANT value (e.g. `stream_top=979`) on essentially every tick
   — EXCEPT one captured sample at `stream_top=940` (= 979 − 39 ≈ **two lines up**). That
   lone sample is now understood to be **the bounce itself, caught** on the one tick the
   synchronous read happened to land during the transient. The "up" direction (smaller Y)
   matches the user's "bounces upwards" report.

3. **`QPlainTextEdit` internal block offset is NOT involved.** A `paintEvent` probe on
   `MinHeightPlainTextEdit` logged `firstVisibleBlock().blockNumber()` and
   `contentOffset().y()` at paint time: **both are always 0**. This DEFINITIVELY kills the
   v1 leading theory (internal block-scroll offset) and explains why the prior session's
   `scrollContentsBy` swallow did nothing.

4. **Per-widget `gy` marches downward as content grows** (expected reflow), heights are
   clean/monotonic. The bounce is a transient ON TOP of this, not the march itself.

5. **The input/spacer relayout path is INNOCENT.** `_on_input_size_hint_changed` logs
   `INPUTHINT skip` on EVERY tick (60+ skips, ZERO `WORK`) while streaming-and-scrolled-up
   — it always early-returns. Not a contributor.

6. **Bounce cadence == reveal-tick cadence.** Throttling the reveal timer 24ms→200ms
   dropped the bounce frequency ~8× (and slowed text to ~1/8). One relayout → one bounce.

7. **Bounce magnitude is INDEPENDENT of how much text arrives per tick.** Because chunk
   size is capped (`min(20, max(2, remaining//18))`), throttling did not give 8× more text
   per tick — so "same shove size" is expected and does NOT imply a fixed-size transient.
   (This corrected an over-interpretation; see lesson below.)

8. **Line-granularity does NOT fix it.** Revealing strictly one whole line per tick (snap
   to next `\n`) still jittered by the same amount. So it is NOT caused by chunks
   straddling line boundaries.

9. **Pure markdown bounces by a SIMILAR (full-line) amount as code blocks.** This is the
   most important NEW fact. The cause is therefore COMMON to both the markdown
   clear-and-rebuild path and the code-block `setFixedHeight` path — i.e. the shared
   per-tick height-relock + relayout, NOT anything code-block-specific.

## Experiments that did NOT stop the bounce (with interpretation)

- `setUpdatesEnabled(False/True)` on **`_messages_container`** around the reveal's
  `set_content` calls: content rendered fine, **bounce unchanged**.
- `setUpdatesEnabled(False/True)` on the **viewport** around the same span: content fine,
  **bounce unchanged**.
- Interpretation (KEY): suppression scoped to the synchronous reveal call cannot catch the
  offending paint. Combined with fact 2 (settled geometry constant) and the lone 940
  sample, this means **the bounce is a DEFERRED relayout/paint that happens AFTER
  `_advance_response_reveal` returns** — almost certainly the `updateGeometry()`-posted
  layout request processed on the next event-loop pass. The suppression window closed
  before it fired.

## Theories RULED OUT (cumulative, v1 + v2)

- Outer scroll-value drift in steady state (value constant). 
- `QPlainTextEdit` internal block/content offset (always 0 at paint time).
- Height oscillation of the final height (heights monotonic).
- Section create/destroy / reclassification (bounce occurs mid-section).
- Smooth-scroll animation (lasts ms; bounce lasts 15+ s).
- Input/spacer relayout cascade (`INPUTHINT` always skip).
- Chunks straddling line boundaries (line-granularity test still bounced).
- Intra-relayout intermediate paint catchable by container OR viewport
  `setUpdatesEnabled` (both failed).
- Pinning inner `verticalScrollBar().setValue(0)` (prior session; offset is independent).

## LEADING HYPOTHESIS at this handover

A **per-tick transient height excursion causing a deferred two-pass relayout**:

- Markdown path: `MarkdownRenderer.visit` CLEARS the whole QTextDocument then refills it
  every tick. There is a transient where the document (hence the section height) briefly
  COLLAPSES before refilling. 
- Code path: `set_content` grows the document; `documentSizeChanged`→`setFixedHeight` may
  apply an intermediate/!final height for a pass.
- Either way, `updateGeometry()` posts a layout request. On a layout pass that lands during
  the collapsed/intermediate moment, the streaming widget (and the lower region) is placed
  **one or two lines too high**, painted, then a subsequent pass corrects it to the settled
  position — the visible "up and back" bounce.

This single mechanism explains EVERY observation: full-line size (a whole section's height
briefly wrong), present in BOTH markdown and code (both relock height per tick), invisible
to synchronous probes (settled value is correct), invisible to reveal-scoped suppression
(the bad paint is deferred), cadence == tick rate, magnitude independent of chars/tick.

## RECOMMENDED next steps (measure first, per the methodology that has worked)

1. **Confirm the deferred-pass / height-collapse directly.** Instrument
   `MinHeightTextEdit._on_content_resized` AND `MinHeightPlainTextEdit._on_content_resized`
   to log `old_height -> new_height` and the widget's viewport-relative top BEFORE and
   AFTER `setFixedHeight`, every fire, for the streaming widget. Watch specifically for a
   tick where `new_height` momentarily DROPS (markdown collapse) or where the top reads
   ~18–36px off. Because the excursion is whole-line-sized it will be unmistakable.

2. **Catch it at paint time, every paint (not on-change).** Re-purpose the `PLAIN`/`TEXT`
   `paintEvent` probes to log the widget's VIEWPORT-RELATIVE top EVERY paint (currently they
   log absolute `gy` only on change, which hid the oscillation inside the downward march).
   Deviations from the settled 979 are pure bounce.

3. **Candidate fix to test once mechanism confirmed:** prevent the deferred second pass /
   transient collapse. Options to try (measure each):
   - For markdown: make `set_content` NOT fully clear+rebuild the document during streaming
     (incremental update like the code path), removing the transient collapse.
   - Coalesce the height relock so layout is invalidated ONCE per tick and painted once
     AFTER settle — but note: this must span the DEFERRED `updateGeometry` pass, not just
     the synchronous reveal call (that is why prior suppression failed). E.g. suppress
     viewport updates and re-enable via `QTimer.singleShot(0, ...)` so the deferred relayout
     paints under suppression.
   - Pin the streaming widget's viewport-relative top across the tick+deferred-pass and
     correct any whole-line excursion.

## Instrumentation CURRENTLY IN THE CODE (remove/finalise next session)

All tagged `TEMPORARY (bounce investigation)` and logged as `BounceDebug ...` at INFO via
existing class loggers.

- `src/desktop/conversation_tab/conversation_widget.py`:
  - `_BounceDebugContainer(QWidget)` subclass with a `paintEvent` hook; `_messages_container`
    is an instance of it; `_on_container_paint_debug` logs `BounceDebug PAINT` (scroll
    value/max, first-msg gy, container-vs-viewport offset) per container paint, gated to
    streaming+scrolled-up.
  - `_advance_response_reveal`: currently wraps the render span in
    `viewport.setUpdatesEnabled(False/True)` (a FAILED experiment — should be REVERTED), and
    logs `BounceDebug REVEAL value/max/first_top/stream_top` per tick (keep or adapt).
  - `_on_input_size_hint_changed`: logs `BounceDebug INPUTHINT skip|WORK` (proved skip;
    can be removed).
  - Reveal timer interval is back to original 24ms.
- `src/desktop/widgets/min_height_plain_text_edit.py`: `paintEvent` override logging
  `BounceDebug PLAIN ... block offset_y gy h` on change; imports `QPoint`, `QPaintEvent`.
- `src/desktop/widgets/min_height_text_edit.py`: `paintEvent` override logging
  `BounceDebug TEXT ... scroll top_pos gy h` on change; module `import logging` +
  `self._logger`; imports `QPoint`, `QPaintEvent`.

NOTE: the line-granularity test and the two `setUpdatesEnabled` experiments have been
reverted in logic EXCEPT the viewport-suppression wrapper currently still present in
`_advance_response_reveal` — REVERT that wrapper first thing.

## Methodology notes (hard-won)

- MEASURE before fixing. Every theory-first fix this session and last FAILED. Every
  advance came from a probe.
- BEWARE the scroll-gesture settling at the start of each capture (~0.5s) — not the bug.
- The bounce is a DEFERRED, PAINT-TIME, WHOLE-LINE transient. Synchronous geometry reads
  see only the settled (correct) value. Probes must sample at paint time, every paint, and
  log VIEWPORT-RELATIVE positions (absolute gy marches and hides the oscillation).
- `filesystem.apply_diff_to_file` works on `min_height_text_edit.py` but historically got
  stuck on `min_height_plain_text_edit.py` — use an EDITOR TAB for that file.

## Tooling note

Log file used this session: `log.log` at mindspace root (large — search, don't read whole).
Filter on `BounceDebug` and the sub-tags `PAINT` / `PLAIN` / `TEXT` / `REVEAL` / `INPUTHINT`.
