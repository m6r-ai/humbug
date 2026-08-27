# ADR-0002: OS neutrality — one experience on Windows, macOS, and Linux

Date: 2026-08-27  
Status: Accepted

## Context

Humbug's users work across all three major desktop platforms.
Many comparable tools treat one of them, usually Windows or Linux, as a second-class citizen.
Humbug aspires to be an operating system for human-AI collaboration: it provides OS-like abstractions (mindspace, tabs, filesystem tools, a terminal) but runs on top of the user's operating system rather than replacing it.

## Decision

One codebase, one experience, three first-class platforms.

- Windows, macOS, and Linux are all first-class: features ship on all three or not at all.
- Platform differences are handled explicitly and contained behind abstractions, never allowed to leak into general code.
  Where behaviour fundamentally differs (terminal and shell handling is the clearest case) Humbug implements each platform's variant itself behind a single interface rather than degrading one platform.
- Each platform receives its native distribution format, built from the same source: a signed and notarised DMG on macOS, an installer on Windows, and an AppImage (x86_64 and ARM64) on Linux.
- Divergence is permitted only where the host OS demands it, such as keyboard shortcuts that follow platform conventions.

## Alternatives considered

- Platform-specific native frontends were rejected because they triple the maintenance, guarantee feature and behavioural divergence, and are infeasible for a bootstrapped project.
- One platform first, porting later was rejected because second-class platforms become permanent, porting debt compounds, and the community that forms on the first platform biases every later decision.
- A lowest-common-denominator experience was rejected because Humbug deliberately integrates with the host OS (terminals, shells, filesystem, version control); flattening the experience to what all platforms share would strip out capabilities that make it useful.

## Consequences

### Positive

- One user manual, one behavioural specification, one codebase for humans and AIs to reason about.
- Users move between platforms without relearning; teams can mix platforms freely.
- A single CI pipeline builds all three platforms from the same source, so platform regressions surface immediately.

### Negative

- Every feature must be verified on three platforms; platform-specific bugs cost more to diagnose and fix.
- Some capabilities require maintaining parallel implementations (the terminal emulator is the standing example).
- Packaging, code signing, and notarisation are a permanent per-platform build responsibility.
- Platform-specific niceties outside the abstractions are delayed or declined.
