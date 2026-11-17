# User Settings

User settings apply across all mindspaces and cover things that are personal to you rather
than specific to a project. Open them from the **Settings** icon in the mindspace panel, from
the **Humbug** menu, or with **Ctrl+,**.

Settings are split into two groups in the dialog's navigation: **All Mindspaces** — the pages in
this chapter — and **This Mindspace**, which holds the project-specific pages described in
[Mindspace Settings](mindspace-settings.md).

---

## AI Backends

Configure the AI providers you want to use. For each backend you can:

- **Enable or disable** it with a toggle
- **Enter an API key** for cloud-based providers
- **Set a custom URL** for backends that support alternative endpoints (Ollama, vLLM, or
  compatible third-party APIs)

Each backend also has buttons to **test the connection**, to **update its model list** from the
provider, and to **remove fetched models** again.

See [AI Models & Backends](ai-models-and-backends.md) for full details.

---

## Appearance

### Language

Choose the language for Humbug's interface: **English**, **Français**, or **العربية**. The change
applies immediately, without restarting. Arabic is laid out right to left, including lists and
tables that would otherwise run left to right.

### Font size

Set a custom base font size. When left at the default, Humbug uses a size appropriate for
your display. You can also adjust zoom at any time with **Ctrl+=** and **Ctrl+-** without
changing this setting permanently.

### Font ligatures

Enable or disable programming ligatures in the editor and other monospaced text areas. When
enabled, character combinations such as `->`, `=>`, and `!=` are rendered as single connected
glyphs. On by default.

### Theme

Choose between:

| Theme | What it is |
|---|---|
| **Automatic** | Follows your system preference, and updates if you change it while Humbug is running |
| **Light** | A light interface |
| **Dark** | A dark interface |
| **Color Blind** | A light interface with a palette chosen to stay distinguishable for colour-blind users |
| **Ocean Light** | A light interface with a cooler, blue-tinted palette |
| **Custom Theme** | Your own colours, chosen with **Customize Colors…** |

**Customize Colors…** opens a colour picker for each part of the interface, so you can build a
theme that suits your display, your eyesight, or your taste. Your custom theme is saved with the
rest of your user settings.

You can also switch theme quickly without opening settings, from **View → Display Theme**. That
submenu lists the same themes, and is the fastest way to flip between light and dark.

---

## Files

### File sort order

Controls how files and folders are ordered in the mindspace tree views. Options include
directories first (the default) and alphabetical sorting with files and folders interleaved.

You can also sort an individual view from its context menu, using **Sort By → Sort by name** or
**Sort by creation time**.

---

## External file access

The AI's filesystem tool can never *change* anything outside the mindspace. What it can *read*
outside the mindspace is controlled here:

- **Allow external file access** — the master switch. With it off, the AI cannot read anything
  outside the mindspace at all
- **Allowlist** — external paths that need no approval. On macOS and Linux it starts with the
  system header and documentation directories, so an AI can read a library's declarations without
  interrupting you; on Windows it starts empty
- **Denylist** — paths that are never accessible, whatever else is set. The denylist is always
  active, even when external access is disabled

Anything outside the mindspace that is not on the allowlist is offered to you for approval each
time, so the switch being on does not mean the AI reads your filesystem freely — it means it may
*ask*. Approving one read does not approve the next.

Entries are glob patterns, one per line. The denylist is pre-filled with the places secrets
usually live — `~/.ssh`, `~/.aws`, `~/.gnupg`, `~/.password-store`, and Humbug's own `~/.humbug`
settings — with the platform equivalents on Windows. The `.humbug` directory of the current
mindspace is added to the deny list automatically, so no setting can expose a mindspace's
internal state to its own AI.

---

## Network access

Every request the AI makes through the HTTP tool is approved by you unless this page says
otherwise. The allowlist starts empty, so out of the box every fetch prompts. This page lets the
safe, repetitive cases run without interrupting you:

- **Allow AI to fetch allowlisted URLs without asking** — master toggle for auto-approval. It
  controls only whether the allowlist applies; it does not cut the AI off from the network, which
  is done by disabling the HTTP tool in the mindspace's AI tools
- **Auto-approved URLs** — one per line. An entry is a host (such as `api.example.com`) or a URL
  with a port (such as `https://api.example.com:8443`). Matching GET and HEAD requests need no
  approval
- **Always denied URLs** — one per line, such as `localhost` or a private address range. These are
  refused outright, whatever else is set

Every other request still prompts, including every request that changes something at the far end —
POST, PUT, PATCH and DELETE are never auto-approved. Redirects are re-checked against these lists
before they are followed, so an allowlisted URL cannot be used to reach a denied one.

The deny list is pre-filled with loopback, link-local and private network ranges, and with the
cloud metadata endpoints that live in them. An allowlisted public host therefore cannot be used,
even by accident, as a route to internal infrastructure.

---

## Updates

**Check for updates automatically** — when enabled, Humbug checks for a newer version on
startup and notifies you if one is available. You can also check manually from the
**Humbug** menu with **Check for Updates…**, which reports the version you are running and whether
a newer one exists.

---

*[Index](index.md) · Previous: [Mission Control](mission-control.md) · Next: [Mindspace Settings](mindspace-settings.md)*
