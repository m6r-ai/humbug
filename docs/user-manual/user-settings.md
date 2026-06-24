# User Settings

User settings apply across all mindspaces and cover things that are personal to you rather
than specific to a project. Open them from the **Settings** icon in the mindspace panel, from
the **Humbug** menu, or with **Ctrl+,**.

---

## AI Backends

Configure the AI providers you want to use. For each backend you can:

- **Enable or disable** it with a toggle
- **Enter an API key** for cloud-based providers
- **Set a custom URL** for backends that support alternative endpoints (Ollama, vLLM, or
  compatible third-party APIs)

See [AI Models & Backends](ai-models-and-backends.md) for full details.

---

## Appearance

### Theme

Choose between **Automatic**, **Light**, and **Dark**. Automatic follows your system
preference and updates if you change it while Humbug is running.

### Font size

Set a custom base font size. When left at the default, Humbug uses a size appropriate for
your display. You can also adjust zoom at any time with **Ctrl+=** and **Ctrl+-** without
changing this setting permanently.

### Font ligatures

Enable or disable programming ligatures in the editor and other monospaced text areas. When
enabled, character combinations such as `->`, `=>`, and `!=` are rendered as single connected
glyphs. On by default.

---

## Files

### File sort order

Controls how files and folders are ordered in the mindspace tree views. Options include
directories first (the default) and alphabetical sorting with files and folders interleaved.

---

## External file access

By default, the AI's filesystem tool can only access files within the current mindspace.
This section lets you extend that:

- **Allow external file access** — master toggle for allowing the AI to read files outside
  the mindspace
- **Allowlist** — a list of paths the AI is permitted to access when external access is
  enabled. Paths not on the allowlist require your approval each time
- **Denylist** — a list of paths the AI can never access, regardless of any other settings.
  The denylist is always active, even when external access is disabled

---

## Updates

**Check for updates automatically** — when enabled, Humbug checks for a newer version on
startup and notifies you if one is available. You can also check manually from the
**Humbug** menu.

---

*[Index](index.md) · Previous: [Token Usage](token-usage.md) · Next: [Mindspace Settings](mindspace-settings.md)*
