# Conversation Settings

Conversation settings control the AI model and behaviour for a single conversation. Changes
take effect from your next message and do not affect other conversations.

Open conversation settings from:
- The **cog button** in the conversation input area
- **Ctrl+Shift+,**

---

## Model settings

### Model

The AI model to use. Only models whose backends are currently enabled in your user settings
appear in this list. You can switch models at any point in the conversation — the new model
receives the full conversation history as context.

### Reasoning

For models that support reasoning, this controls what Humbug does with the model's reasoning
process:

- **No reasoning** — the model answers directly without a reasoning step
- **Hidden reasoning** — the model reasons internally but the reasoning is not shown
- **Visible reasoning** — the model's reasoning appears as a collapsible message before its
  final response

Not all options are available for every model. The control is greyed out for models that
only support one mode.

### Reasoning effort

For models that support variable reasoning depth, this sets how thoroughly the model reasons
before answering. Options from lowest to highest are: **None**, **Minimal**, **Low**,
**Medium**, **High**, and **Extra High**.

Higher effort generally produces better results on complex problems but uses more tokens and
takes longer. Only the effort levels supported by the selected model are shown.

### Temperature

Controls how varied the AI's responses are. Lower values (closer to 0.0) produce more
predictable, focused output. Higher values (closer to 1.0) produce more varied and
exploratory responses.

Temperature is not available for all models, and on some models it is disabled when certain
reasoning effort levels are active. The control is greyed out when unavailable.

---

## Model information

These values are read-only and shown for reference:

- **Context window** — the maximum number of tokens the model can process at once. This
  includes all conversation history, system messages, and attached file contents
- **Maximum output** — the maximum number of tokens the model can generate in a single
  response

The token counts in the status bar let you monitor how close you are to these limits as
the conversation grows.

---

*[Index](index.md) · Previous: [Mindspace Settings](18-mindspace-settings.md) · Next: [Keyboard Shortcuts](20-keyboard-shortcuts.md)*
