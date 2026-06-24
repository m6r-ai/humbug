# AI Models & Backends

Humbug supports AI models from multiple providers. Each provider is accessed through a
**backend** — the connection layer that handles communication between Humbug and that provider's
API. This chapter explains how to configure backends, choose models, and understand the
capabilities that different models offer.

---

## Backends

Humbug supports the following backends:

| Backend | Provider | Notes |
|---|---|---|
| **Anthropic** | Anthropic | Claude models. Requires an API key |
| **DeepSeek** | DeepSeek | Requires an API key |
| **Google** | Google | Gemini models. Requires an API key |
| **Mistral** | Mistral AI | Requires an API key |
| **Ollama** | Local | Runs models locally on your machine. No API key required |
| **OpenAI** | OpenAI | GPT models. Requires an API key |
| **vLLM** | Self-hosted | For running your own vLLM server. Requires a custom URL |
| **xAI** | xAI | Grok models. Requires an API key |
| **Z.ai** | Z.ai | GLM models. Requires an API key |

You only need to configure the backends you want to use. If you only have an Anthropic API key,
you can enable just the Anthropic backend and ignore the rest.

---

## Configuring backends

Backend settings are in the **Settings** dialog, accessible from the settings icon in the
mindspace panel or from the application menu.

For each backend you can:

- **Enable or disable** the backend with a checkbox
- **Enter an API key** for cloud-based providers
- **Set a custom URL** for backends that support non-standard endpoints (useful for self-hosted
  setups, proxies, or compatible third-party APIs)

Changes take effect immediately — you do not need to restart Humbug.

### API keys via environment variables

As an alternative to entering API keys in the settings dialog, you can set them as environment
variables before launching Humbug. The variable names are:

| Backend | Environment variable |
|---|---|
| Anthropic | `ANTHROPIC_API_KEY` |
| DeepSeek | `DEEPSEEK_API_KEY` |
| Google | `GOOGLE_API_KEY` |
| Mistral | `MISTRAL_API_KEY` |
| Ollama | `OLLAMA_API_KEY` |
| OpenAI | `OPENAI_API_KEY` |
| xAI | `XAI_API_KEY` |
| Z.ai | `ZAI_API_KEY` |

If a key is present in the environment when Humbug starts, the corresponding backend is
automatically enabled.

---

## Ollama: running models locally

Ollama lets you run AI models on your own machine without sending data to any external service.
This is useful if you want to work offline, have privacy requirements, or want to experiment with
open models without API costs.

To use Ollama:

1. Install [Ollama](https://ollama.com) on your machine
2. Pull the models you want to use (e.g. `ollama pull llama3.2` in a terminal)
3. Enable the Ollama backend in Humbug's settings — no API key is needed

Ollama must be running on your machine for the backend to work. The default URL that Humbug uses
to connect to Ollama is `http://localhost:11434`. If you run Ollama on a different host or port,
you can change the URL in the backend settings.

---

## vLLM: self-hosted models

The vLLM backend is for users running their own
[vLLM](https://docs.vllm.ai) inference server. Set the URL in the backend settings to point
to your server. No API key is required unless your server is configured to require one.

---

## Selecting a model

The models available in any conversation are those whose backend is currently enabled. You
choose the model for a conversation in the **conversation settings** — click the cog button in
the input area, or right-click the conversation tab label.

Only models whose backends you have configured will appear in the model list.

You can switch models at any point during a conversation. The new model receives the full
conversation history as context, so it is up to speed with everything that has been discussed.

### Default model

Each mindspace has a default model that is used when you start a new conversation. You can set
this in the **mindspace settings**. If no default is set, Humbug picks the first available model
based on which backends are enabled.

---

## Model capabilities

Not all models support the same features. When you select a model, the conversation settings
dialog shows its limits and capabilities.

### Temperature

Temperature controls how varied the AI's responses are. A low temperature (close to 0.0) makes
the model produce more predictable, focused output. A higher temperature (closer to 1.0) makes
responses more varied and exploratory.

Not all models support temperature — some always behave as if temperature is fixed. For models
that do, the temperature control is enabled in conversation settings; for those that don't, it
is greyed out.

### Reasoning

Some models can show — or at least perform — reasoning steps before producing an answer:

- **No reasoning** — the model produces a direct response without any reasoning step
- **Hidden reasoning** — the model reasons internally but the reasoning is not shown in Humbug
- **Visible reasoning** — the model's reasoning appears as a collapsible message before the
  final response, so you can read through its chain of thought

You can choose the reasoning mode in conversation settings for models that support more than one.

### Reasoning effort

Some models let you control how much reasoning they do, from a light pass to an exhaustive
analysis. The effort levels, from lowest to highest, are: **none**, **minimal**, **low**,
**medium**, **high**, and **extra high**. Not every model supports every level — the conversation
settings dialog only shows the levels available for the currently selected model.

Higher effort generally means better results on complex problems, but slower responses and more
tokens used.

Note that on some models, enabling reasoning effort disables the temperature setting — this is a
constraint of the model itself, and Humbug's settings dialog will reflect it automatically.

### Context window and output limits

The conversation settings dialog shows two read-only values for the selected model:

- **Context window** — the maximum number of tokens the model can consider at once, including
  the entire conversation history and any attached files
- **Maximum output** — the maximum number of tokens the model can produce in a single response

As a conversation grows, the context window fills up. The token counts in the status bar let you
monitor how close you are to the limit.

---

## Adding custom models

If you use a model that Humbug does not have built-in support for — for example, a specific
Ollama model you have pulled, or a compatible API that uses the same interface as one of the
supported backends — you can add it by creating a configuration file at:

```text
~/.humbug/user-ai-config.json
```

This file defines additional models that Humbug will register alongside its built-in ones.
Each entry specifies the model's display name, the internal model name used by the API, the
backend provider, context window size, output limit, and capability flags.

An example entry for an Ollama model:

```json
{
  "models": [
    {
      "display_name": "My Custom Model (Ollama)",
      "name": "my-model-name",
      "provider": "ollama",
      "context_window": 131072,
      "max_output_tokens": 4096,
      "supports_temperature": true,
      "reasoning_capabilities": "NO_REASONING",
      "tool_capabilities": "FUNCTION_CALLING"
    }
  ]
}
```

Valid values for `reasoning_capabilities` are `NO_REASONING`, `HIDDEN_REASONING`, and
`VISIBLE_REASONING`. Valid values for `tool_capabilities` are `NO_TOOLS`, `FUNCTION_CALLING`,
and `PARALLEL_TOOLS`.

Any errors in the file are reported when Humbug starts, and the offending entries are skipped.

---

*[Index](index.md) · Previous: [Conversations](conversations.md) · Next: [Managing Files & Folders](managing-files.md)*
