"""Canonical display names for AI backend providers."""


from desktop.language.language_strings import LanguageStrings


BACKEND_IDS: list[str] = [
    "anthropic",
    "deepseek",
    "google",
    "mistral",
    "ollama",
    "ollama-cloud",
    "openai",
    "vllm",
    "xai",
    "zai",
]


def get_backend_display_name(backend_id: str, strings: LanguageStrings) -> str:
    """Return the localised display name for a backend ID, or the raw ID if unknown."""
    mapping = _build_mapping(strings)
    return mapping.get(backend_id, backend_id)


def get_all_backend_display_names(strings: LanguageStrings) -> dict[str, str]:
    """Return an ordered dict mapping every known backend ID to its localised display name."""
    mapping = _build_mapping(strings)
    return {bid: mapping[bid] for bid in BACKEND_IDS}


def _build_mapping(strings: LanguageStrings) -> dict[str, str]:
    return {
        "anthropic": strings.anthropic_backend,
        "deepseek": strings.deepseek_backend,
        "google": strings.google_backend,
        "mistral": strings.mistral_backend,
        "ollama": strings.ollama_backend,
        "ollama-cloud": strings.ollama_cloud_backend,
        "openai": strings.openai_backend,
        "vllm": strings.vllm_backend,
        "xai": strings.xai_backend,
        "zai": strings.zai_backend,
    }
