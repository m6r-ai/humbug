"""Cost calculation utilities for AI model usage."""

from ai.ai_model import AIModel


def calculate_cost(
    prompt_tokens: int,
    completion_tokens: int,
    model: AIModel,
    cache_write_tokens: int = 0,
    cache_read_tokens: int = 0,
) -> float:
    """
    Return the USD cost for a single AI response.

    prompt_tokens is the total input token count (including any cache hits/misses).
    cache_write_tokens and cache_read_tokens are the Anthropic-specific subsets
    that carry different rates; the remainder is charged at the base input rate.

    Returns 0.0 for models with no pricing data (local/unknown models).
    """
    regular_tokens = max(prompt_tokens - cache_write_tokens - cache_read_tokens, 0)
    return (
        (regular_tokens / 1_000_000) * model.input_cost_per_mtok
        + (cache_write_tokens / 1_000_000) * model.cache_write_cost_per_mtok
        + (cache_read_tokens / 1_000_000) * model.cache_read_cost_per_mtok
        + (completion_tokens / 1_000_000) * model.output_cost_per_mtok
    )
