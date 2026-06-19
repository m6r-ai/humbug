"""Tests for AI response cost calculations."""

from pytest import approx

from ai.ai_cost import calculate_cost
from ai.ai_model import AIModel, AIReasoningCapability


def _priced_model() -> AIModel:
    return AIModel(
        name="priced-model",
        provider="test",
        display_name="Priced Model",
        context_window=100_000,
        max_output_tokens=10_000,
        supports_temperature=True,
        reasoning_capabilities=AIReasoningCapability.NO_REASONING,
        input_cost_per_mtok=2.0,
        output_cost_per_mtok=8.0,
        cache_write_cost_per_mtok=3.0,
        cache_read_cost_per_mtok=0.5,
    )


def test_calculate_cost_uses_provider_cache_rates() -> None:
    """Cached input tokens should use their own provider rates."""
    cost = calculate_cost(
        prompt_tokens=1_000_000,
        completion_tokens=500_000,
        model=_priced_model(),
        cache_write_tokens=250_000,
        cache_read_tokens=250_000,
    )

    assert cost == 5.875


def test_calculate_cost_never_charges_negative_regular_input_tokens() -> None:
    """Cache token counts above prompt tokens should not create a negative input charge."""
    cost = calculate_cost(
        prompt_tokens=100,
        completion_tokens=0,
        model=_priced_model(),
        cache_write_tokens=100,
        cache_read_tokens=100,
    )

    assert cost == approx(0.00035)
