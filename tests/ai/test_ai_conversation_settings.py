"""Tests for AIConversationSettings serialisation."""

from ai import AIConversationSettings
from ai.ai_model import AIReasoningCapability


class TestToDict:
    """Tests for AIConversationSettings.to_dict."""

    def test_captures_all_fields(self) -> None:
        """Every setting is represented in the dictionary."""
        settings = AIConversationSettings(
            model="m",
            provider="p",
            temperature=0.3,
            reasoning=AIReasoningCapability.VISIBLE_REASONING,
            reasoning_effort="high",
        )

        data = settings.to_dict()

        assert data["model"] == "m"
        assert data["provider"] == "p"
        assert data["temperature"] == 0.3
        assert data["reasoning"] == "VISIBLE_REASONING"
        assert data["reasoning_effort"] == "high"

    def test_reasoning_is_serialised_by_name(self) -> None:
        """Reasoning capability is stored as its enum name, not its value."""
        settings = AIConversationSettings(
            model="m", provider="p", reasoning=AIReasoningCapability.HIDDEN_REASONING
        )

        assert settings.to_dict()["reasoning"] == "HIDDEN_REASONING"


class TestFromDict:
    """Tests for AIConversationSettings.from_dict."""

    def test_round_trips_all_fields(self) -> None:
        """Settings survive a to_dict/from_dict round trip."""
        original = AIConversationSettings(
            model="m",
            provider="p",
            temperature=0.3,
            reasoning=AIReasoningCapability.VISIBLE_REASONING,
            reasoning_effort="high",
        )

        restored = AIConversationSettings.from_dict(original.to_dict())

        assert restored.model == "m"
        assert restored.provider == "p"
        assert restored.temperature == 0.3
        assert restored.reasoning == AIReasoningCapability.VISIBLE_REASONING
        assert restored.reasoning_effort == "high"

    def test_restores_reasoning_capability_from_name(self) -> None:
        """The reasoning name is mapped back to the capability flag."""
        restored = AIConversationSettings.from_dict({
            "model": "m",
            "provider": "p",
            "temperature": 0.7,
            "reasoning": "NO_REASONING",
            "reasoning_effort": None,
        })

        assert restored.reasoning == AIReasoningCapability.NO_REASONING
