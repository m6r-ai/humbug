"""Per-mindspace token and cost usage tracking."""

from dataclasses import dataclass, field
from typing import Dict, List


@dataclass
class ModelUsageEntry:
    """Aggregated token and cost stats for one (provider, model) pair."""
    provider: str
    model: str
    input_tokens: int = 0
    output_tokens: int = 0
    cache_write_tokens: int = 0
    cache_read_tokens: int = 0
    cost_usd: float = 0.0


class MindspaceUsage:
    """
    Accumulates token usage and estimated cost across all conversations in a mindspace.

    Stored as .humbug/usage.json and updated after each completed AI response.
    """

    def __init__(self) -> None:
        self._entries: Dict[tuple, ModelUsageEntry] = {}

    def record(
        self,
        provider: str,
        model: str,
        input_tokens: int,
        output_tokens: int,
        cost_usd: float,
        cache_write_tokens: int = 0,
        cache_read_tokens: int = 0,
    ) -> None:
        """Add usage from a single AI response to the running totals."""
        key = (provider, model)
        if key not in self._entries:
            self._entries[key] = ModelUsageEntry(provider=provider, model=model)

        entry = self._entries[key]
        entry.input_tokens += input_tokens
        entry.output_tokens += output_tokens
        entry.cache_write_tokens += cache_write_tokens
        entry.cache_read_tokens += cache_read_tokens
        entry.cost_usd += cost_usd

    def total_cost(self) -> float:
        """Return total estimated spend across all models."""
        return sum(e.cost_usd for e in self._entries.values())

    def total_input_tokens(self) -> int:
        """Return total input tokens across all models."""
        return sum(e.input_tokens for e in self._entries.values())

    def total_output_tokens(self) -> int:
        """Return total output tokens across all models."""
        return sum(e.output_tokens for e in self._entries.values())

    def total_cache_write_tokens(self) -> int:
        """Return total cache-write tokens across all models."""
        return sum(e.cache_write_tokens for e in self._entries.values())

    def total_cache_read_tokens(self) -> int:
        """Return total cache-read tokens across all models."""
        return sum(e.cache_read_tokens for e in self._entries.values())

    def entries(self) -> List[ModelUsageEntry]:
        """Return per-model entries sorted by cost descending."""
        return sorted(self._entries.values(), key=lambda e: e.cost_usd, reverse=True)

    def reset(self) -> None:
        """Clear all accumulated usage data."""
        self._entries.clear()

    def to_dict(self) -> dict:
        """Serialise to a JSON-compatible dict."""
        return {
            "version": "0.2",
            "entries": [
                {
                    "provider": e.provider,
                    "model": e.model,
                    "input_tokens": e.input_tokens,
                    "output_tokens": e.output_tokens,
                    "cache_write_tokens": e.cache_write_tokens,
                    "cache_read_tokens": e.cache_read_tokens,
                    "cost_usd": e.cost_usd,
                }
                for e in self._entries.values()
            ],
        }

    @classmethod
    def from_dict(cls, data: dict) -> "MindspaceUsage":
        """Deserialise from a previously saved dict."""
        usage = cls()
        for item in data.get("entries", []):
            key = (item["provider"], item["model"])
            usage._entries[key] = ModelUsageEntry(
                provider=item["provider"],
                model=item["model"],
                input_tokens=item.get("input_tokens", 0),
                output_tokens=item.get("output_tokens", 0),
                cache_write_tokens=item.get("cache_write_tokens", 0),
                cache_read_tokens=item.get("cache_read_tokens", 0),
                cost_usd=item.get("cost_usd", 0.0),
            )

        return usage
