"""Handles streaming response from Deepseek API."""

from typing import Dict

from humbug.ai.ai_stream_response import AIStreamResponse


class DeepseekStreamResponse(AIStreamResponse):
    """Handles streaming response from Deepseek API."""

    def update_from_chunk(self, chunk: Dict) -> None:
        """
        Update from a response chunk and return new content if any.

        Args:
            chunk: Response chunk from Deepseek API
        """
        if "error" in chunk:
            self._handle_error(chunk["error"])
            return

        if "usage" in chunk:
            usage = chunk["usage"]
            if usage:
                self._update_usage(
                    prompt_tokens=usage.get("prompt_tokens", 0),
                    completion_tokens=usage.get("completion_tokens", 0),
                    total_tokens=usage.get("total_tokens", 0)
                )

        if "choices" not in chunk:
            return

        choices = chunk["choices"]
        if not choices:
            return

        delta = choices[0].get("delta", {})

        if "reasoning_content" in delta:
            new_reasoning = delta["reasoning_content"]
            if new_reasoning:
                self.reasoning += new_reasoning

        if "content" in delta:
            new_content = delta["content"]
            if new_content:
                self.content += new_content
