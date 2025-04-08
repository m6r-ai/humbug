"""Handles streaming response from Ollama API."""

from typing import Dict

from humbug.ai.ai_stream_response import AIStreamResponse


class OllamaStreamResponse(AIStreamResponse):
    """Handles streaming response from Ollama API."""

    def update_from_chunk(self, chunk: Dict) -> None:
        """
        Update from a response chunk and return new content if any.

        Args:
            chunk: Response chunk from Ollama API
        """
        if "error" in chunk:
            self._handle_error(chunk["error"])
            return

        if chunk.get("done"):
            self._update_usage(
                prompt_tokens=chunk.get("prompt_eval_count", 0),
                completion_tokens=chunk.get("eval_count", 0),
                total_tokens=chunk.get("prompt_eval_count", 0) + chunk.get("eval_count", 0)
            )

        if "message" in chunk and "content" in chunk["message"]:
            content = chunk["message"]["content"]
            if content:
                self.content += content
