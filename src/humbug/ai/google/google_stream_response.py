"""Handles streaming response from Google Gemini API."""

from typing import Dict

from humbug.ai.ai_stream_response import AIStreamResponse


class GoogleStreamResponse(AIStreamResponse):
    """Handles streaming response from Google Gemini API."""

    def update_from_chunk(self, chunk: Dict) -> None:
        """
        Update from a response chunk and return new content if any.

        Args:
            chunk: Response chunk from Google API
        """
        if "error" in chunk:
            error_data = chunk["error"]
            self._handle_error(error_data)
            return

        if "candidates" in chunk and chunk["candidates"]:
            candidate = chunk["candidates"][0]

            # Extract text content
            if "content" in candidate and "parts" in candidate["content"]:
                for part in candidate["content"]["parts"]:
                    if "text" in part:
                        text = part["text"]
                        if text:
                            self.content += text

            # Check for completion reason
            if candidate.get("finishReason") == "STOP" and "usageMetadata" in chunk:
                metadata = chunk["usageMetadata"]
                self._update_usage(
                    prompt_tokens=metadata.get("promptTokenCount", 0),
                    completion_tokens=metadata.get("candidatesTokenCount", 0),
                    total_tokens=metadata.get("totalTokenCount", 0)
                )
