import re
from typing import Any

from ai import AIConversationHistory, AIMessage
from ai_transcript_conversation import AITranscriptConversation


class ConversationContext:
    """
    Model-layer context for an AI conversation.

    Owns the data operations the AI tools need: reading, searching, and
    querying conversation history.  Has no Qt dependency.

    The visualisation side-effect (scrolling the viewport to a message) is
    emitted as a callback so the Qt ConversationWidget can react without the
    context needing to know about widgets.
    """

    context_type = "conversation"

    def __init__(
        self,
        context_id: str,
        ai_transcript_conversation: AITranscriptConversation,
        on_scroll_to_message: Any = None,
    ) -> None:
        """
        Initialise the conversation context.

        Args:
            context_id: Stable identifier issued by the ContextRegistry.
            ai_transcript_conversation: The backing conversation model.
            on_scroll_to_message: Optional callable(message_id, message_index)
                invoked when the AI requests a scroll.  The Qt widget supplies
                this; a CLI would leave it None.
        """
        self._context_id = context_id
        self._transcript = ai_transcript_conversation
        self._on_scroll_to_message = on_scroll_to_message

    def context_id(self) -> str:
        """Return the stable context identifier."""
        return self._context_id

    def ai_transcript_conversation(self) -> AITranscriptConversation:
        """Return the backing AITranscriptConversation."""
        return self._transcript

    def get_conversation_info(self) -> dict[str, Any]:
        """
        Return high-level metadata about the conversation.

        Returns:
            Dictionary with message_count, timestamps, models_used,
            total_tokens, parent reference, and version.
        """
        history = self._transcript.get_conversation_history()
        messages = history.get_messages()

        if not messages:
            return {
                "message_count": 0,
                "first_message_timestamp": None,
                "last_message_timestamp": None,
                "models_used": [],
                "total_tokens": history.get_token_counts(),
                "parent": self._serialize_parent(history.parent()),
                "version": history.version(),
            }

        models_used = list({msg.model for msg in messages if msg.model})

        return {
            "message_count": len(messages),
            "first_message_timestamp": messages[0].timestamp.isoformat(),
            "last_message_timestamp": messages[-1].timestamp.isoformat(),
            "models_used": models_used,
            "total_tokens": history.get_token_counts(),
            "parent": self._serialize_parent(history.parent()),
            "version": history.version(),
        }

    def read_messages(
        self,
        start_index: int | None = None,
        end_index: int | None = None,
        message_types: list[str] | None = None,
        limit: int | None = None,
    ) -> dict[str, Any]:
        """
        Read messages with optional filtering and pagination.

        Args:
            start_index: First message index to return (0-based, inclusive).
            end_index:   Last message index to return (0-based, inclusive).
            message_types: If given, only messages whose source matches one of
                these type strings are returned.
            limit:       Maximum number of messages to return.

        Returns:
            Dictionary with total_messages, returned_count, start_index,
            end_index, and a messages list.
        """
        if message_types is not None:
            valid_types = AIMessage.get_message_types()
            invalid = set(message_types) - valid_types
            assert not invalid, (
                f"Invalid message types: {', '.join(sorted(invalid))}. "
                f"Valid types: {', '.join(sorted(valid_types))}"
            )

        history = self._transcript.get_conversation_history()
        messages = history.get_messages()

        effective_start = max(0, start_index) if start_index is not None else 0
        effective_end = (
            min(len(messages) - 1, end_index) if end_index is not None
            else len(messages) - 1
        )

        filtered = messages[effective_start:effective_end + 1]

        if message_types:
            filtered = [m for m in filtered if m.source_str() in message_types]

        if limit and limit > 0:
            filtered = filtered[:limit]

        result_messages = []
        for msg in filtered:
            msg_dict = msg.to_transcript_dict()
            msg_dict["index"] = messages.index(msg)
            result_messages.append(msg_dict)

        return {
            "total_messages": len(messages),
            "returned_count": len(result_messages),
            "start_index": effective_start,
            "end_index": effective_end,
            "messages": result_messages,
        }

    def get_message_by_id_or_index(
        self,
        message_id: str | None = None,
        message_index: int | None = None,
    ) -> dict[str, Any] | None:
        """
        Return a single message by UUID or 0-based index.

        Args:
            message_id:    UUID of the message to retrieve.
            message_index: 0-based index of the message to retrieve.

        Returns:
            Message dict (with an added 'index' key) or None if not found.

        Raises:
            ValueError: If neither argument is provided.
        """
        if message_id is None and message_index is None:
            raise ValueError("Must provide either message_id or message_index")

        history = self._transcript.get_conversation_history()
        messages = history.get_messages()

        if message_index is not None:
            if 0 <= message_index < len(messages):
                msg = messages[message_index]
                msg_dict = msg.to_transcript_dict()
                msg_dict["index"] = message_index
                return msg_dict

            return None

        for idx, msg in enumerate(messages):
            if msg.id == message_id:
                msg_dict = msg.to_transcript_dict()
                msg_dict["index"] = idx
                return msg_dict

        return None

    def search_messages(
        self,
        search_text: str,
        case_sensitive: bool = False,
        message_types: list[str] | None = None,
        max_results: int = 50,
        regexp: bool = False,
    ) -> dict[str, Any]:
        """
        Search for text across all messages.

        Args:
            search_text:   Text or regular expression to search for.
            case_sensitive: Whether the search is case-sensitive.
            message_types: If given, only search messages of these types.
            max_results:   Maximum number of matches to return.
            regexp:        If True, treat search_text as a regular expression.

        Returns:
            Dictionary with search_text, case_sensitive, total_matches,
            returned_count, and a matches list.

        Raises:
            ValueError: If regexp is True and search_text is not a valid regex.
        """
        if message_types is not None:
            valid_types = AIMessage.get_message_types()
            invalid = set(message_types) - valid_types
            assert not invalid, (
                f"Invalid message types: {', '.join(sorted(invalid))}. "
                f"Valid types: {', '.join(sorted(valid_types))}"
            )

        if not search_text:
            return {
                "search_text": search_text,
                "case_sensitive": case_sensitive,
                "total_matches": 0,
                "returned_count": 0,
                "matches": [],
            }

        history = self._transcript.get_conversation_history()
        messages = history.get_messages()
        matches: list[dict[str, Any]] = []

        if regexp:
            flags = 0 if case_sensitive else re.IGNORECASE
            try:
                pattern = re.compile(search_text, flags)

            except re.error as e:
                raise ValueError(f"Invalid regular expression: {e}") from e

        else:
            pattern = None

        for idx, msg in enumerate(messages):
            if message_types and msg.source_str() not in message_types:
                continue

            content = msg.content

            if pattern is not None:
                for m in pattern.finditer(content):
                    pos = m.start()
                    context_start = max(0, pos - 50)
                    context_end = min(len(content), m.end() + 50)
                    matches.append({
                        "message_index": idx,
                        "message_id": msg.id,
                        "message_type": msg.source_str(),
                        "timestamp": msg.timestamp.isoformat(),
                        "match_position": pos,
                        "context_before": content[context_start:pos],
                        "match_text": m.group(),
                        "context_after": content[m.end():context_end],
                    })
                    if len(matches) >= max_results:
                        break

            else:
                search_str = search_text if case_sensitive else search_text.lower()
                haystack = content if case_sensitive else content.lower()
                pos = 0
                while True:
                    pos = haystack.find(search_str, pos)
                    if pos == -1:
                        break

                    context_start = max(0, pos - 50)
                    context_end = min(len(content), pos + len(search_text) + 50)
                    matches.append({
                        "message_index": idx,
                        "message_id": msg.id,
                        "message_type": msg.source_str(),
                        "timestamp": msg.timestamp.isoformat(),
                        "match_position": pos,
                        "context_before": content[context_start:pos],
                        "match_text": content[pos:pos + len(search_text)],
                        "context_after": content[pos + len(search_text):context_end],
                    })
                    if len(matches) >= max_results:
                        break

                    pos += 1

            if len(matches) >= max_results:
                break

        return {
            "search_text": search_text,
            "case_sensitive": case_sensitive,
            "total_matches": len(matches),
            "returned_count": len(matches),
            "matches": matches,
        }

    def scroll_to_message(
        self,
        message_id: str | None = None,
        message_index: int | None = None,
    ) -> dict[str, Any]:
        """
        Request that the frontend scroll to a specific message.

        Resolves the target message from history (so the result dict always
        contains the actual index), then fires the on_scroll_to_message
        callback if one was supplied.  A CLI frontend would supply None and
        this becomes a no-op beyond the index resolution.

        Args:
            message_id:    UUID of the message to scroll to.
            message_index: 0-based index of the message to scroll to.

        Returns:
            Dictionary with success, target_index, actual_index, message_id,
            and an optional error key.
        """
        if message_id is None and message_index is None:
            return {"success": False, "error": "Must provide either message_id or message_index"}

        history = self._transcript.get_conversation_history()
        messages = history.get_messages()

        # Resolve index from ID if needed
        if message_id is not None and message_index is None:
            for idx, msg in enumerate(messages):
                if msg.id == message_id:
                    message_index = idx
                    break

            if message_index is None:
                return {"success": False, "error": f"Message not found: {message_id}"}

        if message_index is not None and not 0 <= message_index < len(messages):
            return {
                "success": False,
                "error": f"Message index {message_index} out of range (0-{len(messages) - 1})",
            }

        actual_index = message_index
        actual_id = messages[actual_index].id if actual_index is not None else None

        if self._on_scroll_to_message is not None:
            result = self._on_scroll_to_message(message_id=actual_id, message_index=actual_index)
            if isinstance(result, dict):
                return result

        return {
            "success": True,
            "target_index": message_index,
            "actual_index": actual_index,
            "message_id": actual_id,
        }

    @staticmethod
    def _serialize_parent(parent: Any) -> Any:
        """Serialize an AIConversationParent to a JSON-safe dict, or None."""
        if parent is None:
            return None

        return {"message_id": parent.message_id, "tool_call_id": parent.tool_call_id}

    def _get_history(self) -> AIConversationHistory:
        """Return the current conversation history."""
        return self._transcript.get_conversation_history()
