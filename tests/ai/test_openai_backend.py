"""Tests for the OpenAI Responses API backend."""

from ai.ai_conversation_history import AIConversationHistory
from ai.ai_conversation_settings import AIConversationSettings
from ai.ai_message import AIMessage, AIMessageSource
from ai.ai_model import AIReasoningCapability
from ai.openai.openai_backend import OpenAIBackend
from ai.openai.openai_stream_response import OpenAIStreamResponse
from ai_tool import AIToolCall, AIToolDefinition, AIToolParameter, AIToolResult


def _backend() -> OpenAIBackend:
    """Create an OpenAI backend pointed at the Responses endpoint."""
    return OpenAIBackend(api_key="test-key", api_url="https://api.openai.com/v1/responses")


def _settings() -> AIConversationSettings:
    """Create settings for a tool-capable OpenAI model."""
    return AIConversationSettings(
        model="gpt-6-astra",
        provider="openai",
        temperature=0.7,
        reasoning=AIReasoningCapability.HIDDEN_REASONING,
    )


class TestFetchModels:
    """Tests for model fetching URL derivation."""

    def test_models_url_is_derived_from_responses_url(self) -> None:
        """The models endpoint replaces the responses path segment."""
        backend = _backend()

        assert backend._api_url.replace("/responses", "/models") == "https://api.openai.com/v1/models"


class TestToolDefinitionFormat:
    """Tests for Responses API tool definition formatting."""

    def test_function_tool_is_flattened(self) -> None:
        """Tool definitions use the flattened Responses format without a nested function object."""
        backend = _backend()
        tool_def = AIToolDefinition(
            name="read_file",
            description="Read a file",
            parameters=[
                AIToolParameter(name="path", type="string", description="File path"),
            ],
        )

        formatted = backend._format_tool_definition(tool_def)

        assert formatted["type"] == "function"
        assert formatted["name"] == "read_file"
        assert formatted["description"] == "Read a file"
        assert formatted["strict"] is False
        assert "function" not in formatted
        assert formatted["parameters"]["properties"]["path"]["type"] == "string"
        assert formatted["parameters"]["required"] == ["path"]


class TestRequestConfig:
    """Tests for building the Responses API request configuration."""

    def test_uses_input_and_instructions(self) -> None:
        """The request uses input items and a top-level instructions field."""
        backend = _backend()
        OpenAIBackend.set_system_prompt("You are helpful.")
        history = AIConversationHistory()
        history.add_message(AIMessage.create(AIMessageSource.USER, "Hello"))

        config = backend._build_request_config(history, _settings())

        assert config.url == "https://api.openai.com/v1/responses"
        assert config.data["store"] is False
        assert config.data["stream"] is True
        assert config.data["instructions"] == "You are helpful."
        assert "messages" not in config.data
        assert config.data["input"] == [{"role": "user", "content": "Hello"}]

        OpenAIBackend.set_system_prompt(None)

    def test_reasoning_effort_is_nested(self) -> None:
        """Reasoning effort is sent as a nested reasoning object."""
        backend = _backend()
        history = AIConversationHistory()
        history.add_message(AIMessage.create(AIMessageSource.USER, "Hi"))
        settings = AIConversationSettings(
            model="gpt-6-astra",
            provider="openai",
            reasoning_effort="high",
        )

        config = backend._build_request_config(history, settings)

        assert config.data["reasoning"] == {"effort": "high"}

    def test_temperature_omitted_when_unsupported(self) -> None:
        """Temperature is omitted for models that do not support it."""
        backend = _backend()
        history = AIConversationHistory()
        history.add_message(AIMessage.create(AIMessageSource.USER, "Hi"))

        config = backend._build_request_config(history, _settings())

        assert "temperature" not in config.data


class TestInputFormatting:
    """Tests for formatting conversation history as input items."""

    def test_assistant_tool_calls_become_function_call_items(self) -> None:
        """Assistant tool calls are replayed as function_call items linked by call_id."""
        backend = _backend()
        history = AIConversationHistory()
        history.add_message(AIMessage.create(AIMessageSource.USER, "Do it"))
        history.add_message(AIMessage.create(
            AIMessageSource.AI,
            "",
            tool_calls=[AIToolCall(id="call_1", name="read_file", arguments={"path": "a.txt"})],
        ))
        history.add_message(AIMessage.create(
            AIMessageSource.USER,
            "",
            tool_results=[AIToolResult(id="call_1", name="read_file", content="contents")],
        ))

        items = backend._format_input_for_provider(history)

        assert items[0] == {"role": "user", "content": "Do it"}
        assert items[1]["type"] == "function_call"
        assert items[1]["call_id"] == "call_1"
        assert items[1]["name"] == "read_file"
        assert items[2]["type"] == "function_call_output"
        assert items[2]["call_id"] == "call_1"
        assert items[2]["output"] == "contents"

    def test_encrypted_reasoning_is_replayed(self) -> None:
        """Encrypted reasoning is replayed as a reasoning item before the assistant output."""
        backend = _backend()
        history = AIConversationHistory()
        history.add_message(AIMessage.create(AIMessageSource.USER, "Think"))
        history.add_message(AIMessage.create(
            AIMessageSource.AI,
            "Answer",
            redacted_reasoning="encrypted-blob",
        ))
        history.add_message(AIMessage.create(AIMessageSource.USER, "Next"))

        items = backend._format_input_for_provider(history)

        assert items[1] == {"type": "reasoning", "encrypted_content": "encrypted-blob"}
        assert items[2] == {"type": "message", "role": "assistant", "content": "Answer"}

    def test_unfinished_turn_is_removed(self) -> None:
        """An incomplete assistant turn before the latest user message is discarded."""
        backend = _backend()
        history = AIConversationHistory()
        history.add_message(AIMessage.create(AIMessageSource.USER, "First"))
        history.add_message(AIMessage.create(AIMessageSource.AI, "Partial", completed=False))
        history.add_message(AIMessage.create(AIMessageSource.USER, "Second"))

        items = backend._format_input_for_provider(history)

        assert items == [{"role": "user", "content": "Second"}]


class TestStreamResponse:
    """Tests for parsing Responses API streaming events."""

    def test_output_text_delta_accumulates_content(self) -> None:
        """Text deltas are concatenated into the content."""
        handler = OpenAIStreamResponse()

        handler.update_from_chunk({"type": "response.output_text.delta", "delta": "Hello "})
        handler.update_from_chunk({"type": "response.output_text.delta", "delta": "world"})

        assert handler.content == "Hello world"

    def test_output_item_done_function_call(self) -> None:
        """A completed function_call item yields a tool call."""
        handler = OpenAIStreamResponse()

        handler.update_from_chunk({
            "type": "response.output_item.done",
            "item": {
                "id": "item_1",
                "type": "function_call",
                "call_id": "call_1",
                "name": "read_file",
                "arguments": '{"path": "a.txt"}',
            },
        })

        assert len(handler.tool_calls) == 1
        assert handler.tool_calls[0].id == "call_1"
        assert handler.tool_calls[0].name == "read_file"
        assert handler.tool_calls[0].arguments == {"path": "a.txt"}

    def test_repeated_function_call_events_are_not_duplicated(self) -> None:
        """A function call is recorded once even when its item is emitted more than once."""
        handler = OpenAIStreamResponse()

        item = {
            "id": "item_1",
            "type": "function_call",
            "call_id": "call_1",
            "name": "read_file",
            "arguments": '{"path": "a.txt"}',
        }
        handler.update_from_chunk({"type": "response.output_item.done", "item": item})
        handler.update_from_chunk({"type": "response.output_item.done", "item": item})

        assert len(handler.tool_calls) == 1
        assert handler.tool_calls[0].id == "call_1"

    def test_completed_event_records_usage(self) -> None:
        """The completed event carries token usage."""
        handler = OpenAIStreamResponse()

        handler.update_from_chunk({
            "type": "response.completed",
            "response": {
                "usage": {
                    "input_tokens": 10,
                    "output_tokens": 5,
                    "total_tokens": 15,
                    "input_tokens_details": {"cached_tokens": 3},
                }
            },
        })

        assert handler.usage is not None
        assert handler.usage.prompt_tokens == 10
        assert handler.usage.completion_tokens == 5
        assert handler.usage.total_tokens == 15
        assert handler.usage.cache_read_tokens == 3

    def test_reasoning_item_captures_encrypted_content(self) -> None:
        """A reasoning output item captures its encrypted content."""
        handler = OpenAIStreamResponse()

        handler.update_from_chunk({
            "type": "response.output_item.done",
            "item": {"id": "rs_1", "type": "reasoning", "encrypted_content": "encrypted-blob"},
        })

        assert handler.encrypted_reasoning == "encrypted-blob"

    def test_error_event_sets_error(self) -> None:
        """An error event populates the handler error."""
        handler = OpenAIStreamResponse()

        handler.update_from_chunk({"type": "error", "message": "Something went wrong"})

        assert handler.error is not None
        assert handler.error.message == "Something went wrong"
