"""
Tests for the clock tool
"""
import asyncio
from datetime import datetime
import re
from unittest.mock import patch, MagicMock

import pytest

from ai.tools.ai_tool_clock import AIToolClock
from ai.ai_tool_manager import AIToolDefinition, AIToolParameter, AIToolExecutionError, AITool


@pytest.fixture
def clock_tool():
    """Fixture providing a clock tool instance."""
    return AIToolClock()


@pytest.fixture
def mock_datetime():
    """Fixture providing a mocked datetime for predictable testing."""
    mock_dt = datetime(2023, 12, 25, 14, 30, 45, 123456)
    with patch('ai.tools.ai_tool_clock.datetime') as mock:
        mock.now.return_value = mock_dt
        yield mock_dt


@pytest.fixture
def mock_authorization():
    """Fixture providing a mocked authorization callback."""
    async def mock_auth_callback(tool_name, arguments, context, destructive):
        return True  # Default to authorized

    return mock_auth_callback


class TestAIToolClockDefinition:
    """Test the clock tool definition."""

    def test_get_definition_returns_correct_structure(self, clock_tool):
        """Test that get_definition returns the correct tool definition structure."""
        definition = clock_tool.get_definition()

        assert isinstance(definition, AIToolDefinition)
        assert definition.name == "get_current_time"
        assert definition.description == "Get the current date and time"
        assert len(definition.parameters) == 2

    def test_format_parameter_definition(self, clock_tool):
        """Test the format parameter definition."""
        definition = clock_tool.get_definition()
        format_param = definition.parameters[0]

        assert isinstance(format_param, AIToolParameter)
        assert format_param.name == "format"
        assert format_param.type == "string"
        assert format_param.description == "Time format ('iso', 'human', or 'timestamp')"
        assert format_param.required is False
        assert format_param.enum == ["iso", "human", "timestamp"]

    def test_timezone_parameter_definition(self, clock_tool):
        """Test the timezone parameter definition."""
        definition = clock_tool.get_definition()
        timezone_param = definition.parameters[1]

        assert isinstance(timezone_param, AIToolParameter)
        assert timezone_param.name == "timezone"
        assert timezone_param.type == "string"
        assert timezone_param.description == "Timezone (e.g., 'UTC', 'America/New_York')"
        assert timezone_param.required is False
        assert timezone_param.enum is None


class TestAIToolClockExecution:
    """Test the clock tool execution."""

    def test_execute_default_format(self, clock_tool, mock_datetime, mock_authorization):
        """Test execution with default format (should be iso)."""
        result = asyncio.run(clock_tool.execute({}, mock_authorization))

        assert result == "2023-12-25T14:30:45.123456Z"

    def test_execute_iso_format_explicit(self, clock_tool, mock_datetime, mock_authorization):
        """Test execution with explicit iso format."""
        result = asyncio.run(clock_tool.execute({"format": "iso"}, mock_authorization))

        assert result == "2023-12-25T14:30:45.123456Z"

    def test_execute_human_format(self, clock_tool, mock_datetime, mock_authorization):
        """Test execution with human format."""
        result = asyncio.run(clock_tool.execute({"format": "human"}, mock_authorization))

        assert result == "2023-12-25 14:30:45 UTC"

    def test_execute_timestamp_format(self, clock_tool, mock_datetime, mock_authorization):
        """Test execution with timestamp format."""
        result = asyncio.run(clock_tool.execute({"format": "timestamp"}, mock_authorization))

        # Calculate expected timestamp
        expected_timestamp = str(int(mock_datetime.timestamp()))
        assert result == expected_timestamp

    def test_execute_with_timezone_parameter(self, clock_tool, mock_datetime, mock_authorization):
        """Test execution with timezone parameter (currently ignored by implementation)."""
        result = asyncio.run(clock_tool.execute({
            "format": "iso",
            "timezone": "America/New_York"
        }, mock_authorization))

        # Current implementation ignores timezone, so should return UTC
        assert result == "2023-12-25T14:30:45.123456Z"

    def test_execute_invalid_format_falls_back_to_iso(self, clock_tool, mock_datetime, mock_authorization):
        """Test execution with invalid format falls back to iso."""
        result = asyncio.run(clock_tool.execute({"format": "invalid"}, mock_authorization))

        assert result == "2023-12-25T14:30:45.123456Z"

    def test_execute_empty_arguments(self, clock_tool, mock_datetime, mock_authorization):
        """Test execution with empty arguments dictionary."""
        result = asyncio.run(clock_tool.execute({}, mock_authorization))

        assert result == "2023-12-25T14:30:45.123456Z"

    def test_execute_none_format(self, clock_tool, mock_datetime, mock_authorization):
        """Test execution with None format value."""
        result = asyncio.run(clock_tool.execute({"format": None}, mock_authorization))

        assert result == "2023-12-25T14:30:45.123456Z"


class TestAIToolClockErrorHandling:
    """Test error handling in the clock tool."""

    def test_execute_datetime_exception_wrapped(self, clock_tool, mock_authorization):
        """Test that datetime exceptions are properly wrapped."""
        with patch('ai.tools.ai_tool_clock.datetime') as mock_datetime:
            mock_datetime.now.side_effect = OSError("System clock error")

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(clock_tool.execute({"format": "iso"}, mock_authorization))

            error = exc_info.value
            assert "Failed to get current time: System clock error" in str(error)
            assert error.tool_name == "get_current_time"
            assert error.arguments == {"format": "iso"}
            assert error.__cause__.__class__ == OSError

    def test_execute_timestamp_conversion_error(self, clock_tool, mock_authorization):
        """Test handling of timestamp conversion errors."""
        with patch('ai.tools.ai_tool_clock.datetime') as mock_datetime:
            # Create a datetime that will cause timestamp() to fail
            mock_dt = MagicMock()
            mock_dt.timestamp.side_effect = ValueError("Invalid timestamp")
            mock_dt.isoformat.return_value = "2023-12-25T14:30:45.123456"
            mock_datetime.now.return_value = mock_dt

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(clock_tool.execute({"format": "timestamp"}, mock_authorization))

            error = exc_info.value
            assert "Failed to get current time: Invalid timestamp" in str(error)

    def test_execute_isoformat_error(self, clock_tool, mock_authorization):
        """Test handling of isoformat errors."""
        with patch('ai.tools.ai_tool_clock.datetime') as mock_datetime:
            # Create a datetime that will cause isoformat() to fail
            mock_dt = MagicMock()
            mock_dt.isoformat.side_effect = AttributeError("No isoformat method")
            mock_datetime.now.return_value = mock_dt

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(clock_tool.execute({"format": "iso"}, mock_authorization))

            error = exc_info.value
            assert "Failed to get current time: No isoformat method" in str(error)

    def test_execute_strftime_error(self, clock_tool, mock_authorization):
        """Test handling of strftime errors."""
        with patch('ai.tools.ai_tool_clock.datetime') as mock_datetime:
            # Create a datetime that will cause strftime() to fail
            mock_dt = MagicMock()
            mock_dt.strftime.side_effect = ValueError("Invalid format string")
            mock_datetime.now.return_value = mock_dt

            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(clock_tool.execute({"format": "human"}, mock_authorization))

            error = exc_info.value
            assert "Failed to get current time: Invalid format string" in str(error)


class TestAIToolClockParametrizedFormats:
    """Parametrized tests for different format combinations."""

    @pytest.mark.parametrize("format_type,expected_pattern", [
        ("iso", r"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{6}Z$"),
        ("human", r"^\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2} UTC$"),
        ("timestamp", r"^\d+$"),
        ("invalid", r"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{6}Z$"),  # Falls back to iso
        (None, r"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{6}Z$"),  # Falls back to iso
    ])
    def test_format_patterns(self, clock_tool, mock_authorization, format_type, expected_pattern):
        """Test that different formats produce expected output patterns."""

        arguments = {}
        if format_type is not None:
            arguments["format"] = format_type

        result = asyncio.run(clock_tool.execute(arguments, mock_authorization))

        assert re.match(expected_pattern, result), f"Format {format_type} produced unexpected output: {result}"

    @pytest.mark.parametrize("timezone", [
        "UTC",
        "America/New_York",
        "Europe/London",
        "Asia/Tokyo",
        "Australia/Sydney",
        None,
        "",
        "Invalid/Timezone"
    ])
    def test_timezone_parameter_ignored(self, clock_tool, mock_authorization, timezone):
        """Test that timezone parameter is accepted but currently ignored."""
        arguments = {"format": "iso"}
        if timezone is not None:
            arguments["timezone"] = timezone

        # Should not raise an error regardless of timezone value
        result = asyncio.run(clock_tool.execute(arguments, mock_authorization))

        # Should always return UTC time (current implementation)
        assert result.endswith("Z")

    @pytest.mark.parametrize("arguments", [
        {},
        {"format": "iso"},
        {"timezone": "UTC"},
        {"format": "human", "timezone": "America/New_York"},
        {"extra_param": "ignored"},
        {"format": "timestamp", "timezone": "Europe/London", "extra": "value"}
    ])
    def test_various_argument_combinations(self, clock_tool, mock_authorization, arguments):
        """Test execution with various argument combinations."""
        # Should not raise exceptions for any valid argument combination
        result = asyncio.run(clock_tool.execute(arguments, mock_authorization))

        assert isinstance(result, str)
        assert len(result) > 0


class TestAIToolClockIntegration:
    """Integration tests for the clock tool."""

    def test_real_datetime_execution(self, clock_tool, mock_authorization):
        """Test execution with real datetime (no mocking)."""
        result = asyncio.run(clock_tool.execute({"format": "iso"}, mock_authorization))

        # Should be a valid ISO format string
        assert isinstance(result, str)
        assert "T" in result
        assert result.endswith("Z")

        # Should be parseable as a datetime
        parsed = datetime.fromisoformat(result.replace("Z", "+00:00"))
        assert isinstance(parsed, datetime)

    def test_real_datetime_all_formats(self, clock_tool, mock_authorization):
        """Test all formats with real datetime."""
        formats = ["iso", "human", "timestamp"]

        results = {}
        for fmt in formats:
            results[fmt] = asyncio.run(clock_tool.execute({"format": fmt}, mock_authorization))

        # All should be different formats of the same time
        assert len(results["iso"]) > 20  # ISO format is long
        assert "UTC" in results["human"]  # Human format contains UTC
        assert results["timestamp"].isdigit()  # Timestamp is all digits

    def test_tool_inheritance(self, clock_tool):
        """Test that AIToolClock properly inherits from AITool."""

        assert isinstance(clock_tool, AITool)
        assert hasattr(clock_tool, 'get_definition')
        assert hasattr(clock_tool, 'execute')
        assert callable(clock_tool.get_definition)
        assert callable(clock_tool.execute)
