"""
Tests for the clock tool
"""
import asyncio
from datetime import datetime, timezone, timedelta
import re
from unittest.mock import patch, AsyncMock
import zoneinfo

import pytest

from ai_tool import AIToolDefinition, AIToolParameter, AIToolExecutionError, AITool
from ai_tool.tools.clock_ai_tool import ClockAITool


@pytest.fixture
def clock_tool():
    """Fixture providing a clock tool instance."""
    return ClockAITool()


@pytest.fixture
def mock_datetime():
    """Fixture providing a mocked datetime for predictable testing."""
    mock_dt = datetime(2023, 12, 25, 14, 30, 45, 123456, tzinfo=timezone.utc)
    with patch('ai_tool.tools.clock_ai_tool.datetime') as mock:
        mock.now.return_value = mock_dt
        mock.fromtimestamp.return_value = mock_dt
        mock.fromisoformat.return_value = mock_dt
        yield mock_dt


@pytest.fixture
def mock_authorization():
    """Fixture providing a mocked authorization callback."""
    async def mock_auth_callback(tool_name, arguments, context, destructive):
        return True  # Default to authorized

    return mock_auth_callback


class TestClockAIToolDefinition:
    """Test the clock tool definition."""

    def test_get_definition_returns_correct_structure(self, clock_tool):
        """Test that get_definition returns the correct tool definition structure."""
        definition = clock_tool.get_definition()

        assert isinstance(definition, AIToolDefinition)
        assert definition.name == "clock"
        assert "The clock tool" in definition.description
        assert "Available operations are:" in definition.description
        assert len(definition.parameters) == 5

    def test_operation_parameter_definition(self, clock_tool):
        """Test the operation parameter definition."""
        definition = clock_tool.get_definition()
        operation_param = definition.parameters[0]

        assert isinstance(operation_param, AIToolParameter)
        assert operation_param.name == "operation"
        assert operation_param.type == "string"
        assert operation_param.description == "Clock operation to perform"
        assert operation_param.required is True
        assert operation_param.enum == ["get_time", "sleep", "alarm"]

    def test_format_parameter_definition(self, clock_tool):
        """Test the format parameter definition."""
        definition = clock_tool.get_definition()
        format_param = definition.parameters[1]

        assert isinstance(format_param, AIToolParameter)
        assert format_param.name == "format"
        assert format_param.type == "string"
        assert format_param.description == "Time format ('iso' or 'timestamp')"
        assert format_param.required is False
        assert format_param.enum == ["iso", "timestamp"]

    def test_timezone_parameter_definition(self, clock_tool):
        """Test the timezone parameter definition."""
        definition = clock_tool.get_definition()
        timezone_param = definition.parameters[2]

        assert isinstance(timezone_param, AIToolParameter)
        assert timezone_param.name == "timezone"
        assert timezone_param.type == "string"
        assert timezone_param.description == "Timezone (e.g., 'UTC', 'America/New_York')"
        assert timezone_param.required is False
        assert timezone_param.enum is None

    def test_duration_parameter_definition(self, clock_tool):
        """Test the duration parameter definition."""
        definition = clock_tool.get_definition()
        duration_param = definition.parameters[3]

        assert isinstance(duration_param, AIToolParameter)
        assert duration_param.name == "duration"
        assert duration_param.type == "number"
        assert duration_param.description == "Duration to sleep in seconds (for sleep operation)"
        assert duration_param.required is False

    def test_time_parameter_definition(self, clock_tool):
        """Test the time parameter definition."""
        definition = clock_tool.get_definition()
        time_param = definition.parameters[4]

        assert isinstance(time_param, AIToolParameter)
        assert time_param.name == "time"
        assert time_param.type == "string"
        assert time_param.description == "Target time for alarm (ISO format or timestamp)"
        assert time_param.required is False


class TestGetTimeOperation:
    """Test the get_time operation."""

    def test_execute_get_time_default_format(self, clock_tool, mock_datetime, mock_authorization, make_tool_call):
        """Test get_time with default format (should be iso)."""
        tool_call = make_tool_call("clock", {"operation": "get_time"})
        result = asyncio.run(clock_tool.execute(tool_call, "", mock_authorization))

        assert result.content == "2023-12-25T14:30:45.123456Z"

    def test_execute_get_time_iso_format_explicit(self, clock_tool, mock_datetime, mock_authorization, make_tool_call):
        """Test get_time with explicit iso format."""
        tool_call = make_tool_call("clock", {"operation": "get_time", "format": "iso"})
        result = asyncio.run(clock_tool.execute(tool_call, "", mock_authorization))

        assert result.content == "2023-12-25T14:30:45.123456Z"

    def test_execute_get_time_timestamp_format(self, clock_tool, mock_datetime, mock_authorization, make_tool_call):
        """Test get_time with timestamp format."""
        tool_call = make_tool_call("clock", {"operation": "get_time", "format": "timestamp"})
        result = asyncio.run(clock_tool.execute(tool_call, "", mock_authorization))

        # Calculate expected timestamp
        expected_timestamp = str(int(mock_datetime.timestamp()))
        assert result.content == expected_timestamp

    def test_execute_get_time_with_utc_timezone(self, clock_tool, mock_datetime, mock_authorization, make_tool_call):
        """Test get_time with UTC timezone parameter."""
        tool_call = make_tool_call("clock", {
            "operation": "get_time",
            "format": "iso",
            "timezone": "UTC"
        })
        result = asyncio.run(clock_tool.execute(tool_call, "", mock_authorization))

        assert result.content == "2023-12-25T14:30:45.123456Z"

    def test_execute_get_time_invalid_format_falls_back_to_iso(self, clock_tool, mock_datetime, mock_authorization, make_tool_call):
        """Test get_time with invalid format falls back to iso."""
        tool_call = make_tool_call("clock", {"operation": "get_time", "format": "invalid"})
        result = asyncio.run(clock_tool.execute(tool_call, "", mock_authorization))

        assert result.content == "2023-12-25T14:30:45.123456Z"

    def test_execute_get_time_none_format(self, clock_tool, mock_datetime, mock_authorization, make_tool_call):
        """Test get_time with None format value."""
        tool_call = make_tool_call("clock", {"operation": "get_time", "format": None})
        result = asyncio.run(clock_tool.execute(tool_call, "", mock_authorization))

        assert result.content == "2023-12-25T14:30:45.123456Z"

    @pytest.mark.parametrize("format_type,expected_pattern", [
        ("iso", r"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{6}Z$"),
        ("timestamp", r"^\d+$"),
        ("invalid", r"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{6}Z$"),  # Falls back to iso
        (None, r"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{6}Z$"),  # Falls back to iso
    ])
    def test_get_time_format_patterns(self, clock_tool, mock_datetime, mock_authorization, make_tool_call, format_type, expected_pattern):
        """Test that different formats produce expected output patterns."""
        arguments = {"operation": "get_time"}
        if format_type is not None:
            arguments["format"] = format_type

        tool_call = make_tool_call("clock", arguments)
        result = asyncio.run(clock_tool.execute(tool_call, "", mock_authorization))

        assert re.match(expected_pattern, result.content), f"Format {format_type} produced unexpected output: {result.content}"


class TestSleepOperation:
    """Test the sleep operation."""

    @patch('asyncio.sleep', new_callable=AsyncMock)
    def test_execute_sleep_basic(self, mock_sleep, clock_tool, mock_datetime, mock_authorization, make_tool_call):
        """Test basic sleep operation."""
        tool_call = make_tool_call("clock", {"operation": "sleep", "duration": 5.0})
        result = asyncio.run(clock_tool.execute(tool_call, "", mock_authorization))

        mock_sleep.assert_called_once_with(5.0)
        assert result.content == "2023-12-25T14:30:45.123456Z"

    @patch('asyncio.sleep', new_callable=AsyncMock)
    def test_execute_sleep_with_format(self, mock_sleep, clock_tool, mock_datetime, mock_authorization, make_tool_call):
        """Test sleep operation with timestamp format."""
        tool_call = make_tool_call("clock", {"operation": "sleep", "duration": 2.5, "format": "timestamp"})
        result = asyncio.run(clock_tool.execute(tool_call, "", mock_authorization))

        mock_sleep.assert_called_once_with(2.5)
        expected_timestamp = str(int(mock_datetime.timestamp()))
        assert result.content == expected_timestamp

    @patch('asyncio.sleep', new_callable=AsyncMock)
    def test_execute_sleep_with_timezone(self, mock_sleep, clock_tool, mock_datetime, mock_authorization, make_tool_call):
        """Test sleep operation with timezone parameter."""
        tool_call = make_tool_call("clock", {
            "operation": "sleep",
            "duration": 1.0,
            "format": "iso",
            "timezone": "UTC"
        })
        result = asyncio.run(clock_tool.execute(tool_call, "", mock_authorization))

        mock_sleep.assert_called_once_with(1.0)
        assert result.content == "2023-12-25T14:30:45.123456Z"

    def test_execute_sleep_missing_duration(self, clock_tool, mock_authorization, make_tool_call):
        """Test sleep operation without duration parameter."""
        tool_call = make_tool_call("clock", {"operation": "sleep"})

        with pytest.raises(AIToolExecutionError):
            asyncio.run(clock_tool.execute(tool_call, "", mock_authorization))

    def test_execute_sleep_invalid_duration_type(self, clock_tool, mock_authorization, make_tool_call):
        """Test sleep operation with invalid duration type."""
        tool_call = make_tool_call("clock", {"operation": "sleep", "duration": "invalid"})

        with pytest.raises(AIToolExecutionError):
            asyncio.run(clock_tool.execute(tool_call, "", mock_authorization))

    def test_execute_sleep_negative_duration(self, clock_tool, mock_authorization, make_tool_call):
        """Test sleep operation with negative duration."""
        tool_call = make_tool_call("clock", {"operation": "sleep", "duration": -1.0})

        with pytest.raises(AIToolExecutionError):
            asyncio.run(clock_tool.execute(tool_call, "", mock_authorization))

    @patch('asyncio.sleep', new_callable=AsyncMock)
    def test_execute_sleep_zero_duration(self, mock_sleep, clock_tool, mock_datetime, mock_authorization, make_tool_call):
        """Test sleep operation with zero duration."""
        tool_call = make_tool_call("clock", {"operation": "sleep", "duration": 0.0})
        result = asyncio.run(clock_tool.execute(tool_call, "", mock_authorization))

        mock_sleep.assert_called_once_with(0.0)
        assert result.content == "2023-12-25T14:30:45.123456Z"

    @patch('asyncio.sleep', new_callable=AsyncMock)
    def test_execute_sleep_cancellation(self, mock_sleep, clock_tool, mock_authorization, make_tool_call):
        """Test sleep operation cancellation."""
        mock_sleep.side_effect = asyncio.CancelledError()

        tool_call = make_tool_call("clock", {"operation": "sleep", "duration": 5.0})

        with pytest.raises(asyncio.CancelledError):
            asyncio.run(clock_tool.execute(tool_call, "", mock_authorization))

    @pytest.mark.parametrize("duration", [0.1, 1.0, 5.5, 10, 60.0])
    def test_execute_sleep_various_durations(self, clock_tool, mock_datetime, mock_authorization, make_tool_call, duration):
        """Test sleep operation with various duration values."""
        with patch('asyncio.sleep', new_callable=AsyncMock) as mock_sleep:
            tool_call = make_tool_call("clock", {"operation": "sleep", "duration": duration})
            result = asyncio.run(clock_tool.execute(tool_call, "", mock_authorization))

            mock_sleep.assert_called_once_with(duration)
            assert isinstance(result.content, str)
            assert len(result.content) > 0


class TestAlarmOperation:
    """Test the alarm operation."""

    @patch('asyncio.sleep', new_callable=AsyncMock)
    def test_execute_alarm_future_time_iso(self, mock_sleep, clock_tool, mock_authorization, make_tool_call):
        """Test alarm operation with future time in ISO format."""
        # Mock current time and target time
        current_time = datetime(2023, 12, 25, 14, 30, 45, 123456, tzinfo=timezone.utc)
        target_time = datetime(2023, 12, 25, 15, 30, 45, 123456, tzinfo=timezone.utc)  # 1 hour later

        with patch('ai_tool.tools.clock_ai_tool.datetime') as mock_datetime:
            mock_datetime.now.return_value = current_time
            mock_datetime.fromisoformat.return_value = target_time

            tool_call = make_tool_call("clock", {
                "operation": "alarm",
                "time": "2023-12-25T15:30:45.123456"
            })
            result = asyncio.run(clock_tool.execute(tool_call, "", mock_authorization))

            # Should sleep for 1 hour (3600 seconds)
            mock_sleep.assert_called_once_with(3600.0)
            assert result.content == "2023-12-25T14:30:45.123456Z"

    @patch('asyncio.sleep', new_callable=AsyncMock)
    def test_execute_alarm_future_time_timestamp(self, mock_sleep, clock_tool, mock_authorization, make_tool_call):
        """Test alarm operation with future time as timestamp."""
        current_time = datetime(2023, 12, 25, 14, 30, 45, 123456, tzinfo=timezone.utc)
        target_timestamp = current_time.timestamp() + 1800  # 30 minutes later

        with patch('ai_tool.tools.clock_ai_tool.datetime') as mock_datetime:
            mock_datetime.now.return_value = current_time
            mock_datetime.fromtimestamp.return_value = datetime.fromtimestamp(target_timestamp, tz=timezone.utc)

            tool_call = make_tool_call("clock", {
                "operation": "alarm",
                "time": str(int(target_timestamp))
            })
            result = asyncio.run(clock_tool.execute(tool_call, "", mock_authorization))

            # Should sleep for 30 minutes (1800 seconds)
            mock_sleep.assert_called_once_with(1800.0)
            assert result.content == "2023-12-25T14:30:45.123456Z"

    def test_execute_alarm_past_time(self, clock_tool, mock_authorization, make_tool_call):
        """Test alarm operation with past time (should return immediately)."""
        current_time = datetime(2023, 12, 25, 14, 30, 45, 123456, tzinfo=timezone.utc)
        past_time = datetime(2023, 12, 25, 13, 30, 45, 123456, tzinfo=timezone.utc)  # 1 hour ago

        with patch('ai_tool.tools.clock_ai_tool.datetime') as mock_datetime:
            mock_datetime.now.return_value = current_time
            mock_datetime.fromisoformat.return_value = past_time

            with patch('asyncio.sleep', new_callable=AsyncMock) as mock_sleep:
                tool_call = make_tool_call("clock", {
                    "operation": "alarm",
                    "time": "2023-12-25T13:30:45.123456"
                })
                result = asyncio.run(clock_tool.execute(tool_call, "", mock_authorization))

                # Should not sleep at all
                mock_sleep.assert_not_called()
                assert result.content == "2023-12-25T14:30:45.123456Z"

    @patch('asyncio.sleep', new_callable=AsyncMock)
    def test_execute_alarm_with_format(self, mock_sleep, clock_tool, mock_authorization, make_tool_call):
        """Test alarm operation with timestamp format."""
        current_time = datetime(2023, 12, 25, 14, 30, 45, 123456, tzinfo=timezone.utc)
        target_time = datetime(2023, 12, 25, 14, 30, 46, 123456, tzinfo=timezone.utc)  # 1 second later

        with patch('ai_tool.tools.clock_ai_tool.datetime') as mock_datetime:
            mock_datetime.now.return_value = current_time
            mock_datetime.fromisoformat.return_value = target_time

            tool_call = make_tool_call("clock", {
                "operation": "alarm",
                "time": "2023-12-25T14:30:46.123456",
                "format": "timestamp"
            })
            result = asyncio.run(clock_tool.execute(tool_call, "", mock_authorization))

            mock_sleep.assert_called_once_with(1.0)
            expected_timestamp = str(int(current_time.timestamp()))
            assert result.content == expected_timestamp

    def test_execute_alarm_missing_time(self, clock_tool, mock_authorization, make_tool_call):
        """Test alarm operation without time parameter."""
        tool_call = make_tool_call("clock", {"operation": "alarm"})

        with pytest.raises(AIToolExecutionError):
            asyncio.run(clock_tool.execute(tool_call, "", mock_authorization))

    def test_execute_alarm_invalid_time_type(self, clock_tool, mock_authorization, make_tool_call):
        """Test alarm operation with invalid time type."""
        tool_call = make_tool_call("clock", {"operation": "alarm", "time": 123})

        with pytest.raises(AIToolExecutionError):
            asyncio.run(clock_tool.execute(tool_call, "", mock_authorization))

    def test_execute_alarm_invalid_time_format(self, clock_tool, mock_authorization, make_tool_call):
        """Test alarm operation with invalid time format."""
        with patch('ai_tool.tools.clock_ai_tool.datetime') as mock_datetime:
            mock_datetime.fromisoformat.side_effect = ValueError("Invalid format")

            tool_call = make_tool_call("clock", {"operation": "alarm", "time": "invalid-time"})

            with pytest.raises(AIToolExecutionError):
                asyncio.run(clock_tool.execute(tool_call, "", mock_authorization))

    @patch('asyncio.sleep', new_callable=AsyncMock)
    def test_execute_alarm_cancellation(self, mock_sleep, clock_tool, mock_authorization, make_tool_call):
        """Test alarm operation cancellation."""
        mock_sleep.side_effect = asyncio.CancelledError()

        current_time = datetime(2023, 12, 25, 14, 30, 45, 123456, tzinfo=timezone.utc)
        target_time = datetime(2023, 12, 25, 15, 30, 45, 123456, tzinfo=timezone.utc)

        with patch('ai_tool.tools.clock_ai_tool.datetime') as mock_datetime:
            mock_datetime.now.return_value = current_time
            mock_datetime.fromisoformat.return_value = target_time

            tool_call = make_tool_call("clock", {
                "operation": "alarm",
                "time": "2023-12-25T15:30:45.123456"
            })

            with pytest.raises(asyncio.CancelledError):
                asyncio.run(clock_tool.execute(tool_call, "", mock_authorization))

    @pytest.mark.parametrize("time_offset_seconds", [1, 60, 300, 1800, 3600])
    def test_execute_alarm_various_durations(self, clock_tool, mock_authorization, make_tool_call, time_offset_seconds):
        """Test alarm operation with various future time offsets."""
        current_time = datetime(2023, 12, 25, 14, 30, 45, 123456, tzinfo=timezone.utc)
        target_time = current_time + timedelta(seconds=time_offset_seconds)

        with patch('ai_tool.tools.clock_ai_tool.datetime') as mock_datetime:
            mock_datetime.now.return_value = current_time
            mock_datetime.fromisoformat.return_value = target_time

            with patch('asyncio.sleep', new_callable=AsyncMock) as mock_sleep:
                tool_call = make_tool_call("clock", {
                    "operation": "alarm",
                    "time": target_time.isoformat()
                })
                result = asyncio.run(clock_tool.execute(tool_call, "", mock_authorization))

                mock_sleep.assert_called_once_with(float(time_offset_seconds))
                assert isinstance(result.content, str)
                assert len(result.content) > 0


class TestClockAIToolErrorHandling:
    """Test error handling across all operations."""

    def test_execute_missing_operation_parameter(self, clock_tool, mock_authorization, make_tool_call):
        """Test execution without operation parameter."""
        tool_call = make_tool_call("clock", {"format": "iso"})

        with pytest.raises(AIToolExecutionError):
            asyncio.run(clock_tool.execute(tool_call, "", mock_authorization))

    def test_execute_invalid_operation(self, clock_tool, mock_authorization, make_tool_call):
        """Test execution with invalid operation."""
        tool_call = make_tool_call("clock", {"operation": "invalid_operation"})

        with pytest.raises(AIToolExecutionError):
            asyncio.run(clock_tool.execute(tool_call, "", mock_authorization))

    def test_execute_timezone_error(self, clock_tool, mock_authorization, make_tool_call):
        """Test handling of invalid timezone."""
        with patch('ai_tool.tools.clock_ai_tool.zoneinfo.ZoneInfo') as mock_zoneinfo:
            mock_zoneinfo.side_effect = zoneinfo.ZoneInfoNotFoundError("Invalid timezone")

            tool_call = make_tool_call("clock", {
                "operation": "get_time",
                "timezone": "Invalid/Timezone"
            })

            with pytest.raises(AIToolExecutionError):
                asyncio.run(clock_tool.execute(tool_call, "", mock_authorization))

    def test_execute_datetime_system_error(self, clock_tool, mock_authorization, make_tool_call):
        """Test handling of system datetime errors."""
        with patch('ai_tool.tools.clock_ai_tool.datetime') as mock_datetime:
            mock_datetime.now.side_effect = OSError("System clock error")

            tool_call = make_tool_call("clock", {"operation": "get_time"})

            with pytest.raises(AIToolExecutionError):
                asyncio.run(clock_tool.execute(tool_call, "", mock_authorization))

    def test_execute_unexpected_error_wrapped(self, clock_tool, mock_authorization, make_tool_call):
        """Test that unexpected errors are properly wrapped."""
        with patch('ai_tool.tools.clock_ai_tool.datetime') as mock_datetime:
            mock_datetime.now.side_effect = RuntimeError("Unexpected error")

            tool_call = make_tool_call("clock", {"operation": "get_time"})

            with pytest.raises(AIToolExecutionError):
                asyncio.run(clock_tool.execute(tool_call, "", mock_authorization))


class TestClockAIToolIntegration:
    """Integration tests for the clock tool."""

    def test_real_datetime_get_time_execution(self, clock_tool, mock_authorization, make_tool_call):
        """Test get_time execution with real datetime (no mocking)."""
        tool_call = make_tool_call("clock", {"operation": "get_time", "format": "iso"})
        result = asyncio.run(clock_tool.execute(tool_call, "", mock_authorization))

        # Should be a valid ISO format string
        assert isinstance(result.content, str)
        assert "T" in result.content
        assert result.content.endswith("Z")  # Should now end with Z since we default to UTC

        # Should be parseable as a datetime
        parsed = datetime.fromisoformat(result.content.replace("Z", "+00:00"))
        assert isinstance(parsed, datetime)

    def test_real_datetime_all_operations_basic(self, clock_tool, mock_authorization, make_tool_call):
        """Test basic execution of all operations with real datetime."""
        # Test get_time
        get_time_call = make_tool_call("clock", {"operation": "get_time"})
        get_time_result = asyncio.run(clock_tool.execute(get_time_call, "", mock_authorization))
        assert isinstance(get_time_result.content, str)
        assert len(get_time_result.content) > 0

        # Test sleep (very short duration)
        sleep_call = make_tool_call("clock", {"operation": "sleep", "duration": 0.001})
        sleep_result = asyncio.run(clock_tool.execute(sleep_call, "", mock_authorization))
        assert isinstance(sleep_result.content, str)
        assert len(sleep_result.content) > 0

        # Test alarm (past time - should return immediately)
        past_time = "2020-01-01T00:00:00"
        alarm_call = make_tool_call("clock", {"operation": "alarm", "time": past_time})
        alarm_result = asyncio.run(clock_tool.execute(alarm_call, "", mock_authorization))
        assert isinstance(alarm_result.content, str)
        assert len(alarm_result.content) > 0

    def test_tool_inheritance(self, clock_tool):
        """Test that ClockAITool properly inherits from AITool."""
        assert isinstance(clock_tool, AITool)
        assert hasattr(clock_tool, 'get_definition')
        assert hasattr(clock_tool, 'execute')
        assert callable(clock_tool.get_definition)
        assert callable(clock_tool.execute)

    @pytest.mark.parametrize("operation,format_type", [
        ("get_time", "iso"),
        ("get_time", "timestamp"),
        ("sleep", "iso"),
        ("sleep", "timestamp"),
        ("alarm", "iso"),
        ("alarm", "timestamp"),
    ])
    def test_all_operations_with_formats(self, clock_tool, mock_authorization, make_tool_call, operation, format_type):
        """Test all operations work with different format types."""
        arguments = {"operation": operation, "format": format_type}

        if operation == "sleep":
            arguments["duration"] = 0.001  # Very short sleep

        elif operation == "alarm":
            arguments["time"] = "2020-01-01T00:00:00"  # Past time

        tool_call = make_tool_call("clock", arguments)
        result = asyncio.run(clock_tool.execute(tool_call, "", mock_authorization))

        assert isinstance(result.content, str)
        assert len(result.content) > 0

        if format_type == "timestamp":
            assert result.content.isdigit()

        elif format_type == "iso":
            assert "T" in result.content or "-" in result.content
