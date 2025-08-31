import asyncio
import logging
from datetime import datetime, timezone
from typing import Any, Dict, List
import zoneinfo

from ai_tool import (
    AIToolDefinition, AIToolParameter, AITool, AIToolExecutionError,
    AIToolAuthorizationCallback, AIToolOperationDefinition, AIToolResult, AIToolCall
)


class ClockAITool(AITool):
    """Clock tool that provides time operations including sleep and alarm functionality."""

    def __init__(self):
        """Initialize the clock tool."""
        self._logger = logging.getLogger("ClockAITool")

    def get_definition(self) -> AIToolDefinition:
        """Get the tool definition."""
        operations = self.get_operation_definitions()
        operation_names: List[str] = list(operations.keys())

        # Build description from operations
        base_description = (
            "The clock tool lets you (the AI) perform operations related to the current date and/or time. It supports "
            "a number of different date/time formats:\n"
            "- timestamp: Unix timestamps in seconds\n"
            "- iso: Strict ISO format\n"
        )

        # Generate operations list
        operation_list = []
        for name, op_def in operations.items():
            operation_list.append(f"- {name}: {op_def.description}")

        description = f"{base_description}\nAvailable operations are:\n" + "\n".join(operation_list)


        return AIToolDefinition(
            name="clock",
            description=description,
            parameters=[
                AIToolParameter(
                    name="operation",
                    type="string",
                    description="Clock operation to perform",
                    required=True,
                    enum=operation_names
                ),
                AIToolParameter(
                    name="format",
                    type="string",
                    description="Time format ('iso' or 'timestamp')",
                    required=False,
                    enum=["iso", "timestamp"]
                ),
                AIToolParameter(
                    name="timezone",
                    type="string",
                    description="Timezone (e.g., 'UTC', 'America/New_York')",
                    required=False
                ),
                AIToolParameter(
                    name="duration",
                    type="number",
                    description="Duration to sleep in seconds (for sleep operation)",
                    required=False
                ),
                AIToolParameter(
                    name="time",
                    type="string",
                    description="Target time for alarm (ISO format or timestamp)",
                    required=False
                )
            ]
        )

    def get_operation_definitions(self) -> Dict[str, AIToolOperationDefinition]:
        """Get operation definitions for this tool."""
        return {
            "get_time": AIToolOperationDefinition(
                name="get_time",
                handler=self._get_time,
                allowed_parameters={"format", "timezone"},
                required_parameters=set(),
                description="get the current date and time"
            ),
            "sleep": AIToolOperationDefinition(
                name="sleep",
                handler=self._sleep,
                allowed_parameters={"duration", "format", "timezone"},
                required_parameters={"duration"},
                description="sleep for a specified number of seconds, then return current time"
            ),
            "alarm": AIToolOperationDefinition(
                name="alarm",
                handler=self._alarm,
                allowed_parameters={"time", "format", "timezone"},
                required_parameters={"time"},
                description="sleep until a specific time, then return current time"
            )
        }

    def _format_time(self, dt: datetime, format_type: str) -> str:
        """
        Format datetime according to the specified format.

        Args:
            dt: Datetime to format
            format_type: Format type ('iso' or 'timestamp')

        Returns:
            Formatted time string
        """
        if format_type == "iso":
            return dt.isoformat()[:26] + "Z" if dt.tzinfo == timezone.utc else dt.isoformat()

        if format_type == "timestamp":
            return str(int(dt.timestamp()))

        # Default to ISO format
        return dt.isoformat()[:26] + "Z" if dt.tzinfo == timezone.utc else dt.isoformat()

    def _get_current_time(self, timezone_str: str | None = None) -> datetime:
        """
        Get current time in the specified timezone.

        Args:
            timezone_str: Timezone string, defaults to UTC if None

        Returns:
            Current datetime in specified timezone

        Raises:
            AIToolExecutionError: If timezone is invalid
        """
        try:
            if timezone_str is None:
                # Default to UTC timezone
                return datetime.now(timezone.utc)

            if timezone_str.upper() == "UTC":
                return datetime.now(timezone.utc)

            # Try to parse as timezone
            tz = zoneinfo.ZoneInfo(timezone_str)
            return datetime.now(tz)

        except Exception as e:
            raise AIToolExecutionError(f"Invalid timezone '{timezone_str}': {str(e)}") from e

    def _parse_alarm_time(self, time_str: str, timezone_str: str | None = None) -> datetime:
        """
        Parse alarm time from various formats.

        Args:
            time_str: Time string in ISO or timestamp format
            timezone_str: Timezone to assume if not specified in time_str

        Returns:
            Parsed datetime

        Raises:
            AIToolExecutionError: If time format is invalid
        """
        time_str = time_str.strip()

        # Try timestamp format first (all digits, possibly with decimal)
        try:
            timestamp = float(time_str)
            return datetime.fromtimestamp(timestamp, tz=timezone.utc)

        except ValueError:
            pass

        # Try ISO format and other standard formats using fromisoformat
        try:
            # Handle various ISO-like formats
            parsed_dt = datetime.fromisoformat(time_str.replace('Z', '+00:00'))

            # If no timezone info, apply default or specified timezone
            if parsed_dt.tzinfo is None:
                if timezone_str is None or timezone_str.upper() == "UTC":
                    parsed_dt = parsed_dt.replace(tzinfo=timezone.utc)

                else:
                    tz = zoneinfo.ZoneInfo(timezone_str)
                    parsed_dt = parsed_dt.replace(tzinfo=tz)

            return parsed_dt

        except ValueError:
            pass

        # If all parsing attempts failed
        raise AIToolExecutionError(
            f"Unable to parse time '{time_str}'. Supported formats: ISO format (2024-01-15T14:30:00), timestamp (1705339800)"
        )

    async def execute(
        self,
        tool_call: AIToolCall,
        requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Execute the clock tool operation."""
        arguments = tool_call.arguments

        # Validate operation arguments
        self.validate_operation_arguments(arguments)

        # Get operation definition
        operation_definitions = self.get_operation_definitions()
        operation = arguments["operation"]
        operation_def = operation_definitions[operation]

        try:
            result = await operation_def.handler(arguments)

            return AIToolResult(
                id=tool_call.id,
                name="clock",
                content=result
            )

        except AIToolExecutionError:
            # Re-raise our own errors
            raise

        except Exception as e:
            self._logger.error("Unexpected error in clock operation '%s': %s", operation, str(e), exc_info=True)
            raise AIToolExecutionError(f"Clock operation failed: {str(e)}") from e

    async def _get_time(self, arguments: Dict[str, Any]) -> str:
        """Get current time operation."""
        format_type = arguments.get("format", "iso")
        timezone_str = arguments.get("timezone")

        try:
            now = self._get_current_time(timezone_str)
            return self._format_time(now, format_type)

        except Exception as e:
            raise AIToolExecutionError(f"Failed to get current time: {str(e)}") from e

    async def _sleep(self, arguments: Dict[str, Any]) -> str:
        """Sleep operation."""
        duration = arguments.get("duration")
        format_type = arguments.get("format", "iso")
        timezone_str = arguments.get("timezone")

        # Validate duration
        if duration is None:
            raise AIToolExecutionError("Duration parameter is required for sleep operation")

        if not isinstance(duration, (int, float)):
            raise AIToolExecutionError("Duration must be a number")

        if duration < 0:
            raise AIToolExecutionError("Duration cannot be negative")

        try:
            self._logger.debug("Sleeping for %s seconds", duration)

            # Perform the sleep
            await asyncio.sleep(duration)

            # Return current time after sleep
            now = self._get_current_time(timezone_str)
            result = self._format_time(now, format_type)

            self._logger.debug("Sleep completed, current time: %s", result)
            return result

        except asyncio.CancelledError:
            self._logger.info("Sleep operation was cancelled")
            raise

        except Exception as e:
            self._logger.error("Error during sleep operation: %s", str(e), exc_info=True)
            raise AIToolExecutionError(f"Failed to sleep: {str(e)}") from e

    async def _alarm(self, arguments: Dict[str, Any]) -> str:
        """Alarm operation - sleep until specified time."""
        time_str = arguments.get("time")
        format_type = arguments.get("format", "iso")
        timezone_str = arguments.get("timezone")

        # Validate time parameter
        if time_str is None:
            raise AIToolExecutionError("Time parameter is required for alarm operation")

        if not isinstance(time_str, str):
            raise AIToolExecutionError("Time must be a string")

        try:
            # Parse the target time
            target_time = self._parse_alarm_time(time_str, timezone_str)

            # Get current time (in same timezone as target for comparison)
            current_time = self._get_current_time(timezone_str)

            # If target time is in the past, return immediately
            if target_time <= current_time:
                self._logger.debug("Alarm time %s is in the past, returning immediately", target_time)
                now = self._get_current_time(timezone_str)
                return self._format_time(now, format_type)

            # Calculate sleep duration
            sleep_duration = (target_time - current_time).total_seconds()

            self._logger.debug("Alarm set for %s (sleeping %s seconds)", target_time, sleep_duration)

            # Perform the sleep
            await asyncio.sleep(sleep_duration)

            # Return current time after alarm
            now = self._get_current_time(timezone_str)
            result = self._format_time(now, format_type)

            self._logger.debug("Alarm completed, current time: %s", result)
            return result

        except asyncio.CancelledError:
            self._logger.info("Alarm operation was cancelled")
            raise

        except AIToolExecutionError:
            # Re-raise our own errors
            raise

        except Exception as e:
            self._logger.error("Error during alarm operation: %s", str(e), exc_info=True)
            raise AIToolExecutionError(f"Failed to set alarm: {str(e)}") from e
