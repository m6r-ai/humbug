"""Menai expression evaluator tool."""

import asyncio
import json
import logging
import threading
from pathlib import Path
from typing import Any

from menai import Menai, MenaiError, MenaiCancelledException, get_help
from menai import MenaiTokenError, MenaiASTBuildError, MenaiCodegenError

from ai_tool import (
    AITool, AIToolCall, AIToolDefinition, AIToolParameter, AIToolResult,
    AIToolExecutionError, AIToolTimeoutError, AIToolAuthorizationCallback,
    AIToolOperationDefinition
)
from mindspace.mindspace import Mindspace
from mindspace.mindspace_log_level import MindspaceLogLevel


class MenaiAITool(AITool):
    """Menai tool with LISP-like syntax."""

    def __init__(self) -> None:
        """
        Initialize the Menai tool.

        A fresh Menai instance is created for each evaluation to ensure thread
        safety when multiple conversations use the tool concurrently.  Each
        instance gets its own VM (with its own cancel flag) and its own module
        cache, preventing cross-talk between concurrent evaluations.

        Args:
            mindspace: Set later via set_mindspace
        """
        self._logger = logging.getLogger("MenaiAITool")
        self._expanded_module_path: list[str] = []
        self._mindspace: Mindspace | None = None
        self._active_instances: dict[Menai, Any] = {}
        self._instances_lock = threading.Lock()

    def get_definition(self) -> AIToolDefinition:
        """
        Get the tool definition.

        Returns:
            Tool definition with parameters and description
        """
        return self._build_definition_from_operations(
            name="menai",
            description_prefix=(
                "The menai tool lets you to compile and execute programs written in Menai. "
                "It is ideal for everything from simple calculations to complex algorithms. "
                "Menai has no side effects, so it does not require user approvals to use it."
                "You must call `help` with `get_help` for Menai before using this tool."
            ),
            additional_parameters=[
                AIToolParameter(
                    name="expression",
                    type="string",
                    description="A valid expression written in the Menai language. "
                        "Menai uses Lisp-style prefix syntax: (operator arg1 arg2 ...).",
                    required=True
                )
            ]
        )

    def get_operation_definitions(self) -> dict[str, AIToolOperationDefinition]:
        """
        Get operation definitions for this tool.

        Returns:
            Dictionary mapping operation names to their definitions
        """
        return {
            "evaluate": AIToolOperationDefinition(
                name="evaluate",
                handler=self._evaluate,
                extract_context=self._extract_evaluate_context,
                allowed_parameters={"expression"},
                required_parameters={"expression"},
                description="Evaluate an expression written in the Menai language"
            )
        }

    def set_mindspace(self, mindspace: Mindspace) -> None:
        """
        Set the mindspace for audit logging.

        Args:
            mindspace: The active mindspace
        """
        self._mindspace = mindspace

    def set_module_path(self, module_path: list[str]) -> None:
        """
        Update the module search path.

        This stores the expanded module path for use when creating per-evaluation
        Menai instances.

        Args:
            module_path: List of directories to search for modules.
                        Paths will be expanded and resolved.
        """
        # Expand path
        expanded_path = []
        for path in module_path:
            expanded = str(Path(path).expanduser().resolve())
            expanded_path.append(expanded)

        self._expanded_module_path = expanded_path

    def module_path(self) -> list[str]:
        """
        Get the current expanded module search path.

        Returns:
            List of expanded directories in the module search path
        """
        return self._expanded_module_path

    def cancel(self, requester_ref: Any = None) -> None:
        """
        Cancel any ongoing Menai evaluation.

        When requester_ref is provided, only evaluations belonging to that
        conversation are cancelled.  When None, all active evaluations are
        cancelled.

        Signals all active per-evaluation VM instances to stop execution at the next
        cancellation check point (typically within 1ms for CPU-intensive computations).
        Each evaluation runs on its own Menai instance with its own cancel flag, so
        only the evaluations active at the time of this call are affected.

        This method is thread-safe and can be called while an evaluation is running
        in a thread pool.
        """
        with self._instances_lock:
            for instance, ref in self._active_instances.items():
                if requester_ref is None or ref is requester_ref:
                    instance.vm.cancel()

    def _evaluate_expression_sync(self, expression: str, requester_ref: Any = None) -> str:
        """
        Synchronous helper for expression evaluation.

        Creates a fresh Menai instance for this evaluation, ensuring complete
        isolation of VM state (cancel flag, module cache) from concurrent
        evaluations in other conversations.

        Args:
            expression: Menai expression to evaluate

        Returns:
            Formatted result string

        Raises:
            Various Menai-related exceptions
        """
        tool = Menai(self._expanded_module_path)
        with self._instances_lock:
            self._active_instances[tool] = requester_ref

        try:
            return tool.evaluate_and_format(expression)

        finally:
            with self._instances_lock:
                self._active_instances.pop(tool, None)

    def get_brief_description(self) -> str:
        """Get brief one-line description for system prompt."""
        return "Evaluates expressions using a very efficient pure functional programming language"

    def get_detailed_help(self, operation: str | None = None) -> str:
        """
        Get detailed Menai documentation.

        For Menai, we provide comprehensive syntax documentation since
        the language has its own syntax that needs to be learned.
        """
        if operation is not None:
            # For specific operation, use default implementation
            return self._get_operation_help(operation)

        # Full tool help with comprehensive syntax guide, sourced from the
        # menai package so it always matches the language.
        return get_help()

    def _extract_evaluate_context(self, arguments: dict[str, Any]) -> str | None:
        """
        Extract context for evaluate operation.

        Args:
            arguments: Tool arguments

        Returns:
            Context string if applicable, otherwise None
        """
        expression = arguments.get("expression", "")
        return f"`expression` is:\n```menai\n{expression}\n```"

    async def _evaluate(
        self,
        tool_call: AIToolCall,
        requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """
        Evaluate an Menai expression with timeout protection.

        Args:
            tool_call: Tool call containing the expression to evaluate
            request_authorization: Authorization callback (not used for Menai)

        Returns:
            AIToolResult containing the calculation result

        Raises:
            AIToolExecutionError: If calculation fails or expression is invalid
            AIToolTimeoutError: If calculation takes too long
        """
        arguments = tool_call.arguments

        self.require_menai_help(requester_ref)

        expression = arguments.get("expression", "")

        # Validate expression type
        if not isinstance(expression, str):
            self._logger.error("Menai tool called with non-string expression: %s", type(expression).__name__)
            raise AIToolExecutionError("Expression must be a string")

        try:
            self._logger.debug("Evaluating Menai expression: %s", expression)

            # Run calculation with timeout protection and cancellation support
            # We use a Task so we can cancel the thread execution via the VM's cancel() method
            # Create a task for the thread execution
            task = asyncio.create_task(
                asyncio.to_thread(self._evaluate_expression_sync, expression, requester_ref)
            )

            try:
                # Wait for the task with timeout
                result = await asyncio.wait_for(
                    task,
                    timeout=10.0  # Increased timeout for complex functional programming
                )

            except asyncio.TimeoutError:
                # On timeout, signal the VM to cancel execution
                # This will cause the VM to raise MenaiCancelledException at the next check point
                self._logger.warning("Menai expression evaluation timed out, requesting cancellation: %s", expression)
                self.cancel(requester_ref)

                # The task is already cancelled by wait_for, so we don't need to wait for it again
                # Just signal the VM to cancel and let the thread finish on its own
                # If the VM responds to cancellation, the thread will complete soon
                # If not, the thread will be orphaned but won't block the event loop
                if not task.done():
                    # Task is still running - give it a moment to respond to cancellation
                    try:
                        await asyncio.wait_for(task, timeout=1.0)

                    except (asyncio.TimeoutError, asyncio.CancelledError, MenaiCancelledException):
                        pass  # Expected - task still running, cancelled, or VM responded to cancellation

                    except Exception as e:
                        self._logger.debug("Unexpected exception during cancellation grace period: %s", e)

                raise AIToolTimeoutError("Menai calculation timed out", 10.0)  # pylint: disable=raise-missing-from

            self._logger.debug("Menai evaluation successful: %s = %s", expression, result)

            if self._mindspace is not None:
                self._mindspace.add_interaction(
                    MindspaceLogLevel.INFO,
                    f"AI evaluated Menai expression: '{expression[:80]}{'...' if len(expression) > 80 else ''}'"
                )

            result_object = {
                "result": result,
            }

            return AIToolResult(
                id=tool_call.id,
                name="Menai",
                content=json.dumps(result_object, indent=2),
                context="json"
            )

        except AIToolTimeoutError:
            # Re-raise timeout errors
            raise

        except MenaiCancelledException as e:
            # Treat cancellation as a timeout (which is what triggered it)
            self._logger.info("Menai expression was cancelled: %s", expression)
            raise AIToolTimeoutError("Menai calculation timed out", 10.0) from e

        except MenaiError as e:
            self._logger.warning("Menai error in expression '%s': %s", expression, str(e), exc_info=True)
            # Check if this is a division by zero error specifically
            error_msg = str(e).lower()
            if "division by zero" in error_msg:
                raise AIToolExecutionError("Division by zero") from e

            error_text = str(e)
            if isinstance(e, (MenaiTokenError, MenaiASTBuildError, MenaiCodegenError)):
                error_text += "\n\nNote: You must call `help` with `get_help` for Menai before using this tool."

            raise AIToolExecutionError(error_text) from e

        except Exception as e:
            self._logger.error("Unexpected error evaluating Menai expression '%s': %s", expression, str(e), exc_info=True)
            raise AIToolExecutionError(f"Failed to evaluate Menai expression: {str(e)}") from e
