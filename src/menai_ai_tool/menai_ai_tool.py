"""Menai expression evaluator and test suite runner tool."""

import asyncio
import json
import logging
import os
import re
import threading
from pathlib import Path
from typing import Any

from menai import (
    Menai, MenaiError, MenaiCancelledException, get_help,
    MenaiTokenError, MenaiASTBuildError, MenaiCodegenError,
    TestOutcome, run_test_suite
)

from ai_tool import (
    AITool, AIToolCall, AIToolDefinition, AIToolParameter, AIToolResult,
    AIToolExecutionError, AIToolTimeoutError, AIToolAuthorizationCallback,
    AIToolOperationDefinition
)
from mindspace.mindspace import Mindspace
from mindspace.mindspace_error import MindspaceError
from mindspace.mindspace_log_level import MindspaceLogLevel


class MenaiAITool(AITool):
    """Menai tool with LISP-like syntax."""

    TEST_SUFFIX = ".test.menai"
    TEST_TIMEOUT_SECONDS = 30.0

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
                "It can also run `*.test.menai` suites from the mindspace. "
                "Menai has no side effects, so it does not require user approvals to use it."
                "You must call `help` with `get_help` for Menai before using this tool."
            ),
            additional_parameters=[
                AIToolParameter(
                    name="expression",
                    type="string",
                    description="A valid expression written in the Menai language. "
                        "Menai uses Lisp-style prefix syntax: (operator arg1 arg2 ...).",
                    required=False
                ),
                AIToolParameter(
                    name="test_file",
                    type="string",
                    description="Path to a *.test.menai suite inside the mindspace. "
                        "Relative paths are resolved from the mindspace root.",
                    required=False
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
            ),
            "run_tests": AIToolOperationDefinition(
                name="run_tests",
                handler=self._run_tests,
                extract_context=self._extract_run_tests_context,
                allowed_parameters={"test_file"},
                required_parameters={"test_file"},
                description="Run a *.test.menai suite from the mindspace and report the outcomes"
            )
        }

    def set_mindspace(self, mindspace: Mindspace) -> None:
        """
        Set the mindspace for audit logging.

        Args:
            mindspace: The active mindspace
        """
        self._mindspace = mindspace

    def _module_path(self) -> list[str]:
        """
        Compose the Menai module search path for the active mindspace.

        The path is composed by the Menai library, which places the mindspace's
        ``menai-lib`` directory (the mindspace's reusable Menai library) ahead
        of the standard library.  When no mindspace is open the path falls back
        to the standard library alone.

        Returns:
            The composed module search path.
        """
        source_dir: str | None = None
        if self._mindspace is not None:
            mindspace_path = self._mindspace.mindspace_path()
            if mindspace_path:
                source_dir = str(Path(mindspace_path) / Mindspace.MENAI_LIB_DIR)

        return Menai.build_module_path(source_dir)

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
        tool = Menai(self._module_path())
        with self._instances_lock:
            self._active_instances[tool] = requester_ref

        try:
            return tool.evaluate_and_format(expression)

        finally:
            with self._instances_lock:
                self._active_instances.pop(tool, None)

    def get_brief_description(self) -> str:
        """Get brief one-line description for system prompt."""
        return "Evaluates expressions and runs test suites in a very efficient pure functional programming language"

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
        # menai package so it always matches the language. The mindspace's
        # menai-lib inventory is appended so conversations discover its
        # modules without a separate listing step.
        return get_help() + self._menai_lib_inventory()

    def _menai_lib_inventory(self) -> str:
        """
        Build the mindspace's menai-lib module inventory for tool help.

        Walks the menai-lib directory, collecting .menai files recursively with
        their import names (subdirectories become path segments), the names their
        (export ...) forms declare and the one-line description their first-line
        comment carries. The listing is computed when the help is requested, so it
        always reflects the library as it stands then. Suites are left out: a
        `*.test.menai` file is a test for a module, not a module a conversation
        imports for its own sake.

        Returns:
            An inventory section, empty when there is nothing to list.
        """
        if self._mindspace is None:
            return ""

        mindspace_path = self._mindspace.mindspace_path()
        if not mindspace_path:
            return ""

        menai_lib_dir = Path(mindspace_path) / Mindspace.MENAI_LIB_DIR
        if not menai_lib_dir.is_dir():
            return ""

        entries: list[tuple[str, list[str], str]] = []
        for module_path in sorted(menai_lib_dir.rglob("*.menai")):
            if module_path.name.endswith(self.TEST_SUFFIX):
                continue

            rel = module_path.relative_to(menai_lib_dir)
            module_name = str(rel.with_suffix("")).replace(os.sep, "/")
            exports, description = self._module_summary(module_path)
            entries.append((module_name, exports, description))

        if not entries:
            return ""

        lines = [
            "",
            "",
            "## Mindspace library",
            "",
            f"Modules in this mindspace's {Mindspace.MENAI_LIB_DIR} directory, ahead of the standard library:",
            "",
        ]
        for module_name, exports, description in entries:
            summary = f" — {description}" if description else ""

            if exports:
                lines.append(f"- {module_name} (exports: {', '.join(exports)}){summary}")

            else:
                lines.append(f"- {module_name}{summary}")

        lines.extend([
            "",
            "Each module's first line is a `; <name> — <one-line description>` comment, the",
            "convention the standard library follows. Those descriptions are read from the",
            "modules' own first lines, so give every module you add there one, saying what the",
            "module is for, and a module's comment block should go on to explain how to use it.",
        ])

        return "\n".join(lines) + "\n"

    def _module_summary(self, module_path: Path) -> tuple[list[str], str]:
        """
        Extract a module's exported names and its one-line description.

        The description is the first line of the file with its leading `; ` and
        its `<name> — ` prefix stripped, which is how the Menai standard library
        documents itself and how `menai.get_help()` reads that documentation.

        Args:
            module_path: Path to a .menai module file.

        Returns:
            Exported names in declaration order and the description; both are
            empty when the file cannot be read, and the description is empty when
            the file has no first-line comment.
        """
        try:
            with open(module_path, encoding="utf-8") as f:
                source = f.read()

        except OSError:
            return [], ""

        exports: list[str] = []
        for match in re.finditer(r"\(export\s+([^)]+)\)", source):
            exports.extend(match.group(1).split())

        first_line = source.split("\n", 1)[0]
        if not first_line.startswith("; ") or first_line.startswith(";;"):
            return exports, ""

        return exports, first_line.removeprefix("; ").split(" — ", 1)[-1].strip()

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

    def _extract_run_tests_context(self, arguments: dict[str, Any]) -> str | None:
        """
        Extract context for run_tests operation.

        Args:
            arguments: Tool arguments

        Returns:
            Context string if applicable, otherwise None
        """
        test_file = arguments.get("test_file", "")
        return f"`test_file` is:\n```text\n{test_file}\n```"

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
            self._logger.info("Evaluating Menai expression: %s", expression)

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

            self._logger.info("Menai evaluation successful: %s = %s", expression, result)

            if self._mindspace is not None:
                self._mindspace.add_interaction(
                    MindspaceLogLevel.INFO,
                    f"AI evaluated Menai expression: '{expression[:80]}{'...' if len(expression) > 80 else ''}'",
                    context=self._mindspace.conversation_log_context(requester_ref)
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

    def _resolve_test_file(self, test_file: str) -> tuple[str, str]:
        """
        Resolve and validate a suite path inside the active mindspace.

        Args:
            test_file: Absolute or mindspace-relative path to a suite

        Returns:
            Tuple of (absolute path, mindspace-relative path)

        Raises:
            AIToolExecutionError: If no mindspace is open, the path resolves outside
                the mindspace or inside .humbug/, the file is not a suite, or it
                does not exist
        """
        if self._mindspace is None:
            raise AIToolExecutionError("No mindspace is open")

        mindspace_path = self._mindspace.mindspace_path()
        if not mindspace_path:
            raise AIToolExecutionError("No mindspace is open")

        if os.path.isabs(test_file):
            abs_path = test_file

        else:
            abs_path = os.path.join(mindspace_path, test_file)

        try:
            resolved, relative_path = self._mindspace.resolve_tool_path(abs_path)

        except MindspaceError as e:
            raise AIToolExecutionError(str(e)) from e

        if not Path(resolved).name.endswith(self.TEST_SUFFIX):
            raise AIToolExecutionError(
                f"test_file must name a {self.TEST_SUFFIX} file, got: {test_file}"
            )

        if not Path(resolved).is_file():
            raise AIToolExecutionError(f"No such test file: {test_file}")

        return resolved, relative_path

    def _run_tests_sync(self, test_file: str) -> list[TestOutcome]:
        """
        Run one suite off the event loop.

        The Menai runner makes its own instance for the run, so there is no
        instance to track here and nothing for cancel() to reach.  The suite's own
        directory goes first on the module path so that the requested file wins
        over a same-named suite elsewhere, and so modules stored beside the suite
        resolve ahead of the rest of the library.

        Args:
            test_file: Absolute path to the suite

        Returns:
            One outcome per test leaf, in the order the suite declares them
        """
        return run_test_suite(test_file, [str(Path(test_file).parent)] + self._module_path())

    async def _run_tests(
        self,
        tool_call: AIToolCall,
        requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """
        Run a Menai test suite with timeout protection.

        A failing test is an ordinary result and is reported in the payload.  Only
        a suite that cannot be loaded or run raises an error.

        Args:
            tool_call: Tool call containing the suite path to run
            requester_ref: The conversation making the request
            _request_authorization: Authorization callback (not used for Menai)

        Returns:
            AIToolResult containing the totals and the failures, if any

        Raises:
            AIToolExecutionError: If the suite path is invalid or the suite cannot be run
            AIToolTimeoutError: If the suite takes too long
        """
        self.require_menai_help(requester_ref)

        test_file = tool_call.arguments.get("test_file", "")
        if not isinstance(test_file, str) or not test_file:
            raise AIToolExecutionError("test_file must be a non-empty string")

        resolved, relative_path = self._resolve_test_file(test_file)
        self._logger.info("Running Menai test suite: %s", resolved)

        task = asyncio.create_task(asyncio.to_thread(self._run_tests_sync, resolved))
        try:
            outcomes = await asyncio.wait_for(task, timeout=self.TEST_TIMEOUT_SECONDS)

        except asyncio.TimeoutError as e:
            self._logger.warning("Menai test suite timed out: %s", resolved)
            raise AIToolTimeoutError("Menai test run timed out", self.TEST_TIMEOUT_SECONDS) from e

        except ValueError as e:
            self._logger.warning("Invalid Menai test suite '%s': %s", resolved, str(e))
            raise AIToolExecutionError(str(e)) from e

        except MenaiError as e:
            self._logger.warning("Menai error running test suite '%s': %s", resolved, str(e))
            raise AIToolExecutionError(str(e)) from e

        failed = [outcome for outcome in outcomes if not outcome.passed]
        payload = {
            "file": relative_path,
            "total": len(outcomes),
            "passed": len(outcomes) - len(failed),
            "failed": len(failed),
            "failures": [
                {"test": " > ".join(outcome.path), "error": outcome.error}
                for outcome in failed
            ]
        }

        self._logger.info(
            "Menai test suite %s: %d/%d passed", relative_path, payload["passed"], payload["total"]
        )

        if self._mindspace is not None:
            self._mindspace.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI ran Menai tests: '{relative_path}' ({payload['passed']}/{payload['total']} passed)",
                context=self._mindspace.conversation_log_context(requester_ref)
            )

        return AIToolResult(
            id=tool_call.id,
            name="Menai",
            content=json.dumps(payload, indent=2),
            context="json"
        )
