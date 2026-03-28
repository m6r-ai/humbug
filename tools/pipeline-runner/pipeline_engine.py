"""Pipeline execution engine."""

import sys
import time
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from menai import Menai, MenaiError
from menai.menai_value import (
    MenaiBoolean, MenaiDict, MenaiFloat, MenaiInteger, MenaiList, MenaiNone,
    MenaiString, MenaiValue
)

from pipeline_step import MenaiStep, Pipeline, PipelineStep, ToolStep, resolve_step_expression
from pipeline_tools import (
    ClockTool, ConsoleTool, FilesystemTool,
    PipelineAuthorizationDenied, PipelineToolError
)


@dataclass
class StepResult:
    """Result from executing a single pipeline step."""

    step_id: str
    success: bool
    value: str = ""
    error: str = ""
    elapsed_s: float = 0.0


@dataclass
class PipelineResult:
    """Result from executing a complete pipeline."""

    success: bool
    step_results: List[StepResult] = field(default_factory=list)
    error: str = ""


class PipelineExecutionError(Exception):
    """Raised when pipeline execution fails."""


def _menai_value_to_python(value: MenaiValue) -> Any:
    """
    Convert a MenaiValue to a plain Python value for use in tool arguments.

    Args:
        value: Menai value to convert

    Returns:
        Equivalent Python value

    Raises:
        PipelineExecutionError: If the value type cannot be converted
    """
    if isinstance(value, MenaiString):
        return value.value

    if isinstance(value, MenaiInteger):
        return value.value

    if isinstance(value, MenaiFloat):
        return value.value

    if isinstance(value, MenaiBoolean):
        return value.value

    if isinstance(value, MenaiList):
        return [_menai_value_to_python(item) for item in value.items]

    if isinstance(value, MenaiDict):
        return {
            _menai_value_to_python(k): _menai_value_to_python(v)
            for k, v in value.pairs
        }

    raise PipelineExecutionError(
        f"Cannot convert Menai value of type '{type(value).__name__}' to a tool argument"
    )


def _python_to_menai_literal(value: Any) -> str:
    """
    Convert a plain Python value to a Menai literal expression string.

    Strings are escaped for safe embedding in Menai source.  Numbers and
    booleans are rendered directly.  Lists and dicts are rendered recursively.

    Args:
        value: Python value to convert

    Returns:
        Menai source string representing the value

    Raises:
        PipelineExecutionError: If the value type cannot be represented
    """
    if value is None:
        return "#none"

    if isinstance(value, bool):
        return "#t" if value else "#f"

    if isinstance(value, int):
        return str(value)

    if isinstance(value, float):
        return repr(value)

    if isinstance(value, str):
        escaped = (
            value
            .replace("\\", "\\\\")
            .replace('"', '\\"')
            .replace("\n", "\\n")
            .replace("\t", "\\t")
        )
        return f'"{escaped}"'

    if isinstance(value, list):
        items = " ".join(_python_to_menai_literal(item) for item in value)
        return f"(list {items})" if items else "(list)"

    if isinstance(value, dict):
        if not value:
            return "(dict)"

        pairs = " ".join(
            f"{_python_to_menai_literal(k)} {_python_to_menai_literal(v)}"
            for k, v in value.items()
        )
        return f"(dict {pairs})"

    raise PipelineExecutionError(
        f"Cannot convert Python value of type '{type(value).__name__}' to a Menai literal"
    )


def _build_menai_expression(step: MenaiStep, step_outputs: Dict[str, str]) -> str:
    """
    Build the complete Menai expression for a step by wrapping the step's
    expression body in a let binding that injects all named inputs.

    For tool step sources, the output is stored directly under the step ID.
    For Menai step sources, the output is stored as 'step_id.input_name',
    so we try both forms.

    Args:
        step: The Menai step to build an expression for
        step_outputs: Map of step_id or step_id.key -> string output from prior steps

    Returns:
        Complete Menai expression string ready for evaluation

    Raises:
        PipelineExecutionError: If a required input step output is missing
    """
    expression = resolve_step_expression(step)

    if not step.inputs:
        return expression

    bindings: List[str] = []
    for input_name, source_step_id in step.inputs.items():
        composite_key = f"{source_step_id}.{input_name}"
        if composite_key in step_outputs:
            value = step_outputs[composite_key]
        elif source_step_id in step_outputs:
            value = step_outputs[source_step_id]
        else:
            raise PipelineExecutionError(
                f"Menai step '{step.step_id}': input '{input_name}' requires output "
                f"from step '{source_step_id}' which has not been executed"
            )

        literal = _python_to_menai_literal(value)
        bindings.append(f'"{input_name}" {literal}')

    inputs_expr = f"(dict {' '.join(bindings)})"
    return f"(let ((inputs {inputs_expr})) {expression})"


def _execute_menai_step(
    step: MenaiStep,
    step_outputs: Dict[str, str],
    menai: Menai
) -> Dict[str, str]:
    """
    Execute a Menai step and return a map of output key -> string value.

    The step expression must evaluate to a Menai dict.  Each key in the
    dict that appears in step.outputs is extracted and stored.  Keys with
    a #none value are skipped (treated as absent outputs).

    Args:
        step: The Menai step to execute
        step_outputs: Map of step_id -> raw string output from prior steps
        menai: Menai evaluator instance

    Returns:
        Map of output key -> string value for all non-none outputs

    Raises:
        PipelineExecutionError: If evaluation fails or result is not a dict
    """
    expression = _build_menai_expression(step, step_outputs)

    try:
        result = menai._evaluate_raw(expression)

    except MenaiError as e:
        raise PipelineExecutionError(
            f"Menai step '{step.step_id}' evaluation failed: {e}"
        ) from e

    if not isinstance(result, MenaiDict):
        raise PipelineExecutionError(
            f"Menai step '{step.step_id}' must return a dict, "
            f"got '{type(result).__name__}'"
        )

    outputs: Dict[str, str] = {}
    for key_value, val in result.pairs:
        if not isinstance(key_value, MenaiString):
            raise PipelineExecutionError(
                f"Menai step '{step.step_id}': output dict keys must be strings"
            )

        key = key_value.value

        if isinstance(val, MenaiNone):
            continue

        if not isinstance(val, MenaiString):
            raise PipelineExecutionError(
                f"Menai step '{step.step_id}': output dict value for key '{key}' "
                f"must be a string or #none, got '{type(val).__name__}'"
            )

        outputs[key] = val.value

    return outputs


def _get_tool(tool_name: str) -> Any:
    """
    Return the tool instance for the given tool name.

    Args:
        tool_name: Name of the tool

    Returns:
        Tool instance

    Raises:
        PipelineExecutionError: If the tool name is unknown
    """
    tools = {
        "filesystem": FilesystemTool(),
        "clock": ClockTool(),
        "console": ConsoleTool(),
    }

    tool = tools.get(tool_name)
    if tool is None:
        raise PipelineExecutionError(f"Unknown tool: '{tool_name}'")

    return tool


def _resolve_value_from(
    value_from: Optional[str],
    step_outputs: Dict[str, str],
    step_id: str
) -> Optional[str]:
    """
    Resolve a value_from reference to a string value.

    Args:
        value_from: 'step_id.key' reference string, or None
        step_outputs: Map of step_id -> raw string output from prior steps
        step_id: Current step ID (for error messages)

    Returns:
        Resolved string value, or None if value_from is None

    Raises:
        PipelineExecutionError: If the reference cannot be resolved
    """
    if value_from is None:
        return None

    ref_step_id, key = value_from.split(".", 1)

    composite_key = f"{ref_step_id}.{key}"
    if composite_key in step_outputs:
        return step_outputs[composite_key]

    raise PipelineExecutionError(
        f"Step '{step_id}': 'value_from' references '{value_from}' "
        f"but key '{key}' was not found in output of step '{ref_step_id}'"
    )


def _execute_tool_step(
    step: ToolStep,
    step_outputs: Dict[str, str]
) -> str:
    """
    Execute a tool step and return its string output.

    If the step has a value_from, the resolved string is injected into
    the step arguments as 'content' (the standard write parameter).

    Args:
        step: The tool step to execute
        step_outputs: Map of step_id -> raw string output and 'step.key' entries

    Returns:
        String output from the tool

    Raises:
        PipelineExecutionError: If execution fails
        PipelineAuthorizationDenied: If the user denies authorization
    """
    arguments = dict(step.arguments)

    if step.value_from is not None:
        resolved = _resolve_value_from(step.value_from, step_outputs, step.step_id)
        if resolved is not None:
            arguments["content"] = resolved

    tool = _get_tool(step.tool)

    try:
        return tool.execute(step.operation, arguments)

    except PipelineToolError as e:
        raise PipelineExecutionError(
            f"Step '{step.step_id}' ({step.tool}.{step.operation}) failed: {e}"
        ) from e


def execute_pipeline(
    pipeline: Pipeline,
    on_step_start: Optional[Callable[[str], None]] = None,
    on_step_done: Optional[Callable[['StepResult'], None]] = None,
) -> PipelineResult:
    """
    Execute a pipeline, running each step in order.

    Tool step outputs are stored by step ID.  Menai step outputs are stored
    by 'step_id.key' for each key in the output dict, allowing downstream
    tool steps to reference them via value_from.

    Args:
        pipeline: The pipeline to execute
        on_step_start: Optional callback invoked with the step ID before each step runs
        on_step_done: Optional callback invoked with the StepResult after each step

    Returns:
        PipelineResult with per-step results and overall success/failure
    """
    menai = Menai(module_path=[
        str(pipeline.directory),
        str(Path(__file__).parent.parent.parent / "menai_modules"),
    ])

    step_outputs: Dict[str, str] = {}
    step_results: List[StepResult] = []

    for step in pipeline.steps:
        if on_step_start is not None:
            on_step_start(step.step_id)
        step_start = time.monotonic()
        try:
            if isinstance(step, MenaiStep):
                outputs = _execute_menai_step(step, step_outputs, menai)
                for key, value in outputs.items():
                    step_outputs[f"{step.step_id}.{key}"] = value

                step_results.append(StepResult(
                    step_id=step.step_id,
                    success=True,
                    value=str(outputs),
                    elapsed_s=time.monotonic() - step_start,
                ))
                if on_step_done is not None:
                    on_step_done(step_results[-1])

            elif isinstance(step, ToolStep):
                output = _execute_tool_step(step, step_outputs)
                step_outputs[step.step_id] = output
                step_results.append(StepResult(
                    step_id=step.step_id,
                    success=True,
                    value=output,
                    elapsed_s=time.monotonic() - step_start,
                ))
                if on_step_done is not None:
                    on_step_done(step_results[-1])

        except PipelineAuthorizationDenied as e:
            step_results.append(StepResult(
                step_id=step.step_id,
                success=False,
                error=f"Authorization denied: {e}",
                elapsed_s=time.monotonic() - step_start,
            ))
            if on_step_done is not None:
                on_step_done(step_results[-1])
            return PipelineResult(
                success=False,
                step_results=step_results,
                error=f"Pipeline stopped: authorization denied at step '{step.step_id}'"
            )

        except PipelineExecutionError as e:
            step_results.append(StepResult(
                step_id=step.step_id,
                success=False,
                error=str(e),
                elapsed_s=time.monotonic() - step_start,
            ))
            if on_step_done is not None:
                on_step_done(step_results[-1])
            return PipelineResult(
                success=False,
                step_results=step_results,
                error=f"Pipeline stopped at step '{step.step_id}': {e}"
            )

    return PipelineResult(success=True, step_results=step_results)
