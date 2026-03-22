"""Pipeline JSON parser and validator."""

import json
from pathlib import Path
from typing import Any, Dict, List

from pipeline_step import MenaiStep, Pipeline, PipelineStep, ToolStep


class PipelineParseError(Exception):
    """Raised when a pipeline definition cannot be parsed or is invalid."""


def _parse_tool_step(step_id: str, data: Dict[str, Any], all_step_ids: List[str]) -> ToolStep:
    """
    Parse a tool step from its JSON representation.

    Args:
        step_id: The step identifier
        data: Raw step data from JSON
        all_step_ids: All step IDs defined so far (for reference validation)

    Returns:
        Parsed ToolStep

    Raises:
        PipelineParseError: If the step definition is invalid
    """
    tool = data.get("tool")
    if not isinstance(tool, str) or not tool:
        raise PipelineParseError(f"Step '{step_id}': 'tool' must be a non-empty string")

    operation = data.get("operation")
    if not isinstance(operation, str) or not operation:
        raise PipelineParseError(f"Step '{step_id}': 'operation' must be a non-empty string")

    arguments: Dict[str, Any] = {}
    known_keys = {"id", "tool", "operation", "value_from"}
    for key, value in data.items():
        if key not in known_keys:
            arguments[key] = value

    value_from = data.get("value_from")
    if value_from is not None:
        if not isinstance(value_from, str):
            raise PipelineParseError(f"Step '{step_id}': 'value_from' must be a string")

        parts = value_from.split(".", 1)
        if len(parts) != 2:
            raise PipelineParseError(
                f"Step '{step_id}': 'value_from' must be in the form 'step_id.key', got '{value_from}'"
            )

        ref_step_id = parts[0]
        if ref_step_id not in all_step_ids:
            raise PipelineParseError(
                f"Step '{step_id}': 'value_from' references unknown step '{ref_step_id}'"
            )

    return ToolStep(
        step_id=step_id,
        tool=tool,
        operation=operation,
        arguments=arguments,
        value_from=value_from
    )


def _parse_menai_step(step_id: str, data: Dict[str, Any], all_step_ids: List[str]) -> MenaiStep:
    """
    Parse a Menai step from its JSON representation.

    Args:
        step_id: The step identifier
        data: Raw step data from JSON
        all_step_ids: All step IDs defined so far (for reference validation)

    Returns:
        Parsed MenaiStep

    Raises:
        PipelineParseError: If the step definition is invalid
    """
    expression = data.get("expression")
    module = data.get("module")

    if expression is not None and module is not None:
        raise PipelineParseError(
            f"Step '{step_id}': 'expression' and 'module' are mutually exclusive"
        )

    if expression is None and module is None:
        raise PipelineParseError(
            f"Step '{step_id}': one of 'expression' or 'module' is required"
        )

    if expression is not None and (not isinstance(expression, str) or not expression):
        raise PipelineParseError(f"Step '{step_id}': 'expression' must be a non-empty string")

    if module is not None and (not isinstance(module, str) or not module):
        raise PipelineParseError(f"Step '{step_id}': 'module' must be a non-empty string")

    inputs_raw = data.get("inputs", {})
    if not isinstance(inputs_raw, dict):
        raise PipelineParseError(f"Step '{step_id}': 'inputs' must be a dict")

    inputs: Dict[str, str] = {}
    for input_name, source_step_id in inputs_raw.items():
        if not isinstance(input_name, str) or not input_name:
            raise PipelineParseError(f"Step '{step_id}': input key must be a non-empty string")

        if not isinstance(source_step_id, str) or not source_step_id:
            raise PipelineParseError(
                f"Step '{step_id}': input '{input_name}' source must be a non-empty string step ID"
            )

        if source_step_id not in all_step_ids:
            raise PipelineParseError(
                f"Step '{step_id}': input '{input_name}' references unknown step '{source_step_id}'"
            )

        inputs[input_name] = source_step_id

    outputs_raw = data.get("outputs", {})
    if not isinstance(outputs_raw, dict):
        raise PipelineParseError(f"Step '{step_id}': 'outputs' must be a dict")

    outputs: Dict[str, str] = {}
    for output_key, target_step_id in outputs_raw.items():
        if not isinstance(output_key, str) or not output_key:
            raise PipelineParseError(f"Step '{step_id}': output key must be a non-empty string")

        if not isinstance(target_step_id, str) or not target_step_id:
            raise PipelineParseError(
                f"Step '{step_id}': output '{output_key}' target must be a non-empty string step ID"
            )

        outputs[output_key] = target_step_id

    return MenaiStep(
        step_id=step_id,
        inputs=inputs,
        expression=expression or "",
        module=module,
        outputs=outputs
    )


def _parse_step(data: Any, index: int, all_step_ids: List[str]) -> PipelineStep:
    """
    Parse a single pipeline step.

    Args:
        data: Raw step data from JSON
        index: Step index (for error messages)
        all_step_ids: All step IDs defined so far (for reference validation)

    Returns:
        Parsed PipelineStep

    Raises:
        PipelineParseError: If the step definition is invalid
    """
    if not isinstance(data, dict):
        raise PipelineParseError(f"Step {index}: must be a JSON object")

    step_id = data.get("id")
    if not isinstance(step_id, str) or not step_id:
        raise PipelineParseError(f"Step {index}: 'id' must be a non-empty string")

    if step_id in all_step_ids:
        raise PipelineParseError(f"Step {index}: duplicate step id '{step_id}'")

    tool = data.get("tool")
    if tool == "menai":
        return _parse_menai_step(step_id, data, all_step_ids)

    return _parse_tool_step(step_id, data, all_step_ids)


def parse_pipeline(data: Dict[str, Any]) -> Pipeline:
    """
    Parse and validate a pipeline from its JSON representation.

    Args:
        data: Raw pipeline data from JSON

    Returns:
        Parsed Pipeline

    Raises:
        PipelineParseError: If the pipeline definition is invalid
    """
    if not isinstance(data, dict):
        raise PipelineParseError("Pipeline must be a JSON object")

    steps_raw = data.get("steps")
    if not isinstance(steps_raw, list):
        raise PipelineParseError("Pipeline must have a 'steps' array")

    if not steps_raw:
        raise PipelineParseError("Pipeline must have at least one step")

    steps: List[PipelineStep] = []
    seen_ids: List[str] = []

    for index, step_data in enumerate(steps_raw):
        step = _parse_step(step_data, index, seen_ids)
        seen_ids.append(step.step_id)
        steps.append(step)

    return Pipeline(steps=steps)  # directory set by load_pipeline


def load_pipeline(path: Path) -> Pipeline:
    """
    Load and parse a pipeline from a JSON file.

    Args:
        path: Path to the pipeline JSON file

    Returns:
        Parsed Pipeline

    Raises:
        PipelineParseError: If the file cannot be read or the pipeline is invalid
    """
    try:
        text = path.read_text(encoding="utf-8")

    except OSError as e:
        raise PipelineParseError(f"Cannot read pipeline file '{path}': {e}") from e

    try:
        data = json.loads(text)

    except json.JSONDecodeError as e:
        raise PipelineParseError(f"Invalid JSON in pipeline file '{path}': {e}") from e

    pipeline = parse_pipeline(data)
    pipeline.directory = path.parent.resolve()
    return pipeline
