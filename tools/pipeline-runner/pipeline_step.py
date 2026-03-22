"""Pipeline step definitions and parsed pipeline representation."""

from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Dict, List, Optional


@dataclass
class ToolStep:
    """A step that invokes a tool directly."""

    step_id: str
    tool: str
    operation: str
    arguments: Dict[str, Any] = field(default_factory=dict)
    value_from: Optional[str] = None


@dataclass
class MenaiStep:
    """A step that evaluates a Menai expression."""

    step_id: str
    inputs: Dict[str, str]
    expression: str
    module: Optional[str]
    outputs: Dict[str, str]


PipelineStep = ToolStep | MenaiStep


@dataclass
class Pipeline:
    """A parsed and validated pipeline ready for execution."""

    steps: List[PipelineStep] = field(default_factory=list)
    directory: Path = field(default_factory=Path)


def resolve_step_expression(step: MenaiStep) -> str:
    """
    Return the Menai expression body for a step.

    For expression steps this is the expression string directly.  For module
    steps this expands to an import-and-call that passes inputs to the
    module's 'run' function.

    Args:
        step: The Menai step

    Returns:
        Menai expression string (without inputs injection)
    """
    if step.module is not None:
        escaped = step.module.replace("\\", "\\\\").replace('"', '\\"')
        return f'(let ((mod (import "{escaped}"))) ((dict-get mod "run") inputs))'

    return step.expression
