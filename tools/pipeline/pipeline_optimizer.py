"""Pipeline optimizer: collapses adjacent Menai steps into a single step."""

from typing import List

from pipeline_step import MenaiStep, Pipeline, PipelineStep, ToolStep, resolve_step_expression


def _merge_two_menai_steps(first: MenaiStep, second: MenaiStep) -> MenaiStep:
    """
    Merge two adjacent Menai steps into one combined step.

    The first step's expression is bound to an intermediate dict, and the
    second step's inputs that reference the first step are rewritten to
    use dict-get on that intermediate value.  All other inputs and outputs
    are preserved unchanged.

    Args:
        first: The earlier Menai step
        second: The later Menai step, whose inputs may reference first

    Returns:
        A single merged MenaiStep equivalent to running first then second
    """
    intermediate = f"_merged_{first.step_id}"

    rewritten_inputs: dict[str, str] = {}
    injected_from_first: set[str] = set()

    for input_name, source_id in second.inputs.items():
        if source_id == first.step_id:
            injected_from_first.add(input_name)
        else:
            rewritten_inputs[input_name] = source_id

    for input_name, source_id in first.inputs.items():
        rewritten_inputs[input_name] = source_id

    first_expr = resolve_step_expression(first)
    second_expr = resolve_step_expression(second)

    if injected_from_first:
        second_inputs_pairs = " ".join(
            f'(list "{name}" (dict-get {intermediate} "{name}"))'
            for name in sorted(injected_from_first)
        )
        second_inputs_expr = f"(dict {second_inputs_pairs})"
        combined_expression = (
            f'(let* (({intermediate} {first_expr})'
            f' (inputs {second_inputs_expr}))'
            f' {second_expr})'
        )
    else:
        combined_expression = (
            f'(let (({intermediate} {first_expr}))'
            f' {second_expr})'
        )

    return MenaiStep(
        step_id=second.step_id,
        inputs=rewritten_inputs,
        expression=combined_expression,
        module=None,
        outputs=second.outputs
    )


def _collapse_adjacent_menai_steps(steps: List[PipelineStep]) -> List[PipelineStep]:
    """
    Perform a single pass of adjacent Menai step collapsing.

    Scans the step list for the first pair of adjacent Menai steps where
    the second step's only references to the first are via its inputs dict,
    merges them, and returns the updated list.

    Args:
        steps: Current list of pipeline steps

    Returns:
        Updated list, possibly with one fewer step; unchanged if no merge was possible
    """
    for i in range(len(steps) - 1):
        current = steps[i]
        nxt = steps[i + 1]

        if not isinstance(current, MenaiStep) or not isinstance(nxt, MenaiStep):
            continue

        result: List[PipelineStep] = list(steps[:i])
        result.append(_merge_two_menai_steps(current, nxt))
        result.extend(steps[i + 2:])
        return result

    return steps


def optimize_pipeline(pipeline: Pipeline) -> Pipeline:
    """
    Optimize a pipeline by collapsing all adjacent Menai steps.

    Repeatedly applies the single-pass collapse until no further merges
    are possible.  Because Menai is pure, this transformation is always
    semantically equivalent.

    Args:
        pipeline: The pipeline to optimize

    Returns:
        Optimized pipeline with adjacent Menai steps merged
    """
    steps = list(pipeline.steps)

    while True:
        new_steps = _collapse_adjacent_menai_steps(steps)
        if new_steps is steps:
            break

        steps = new_steps

    return Pipeline(steps=steps, directory=pipeline.directory)
