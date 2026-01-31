"""Compilation plan data structures for AIFPL two-phase compiler.

The compilation plan represents the result of the analysis phase.
It contains all the information needed for code generation without
requiring any further analysis.
"""

from dataclasses import dataclass
from typing import List, Optional, Union, Set

from aifpl.aifpl_value import AIFPLValue
from aifpl.aifpl_dependency_analyzer import AIFPLBindingGroup


@dataclass
class ConstantPlan:
    """Plan for compiling a constant value."""
    value: AIFPLValue


@dataclass
class VariablePlan:
    """Plan for compiling a variable reference."""
    name: str
    var_type: str  # 'local' or 'global'
    depth: int     # Scope depth (0 for current frame)
    index: int     # Variable index (local index or name index)


@dataclass
class IfPlan:
    """Plan for compiling an if expression."""
    condition_plan: 'ExprPlan'
    then_plan: 'ExprPlan'
    else_plan: 'ExprPlan'
    in_tail_position: bool


@dataclass
class AndPlan:
    """Plan for compiling an and expression with short-circuit evaluation."""
    arg_plans: List['ExprPlan']


@dataclass
class OrPlan:
    """Plan for compiling an or expression with short-circuit evaluation."""
    arg_plans: List['ExprPlan']


@dataclass
class QuotePlan:
    """Plan for compiling a quote expression."""
    quoted_value: AIFPLValue


@dataclass
class ErrorPlan:
    """Plan for compiling an error expression."""
    message: AIFPLValue


@dataclass
class LetPlan:
    """Plan for compiling a let expression."""
    bindings: List[tuple[str, 'ExprPlan', int]]  # (name, value_plan, var_index)
    body_plan: 'ExprPlan'
    in_tail_position: bool


@dataclass
class LetrecPlan:
    """Plan for compiling a letrec expression with recursive bindings."""
    bindings: List[tuple[str, 'ExprPlan', int]]  # (name, value_plan, var_index)
    body_plan: 'ExprPlan'
    binding_groups: List[AIFPLBindingGroup]
    recursive_bindings: Set[str]  # Names of bindings that are recursive
    in_tail_position: bool


@dataclass
class LambdaPlan:
    """Plan for compiling a lambda expression."""
    params: List[str]
    body_plan: 'ExprPlan'
    free_vars: List[str]  # Names of variables to capture
    free_var_plans: List['VariablePlan']  # Plans for loading free variables
    param_count: int
    binding_name: Optional[str]  # Name if bound in let/letrec (for recursion)
    is_self_recursive: bool  # Does this lambda call itself?
    sibling_bindings: List[str]  # Sibling bindings for mutual recursion
    max_locals: int  # Maximum locals needed in lambda body


@dataclass
class CallPlan:
    """Plan for compiling a function call."""
    func_plan: 'ExprPlan'
    arg_plans: List['ExprPlan']
    is_tail_call: bool
    is_tail_recursive: bool  # True if this is a tail-recursive self-call
    is_builtin: bool
    builtin_index: Optional[int]  # Index in builtin table if is_builtin=True


@dataclass
class EmptyListPlan:
    """Plan for compiling an empty list literal."""


# Union type for all expression plans
ExprPlan = Union[
    ConstantPlan,
    VariablePlan,
    IfPlan,
    AndPlan,
    OrPlan,
    QuotePlan,
    ErrorPlan,
    LetPlan,
    LetrecPlan,
    LambdaPlan,
    CallPlan,
    EmptyListPlan,
]
