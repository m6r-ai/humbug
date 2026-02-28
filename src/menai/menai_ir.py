"""
Compilation plan data structures for Menai two-phase compiler.

The compilation plan represents the result of the analysis phase.
It contains all the information needed for code generation without
requiring any further analysis.
"""

from dataclasses import dataclass, field
from typing import List, Optional, Union, Set

from menai.menai_value import MenaiValue
from menai.menai_dependency_analyzer import MenaiBindingGroup


@dataclass
class MenaiIRConstant:
    """Plan for compiling a constant value."""
    value: MenaiValue


@dataclass
class MenaiIRVariable:
    """Plan for compiling a variable reference."""
    name: str
    var_type: str       # 'local' or 'global'
    depth: int          # Scope depth (0 for current frame, 1+ for parent frames)
    index: int          # Variable index (local index or name index)
    is_parent_ref: bool = False  # True if this loads from parent frame (for recursive bindings)


@dataclass
class MenaiIRIf:
    """Plan for compiling an if expression."""
    condition_plan: 'MenaiIRExpr'
    then_plan: 'MenaiIRExpr'
    else_plan: 'MenaiIRExpr'
    in_tail_position: bool


@dataclass
class MenaiIRQuote:
    """Plan for compiling a quote expression."""
    quoted_value: MenaiValue


@dataclass
class MenaiIRError:
    """Plan for compiling an error expression."""
    message: MenaiValue


@dataclass
class MenaiIRLet:
    """Plan for compiling a let expression."""
    bindings: List[tuple[str, 'MenaiIRExpr', int]]  # (name, value_plan, var_index)
    body_plan: 'MenaiIRExpr'
    in_tail_position: bool


@dataclass
class MenaiIRLetrec:
    """Plan for compiling a letrec expression with recursive bindings."""
    bindings: List[tuple[str, 'MenaiIRExpr', int]]  # (name, value_plan, var_index)
    body_plan: 'MenaiIRExpr'
    binding_groups: List[MenaiBindingGroup]
    recursive_bindings: Set[str]  # Names of bindings that are recursive
    in_tail_position: bool


@dataclass
class MenaiIRLambda:
    """Plan for compiling a lambda expression."""
    params: List[str]
    body_plan: 'MenaiIRExpr'
    free_vars: List[str]  # Names of variables to capture
    free_var_plans: List['MenaiIRExpr']  # Plans for loading free variables
    param_count: int
    is_variadic: bool  # True if last param is a rest parameter
    max_locals: int  # Maximum locals needed in lambda body
    binding_name: Optional[str] = None  # Name if bound in let/letrec (for recursion)
    sibling_bindings: List[str] = field(default_factory=list)  # Sibling bindings for mutual recursion
    parent_refs: List[str] = field(default_factory=list)  # Names of parent frame references (recursive bindings)
    parent_ref_plans: List['MenaiIRExpr'] = field(default_factory=list)  # Plans for loading parent references
    source_line: int = 0  # Line number in source where this lambda is defined
    source_file: str = ""  # Source file name where this lambda is defined


@dataclass
class MenaiIRCall:
    """Plan for compiling a function call."""
    func_plan: 'MenaiIRExpr'
    arg_plans: List['MenaiIRExpr']
    is_tail_call: bool
    is_tail_recursive: bool  # True if this is a tail-recursive self-call
    is_builtin: bool
    builtin_name: Optional[str]  # Builtin name if is_builtin=True, else None


@dataclass
class MenaiIREmptyList:
    """Plan for compiling an empty list literal."""


@dataclass
class MenaiIRReturn:
    """Plan for compiling a return statement."""
    value_plan: 'MenaiIRExpr'


@dataclass
class MenaiIRTrace:
    """Plan for compiling a trace expression."""
    message_plans: List['MenaiIRExpr']  # Messages to emit
    value_plan: 'MenaiIRExpr'           # Expression to evaluate and return


# Union type for all expression plans
MenaiIRExpr = Union[
    MenaiIRConstant,
    MenaiIRVariable,
    MenaiIRIf,
    MenaiIRQuote,
    MenaiIRError,
    MenaiIRLet,
    MenaiIRLetrec,
    MenaiIRLambda,
    MenaiIRCall,
    MenaiIREmptyList,
    MenaiIRReturn,
    MenaiIRTrace,
]
