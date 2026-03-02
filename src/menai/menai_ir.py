"""
Compilation plan data structures for Menai two-phase compiler.

The compilation plan represents the result of the analysis phase.
It contains all the information needed for code generation without
requiring any further analysis.
"""

from dataclasses import dataclass, field
from typing import List, Optional, Union

from menai.menai_value import MenaiValue


@dataclass
class MenaiIRConstant:
    """Plan for compiling a constant value."""
    value: MenaiValue


@dataclass
class MenaiIRVariable:
    """Plan for compiling a variable reference."""
    name: str
    var_type: str       # 'local' or 'global'
    depth: int = -1     # Scope depth (0 for current frame, 1+ for parent frames).
                        # -1 means unresolved — set by MenaiIRAddresser before codegen.
    index: int = -1     # Variable index (local slot index for locals; unused for globals
                        # until codegen assigns name-table indices).
                        # -1 means unresolved — set by MenaiIRAddresser before codegen.
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
    """Plan for compiling a letrec expression.

    After letrec splitting in the desugarer, every letrec reaching this point
    is guaranteed to be a single fully-mutually-recursive group of lambdas.
    All non-recursive and non-lambda bindings have been hoisted to let forms.
    """
    bindings: List[tuple[str, 'MenaiIRExpr', int]]  # (name, value_plan, var_index)
    body_plan: 'MenaiIRExpr'
    in_tail_position: bool


@dataclass
class MenaiIRLambda:
    """Plan for compiling a lambda expression."""
    params: List[str]
    body_plan: 'MenaiIRExpr'
    sibling_free_vars: List[str]       # Names captured from the immediately enclosing letrec group
    sibling_free_var_plans: List['MenaiIRExpr']  # Plans for loading sibling captures
    outer_free_vars: List[str]         # Names captured from outside the enclosing letrec group
    outer_free_var_plans: List['MenaiIRExpr']    # Plans for loading outer captures
    param_count: int
    is_variadic: bool  # True if last param is a rest parameter
    max_locals: int  # Maximum locals needed in lambda body
    binding_name: Optional[str] = None  # Name if bound in let/letrec (for recursion)
    source_line: int = 0  # Line number in source where this lambda is defined
    source_file: str = ""  # Source file name where this lambda is defined


@dataclass
class MenaiIRCall:
    """Plan for compiling a function call."""
    func_plan: 'MenaiIRExpr'
    arg_plans: List['MenaiIRExpr']
    is_tail_call: bool
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
