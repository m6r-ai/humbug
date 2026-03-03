"""
Compilation plan data structures for the Menai compiler.

These IR nodes are produced by MenaiIRBuilder from a desugared AST and
consumed by the CFG backend (MenaiCFGBuilder).  All variable references
remain symbolic (depth=-1, index=-1) throughout — slot allocation is
handled by MenaiCFGBuilder, not by a separate addressing pass.
"""

from dataclasses import dataclass
from typing import List

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
    depth: int = -1     # Scope depth; always -1 (symbolic) in the CFG pipeline.
    index: int = -1     # Slot index; always -1 (symbolic) in the CFG pipeline.
    is_parent_ref: bool = False
                        # True if this is a recursive back-reference through
                        # a lambda boundary to a letrec-bound name.


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
    """Plan for compiling a let expression.

    Bindings are (name, value_plan) pairs.
    """
    bindings: List[tuple[str, 'MenaiIRExpr']]  # (name, value_plan)
    body_plan: 'MenaiIRExpr'
    in_tail_position: bool


@dataclass
class MenaiIRLetrec:
    """Plan for compiling a letrec expression.

    After letrec splitting in the desugarer, every letrec reaching this point
    is guaranteed to be a single fully-mutually-recursive group of lambdas.
    All non-recursive and non-lambda bindings have been hoisted to let forms.
    """
    bindings: List[tuple[str, 'MenaiIRExpr']]  # (name, value_plan)
    body_plan: 'MenaiIRExpr'
    in_tail_position: bool


@dataclass
class MenaiIRLambda:
    """Plan for compiling a lambda expression.

    """
    params: List[str]
    body_plan: 'MenaiIRExpr'
    sibling_free_vars: List[str]       # Names captured from the immediately enclosing letrec group
    sibling_free_var_plans: List['MenaiIRExpr']  # Plans for loading sibling captures
    outer_free_vars: List[str]         # Names captured from outside the enclosing letrec group
    outer_free_var_plans: List['MenaiIRExpr']    # Plans for loading outer captures
    param_count: int
    is_variadic: bool  # True if last param is a rest parameter
    binding_name: str | None = None  # Name if bound in let/letrec (for recursion detection)
    source_line: int = 0  # Line number in source where this lambda is defined
    source_file: str = ""  # Source file name where this lambda is defined


@dataclass
class MenaiIRCall:
    """Plan for compiling a function call."""
    func_plan: 'MenaiIRExpr'
    arg_plans: List['MenaiIRExpr']
    is_tail_call: bool
    is_builtin: bool
    builtin_name: str | None  # Builtin name if is_builtin=True, else None


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
MenaiIRExpr = (
    MenaiIRConstant |
    MenaiIRVariable |
    MenaiIRIf |
    MenaiIRQuote |
    MenaiIRError |
    MenaiIRLet |
    MenaiIRLetrec |
    MenaiIRLambda |
    MenaiIRCall |
    MenaiIREmptyList |
    MenaiIRReturn |
    MenaiIRTrace
)
