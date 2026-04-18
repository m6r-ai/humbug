"""
Compilation plan data structures for the Menai compiler.

These IR nodes are produced by MenaiIRBuilder from a desugared AST and
consumed by the CFG backend (MenaiCFGBuilder).  All variable references
remain symbolic throughout — MenaiIRVariable carries only a name and
var_type; slot allocation is handled by MenaiCFGBuilder.
"""

from dataclasses import dataclass
from typing import List, Tuple

from menai.menai_value import MenaiValue, MenaiStructType


@dataclass
class MenaiIRConstant:
    """Plan for compiling a constant value."""
    value: MenaiValue


@dataclass
class MenaiIRVariable:
    """Plan for compiling a variable reference."""
    name: str
    var_type: str       # 'local' or 'global'


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
    """Plan for compiling an error expression. message is an IR expression that must evaluate to a string."""
    message: 'MenaiIRExpr'


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
class MenaiIRBuildList:
    """Plan for compiling a (list e1 e2 ... eN) literal.

    Carries a flat list of element plans.  The constant folder can evaluate
    this to a MenaiIRConstant when all elements are compile-time constants.
    The VM codegen lowers it to LOAD_EMPTY_LIST followed by N LIST_APPEND
    register ops, accumulating the result in a single register slot.
    """
    element_plans: List['MenaiIRExpr']


@dataclass
class MenaiIREmptyList:
    """Plan for compiling an empty list literal."""


@dataclass
class MenaiIRReturn:
    """Plan for compiling a return statement."""
    value_plan: 'MenaiIRExpr'


@dataclass
class MenaiIRBuildDict:
    """Plan for compiling a (dict (list k1 v1) (list k2 v2) ...) literal.

    Carries a flat list of (key_plan, value_plan) pairs.  The constant folder
    can evaluate this to a MenaiIRConstant when all keys and values are
    compile-time constants.  The VM codegen lowers it to LOAD_EMPTY_DICT
    followed by N DICT_SET register ops, accumulating the result in a single
    register slot.

    Only emitted when every argument is a literal (list key value) form.
    Non-literal arguments fall through to the runtime prelude lambda instead.
    """
    pair_plans: List[Tuple['MenaiIRExpr', 'MenaiIRExpr']]


@dataclass
class MenaiIRBuildSet:
    """Plan for compiling a (set e1 e2 ... eN) literal.

    Carries a flat list of element plans.  The VM codegen lowers it to
    LOAD_EMPTY_SET followed by N SET_ADD register ops, accumulating the
    result in a single register slot.  Duplicate elements are resolved at
    runtime by SET_ADD (which is a no-op for already-present members).
    """
    element_plans: List['MenaiIRExpr']


@dataclass
class MenaiIRTrace:
    """Plan for compiling a trace expression."""
    message_plans: List['MenaiIRExpr']  # Messages to emit
    value_plan: 'MenaiIRExpr'           # Expression to evaluate and return


@dataclass
class MenaiIRBuildStruct:
    """Plan for compiling a struct constructor call (TypeName f1 f2 ... fN).

    Carries the MenaiStructType descriptor (known at compile time) and a flat
    list of field value plans.  The VM codegen lowers it to LOAD_STRUCT_TYPE
    followed by MAKE_STRUCT, which pops the N field registers and pushes the
    new MenaiStruct instance.
    """
    struct_type: MenaiStructType
    field_plans: List['MenaiIRExpr']


# Union type for all expression plans
MenaiIRExpr = (  # pylint: disable=invalid-name
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
    MenaiIRBuildList |
    MenaiIRBuildDict |
    MenaiIRBuildSet |
    MenaiIRBuildStruct |
    MenaiIRReturn |
    MenaiIRTrace
)
