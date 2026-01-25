"""Analysis Intermediate Representation for AIFPL two-pass compiler.

This module defines the IR (Intermediate Representation) produced by Pass 1 (analysis)
and consumed by Pass 2 (code generation).

Each AnalyzedExpression subclass represents an analyzed AST node with all information
needed for code generation pre-calculated (jump offsets, variable indices, etc.).
"""

from dataclasses import dataclass, field
from typing import List, Set, Tuple

from aifpl.aifpl_value import AIFPLValue
from aifpl.aifpl_dependency_analyzer import AIFPLBindingGroup


@dataclass
class AnalyzedExpression:
    """Base class for all analyzed expressions.
    
    All analyzed expressions track:
    - expr_type: String identifying the expression type
    - source_expr: Original AST node (for error messages)
    - instruction_count: Number of bytecode instructions this will generate
    """
    expr_type: str
    source_expr: AIFPLValue
    instruction_count: int = field(kw_only=True, default=0)
    
    def __repr__(self) -> str:
        """Human-readable representation for debugging."""
        return f"{self.__class__.__name__}(type={self.expr_type}, instrs={self.instruction_count})"


@dataclass
class AnalyzedLiteral(AnalyzedExpression):
    """Analyzed literal value (number, string, boolean).
    
    Literals are self-evaluating and map to LOAD_CONST or LOAD_TRUE/FALSE.
    """
    value: AIFPLValue
    const_index: int  # Index in constant pool
    
    def __repr__(self) -> str:
        return f"AnalyzedLiteral({self.value}, const_idx={self.const_index})"


@dataclass
class AnalyzedVariable(AnalyzedExpression):
    """Analyzed variable reference.
    
    Variables can be:
    - local: Lexically scoped variable (LOAD_VAR)
    - global: Global variable (LOAD_NAME)
    - builtin: Builtin function (LOAD_NAME)
    """
    name: str
    var_type: str  # 'local', 'global', 'builtin'
    depth: int     # Scope depth (for locals)
    index: int     # Variable index or name index
    
    def __repr__(self) -> str:
        if self.var_type == 'local':
            return f"AnalyzedVariable({self.name}, local[{self.depth}][{self.index}])"
        return f"AnalyzedVariable({self.name}, {self.var_type}[{self.index}])"


@dataclass
class AnalyzedIf(AnalyzedExpression):
    """Analyzed if expression.
    
    Jump offsets are pre-calculated during analysis so code generation
    can emit correct targets without patching.
    """
    condition: AnalyzedExpression
    then_branch: AnalyzedExpression
    else_branch: AnalyzedExpression
    
    # Jump offsets (relative to start of if expression)
    jump_to_else_offset: int    # Where to jump if condition is false
    jump_past_else_offset: int  # Where to jump after then branch
    
    def __repr__(self) -> str:
        return (f"AnalyzedIf(else_offset={self.jump_to_else_offset}, "
                f"end_offset={self.jump_past_else_offset})")


@dataclass
class AnalyzedLet(AnalyzedExpression):
    """Analyzed let expression.
    
    Bindings are analyzed in dependency order with recursive bindings identified.
    This allows code generation to create closures correctly without patching.
    """
    # List of (name, analyzed_value, var_index) tuples
    bindings: List[Tuple[str, AnalyzedExpression, int]]
    body: AnalyzedExpression
    
    # Dependency analysis results
    binding_groups: List[AIFPLBindingGroup]
    recursive_bindings: Set[str]  # Names of recursive bindings
    sibling_groups: List[Set[str]]  # Groups of mutually recursive bindings
    
    def __repr__(self) -> str:
        binding_names = [name for name, _, _ in self.bindings]
        return (f"AnalyzedLet(bindings={binding_names}, "
                f"recursive={self.recursive_bindings})")


@dataclass
class AnalyzedLambda(AnalyzedExpression):
    """Analyzed lambda expression.
    
    Free variables are identified during analysis so closures can be
    created with the correct captures.
    """
    params: List[str]
    body: AnalyzedExpression
    free_vars: List[str]  # Variable names to capture from outer scope
    free_var_info: List[Tuple[str, int, int]]  # (name, depth, index) for each free var
    
    # Recursion information
    is_recursive: bool  # Does this lambda call itself?
    recursive_siblings: List[str]  # For mutual recursion
    
    code_index: int  # Index in code_objects pool
    
    def __repr__(self) -> str:
        return (f"AnalyzedLambda(params={self.params}, free_vars={self.free_vars}, "
                f"recursive={self.is_recursive})")


@dataclass
class AnalyzedCall(AnalyzedExpression):
    """Analyzed function call.
    
    Builtin calls are identified during analysis for efficient code generation.
    Tail calls are also identified for optimization.
    """
    func: AnalyzedExpression
    args: List[AnalyzedExpression]
    
    # Call properties
    is_tail_call: bool
    is_builtin: bool
    builtin_index: int = -1  # Index in builtin table (if is_builtin)
    
    def __repr__(self) -> str:
        flags = []
        if self.is_tail_call:
            flags.append("tail")
        if self.is_builtin:
            flags.append(f"builtin[{self.builtin_index}]")
        flag_str = f" ({', '.join(flags)})" if flags else ""
        return f"AnalyzedCall(args={len(self.args)}{flag_str})"


@dataclass
class AnalyzedAnd(AnalyzedExpression):
    """Analyzed 'and' expression with short-circuit evaluation.
    
    Jump offsets are calculated during code generation.
    """
    args: List[AnalyzedExpression]
    
    def __repr__(self) -> str:
        return f"AnalyzedAnd(args={len(self.args)})"


@dataclass
class AnalyzedOr(AnalyzedExpression):
    """Analyzed 'or' expression with short-circuit evaluation.
    
    Jump offsets are calculated during code generation.
    """
    args: List[AnalyzedExpression]
    
    def __repr__(self) -> str:
        return f"AnalyzedOr(args={len(self.args)})"


@dataclass
class AnalyzedMatch(AnalyzedExpression):
    """Analyzed match expression.
    
    Pattern matching is complex, so we store analyzed patterns and
    temporary variable allocations.
    """
    value: AnalyzedExpression
    match_temp_index: int  # Index of temp var holding match value
    saved_bindings_count: int  # Number of bindings to restore after match
    saved_next_index: int  # Next index to restore after match
    max_locals_used: int  # Maximum number of locals used (for code generation)
    clauses: List['AnalyzedMatchClause']
    
    def __repr__(self) -> str:
        return f"AnalyzedMatch(clauses={len(self.clauses)})"


@dataclass
class AnalyzedMatchClause:
    """A single clause in a match expression.
    
    Each clause has a pattern and a result expression.
    Jump offsets are calculated during code generation.
    """
    pattern: 'AnalyzedPattern'
    result: AnalyzedExpression
    
    def __repr__(self) -> str:
        return f"AnalyzedMatchClause(pattern={self.pattern})"


@dataclass
class AnalyzedPattern:
    """Base class for analyzed patterns.
    
    Patterns are used in match expressions and need their own IR
    because they have complex matching logic.
    """
    pattern_type: str
    source_expr: AIFPLValue
    instruction_count: int = field(kw_only=True, default=0)


@dataclass
class AnalyzedLiteralPattern(AnalyzedPattern):
    """Pattern that matches a literal value."""
    value: AIFPLValue
    const_index: int
    match_value_index: int  # Index of temp var holding value to match


@dataclass
class AnalyzedVariablePattern(AnalyzedPattern):
    """Pattern that binds a variable."""
    name: str
    var_index: int
    match_value_index: int  # Index of temp var holding value to match


@dataclass
class AnalyzedWildcardPattern(AnalyzedPattern):
    """Pattern that matches anything (_)."""
    # Wildcard doesn't need match_value_index since it always matches


@dataclass
class AnalyzedTypePattern(AnalyzedPattern):
    """Pattern that matches a type: (number? x)."""
    type_predicate: str  # 'number?', 'string?', etc.
    var_name: str
    var_index: int  # Index where to bind the variable if type matches
    match_value_index: int  # Index of temp var holding value to match


@dataclass
class AnalyzedEmptyListPattern(AnalyzedPattern):
    """Pattern that matches empty list: ()."""
    match_value_index: int  # Index of temp var holding value to match


@dataclass
class AnalyzedFixedListPattern(AnalyzedPattern):
    """Pattern that matches a fixed-length list: (a b c)."""
    element_patterns: List['AnalyzedPattern']
    element_temp_indices: List[int]  # Temp indices for extracted elements
    match_value_index: int  # Index of temp var holding value to match


@dataclass
class AnalyzedConsPattern(AnalyzedPattern):
    """Pattern that matches head and tail: (a b . rest)."""
    head_patterns: List['AnalyzedPattern']
    head_temp_indices: List[int]  # Temp indices for head elements
    tail_pattern: 'AnalyzedPattern'
    tail_temp_index: int  # Temp index for tail
    match_value_index: int  # Index of temp var holding value to match
    dot_position: int  # Position of the dot in the pattern


@dataclass
class AnalyzedQuote(AnalyzedExpression):
    """Analyzed quote expression.
    
    Quote simply returns the quoted value as a constant.
    """
    quoted_value: AIFPLValue
    const_index: int
    
    def __repr__(self) -> str:
        return f"AnalyzedQuote(value={self.quoted_value}, const_idx={self.const_index})"


@dataclass
class AnalyzedMakeList(AnalyzedExpression):
    """Analyzed list construction.
    
    Used for explicit list construction like (list 1 2 3).
    """
    elements: List[AnalyzedExpression]
    
    def __repr__(self) -> str:
        return f"AnalyzedMakeList(elements={len(self.elements)})"


# Helper function for debugging
def dump_analyzed_ir(analyzed: AnalyzedExpression, indent: int = 0) -> str:
    """Pretty-print analyzed IR for debugging.
    
    Args:
        analyzed: The analyzed expression to dump
        indent: Current indentation level
        
    Returns:
        String representation of the IR tree
    """
    prefix = "  " * indent
    lines = [f"{prefix}{analyzed}"]
    
    # Recursively dump children
    if isinstance(analyzed, AnalyzedIf):
        lines.append(f"{prefix}  condition:")
        lines.append(dump_analyzed_ir(analyzed.condition, indent + 2))
        lines.append(f"{prefix}  then:")
        lines.append(dump_analyzed_ir(analyzed.then_branch, indent + 2))
        lines.append(f"{prefix}  else:")
        lines.append(dump_analyzed_ir(analyzed.else_branch, indent + 2))
    
    elif isinstance(analyzed, AnalyzedLet):
        lines.append(f"{prefix}  bindings:")
        for name, value, idx in analyzed.bindings:
            lines.append(f"{prefix}    {name} (var[{idx}]):")
            lines.append(dump_analyzed_ir(value, indent + 3))
        lines.append(f"{prefix}  body:")
        lines.append(dump_analyzed_ir(analyzed.body, indent + 2))
    
    elif isinstance(analyzed, AnalyzedLambda):
        lines.append(f"{prefix}  body:")
        lines.append(dump_analyzed_ir(analyzed.body, indent + 2))
    
    elif isinstance(analyzed, AnalyzedCall):
        lines.append(f"{prefix}  func:")
        lines.append(dump_analyzed_ir(analyzed.func, indent + 2))
        for i, arg in enumerate(analyzed.args):
            lines.append(f"{prefix}  arg[{i}]:")
            lines.append(dump_analyzed_ir(arg, indent + 2))
    
    elif isinstance(analyzed, (AnalyzedAnd, AnalyzedOr)):
        for i, arg in enumerate(analyzed.args):
            lines.append(f"{prefix}  arg[{i}]:")
            lines.append(dump_analyzed_ir(arg, indent + 2))
    
    elif isinstance(analyzed, AnalyzedMatch):
        lines.append(f"{prefix}  value:")
        lines.append(dump_analyzed_ir(analyzed.value, indent + 2))
        for i, clause in enumerate(analyzed.clauses):
            lines.append(f"{prefix}  clause[{i}]: {clause.pattern}")
            lines.append(dump_analyzed_ir(clause.result, indent + 2))
    
    elif isinstance(analyzed, AnalyzedMakeList):
        for i, elem in enumerate(analyzed.elements):
            lines.append(f"{prefix}  elem[{i}]:")
            lines.append(dump_analyzed_ir(elem, indent + 2))
    
    return "\n".join(lines)
