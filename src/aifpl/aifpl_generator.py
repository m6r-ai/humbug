"""AIFPL Generator - Pass 2 of two-pass compiler.

The generator walks the analyzed IR (from Pass 1) and emits bytecode.
Since all analysis is complete, code generation is straightforward:
- No variable resolution needed (already done)
- No jump offset calculation needed (already done)
- No patching needed (all values correct from the start)

This is a simple tree walk that translates IR nodes to bytecode instructions.
"""

from typing import List, Dict, Optional

from aifpl.aifpl_analysis_ir import (
    AnalyzedExpression,
    AnalyzedLiteral,
    AnalyzedVariable,
    AnalyzedIf,
    AnalyzedLet,
    AnalyzedLambda,
    AnalyzedCall,
    AnalyzedQuote,
    AnalyzedMakeList,
    AnalyzedAnd,
    AnalyzedOr,
    AnalyzedMatch,
    AnalyzedLiteralPattern,
    AnalyzedVariablePattern,
    AnalyzedWildcardPattern,
    AnalyzedTypePattern,
    AnalyzedEmptyListPattern,
    AnalyzedFixedListPattern,
    AnalyzedConsPattern,
)
from aifpl.aifpl_bytecode import CodeObject, Instruction, Opcode, make_instruction
from aifpl.aifpl_value import AIFPLValue, AIFPLBoolean
from aifpl.aifpl_error import AIFPLEvalError


class AIFPLGenerator:
    """Pass 2: Generates bytecode from analyzed IR.
    
    The generator is much simpler than a single-pass compiler because:
    1. All variables are already resolved to (depth, index) pairs
    2. All jump offsets are pre-calculated
    3. All free variables are identified
    4. All recursion is detected
    
    We just walk the IR tree and emit the corresponding bytecode.
    """
    
    def __init__(self, 
                 analyzed: AnalyzedExpression,
                 constants: List[AIFPLValue],
                 names: List[str],
                 code_objects: List[AnalyzedLambda]):
        """Initialize generator.
        
        Args:
            analyzed: Root analyzed expression from Pass 1
            constants: Constant pool from analyzer
            names: Name pool from analyzer
            code_objects: Code objects (lambdas) from analyzer
        """
        self.analyzed = analyzed
        self.constants = constants
        self.names = names
        self.analyzed_code_objects = code_objects
        
        # Output: bytecode instructions
        self.instructions: List[Instruction] = []
        
        # Generated code objects (for lambdas)
        self.code_objects: List[CodeObject] = []
        
        # Track maximum locals needed (for stack allocation)
        self.max_locals = 0
    
    def generate(self, name: str = "<module>") -> CodeObject:
        """Generate bytecode from analyzed IR.
        
        Args:
            name: Name for the code object (for debugging)
            
        Returns:
            CodeObject with bytecode ready for VM execution
        """
        # Generate code for the main expression
        self._generate_expression(self.analyzed)
        
        # Add RETURN instruction at the end
        self._emit(Opcode.RETURN)
        
        # Create and return code object
        return CodeObject(
            instructions=self.instructions,
            constants=self.constants,
            names=self.names,
            code_objects=self.code_objects,
            param_count=0,  # Module level has no parameters
            local_count=self._calculate_max_locals_from_analyzed(self.analyzed),
            name=name
        )
    
    def _generate_expression(self, analyzed: AnalyzedExpression) -> None:
        """Generate code for an analyzed expression.
        
        This is the main dispatch method that routes to specific generators
        based on the expression type.
        
        Args:
            analyzed: Analyzed expression to generate code for
        """
        # Dispatch based on type
        if isinstance(analyzed, AnalyzedLiteral):
            self._generate_literal(analyzed)
        
        elif isinstance(analyzed, AnalyzedVariable):
            self._generate_variable(analyzed)
        
        elif isinstance(analyzed, AnalyzedIf):
            self._generate_if(analyzed)
        
        elif isinstance(analyzed, AnalyzedLet):
            self._generate_let(analyzed)
        
        elif isinstance(analyzed, AnalyzedLambda):
            self._generate_lambda(analyzed)
        
        elif isinstance(analyzed, AnalyzedCall):
            self._generate_call(analyzed)
        
        elif isinstance(analyzed, AnalyzedQuote):
            self._generate_quote(analyzed)
        
        elif isinstance(analyzed, AnalyzedMakeList):
            self._generate_make_list(analyzed)
        
        elif isinstance(analyzed, AnalyzedAnd):
            self._generate_and(analyzed)
        
        elif isinstance(analyzed, AnalyzedOr):
            self._generate_or(analyzed)
        
        elif isinstance(analyzed, AnalyzedMatch):
            self._generate_match(analyzed)
        
        else:
            raise AIFPLEvalError(
                message=f"Cannot generate code for expression type: {type(analyzed).__name__}",
                received=str(analyzed)
            )
    
    def _generate_literal(self, analyzed: AnalyzedLiteral) -> None:
        """Generate code for a literal value.
        
        Literals are simple: just load from constant pool.
        Booleans get special opcodes for efficiency.
        
        Args:
            analyzed: Analyzed literal
        """
        # Booleans get special opcodes for efficiency
        if isinstance(analyzed.value, AIFPLBoolean):
            if analyzed.value.value:
                self._emit(Opcode.LOAD_TRUE)
            else:
                self._emit(Opcode.LOAD_FALSE)
        else:
            # All other literals: load from constant pool
            self._emit(Opcode.LOAD_CONST, analyzed.const_index)
    
    def _generate_variable(self, analyzed: AnalyzedVariable) -> None:
        """Generate code for a variable reference.
        
        Variables are already resolved to either:
        - Local: (depth, index) pair -> LOAD_VAR
        - Global/Builtin: name index -> LOAD_NAME
        
        Args:
            analyzed: Analyzed variable
        """
        if analyzed.var_type == 'local':
            # Local variable: use lexical addressing
            self._emit(Opcode.LOAD_VAR, analyzed.depth, analyzed.index)
        else:
            # Global or builtin: use name lookup
            self._emit(Opcode.LOAD_NAME, analyzed.index)
    
    def _generate_if(self, analyzed: AnalyzedIf) -> None:
        """Generate code for if expression.
        
        Uses jump patching during code generation (like 1-pass compiler).
        
        Args:
            analyzed: Analyzed if expression
        """
        # Generate condition
        self._generate_expression(analyzed.condition)
        
        # Jump to else if condition is false (will patch later)
        jump_to_else = self._emit(Opcode.POP_JUMP_IF_FALSE, 0)
        
        # Generate then branch
        self._generate_expression(analyzed.then_branch)
        
        # Jump past else branch (will patch later)
        jump_past_else = self._emit(Opcode.JUMP, 0)
        
        # Patch jump to else
        else_start = self._current_ip()
        self._patch_jump(jump_to_else, else_start)
        
        # Generate else branch
        self._generate_expression(analyzed.else_branch)
        
        # Patch jump past else
        after_else = self._current_ip()
        self._patch_jump(jump_past_else, after_else)
    
    def _generate_let(self, analyzed: AnalyzedLet) -> None:
        """Generate code for let expression.
        
        Let bindings are already analyzed for dependencies and recursion.
        We just generate code for each binding and the body.
        
        Args:
            analyzed: Analyzed let expression
        """
        # Generate code for each binding
        for name, value_analyzed, var_index in analyzed.bindings:
            # Generate code for the value
            self._generate_expression(value_analyzed)
            
            # Store in local variable
            self._emit(Opcode.STORE_VAR, 0, var_index)
        
        # Emit PATCH opcodes for recursive closures
        # TODO: Future optimization - eliminate PATCH opcodes by having VM
        # support self-referential closures directly
        for name, value_analyzed, var_index in analyzed.bindings:
            if isinstance(value_analyzed, AnalyzedLambda) and value_analyzed.is_recursive:
                # PATCH_CLOSURE_SELF: makes closure reference itself
                name_index = self.names.index(name) if name in self.names else len(self.names)
                if name_index == len(self.names):
                    self.names.append(name)
                self._emit(Opcode.PATCH_CLOSURE_SELF, name_index, var_index)
        
        # Generate code for body
        self._generate_expression(analyzed.body)
    
    def _generate_lambda(self, analyzed: AnalyzedLambda) -> None:
        """Generate code for lambda expression.
        
        Lambda creates a closure. Free variables are already identified,
        so we just need to load them and create the closure.
        
        Args:
            analyzed: Analyzed lambda expression
        """
        # Generate nested code object for the lambda body
        lambda_code = self._generate_lambda_code_object(analyzed)
        
        # Add to code objects list
        code_index = len(self.code_objects)
        self.code_objects.append(lambda_code)
        
        # Load free variables onto stack for capture
        # These will be captured into the closure environment
        # The analyzer has resolved them to (name, depth, index) tuples
        for name, depth, index in analyzed.free_var_info:
            # Load the free variable from the parent scope
            # This pushes it onto the stack for MAKE_CLOSURE to capture
            self._emit(Opcode.LOAD_VAR, depth, index)
        
        # Create closure
        # MAKE_CLOSURE takes: code_index, capture_count
        # Use free_var_info length (which excludes self-recursive references)
        self._emit(Opcode.MAKE_CLOSURE, code_index, len(analyzed.free_var_info))
    
    def _generate_lambda_code_object(self, analyzed: AnalyzedLambda) -> CodeObject:
        """Generate a CodeObject for a lambda body.
        
        Args:
            analyzed: Analyzed lambda expression
            
        Returns:
            CodeObject for the lambda
        """
        # Create a new generator for the lambda body
        # The lambda body has its own instruction stream
        body_generator = AIFPLGenerator(
            analyzed=analyzed.body,
            constants=self.constants,  # Share constant pool
            names=self.names,  # Share name pool
            code_objects=self.code_objects  # Share code objects pool
        )
        
        # Generate STORE_VAR instructions for each parameter
        # Arguments are on stack in order: arg0 at bottom, argN at top
        # Stack is LIFO, so we pop in reverse: argN first, then arg(N-1), ..., arg0
        # Store them in locals: argN -> local(N-1), ..., arg0 -> local(0)
        # This means we emit STORE_VAR in reverse order
        for i in range(len(analyzed.params) - 1, -1, -1):
            # STORE_VAR depth=0 (current frame), index=i (parameter position)
            body_generator._emit(Opcode.STORE_VAR, 0, i)
        
        # Generate code for the lambda body expression
        body_generator._generate_expression(analyzed.body)
        
        # Add RETURN
        body_generator._emit(Opcode.RETURN)
        
        # Build the code object
        code = CodeObject(
            instructions=body_generator.instructions,
            constants=body_generator.constants,
            names=body_generator.names,
            code_objects=body_generator.code_objects,
            param_count=len(analyzed.params),  # Number of parameters
            # Local count includes parameters + captured free variables
            local_count=len(analyzed.params) + len(analyzed.free_var_info),
            name="<lambda>",
            # Free vars list - includes names of all free variables (even if not captured yet)
            # This is needed for PATCH_CLOSURE_SELF to work
            free_vars=analyzed.free_vars  # Use original list, includes self-recursive refs
        )
        
        return code
    
    def _generate_call(self, analyzed: AnalyzedCall) -> None:
        """Generate code for function call.
        
        Calls are already analyzed to identify:
        - Builtin calls (use CALL_BUILTIN)
        - Tail calls (use tail call optimization)
        - Regular calls (use CALL_FUNCTION)
        
        Args:
            analyzed: Analyzed call expression
        """
        # Generate call instruction
        if analyzed.is_builtin:
            # Direct builtin call - don't load the function, just generate args and call
            pass  # Skip loading function for builtins
        else:
            # Regular or tail call - load the function first
            self._generate_expression(analyzed.func)
        
        # Generate code for arguments
        for arg in analyzed.args:
            self._generate_expression(arg)
        
        if analyzed.is_builtin:
            self._emit(Opcode.CALL_BUILTIN, analyzed.builtin_index, len(analyzed.args))
        elif analyzed.is_tail_call:
            # Tail call optimization - reuse current frame
            # TODO: Implement proper tail call handling
            # For now, just use regular call
            self._emit(Opcode.CALL_FUNCTION, len(analyzed.args))
        else:
            # Regular function call
            self._emit(Opcode.CALL_FUNCTION, len(analyzed.args))
    
    def _generate_quote(self, analyzed: AnalyzedQuote) -> None:
        """Generate code for quote expression.
        
        Quote is trivial: just load the quoted value as a constant.
        
        Args:
            analyzed: Analyzed quote expression
        """
        # Quote is trivial: just load the quoted value as a constant
        self._emit(Opcode.LOAD_CONST, analyzed.const_index)
    
    def _generate_make_list(self, analyzed: AnalyzedMakeList) -> None:
        """Generate code for list construction.
        
        Generate code for each element, then MAKE_LIST.
        
        Args:
            analyzed: Analyzed list construction
        """
        # Generate code for each element
        for element in analyzed.elements:
            self._generate_expression(element)
        
        # Create list from stack items
        self._emit(Opcode.MAKE_LIST, len(analyzed.elements))
    
    def _emit(self, opcode: Opcode, arg1: int = 0, arg2: int = 0) -> int:
        """Emit a bytecode instruction.
        
        Args:
            opcode: Instruction opcode
            arg1: First argument (default 0)
            arg2: Second argument (default 0)
            
        Returns:
            Index of the emitted instruction (for reference)
        """
        instr = make_instruction(opcode, arg1, arg2)
        index = len(self.instructions)
        self.instructions.append(instr)
        return index
    
    def _current_ip(self) -> int:
        """Get current instruction pointer (next instruction index).
        
        Returns:
            Index where next instruction will be emitted
        """
        return len(self.instructions)
    
    def _patch_jump(self, instr_index: int, target: int) -> None:
        """Patch a jump instruction to point to target.
        
        Args:
            instr_index: Index of the jump instruction to patch
            target: Target instruction index to jump to
        """
        self.instructions[instr_index].arg1 = target
    
    def _calculate_max_locals(self) -> int:
        """Calculate maximum number of local variables needed.
        
        This is a simplified version - in a full implementation,
        we'd track locals per scope during generation.
        
        Returns:
            Maximum number of locals (currently just returns 0)
        """
        # TODO: Properly track locals during generation
        return self.max_locals
    
    def _generate_and(self, analyzed: AnalyzedAnd) -> None:
        """Generate code for 'and' expression with short-circuit evaluation.
        
        Bytecode structure (matching 1-pass compiler):
        - Evaluate arg1
        - POP_JUMP_IF_FALSE <false_section>
        - Evaluate arg2
        - POP_JUMP_IF_FALSE <false_section>
        - ...
        - LOAD_TRUE
        - JUMP <end>
        - false_section: LOAD_FALSE
        - end:
        
        Args:
            analyzed: Analyzed and expression
        """
        # Handle empty case: (and) -> #t
        if len(analyzed.args) == 0:
            self._emit(Opcode.LOAD_TRUE)
            return
        
        # Track jumps to false section
        jump_to_false = []
        
        # Generate each argument with its jump
        for arg in analyzed.args:
            # Generate argument
            self._generate_expression(arg)
            
            # Jump to false section if this arg is false
            jump = self._emit(Opcode.POP_JUMP_IF_FALSE, 0)
            jump_to_false.append(jump)
        
        # All arguments were true - return #t
        self._emit(Opcode.LOAD_TRUE)
        jump_to_end = self._emit(Opcode.JUMP, 0)
        
        # False section
        false_section = self._current_ip()
        for jump in jump_to_false:
            self._patch_jump(jump, false_section)
        
        self._emit(Opcode.LOAD_FALSE)
        
        # Patch jump to end
        end = self._current_ip()
        self._patch_jump(jump_to_end, end)
    
    def _generate_or(self, analyzed: AnalyzedOr) -> None:
        """Generate code for 'or' expression with short-circuit evaluation.
        
        Bytecode structure (matching 1-pass compiler):
        - Evaluate arg1
        - POP_JUMP_IF_TRUE <true_section>
        - Evaluate arg2
        - POP_JUMP_IF_TRUE <true_section>
        - ...
        - LOAD_FALSE
        - JUMP <end>
        - true_section: LOAD_TRUE
        - end:
        
        Args:
            analyzed: Analyzed or expression
        """
        # Handle empty case: (or) -> #f
        if len(analyzed.args) == 0:
            self._emit(Opcode.LOAD_FALSE)
            return
        
        # Track jumps to true section
        jump_to_true = []
        
        # Generate each argument with its jump
        for arg in analyzed.args:
            # Generate argument
            self._generate_expression(arg)
            
            # Jump to true section if this arg is true
            jump = self._emit(Opcode.POP_JUMP_IF_TRUE, 0)
            jump_to_true.append(jump)
        
        # All arguments were false - return #f
        self._emit(Opcode.LOAD_FALSE)
        jump_to_end = self._emit(Opcode.JUMP, 0)
        
        # True section
        true_section = self._current_ip()
        for jump in jump_to_true:
            self._patch_jump(jump, true_section)
        
        self._emit(Opcode.LOAD_TRUE)
        
        # Patch jump to end
        end = self._current_ip()
        self._patch_jump(jump_to_end, end)
    
    def _generate_match(self, analyzed: AnalyzedMatch) -> None:
        """Generate code for match expression.
        
        Uses jump patching during code generation (like 1-pass compiler).
        
        Args:
            analyzed: Analyzed match expression
        """
        # Generate value expression
        self._generate_expression(analyzed.value)
        
        # Store in match temp
        self._emit(Opcode.STORE_VAR, 0, analyzed.match_temp_index)
        
        # Generate each clause
        jump_to_end_indices = []
        
        for i, clause in enumerate(analyzed.clauses):
            # Generate pattern matching code
            self._generate_pattern(clause.pattern)
            
            # If pattern didn't match, jump to next pattern
            if i < len(analyzed.clauses) - 1:
                # Not the last pattern, jump to next on failure
                next_pattern_jump = self._emit(Opcode.POP_JUMP_IF_FALSE, 0)
            else:
                # Last pattern - if it fails, we need to error
                next_pattern_jump = self._emit(Opcode.POP_JUMP_IF_FALSE, 0)
            
            # Pattern matched! Generate result expression
            self._generate_expression(clause.result)
            
            # Jump to end
            jump_idx = self._emit(Opcode.JUMP, 0)
            jump_to_end_indices.append(jump_idx)
            
            # Patch the jump to next pattern
            if i < len(analyzed.clauses) - 1:
                next_pattern_start = self._current_ip()
                self._patch_jump(next_pattern_jump, next_pattern_start)
            else:
                # Last pattern failed - generate error
                error_location = self._current_ip()
                self._patch_jump(next_pattern_jump, error_location)
                
                # Emit error for no match
                from aifpl.aifpl_value import AIFPLString
                error_msg_index = len(self.constants)
                self.constants.append(AIFPLString("No patterns matched in match expression"))
                self._emit(Opcode.RAISE_ERROR, error_msg_index)
        
        # Patch all jumps to end
        end_location = self._current_ip()
        for jump_idx in jump_to_end_indices:
            self._patch_jump(jump_idx, end_location)
    
    def _generate_pattern(self, pattern):
        """Generate pattern matching code.
        
        Leaves a boolean on the stack:
        - True if pattern matches (and binds any variables)
        - False if pattern doesn't match
        
        Args:
            pattern: AnalyzedPattern to generate code for
        """
        if isinstance(pattern, AnalyzedLiteralPattern):
            self._generate_literal_pattern(pattern)
        
        elif isinstance(pattern, AnalyzedVariablePattern):
            self._generate_variable_pattern(pattern)
        
        elif isinstance(pattern, AnalyzedWildcardPattern):
            self._generate_wildcard_pattern(pattern)
        
        elif isinstance(pattern, AnalyzedTypePattern):
            self._generate_type_pattern(pattern)
        
        elif isinstance(pattern, AnalyzedEmptyListPattern):
            self._generate_empty_list_pattern(pattern)
        
        elif isinstance(pattern, AnalyzedFixedListPattern):
            self._generate_fixed_list_pattern(pattern)
        
        elif isinstance(pattern, AnalyzedConsPattern):
            self._generate_cons_pattern(pattern)
        
        else:
            raise AIFPLEvalError(f"Unknown pattern type: {type(pattern).__name__}")
    
    def _generate_literal_pattern(self, pattern: AnalyzedLiteralPattern) -> None:
        """Generate code for literal pattern."""
        # Load value and compare
        self._emit(Opcode.LOAD_VAR, 0, pattern.match_value_index)
        self._emit(Opcode.LOAD_CONST, pattern.const_index)
        
        # Get builtin index for '='
        from aifpl.aifpl_compiler import AIFPLCompiler
        builtin_index = AIFPLCompiler.BUILTIN_TABLE.index('=')
        self._emit(Opcode.CALL_BUILTIN, builtin_index, 2)
    
    def _generate_variable_pattern(self, pattern: AnalyzedVariablePattern) -> None:
        """Generate code for variable pattern."""
        # Load value and bind it
        self._emit(Opcode.LOAD_VAR, 0, pattern.match_value_index)
        self._emit(Opcode.STORE_VAR, 0, pattern.var_index)
        self._emit(Opcode.LOAD_TRUE)
    
    def _generate_wildcard_pattern(self, pattern: AnalyzedWildcardPattern) -> None:
        """Generate code for wildcard pattern."""
        # Always matches
        self._emit(Opcode.LOAD_TRUE)
    
    def _generate_type_pattern(self, pattern: AnalyzedTypePattern) -> None:
        """Generate code for type pattern."""
        from aifpl.aifpl_compiler import AIFPLCompiler
        
        # Load value and call type predicate
        self._emit(Opcode.LOAD_VAR, 0, pattern.match_value_index)
        
        # Type predicates need special handling
        if pattern.type_predicate in AIFPLCompiler.BUILTIN_TABLE:
            builtin_index = AIFPLCompiler.BUILTIN_TABLE.index(pattern.type_predicate)
            self._emit(Opcode.CALL_BUILTIN, builtin_index, 1)
        else:
            # Use LOAD_NAME for type predicates not in builtin table
            name_index = self.names.index(pattern.type_predicate) if pattern.type_predicate in self.names else len(self.names)
            if name_index == len(self.names):
                self.names.append(pattern.type_predicate)
            self._emit(Opcode.LOAD_NAME, name_index)
            self._emit(Opcode.CALL_FUNCTION, 1)
        
        # If type matches, bind the variable
        fail_label = self._emit(Opcode.POP_JUMP_IF_FALSE, 0)
        
        # Type matched - bind variable if not wildcard
        if pattern.var_index >= 0:
            self._emit(Opcode.LOAD_VAR, 0, pattern.match_value_index)
            self._emit(Opcode.STORE_VAR, 0, pattern.var_index)
        
        self._emit(Opcode.LOAD_TRUE)
        success_jump = self._emit(Opcode.JUMP, 0)
        
        # Type didn't match
        fail_location = self._current_ip()
        self._patch_jump(fail_label, fail_location)
        self._emit(Opcode.LOAD_FALSE)
        
        # Patch success jump
        success_location = self._current_ip()
        self._patch_jump(success_jump, success_location)
    
    def _generate_empty_list_pattern(self, pattern: AnalyzedEmptyListPattern) -> None:
        """Generate code for empty list pattern."""
        from aifpl.aifpl_compiler import AIFPLCompiler
        
        # Check if value is empty list
        self._emit(Opcode.LOAD_VAR, 0, pattern.match_value_index)
        builtin_index = AIFPLCompiler.BUILTIN_TABLE.index('null?')
        self._emit(Opcode.CALL_BUILTIN, builtin_index, 1)
    
    def _generate_fixed_list_pattern(self, pattern: AnalyzedFixedListPattern) -> None:
        """Generate code for fixed-length list pattern."""
        from aifpl.aifpl_compiler import AIFPLCompiler
        from aifpl.aifpl_value import AIFPLNumber
        
        # Check if value is a list
        self._emit(Opcode.LOAD_VAR, 0, pattern.match_value_index)
        builtin_index = AIFPLCompiler.BUILTIN_TABLE.index('list?')
        self._emit(Opcode.CALL_BUILTIN, builtin_index, 1)
        fail_jump = self._emit(Opcode.POP_JUMP_IF_FALSE, 0)
        
        # Check length
        self._emit(Opcode.LOAD_VAR, 0, pattern.match_value_index)
        builtin_index = AIFPLCompiler.BUILTIN_TABLE.index('length')
        self._emit(Opcode.CALL_BUILTIN, builtin_index, 1)
        
        length_const_index = len(self.constants)
        self.constants.append(AIFPLNumber(len(pattern.element_patterns)))
        self._emit(Opcode.LOAD_CONST, length_const_index)
        
        builtin_index = AIFPLCompiler.BUILTIN_TABLE.index('=')
        self._emit(Opcode.CALL_BUILTIN, builtin_index, 2)
        length_fail_jump = self._emit(Opcode.POP_JUMP_IF_FALSE, 0)
        
        # Match each element
        fail_jumps = []
        for i, (elem_pattern, elem_temp_index) in enumerate(zip(pattern.element_patterns, pattern.element_temp_indices)):
            # Extract element into temporary
            self._emit(Opcode.LOAD_VAR, 0, pattern.match_value_index)
            
            index_const = len(self.constants)
            self.constants.append(AIFPLNumber(i))
            self._emit(Opcode.LOAD_CONST, index_const)
            
            builtin_index = AIFPLCompiler.BUILTIN_TABLE.index('list-ref')
            self._emit(Opcode.CALL_BUILTIN, builtin_index, 2)
            self._emit(Opcode.STORE_VAR, 0, elem_temp_index)
            
            # Recursively match the element pattern
            self._generate_pattern(elem_pattern)
            
            # If element pattern fails, whole list pattern fails
            elem_fail = self._emit(Opcode.POP_JUMP_IF_FALSE, 0)
            fail_jumps.append(elem_fail)
        
        # Success
        self._emit(Opcode.LOAD_TRUE)
        success_jump = self._emit(Opcode.JUMP, 0)
        
        # Failure paths
        fail_location = self._current_ip()
        self._patch_jump(fail_jump, fail_location)
        self._patch_jump(length_fail_jump, fail_location)
        for fj in fail_jumps:
            self._patch_jump(fj, fail_location)
        
        self._emit(Opcode.LOAD_FALSE)
        
        success_location = self._current_ip()
        self._patch_jump(success_jump, success_location)
    
    def _generate_cons_pattern(self, pattern: AnalyzedConsPattern) -> None:
        """Generate code for cons pattern like (head . tail) or (a b . rest)."""
        from aifpl.aifpl_compiler import AIFPLCompiler
        from aifpl.aifpl_value import AIFPLNumber
        
        # Check if value is a list
        self._emit(Opcode.LOAD_VAR, 0, pattern.match_value_index)
        builtin_index = AIFPLCompiler.BUILTIN_TABLE.index('list?')
        self._emit(Opcode.CALL_BUILTIN, builtin_index, 1)
        fail_jump = self._emit(Opcode.POP_JUMP_IF_FALSE, 0)
        
        # Check not empty
        self._emit(Opcode.LOAD_VAR, 0, pattern.match_value_index)
        builtin_index = AIFPLCompiler.BUILTIN_TABLE.index('null?')
        self._emit(Opcode.CALL_BUILTIN, builtin_index, 1)
        empty_fail_jump = self._emit(Opcode.POP_JUMP_IF_TRUE, 0)  # Jump to fail if empty
        
        # Check list has enough elements for the head patterns
        # For pattern (a b . rest), we need at least 2 elements
        length_fail_jump = None
        if pattern.dot_position > 0:
            self._emit(Opcode.LOAD_VAR, 0, pattern.match_value_index)
            builtin_index = AIFPLCompiler.BUILTIN_TABLE.index('length')
            self._emit(Opcode.CALL_BUILTIN, builtin_index, 1)
            
            length_const_index = len(self.constants)
            self.constants.append(AIFPLNumber(pattern.dot_position))
            self._emit(Opcode.LOAD_CONST, length_const_index)
            
            builtin_index = AIFPLCompiler.BUILTIN_TABLE.index('>=')
            self._emit(Opcode.CALL_BUILTIN, builtin_index, 2)
            length_fail_jump = self._emit(Opcode.POP_JUMP_IF_FALSE, 0)
        
        # Extract head elements (before dot)
        fail_jumps = []
        for i, (head_pattern, head_temp_index) in enumerate(zip(pattern.head_patterns, pattern.head_temp_indices)):
            # Extract element into temporary
            self._emit(Opcode.LOAD_VAR, 0, pattern.match_value_index)
            
            index_const = len(self.constants)
            self.constants.append(AIFPLNumber(i))
            self._emit(Opcode.LOAD_CONST, index_const)
            
            builtin_index = AIFPLCompiler.BUILTIN_TABLE.index('list-ref')
            self._emit(Opcode.CALL_BUILTIN, builtin_index, 2)
            self._emit(Opcode.STORE_VAR, 0, head_temp_index)
            
            # Recursively match the element pattern
            self._generate_pattern(head_pattern)
            
            # If element pattern fails, whole cons pattern fails
            elem_fail = self._emit(Opcode.POP_JUMP_IF_FALSE, 0)
            fail_jumps.append(elem_fail)
        
        # Extract tail (after dot)
        # Use drop to get the tail
        drop_const = len(self.constants)
        self.constants.append(AIFPLNumber(pattern.dot_position))
        self._emit(Opcode.LOAD_CONST, drop_const)
        
        self._emit(Opcode.LOAD_VAR, 0, pattern.match_value_index)
        builtin_index = AIFPLCompiler.BUILTIN_TABLE.index('drop')
        self._emit(Opcode.CALL_BUILTIN, builtin_index, 2)
        self._emit(Opcode.STORE_VAR, 0, pattern.tail_temp_index)
        
        # Recursively match the tail pattern
        self._generate_pattern(pattern.tail_pattern)
        tail_fail = self._emit(Opcode.POP_JUMP_IF_FALSE, 0)
        fail_jumps.append(tail_fail)
        
        # Success
        self._emit(Opcode.LOAD_TRUE)
        success_jump = self._emit(Opcode.JUMP, 0)
        
        # Failure paths
        fail_location = self._current_ip()
        self._patch_jump(fail_jump, fail_location)
        self._patch_jump(empty_fail_jump, fail_location)
        if length_fail_jump is not None:
            self._patch_jump(length_fail_jump, fail_location)
        for fj in fail_jumps:
            self._patch_jump(fj, fail_location)
        
        self._emit(Opcode.LOAD_FALSE)
        
        success_location = self._current_ip()
        self._patch_jump(success_jump, success_location)
    
    def _calculate_max_locals_from_analyzed(self, analyzed: AnalyzedExpression) -> int:
        """Recursively calculate max locals from analyzed IR.
        
        Args:
            analyzed: Analyzed expression
            
        Returns:
            Maximum number of locals needed
        """
        if isinstance(analyzed, AnalyzedLet):
            # Let introduces new locals
            num_bindings = len(analyzed.bindings)
            # Also check body for nested lets
            body_max = self._calculate_max_locals_from_analyzed(analyzed.body)
            return num_bindings + body_max
        
        elif isinstance(analyzed, AnalyzedIf):
            # Check both branches
            then_max = self._calculate_max_locals_from_analyzed(analyzed.then_branch)
            else_max = self._calculate_max_locals_from_analyzed(analyzed.else_branch)
            return max(then_max, else_max)
        
        elif isinstance(analyzed, AnalyzedCall):
            # Check arguments
            return max((self._calculate_max_locals_from_analyzed(arg) for arg in analyzed.args), default=0)
        
        elif isinstance(analyzed, AnalyzedMatch):
            # Match introduces match temp + pattern temps
            return analyzed.max_locals_used
        
        # For other types, return 0
        return 0
