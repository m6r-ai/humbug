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
        
        This is the KEY TEST of the two-pass approach!
        Jump offsets are pre-calculated, so we emit correct values
        from the start - NO PATCHING!
        
        Args:
            analyzed: Analyzed if expression
        """
        # Record starting position for calculating absolute jump targets
        start_ip = self._current_ip()
        
        # Generate condition
        self._generate_expression(analyzed.condition)
        
        # Emit POP_JUMP_IF_FALSE with PRE-CALCULATED offset
        # The offset was calculated during analysis - no patching needed!
        else_target = start_ip + analyzed.jump_to_else_offset
        self._emit(Opcode.POP_JUMP_IF_FALSE, else_target)
        
        # Generate then branch
        self._generate_expression(analyzed.then_branch)
        
        # Emit JUMP past else branch (if needed)
        # Check if we need the jump by comparing offsets
        # If jump_past_else_offset > jump_to_else_offset, we need the JUMP
        if analyzed.jump_past_else_offset > analyzed.jump_to_else_offset:
            end_target = start_ip + analyzed.jump_past_else_offset
            self._emit(Opcode.JUMP, end_target)
        
        # Generate else branch
        self._generate_expression(analyzed.else_branch)
        
        # NO PATCHING! All jump targets were correct from the start!
        # This is the key innovation of the two-pass approach.
    
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
        for free_var in analyzed.free_vars:
            # Free variables are local variables from outer scopes
            # We need to emit LOAD_VAR to load them
            # But we need to know their depth and index...
            # For now, we'll emit LOAD_NAME and let runtime resolve
            # TODO: This needs proper implementation with scope tracking
            pass
        
        # Create closure
        # MAKE_CLOSURE takes: code_index, capture_count
        self._emit(Opcode.MAKE_CLOSURE, code_index, len(analyzed.free_vars))
    
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
        # The VM pushes arguments onto the stack before calling the lambda
        # We need to pop them and store them in locals
        for i, param in enumerate(analyzed.params):
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
            local_count=len(analyzed.params),  # Parameters are locals
            name="<lambda>"
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
    
    def _calculate_max_locals(self) -> int:
        """Calculate maximum number of local variables needed.
        
        This is a simplified version - in a full implementation,
        we'd track locals per scope during generation.
        
        Returns:
            Maximum number of locals (currently just returns 0)
        """
        # TODO: Properly track locals during generation
        return self.max_locals
    
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
        
        # For other types, return 0
        return 0
