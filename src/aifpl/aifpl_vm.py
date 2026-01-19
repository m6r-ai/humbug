"""AIFPL Virtual Machine - executes bytecode."""

import math
import cmath
from typing import List, Dict, Any, Optional, Tuple
from dataclasses import dataclass

from aifpl.aifpl_value import (
    AIFPLValue, AIFPLNumber, AIFPLString, AIFPLBoolean,
    AIFPLList, AIFPLFunction, AIFPLSymbol, AIFPLBuiltinFunction, AIFPLAlist
)
from aifpl.aifpl_bytecode import CodeObject, Opcode
from aifpl.aifpl_error import AIFPLEvalError, ErrorMessageBuilder
from aifpl.aifpl_environment import AIFPLEnvironment


@dataclass
class Frame:
    """Execution frame for function calls.

    Each frame has its own locals and instruction pointer.
    """
    code: CodeObject
    ip: int = 0  # Instruction pointer
    locals: List[Optional[AIFPLValue]] = None  # Local variables
    closure_env: Any = None  # Closure environment for this frame


    def __post_init__(self):
        if self.locals is None:
            self.locals = [None] * self.code.local_count


class AIFPLVM:
    """Virtual machine for executing AIFPL bytecode.

    Uses a stack-based architecture with lexically-scoped frames.
    """

    def __init__(self, evaluator=None):
        """
        Initialize VM.

        Args:
            evaluator: Reference to AIFPLEvaluator for builtin functions
        """
        self.evaluator = evaluator
        self.stack: List[AIFPLValue] = []
        self.frames: List[Frame] = []
        self.globals: Dict[str, AIFPLValue] = {}
        self.message_builder = ErrorMessageBuilder()

    def set_globals(self, globals_dict: Dict[str, AIFPLValue]) -> None:
        """Set global variables (builtins, constants, etc.)."""
        self.globals = globals_dict.copy()

    # ========== Helper Methods for Error Handling ==========

    def _format_result(self, result: AIFPLValue) -> str:
        """
        Format result for display in error messages, using LISP conventions.

        Args:
            result: The result to format

        Returns:
            String representation of the result
        """
        if isinstance(result, AIFPLBoolean):
            return "#t" if result.value else "#f"

        if isinstance(result, AIFPLString):
            # For error messages, show strings with quotes
            return f'\"{result.value}\"'

        if isinstance(result, AIFPLNumber):
            return str(result.value)

        if isinstance(result, AIFPLList):
            # Format list in LISP notation: (element1 element2 ...)
            if result.is_empty():
                return "()"

            formatted_elements = []
            for element in result.elements:
                formatted_elements.append(self._format_result(element))

            return f"({' '.join(formatted_elements)})"

        if isinstance(result, AIFPLAlist):
            # Format alist in LISP notation
            if result.is_empty():
                return "(alist)"

            formatted_pairs = []
            for key, value in result.pairs:
                formatted_key = self._format_result(key)
                formatted_value = self._format_result(value)
                formatted_pairs.append(f"({formatted_key} {formatted_value})")

            pairs_str = ' '.join(formatted_pairs)
            return f"(alist {pairs_str})"

        if isinstance(result, AIFPLFunction):
            # Format lambda functions
            param_str = " ".join(result.parameters)
            return f"<lambda ({param_str})>"

        if isinstance(result, AIFPLBuiltinFunction):
            # Format builtin functions
            return f"<builtin {result.name}>"

        # For other types, use standard string representation
        return str(result)

    def _get_available_globals(self) -> List[str]:
        """
        Get list of available global variable names.

        Returns:
            List of global variable names
        """
        return list(self.globals.keys())

    def _values_equal(self, val1: AIFPLValue, val2: AIFPLValue) -> bool:
        """
        Check if two values are equal.
        
        Args:
            val1: First value
            val2: Second value
            
        Returns:
            True if values are equal, False otherwise
        """
        # Different types are not equal
        if type(val1) != type(val2):
            return False
        
        # Simple types: compare values directly
        if isinstance(val1, (AIFPLNumber, AIFPLString, AIFPLBoolean)):
            return val1.value == val2.value
        
        # Lists: compare element by element
        if isinstance(val1, AIFPLList):
            if len(val1.elements) != len(val2.elements):
                return False
            return all(self._values_equal(e1, e2) for e1, e2 in zip(val1.elements, val2.elements))
        
        # For other types (functions, etc.), use identity comparison
        return val1 is val2
    def _get_function_name(self, func: AIFPLValue) -> str:
        """
        Get function name for error messages.

        Args:
            func: Function value

        Returns:
            Function name or description
        """
        if isinstance(func, AIFPLFunction):
            return func.name or "<lambda>"
        elif isinstance(func, AIFPLBuiltinFunction):
            return func.name
        else:
            return f"<{type(func).__name__}>"

    def _format_call_stack(self) -> str:
        """
        Format call stack for error messages.

        Returns:
            Formatted call stack trace
        """
        if not self.frames:
            return "No call stack"

        lines = []
        for i, frame in enumerate(self.frames):
            depth = len(self.frames) - i - 1
            func_name = frame.code.name or "<module>"
            lines.append(f"  Frame {depth}: {func_name}")

        return "\\n".join(lines)

    def _get_current_function_name(self) -> str:
        """
        Get name of currently executing function.

        Returns:
            Function name or '<module>' if at top level
        """
        if not self.frames:
            return "<module>"

        return self.frames[-1].code.name or "<lambda>"

    # ========== Execution Methods ==========

    def execute(self, code: CodeObject) -> AIFPLValue:
        """Execute a code object and return the result.

        Args:
            code: Compiled code object to execute

        Returns:
            Result value
        """
        # Reset state
        self.stack = []
        self.frames = [Frame(code)]

        # Execute until we return
        while self.frames:
            result = self._execute_frame()
            if result is not None:
                return result

        # Should not reach here
        raise AIFPLEvalError("Execution ended without return value")

    def _execute_frame(self) -> Optional[AIFPLValue]:
        """Execute current frame until it returns or calls another function.

        Returns:
            Result value if frame returns, None if continuing execution
        """
        if not self.frames:
            return None

        frame = self.frames[-1]
        code = frame.code

        while frame.ip < len(code.instructions):
            instr = code.instructions[frame.ip]
            opcode = instr.opcode
            arg1 = instr.arg1
            arg2 = instr.arg2

            # Increment IP before executing (so jumps can override)
            frame.ip += 1

            # Execute instruction
            if opcode == Opcode.LOAD_CONST:
                self.stack.append(code.constants[arg1])

            elif opcode == Opcode.LOAD_TRUE:
                self.stack.append(AIFPLBoolean(True))

            elif opcode == Opcode.LOAD_FALSE:
                self.stack.append(AIFPLBoolean(False))

            elif opcode == Opcode.LOAD_EMPTY_LIST:
                self.stack.append(AIFPLList(()))

            elif opcode == Opcode.LOAD_VAR:
                depth = arg1
                index = arg2

                # Load from frame at depth  
                # depth=0 is current frame, depth=1 is parent, etc.
                frame_index = len(self.frames) - 1 - depth

                if frame_index < 0 or frame_index >= len(self.frames):
                    # Frame doesn't exist - this can happen with closures
                    # Try to load from globals instead
                    raise AIFPLEvalError(f"Frame at depth {depth} doesn't exist (have {len(self.frames)} frames)")

                target_frame = self.frames[frame_index]

                if index >= len(target_frame.locals):
                    raise AIFPLEvalError(f"Local variable index {index} out of range (frame has {len(target_frame.locals)} locals)")

                value = target_frame.locals[index]
                if value is None:
                    # Variable not initialized yet - could be recursive reference
                    # This is OK, it will be initialized before it's used
                    raise AIFPLEvalError(f"Uninitialized local variable at depth {depth}, index {index}")
                
                self.stack.append(value)

            elif opcode == Opcode.STORE_VAR:
                depth = arg1
                index = arg2
                value = self.stack.pop()
                # Store to frame at depth
                target_frame = self.frames[-(depth + 1)]
                target_frame.locals[index] = value

            elif opcode == Opcode.LOAD_NAME:
                name = code.names[arg1]

                # First check closure environment (for recursive closures)
                if frame.closure_env and name in frame.closure_env.bindings:
                    self.stack.append(frame.closure_env.bindings[name])
                elif name in self.globals:
                    self.stack.append(self.globals[name])
                else:
                    available_vars = self._get_available_globals()
                    similar = self.message_builder.suggest_similar_functions(name, available_vars, max_suggestions=3)

                    suggestion_text = f"Did you mean: {', '.join(similar)}?" if similar else "Check spelling or define it in a let binding"

                    raise AIFPLEvalError(
                        message=f"Undefined variable: '{name}'",
                        context=f"Available variables: {', '.join(sorted(available_vars)[:10])}{'...' if len(available_vars) > 10 else ''}",
                        suggestion=suggestion_text,
                        example=f"(let (({name} some-value)) ...)"
                    )

            elif opcode == Opcode.JUMP:
                frame.ip = arg1

            elif opcode == Opcode.POP_JUMP_IF_FALSE:
                condition = self.stack.pop()
                if not isinstance(condition, AIFPLBoolean):
                    raise AIFPLEvalError("If condition must be boolean")
                if not condition.value:
                    frame.ip = arg1

            elif opcode == Opcode.POP_JUMP_IF_TRUE:
                condition = self.stack.pop()
                if not isinstance(condition, AIFPLBoolean):
                    raise AIFPLEvalError("If condition must be boolean")
                if condition.value:
                    frame.ip = arg1

            elif opcode == Opcode.RAISE_ERROR:
                # Raise an error with the message from the constant pool
                error_msg = code.constants[arg1]
                if not isinstance(error_msg, AIFPLString):
                    raise AIFPLEvalError("RAISE_ERROR requires a string constant")
                raise AIFPLEvalError(error_msg.value)

            elif opcode == Opcode.MAKE_CLOSURE:
                # Get code object for closure
                closure_code = code.code_objects[arg1]
                capture_count = arg2

                # Pop captured values from stack (in reverse order)
                captured_values = []
                for _ in range(capture_count):
                    captured_values.append(self.stack.pop())
                captured_values.reverse()

                # Create a dict mapping free var names to captured values
                captured_dict = {}
                if capture_count > 0:
                    for i, var_name in enumerate(closure_code.free_vars):
                        if i < len(captured_values):
                            captured_dict[var_name] = captured_values[i]

                # Create closure with captured environment  
                # The closure environment will be used to look up free variables
                # For self-referential closures, we'll also need access to the current frame
                # We handle this by making the closure's environment have a parent
                # that points to the current frame's locals

                # Create environment with captured values
                # Parent is None for now - we'll handle self-references differently
                closure = AIFPLFunction(
                    parameters=[f"param{i}" for i in range(closure_code.param_count)],
                    body=None,  # Body is in bytecode, not AST
                    closure_environment=AIFPLEnvironment(bindings=captured_dict),
                    name=closure_code.name,
                    bytecode=closure_code,  # Store bytecode in function
                    captured_values=tuple(captured_values)  # Store for debugging
                )
                self.stack.append(closure)

            elif opcode == Opcode.CALL_FUNCTION:
                arity = arg1
                # Pop arguments
                args = [self.stack.pop() for _ in range(arity)]
                args.reverse()  # Restore correct order
                # Pop function
                func = self.stack.pop()

                # Handle builtin functions
                if isinstance(func, AIFPLBuiltinFunction):
                    # Call builtin through its native implementation
                    if self.evaluator is None:
                        raise AIFPLEvalError("Cannot call builtin without evaluator")
                    result = func.native_impl(args, AIFPLEnvironment(), 0)
                    self.stack.append(result)
                elif isinstance(func, AIFPLFunction):
                    # Handle lambda functions
                    # Check if function has bytecode
                    if hasattr(func, 'bytecode') and func.bytecode is not None:
                        # Check for tail call optimization opportunity
                        # A tail call is when the next instruction is RETURN
                        current_frame = self.frames[-1] if self.frames else None
                        is_tail_call = False
                        
                        if current_frame:
                            next_ip = current_frame.ip
                            if next_ip < len(current_frame.code.instructions):
                                next_instr = current_frame.code.instructions[next_ip]
                                is_tail_call = next_instr.opcode == Opcode.RETURN
                        
                        # Check if it's a self-recursive tail call
                        is_self_recursive = (
                            current_frame and 
                            hasattr(func, 'bytecode') and 
                            func.bytecode == current_frame.code
                        )
                        
                        if is_tail_call and is_self_recursive:
                            # Tail call optimization: reuse current frame
                            # This will reset the frame and continue execution
                            self._tail_call_bytecode_function(func, args, current_frame)
                            # Don't append result or increment IP - the frame was reset
                        else:
                            # Normal call: create new frame
                            result = self._call_bytecode_function(func, args)
                            self.stack.append(result)
                    else:
                        # Fall back to interpreter for AST-based functions
                        if self.evaluator is None:
                            raise AIFPLEvalError("Cannot call AST function without evaluator")
                        result = self.evaluator._call_lambda_function(
                            func, args, AIFPLEnvironment(), 0
                        )
                        self.stack.append(result)
                else:
                    # Not a function at all
                    raise AIFPLEvalError(f"Cannot call non-function: {type(func).__name__}")

            elif opcode == Opcode.CALL_BUILTIN:
                builtin_index = arg1
                arity = arg2
                # Pop arguments
                args = [self.stack.pop() for _ in range(arity)]
                args.reverse()  # Restore correct order

                # Call builtin through evaluator
                if self.evaluator is None:
                    raise AIFPLEvalError("Cannot call builtin without evaluator")

                result = self._call_builtin(builtin_index, args)
                self.stack.append(result)

            elif opcode == Opcode.RETURN:
                # Pop frame and return value
                self.frames.pop()
                if self.stack:
                    return self.stack.pop()
                else:
                    raise AIFPLEvalError("RETURN with empty stack")

            elif opcode == Opcode.PATCH_CLOSURE_SELF:
                # Patch a closure to reference itself (for recursive functions)
                # arg1 = name index (variable name), arg2 = local var index
                name_index = arg1
                var_index = arg2
                var_name = code.names[name_index]

                # Load the closure from the current frame (depth 0)
                # Since STORE_LOCAL stores at depth 0, we load from depth 0
                target_frame = self.frames[-1]  # Current frame (depth 0)
                
                if var_index >= len(target_frame.locals):
                    raise AIFPLEvalError(f"PATCH_CLOSURE_SELF: variable index {var_index} out of range")
                
                closure = target_frame.locals[var_index]

                if not isinstance(closure, AIFPLFunction):
                    raise AIFPLEvalError("PATCH_CLOSURE_SELF requires a function")

                # Create a new environment with a placeholder for self-reference
                new_bindings = {**closure.closure_environment.bindings}
                new_env = AIFPLEnvironment(bindings=new_bindings, parent=closure.closure_environment.parent)

                # Create patched closure
                patched_closure = AIFPLFunction(
                    parameters=closure.parameters,
                    body=closure.body,
                    closure_environment=new_env,
                    name=closure.name,
                    bytecode=closure.bytecode,
                    captured_values=closure.captured_values
                )

                # Now add the patched closure to its own environment (self-reference)
                new_env.bindings[var_name] = patched_closure

                # Store the patched closure back
                target_frame.locals[var_index] = patched_closure

            elif opcode == Opcode.PATCH_CLOSURE_SIBLING:
                # Patch a closure to add a sibling reference (for mutual recursion)
                # arg1 = closure_var_index (which closure to patch)  
                # arg2 = const_index (contains [sibling_var_index, name_index])
                closure_var_index = arg1
                const_index = arg2
                
                # Load patch info from constants
                patch_info = code.constants[const_index]
                if not isinstance(patch_info, AIFPLList) or len(patch_info.elements) != 2:
                    raise AIFPLEvalError("PATCH_CLOSURE_SIBLING: invalid patch info")
                
                sibling_var_index = int(patch_info.elements[0].value)
                name_index = int(patch_info.elements[1].value)
                sibling_name = code.names[name_index]
                
                # Get current frame
                target_frame = self.frames[-1]
                
                # Load the closure to patch
                if closure_var_index >= len(target_frame.locals):
                    raise AIFPLEvalError(f"PATCH_CLOSURE_SIBLING: closure index {closure_var_index} out of range")
                
                closure = target_frame.locals[closure_var_index]
                if not isinstance(closure, AIFPLFunction):
                    raise AIFPLEvalError(f"PATCH_CLOSURE_SIBLING: closure at index {closure_var_index} is not a function")
                
                # Load the sibling from locals
                if sibling_var_index >= len(target_frame.locals):
                    raise AIFPLEvalError(f"PATCH_CLOSURE_SIBLING: sibling index {sibling_var_index} out of range")
                
                sibling = target_frame.locals[sibling_var_index]
                
                # Add sibling to closure's environment
                closure.closure_environment.bindings[sibling_name] = sibling

            elif opcode == Opcode.MAKE_LIST:
                n = arg1
                elements = [self.stack.pop() for _ in range(n)]
                elements.reverse()
                self.stack.append(AIFPLList(tuple(elements)))

            elif opcode == Opcode.DUP:
                if not self.stack:
                    raise AIFPLEvalError("DUP on empty stack")
                self.stack.append(self.stack[-1])

            else:
                raise AIFPLEvalError(f"Unimplemented opcode: {opcode.name}")

        # Frame finished without explicit return
        return None

    def _call_bytecode_function(self, func: AIFPLFunction, args: List[AIFPLValue]) -> AIFPLValue:
        """Call a bytecode function."""
        code = func.bytecode

        # Check arity
        if len(args) != code.param_count:
            param_list = ", ".join(func.parameters) if func.parameters else "(no parameters)"
            arg_list = ", ".join(self._format_result(arg) for arg in args) if args else "(no arguments)"

            raise AIFPLEvalError(
                message=f"Function '{func.name}' expects {code.param_count} arguments, got {len(args)}",
                received=f"Arguments provided: {arg_list}",
                expected=f"Parameters expected: {param_list}",
                example=(f"({func.name} {' '.join(['arg' + str(i+1) for i in range(code.param_count)])})"
                    if code.param_count > 0 else f"({func.name})"),
                suggestion=f"Provide exactly {code.param_count} argument{'s' if code.param_count != 1 else ''}"
            )


        # Create new frame
        new_frame = Frame(code)
        new_frame.closure_env = func.closure_environment

        # Store arguments in locals (parameters come first)
        for i, arg in enumerate(args):
            new_frame.locals[i] = arg

        # Store captured values in locals (after parameters)
        # The lambda compiler puts captured vars after parameters in the local space
        if hasattr(func, 'captured_values') and func.captured_values:
            for i, captured_val in enumerate(func.captured_values):
                # Captured values start after parameters
                new_frame.locals[code.param_count + i] = captured_val

        # Push frame
        self.frames.append(new_frame)

        # Execute until return
        while self.frames and self.frames[-1] == new_frame:
            result = self._execute_frame()
            if result is not None:
                return result

        raise AIFPLEvalError("Function did not return a value")

    def _tail_call_bytecode_function(self, func: AIFPLFunction, args: List[AIFPLValue], current_frame: Frame) -> None:
        """Perform a tail call by reusing the current frame.
        
        This implements tail call optimization (TCO) by resetting the current
        frame instead of creating a new one, preventing stack overflow for
        recursive functions.
        """
        code = func.bytecode
        
        # Check arity (same as regular call)
        if len(args) != code.param_count:
            param_list = ", ".join(func.parameters) if func.parameters else "(no parameters)"
            arg_list = ", ".join(self._format_result(arg) for arg in args) if args else "(no arguments)"
            raise AIFPLEvalError(
                message=f"Function '{func.name}' expects {code.param_count} arguments, got {len(args)}",
                received=f"Arguments provided: {arg_list}",
                expected=f"Parameters expected: {param_list}",
                example=(f"({func.name} {' '.join(['arg' + str(i+1) for i in range(code.param_count)])})"
                    if code.param_count > 0 else f"({func.name})"),
                suggestion=f"Provide exactly {code.param_count} argument{'s' if code.param_count != 1 else ''}"
            )
        
        # Reset the instruction pointer to the beginning of the function
        current_frame.ip = 0
        
        # Update locals with new arguments
        for i, arg in enumerate(args):
            current_frame.locals[i] = arg
        
        # Update captured values if any
        if hasattr(func, 'captured_values') and func.captured_values:
            for i, captured_val in enumerate(func.captured_values):
                current_frame.locals[code.param_count + i] = captured_val
        
        # The frame IP has been reset to 0
        # The frame execution loop will continue from the beginning
        # with the new arguments, effectively implementing the tail call
        # without creating a new Python call stack frame

    def _call_builtin(self, builtin_index: int, args: List[AIFPLValue]) -> AIFPLValue:
        """Call a builtin function by index."""
        from aifpl.aifpl_compiler import AIFPLCompiler

        builtin_name = AIFPLCompiler.BUILTIN_TABLE[builtin_index]

        # Simple arithmetic operations
        if builtin_name == '+':
            if not args:
                # Empty + returns 0 (identity)
                return AIFPLNumber(0)
            total = 0
            for i, arg in enumerate(args):
                if not isinstance(arg, AIFPLNumber):
                    raise AIFPLEvalError(
                        f"Function '+' requires numeric arguments, got {arg.type_name()}"
                    )
                total += arg.value
            return AIFPLNumber(total)

        elif builtin_name == '-':
            if not args:
                raise AIFPLEvalError(
                    message="Subtraction requires at least 1 argument",
                    expected="At least 1 number",
                    example="(- 5 2) → 3 or (- 5) → -5",
                    suggestion="Provide at least one number to subtract"
                )
            if not isinstance(args[0], AIFPLNumber):
                raise AIFPLEvalError(
                    f"Function '-' requires numeric arguments, got {args[0].type_name()}"
                )

            if len(args) == 1:
                return AIFPLNumber(-args[0].value)

            result = args[0].value
            for i, arg in enumerate(args[1:], start=2):
                if not isinstance(arg, AIFPLNumber):
                    raise AIFPLEvalError(
                        f"Function '-' requires numeric arguments, got {arg.type_name()}"
                    )
                result -= arg.value
            return AIFPLNumber(result)

        elif builtin_name == '*':
            if not args:
                # Empty * returns 1 (identity)
                return AIFPLNumber(1)
            result = 1
            for i, arg in enumerate(args):
                if not isinstance(arg, AIFPLNumber):
                    raise AIFPLEvalError(
                        f"Function '*' requires numeric arguments, got {arg.type_name()}"
                    )
                result *= arg.value
            return AIFPLNumber(result)

        elif builtin_name == '/':
            if len(args) < 2:
                raise AIFPLEvalError(
                    message="Division requires at least 2 arguments",
                    received=f"Got {len(args)} arguments",
                    expected="At least 2 numbers",
                    example="(/ 12 3) → 4",
                    suggestion="Provide dividend and at least one divisor"
                )
            if not isinstance(args[0], AIFPLNumber):
                raise AIFPLEvalError(
                    f"Function '/' requires numeric arguments, got {args[0].type_name()}"
                )

            result = args[0].value
            for i, arg in enumerate(args[1:], start=2):
                if not isinstance(arg, AIFPLNumber):
                    raise AIFPLEvalError(
                        f"Function '/' requires numeric arguments, got {arg.type_name()}"
                    )
                if arg.value == 0:
                    raise AIFPLEvalError(
                        message=f"Division by zero at argument {i}",
                        received=f"Attempting to divide {self._format_result(args[0])} by 0",
                        expected="Non-zero divisor",
                        example="(/ 10 2) → 5",
                        suggestion="Ensure divisor is not zero"
                    )
                result /= arg.value
            return AIFPLNumber(result)

        # Comparisons
        elif builtin_name == '=':
            if len(args) < 2:
                raise AIFPLEvalError(
                    message="Equality comparison requires at least 2 arguments",
                    received=f"Got {len(args)} arguments",
                    expected="At least 2 values",
                    example="(= 1 1 1) → #t",
                    suggestion="Provide at least two values to compare"
                )
            first = args[0]
            for arg in args[1:]:
                if not self._values_equal(first, arg):
                    return AIFPLBoolean(False)
            return AIFPLBoolean(True)

        elif builtin_name == '<':
            if len(args) < 2:
                raise AIFPLEvalError(
                    message="Less-than comparison requires at least 2 arguments",
                    received=f"Got {len(args)} arguments",
                    expected="At least 2 numbers",
                    example="(< 1 2 3) → #t",
                    suggestion="Provide at least two numbers to compare"
                )
            for i in range(len(args) - 1):
                if not isinstance(args[i], AIFPLNumber) or not isinstance(args[i+1], AIFPLNumber):
                    # Determine which argument is wrong
                    if not isinstance(args[i], AIFPLNumber):
                        raise AIFPLEvalError(f"Function '<' requires numeric arguments, argument {i+1} is {args[i].type_name()}")
                    else:
                        raise AIFPLEvalError(f"Function '<' requires numeric arguments, argument {i+2} is {args[i+1].type_name()}")
                # Check for complex numbers
                if isinstance(args[i].value, complex) or isinstance(args[i+1].value, complex):
                    raise AIFPLEvalError(
                        message="Function '<' does not support complex numbers",
                        suggestion="Use only real numbers with comparison operators"
                    )
                if not (args[i].value < args[i+1].value):
                    return AIFPLBoolean(False)
            return AIFPLBoolean(True)

        elif builtin_name == '>':
            if len(args) < 2:
                raise AIFPLEvalError(
                    message="Greater-than comparison requires at least 2 arguments",
                    received=f"Got {len(args)} arguments",
                    expected="At least 2 numbers",
                    example="(> 3 2 1) → #t",
                    suggestion="Provide at least two numbers to compare"
                )
            for i in range(len(args) - 1):
                if not isinstance(args[i], AIFPLNumber) or not isinstance(args[i+1], AIFPLNumber):
                    # Determine which argument is wrong
                    if not isinstance(args[i], AIFPLNumber):
                        raise AIFPLEvalError(f"Function '>' requires numeric arguments, argument {i+1} is {args[i].type_name()}")
                    else:
                        raise AIFPLEvalError(f"Function '>' requires numeric arguments, argument {i+2} is {args[i+1].type_name()}")
                # Check for complex numbers
                if isinstance(args[i].value, complex) or isinstance(args[i+1].value, complex):
                    raise AIFPLEvalError(
                        message="Function '>' does not support complex numbers",
                        suggestion="Use only real numbers with comparison operators"
                    )
                if not (args[i].value > args[i+1].value):
                    return AIFPLBoolean(False)
            return AIFPLBoolean(True)

        elif builtin_name == '<=':
            if len(args) < 2:
                raise AIFPLEvalError(
                    message="Less-than-or-equal comparison requires at least 2 arguments",
                    received=f"Got {len(args)} arguments",
                    expected="At least 2 numbers",
                    example="(<= 1 1 2) → #t",
                    suggestion="Provide at least two numbers to compare"
                )
            for i in range(len(args) - 1):
                if not isinstance(args[i], AIFPLNumber) or not isinstance(args[i+1], AIFPLNumber):
                    raise AIFPLEvalError(
                        message="Less-than-or-equal comparison requires numbers",
                        received=f"Arguments: {self._format_result(args[i])}, {self._format_result(args[i+1])}",
                        expected="Numbers only",
                        example="(<= 1 1 2) → #t",
                        suggestion="Use <= only with numeric values"
                    )
                # Check for complex numbers
                if isinstance(args[i].value, complex) or isinstance(args[i+1].value, complex):
                    raise AIFPLEvalError(
                        message="Function '<=' does not support complex numbers",
                        suggestion="Use only real numbers with comparison operators"
                    )
                if not (args[i].value <= args[i+1].value):
                    return AIFPLBoolean(False)
            return AIFPLBoolean(True)

        elif builtin_name == '>=':
            if len(args) < 2:
                raise AIFPLEvalError(
                    message="Greater-than-or-equal comparison requires at least 2 arguments",
                    received=f"Got {len(args)} arguments",
                    expected="At least 2 numbers",
                    example="(>= 3 2 2) → #t",
                    suggestion="Provide at least two numbers to compare"
                )
            for i in range(len(args) - 1):
                if not isinstance(args[i], AIFPLNumber) or not isinstance(args[i+1], AIFPLNumber):
                    raise AIFPLEvalError(
                        message="Greater-than-or-equal comparison requires numbers",
                        received=f"Arguments: {self._format_result(args[i])}, {self._format_result(args[i+1])}",
                        expected="Numbers only",
                        example="(>= 3 2 2) → #t",
                        suggestion="Use >= only with numeric values"
                    )
                # Check for complex numbers
                if isinstance(args[i].value, complex) or isinstance(args[i+1].value, complex):
                    raise AIFPLEvalError(
                        message="Function '>=' does not support complex numbers",
                        suggestion="Use only real numbers with comparison operators"
                    )
                if not (args[i].value >= args[i+1].value):
                    return AIFPLBoolean(False)
            return AIFPLBoolean(True)

        elif builtin_name == '!=':
            if len(args) < 2:
                raise AIFPLEvalError(
                    message="Not-equal comparison requires at least 2 arguments",
                    received=f"Got {len(args)} arguments",
                    expected="At least 2 values",
                    example="(!= 1 2) → #t",
                    suggestion="Provide at least two values to compare"
                )
            # Check if any pair is not equal
            first = args[0]
            for arg in args[1:]:
                # Simple equality check
                if type(first) != type(arg):
                    return AIFPLBoolean(True)  # Different types are not equal
                if isinstance(first, (AIFPLNumber, AIFPLString, AIFPLBoolean)):
                    if first.value != arg.value:
                        return AIFPLBoolean(True)  # Found inequality
            return AIFPLBoolean(False)  # All equal

        elif builtin_name == '//':
            if len(args) != 2:
                raise AIFPLEvalError(
                    message="Floor division takes exactly 2 arguments",
                    received=f"Got {len(args)} arguments",
                    expected="2 numbers: dividend and divisor",
                    example="(// 7 2) → 3",
                    suggestion="Provide dividend and divisor"
                )
            if not isinstance(args[0], AIFPLNumber) or not isinstance(args[1], AIFPLNumber):
                raise AIFPLEvalError(
                    message="Floor division requires numbers",
                    received=f"Arguments: {self._format_result(args[0])}, {self._format_result(args[1])}",
                    expected="Two numbers",
                    example="(// 7 2) → 3",
                    suggestion="All arguments to // must be numbers"
                )
            if args[1].value == 0:
                raise AIFPLEvalError(
                    message="Division by zero",
                    received=f"Attempting to divide {self._format_result(args[0])} by 0",
                    expected="Non-zero divisor",
                    example="(// 10 2) → 5",
                    suggestion="Ensure divisor is not zero"
                )
            return AIFPLNumber(args[0].value // args[1].value)

        elif builtin_name == '%':
            if len(args) != 2:
                raise AIFPLEvalError(
                    message="Modulo requires exactly 2 arguments",
                    received=f"Got {len(args)} arguments",
                    expected="2 numbers: dividend and divisor",
                    example="(% 7 3) → 1",
                    suggestion="Provide dividend and divisor"
                )
            if not isinstance(args[0], AIFPLNumber) or not isinstance(args[1], AIFPLNumber):
                raise AIFPLEvalError(
                    message="Modulo requires numbers",
                    received=f"Arguments: {self._format_result(args[0])}, {self._format_result(args[1])}",
                    expected="Two numbers",
                    example="(% 7 3) → 1",
                    suggestion="Both arguments to % must be numbers"
                )
            if args[1].value == 0:
                raise AIFPLEvalError(
                    message="Modulo by zero",
                    received=f"Attempting to compute {self._format_result(args[0])} % 0",
                    expected="Non-zero divisor",
                    example="(% 7 3) → 1",
                    suggestion="Ensure divisor is not zero"
                )
            return AIFPLNumber(args[0].value % args[1].value)

        elif builtin_name == '**':
            if len(args) != 2:
                raise AIFPLEvalError(
                    message="Power takes exactly 2 arguments",
                    received=f"Got {len(args)} arguments",
                    expected="2 numbers: base and exponent",
                    example="(** 2 3) → 8",
                    suggestion="Provide base and exponent"
                )
            if not isinstance(args[0], AIFPLNumber) or not isinstance(args[1], AIFPLNumber):
                raise AIFPLEvalError(
                    message="Power requires numbers",
                    received=f"Arguments: {self._format_result(args[0])}, {self._format_result(args[1])}",
                    expected="Two numbers",
                    example="(** 2 3) → 8",
                    suggestion="Both arguments to ** must be numbers"
                )
            return AIFPLNumber(args[0].value ** args[1].value)

        # Boolean logic operations
        elif builtin_name == 'and':
            # Short-circuit evaluation: return first false or last value
            if len(args) == 0:
                return AIFPLBoolean(True)  # Empty and is true
            for i, arg in enumerate(args, 1):
                if not isinstance(arg, AIFPLBoolean):
                    raise AIFPLEvalError(f"And operator argument {i} must be boolean")
                if not arg.value:
                    return AIFPLBoolean(False)  # Short-circuit on first false
            return AIFPLBoolean(True)  # All true

        elif builtin_name == 'or':
            # Short-circuit evaluation: return first true or last value
            if len(args) == 0:
                return AIFPLBoolean(False)  # Empty or is false
            for i, arg in enumerate(args, 1):
                if not isinstance(arg, AIFPLBoolean):
                    raise AIFPLEvalError(f"Or operator argument {i} must be boolean")
                if arg.value:
                    return AIFPLBoolean(True)  # Short-circuit on first true
            return AIFPLBoolean(False)  # All false

        elif builtin_name == 'not':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="Boolean 'not' requires exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 boolean value",
                    example="(not #t) → #f",
                    suggestion="Provide a single boolean value"
                )
            if not isinstance(args[0], AIFPLBoolean):
                raise AIFPLEvalError(
                    f"Function 'not' requires boolean arguments, got {args[0].type_name()}"
                )
            return AIFPLBoolean(not args[0].value)

        # List operations
        elif builtin_name == 'list':
            return AIFPLList(tuple(args))

        elif builtin_name == 'cons':
            if len(args) != 2:
                raise AIFPLEvalError(
                    message="Cons requires exactly 2 arguments",
                    received=f"Got {len(args)} arguments",
                    expected="2 arguments: element and list",
                    example="(cons 1 (list 2 3)) → (1 2 3)",
                    suggestion="Provide an element and a list"
                )
            head = args[0]
            tail = args[1]
            if not isinstance(tail, AIFPLList):
                raise AIFPLEvalError(
                    message="Cons second argument must be a list",
                    received=f"Second argument: {self._format_result(tail)} ({tail.type_name()})",
                    expected="List",
                    example="(cons 1 (list 2 3)) → (1 2 3)",
                    suggestion="Use (list ...) to create a list for the second argument"
                )
            return AIFPLList((head,) + tail.elements)

        elif builtin_name == 'append':
            result_elements = []
            for i, arg in enumerate(args):
                if not isinstance(arg, AIFPLList):
                    raise AIFPLEvalError(
                        message="Append requires all arguments to be lists",
                        received=f"Argument {i+1}: {self._format_result(arg)} ({arg.type_name()})",
                        expected="List",
                        example="(append (list 1 2) (list 3 4)) → (1 2 3 4)",
                        suggestion="All arguments to append must be lists"
                    )
                result_elements.extend(arg.elements)
            return AIFPLList(tuple(result_elements))

        elif builtin_name == 'reverse':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="Reverse takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 list",
                    example="(reverse (list 1 2 3)) → (3 2 1)",
                    suggestion="Provide a single list to reverse"
                )
            if not isinstance(args[0], AIFPLList):
                raise AIFPLEvalError(
                    message="Reverse requires a list",
                    received=f"Argument: {self._format_result(args[0])} ({args[0].type_name()})",
                    expected="List",
                    example="(reverse (list 1 2 3)) → (3 2 1)",
                    suggestion="Use reverse only with lists"
                )
            return AIFPLList(tuple(reversed(args[0].elements)))

        elif builtin_name == 'first':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="First takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 list",
                    example="(first (list 1 2 3)) → 1",
                    suggestion="Provide a single list"
                )
            if not isinstance(args[0], AIFPLList):
                raise AIFPLEvalError(
                    message="First requires a list",
                    received=f"Argument: {self._format_result(args[0])} ({args[0].type_name()})",
                    expected="List",
                    example="(first (list 1 2 3)) → 1",
                    suggestion="Use first only with lists"
                )
            if args[0].is_empty():
                raise AIFPLEvalError(
                    message="Cannot get first element of empty list",
                    received="Empty list: ()",
                    expected="Non-empty list",
                    example="(first (list 1 2 3)) → 1",
                    suggestion="Check that list is not empty before calling first, use (null? list) to test"
                )
            return args[0].first()

        elif builtin_name == 'rest':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="Rest takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 list",
                    example="(rest (list 1 2 3)) → (2 3)",
                    suggestion="Provide a single list"
                )
            if not isinstance(args[0], AIFPLList):
                raise AIFPLEvalError(
                    message="Rest requires a list",
                    received=f"Argument: {self._format_result(args[0])} ({args[0].type_name()})",
                    expected="List",
                    example="(rest (list 1 2 3)) → (2 3)",
                    suggestion="Use rest only with lists"
                )
            if args[0].is_empty():
                raise AIFPLEvalError(
                    message="Cannot get rest of empty list",
                    received="Empty list: ()",
                    expected="Non-empty list",
                    example="(rest (list 1 2 3)) → (2 3)",
                    suggestion="Check that list is not empty before calling rest, use (null? list) to test"
                )
            return AIFPLList(args[0].elements[1:])

        elif builtin_name == 'last':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="Last takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 list",
                    example="(last (list 1 2 3)) → 3",
                    suggestion="Provide a single list"
                )
            if not isinstance(args[0], AIFPLList):
                raise AIFPLEvalError(
                    message="Last requires a list",
                    received=f"Argument: {self._format_result(args[0])} ({args[0].type_name()})",
                    expected="List",
                    example="(last (list 1 2 3)) → 3",
                    suggestion="Use last only with lists"
                )
            if args[0].is_empty():
                raise AIFPLEvalError(
                    message="Cannot get last element of empty list",
                    received="Empty list: ()",
                    expected="Non-empty list",
                    example="(last (list 1 2 3)) → 3",
                    suggestion="Check that list is not empty before calling last, use (null? list) to test"
                )
            return args[0].elements[-1]

        elif builtin_name == 'length':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="Length takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 list",
                    example="(length (list 1 2 3)) → 3",
                    suggestion="Provide a single list"
                )
            if not isinstance(args[0], AIFPLList):
                raise AIFPLEvalError(
                    message="Length requires a list",
                    received=f"Argument: {self._format_result(args[0])} ({args[0].type_name()})",
                    expected="List",
                    example="(length (list 1 2 3)) → 3",
                    suggestion="Use length only with lists"
                )
            return AIFPLNumber(len(args[0].elements))

        elif builtin_name == 'null?':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="Null? takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 value",
                    example="(null? ()) → #t",
                    suggestion="Provide a single value to test"
                )
            if isinstance(args[0], AIFPLList):
                return AIFPLBoolean(args[0].is_empty())
            return AIFPLBoolean(False)

        elif builtin_name == 'member?':
            if len(args) != 2:
                raise AIFPLEvalError(
                    message="Member? takes exactly 2 arguments",
                    received=f"Got {len(args)} arguments",
                    expected="2 arguments: item and list",
                    example="(member? 2 (list 1 2 3)) → #t",
                    suggestion="Provide an item and a list"
                )
            item = args[0]
            lst = args[1]
            if not isinstance(lst, AIFPLList):
                raise AIFPLEvalError(
                    message="Member? second argument must be a list",
                    received=f"Second argument: {self._format_result(lst)} ({lst.type_name()})",
                    expected="List",
                    example="(member? 2 (list 1 2 3)) → #t",
                    suggestion="Use member? with a list as the second argument"
                )

            # Deep equality check
            for elem in lst.elements:
                if self._values_equal(item, elem):
                    return AIFPLBoolean(True)
            return AIFPLBoolean(False)

        elif builtin_name == 'position':
            if len(args) != 2:
                raise AIFPLEvalError(
                    message="Position takes exactly 2 arguments",
                    received=f"Got {len(args)} arguments",
                    expected="2 arguments: item and list",
                    example="(position 2 (list 1 2 3)) → 1",
                    suggestion="Provide an item and a list"
                )
            item = args[0]
            lst = args[1]
            if not isinstance(lst, AIFPLList):
                raise AIFPLEvalError(
                    message="Position second argument must be a list",
                    received=f"Second argument: {self._format_result(lst)} ({lst.type_name()})",
                    expected="List",
                    example="(position 2 (list 1 2 3)) → 1",
                    suggestion="Use position with a list as the second argument"
                )

            for i, elem in enumerate(lst.elements):
                if self._values_equal(item, elem):
                    return AIFPLNumber(i)
            return AIFPLBoolean(False)

        elif builtin_name == 'take':
            if len(args) != 2:
                raise AIFPLEvalError(
                    message="Take takes exactly 2 arguments",
                    received=f"Got {len(args)} arguments",
                    expected="2 arguments: count and list",
                    example="(take 2 (list 1 2 3)) → (1 2)",
                    suggestion="Provide a count and a list"
                )
            if not isinstance(args[0], AIFPLNumber):
                raise AIFPLEvalError(
                    message="Take first argument must be a number",
                    received=f"First argument: {self._format_result(args[0])} ({args[0].type_name()})",
                    expected="Number (integer)",
                    example="(take 2 (list 1 2 3)) → (1 2)",
                    suggestion="Provide a numeric count"
                )
            if not isinstance(args[1], AIFPLList):
                raise AIFPLEvalError(
                    message="Take second argument must be a list",
                    received=f"Second argument: {self._format_result(args[1])} ({args[1].type_name()})",
                    expected="List",
                    example="(take 2 (list 1 2 3)) → (1 2)",
                    suggestion="Use take with a list as the second argument"
                )
            n = int(args[0].value)
            return AIFPLList(args[1].elements[:n])

        elif builtin_name == 'drop':
            if len(args) != 2:
                raise AIFPLEvalError(
                    message="Drop takes exactly 2 arguments",
                    received=f"Got {len(args)} arguments",
                    expected="2 arguments: count and list",
                    example="(drop 2 (list 1 2 3)) → (3)",
                    suggestion="Provide a count and a list"
                )
            if not isinstance(args[0], AIFPLNumber):
                raise AIFPLEvalError(
                    message="Drop first argument must be a number",
                    received=f"First argument: {self._format_result(args[0])} ({args[0].type_name()})",
                    expected="Number (integer)",
                    example="(drop 2 (list 1 2 3)) → (3)",
                    suggestion="Provide a numeric count"
                )
            if not isinstance(args[1], AIFPLList):
                raise AIFPLEvalError(
                    message="Drop second argument must be a list",
                    received=f"Second argument: {self._format_result(args[1])} ({args[1].type_name()})",
                    expected="List",
                    example="(drop 2 (list 1 2 3)) → (3)",
                    suggestion="Use drop with a list as the second argument"
                )
            n = int(args[0].value)
            return AIFPLList(args[1].elements[n:])

        elif builtin_name == 'remove':
            if len(args) != 2:
                raise AIFPLEvalError(
                    message="Remove takes exactly 2 arguments",
                    received=f"Got {len(args)} arguments",
                    expected="2 arguments: item and list",
                    example="(remove 2 (list 1 2 3)) → (1 3)",
                    suggestion="Provide an item and a list"
                )
            item = args[0]
            lst = args[1]
            if not isinstance(lst, AIFPLList):
                raise AIFPLEvalError(
                    message="Remove second argument must be a list",
                    received=f"Second argument: {self._format_result(lst)} ({lst.type_name()})",
                    expected="List",
                    example="(remove 2 (list 1 2 3)) → (1 3)",
                    suggestion="Use remove with a list as the second argument"
                )

            result = []
            for elem in lst.elements:
                # Skip matching elements
                if self._values_equal(item, elem):
                    continue
                result.append(elem)
            return AIFPLList(tuple(result))

        elif builtin_name == 'list-ref':
            if len(args) != 2:
                raise AIFPLEvalError(
                    message="List-ref takes exactly 2 arguments",
                    received=f"Got {len(args)} arguments",
                    expected="2 arguments: list and index",
                    example="(list-ref (list \"a\" \"b\" \"c\") 1) → \"b\"",
                    suggestion="Provide a list and an index"
                )
            lst = args[0]
            index_val = args[1]
            
            if not isinstance(lst, AIFPLList):
                raise AIFPLEvalError(
                    message="List-ref first argument must be a list",
                    received=f"First argument: {self._format_result(lst)} ({lst.type_name()})",
                    expected="List",
                    example="(list-ref (list \"a\" \"b\" \"c\") 1) → \"b\"",
                    suggestion="Use list-ref only with lists"
                )
            
            if not isinstance(index_val, AIFPLNumber):
                raise AIFPLEvalError(
                    message="List-ref second argument must be a number",
                    received=f"Second argument: {self._format_result(index_val)} ({index_val.type_name()})",
                    expected="Number (integer index)",
                    example="(list-ref (list \"a\" \"b\" \"c\") 1) → \"b\"",
                    suggestion="Provide a numeric index"
                )
            
            index = int(index_val.value)
            if index < 0 or index >= len(lst.elements):
                raise AIFPLEvalError(
                    message=f"List index out of range",
                    received=f"Index: {index}, List length: {len(lst.elements)}",
                    expected=f"Index in range 0 to {len(lst.elements) - 1}",
                    example="(list-ref (list \"a\" \"b\" \"c\") 1) → \"b\"",
                    suggestion=f"Use an index between 0 and {len(lst.elements) - 1}"
                )
            
            return lst.elements[index]

        # Type predicates
        elif builtin_name == 'number?':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="Number? takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 value",
                    example="(number? 42) → #t",
                    suggestion="Provide a single value to test"
                )
            return AIFPLBoolean(isinstance(args[0], AIFPLNumber))

        elif builtin_name == 'integer?':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="Integer? takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 value",
                    example="(integer? 42) → #t",
                    suggestion="Provide a single value to test"
                )
            return AIFPLBoolean(isinstance(args[0], AIFPLNumber) and args[0].is_integer())

        elif builtin_name == 'float?':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="Float? takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 value",
                    example="(float? 3.14) → #t",
                    suggestion="Provide a single value to test"
                )
            return AIFPLBoolean(isinstance(args[0], AIFPLNumber) and args[0].is_float())

        elif builtin_name == 'complex?':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="Complex? takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 value",
                    example="(complex? (complex 1 2)) → #t",
                    suggestion="Provide a single value to test"
                )
            # Check if value is a number and is complex (has imaginary part)
            if isinstance(args[0], AIFPLNumber):
                value = args[0].value
                # A complex number is one that has an imaginary component
                # In Python, complex type always has imag attribute
                if isinstance(value, complex):
                    return AIFPLBoolean(True)
                else:
                    # int or float are not complex
                    return AIFPLBoolean(False)
            return AIFPLBoolean(False)

        elif builtin_name == 'string?':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="String? takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 value",
                    example="(string? \"hello\") → #t",
                    suggestion="Provide a single value to test"
                )
            return AIFPLBoolean(isinstance(args[0], AIFPLString))

        elif builtin_name == 'boolean?':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="Boolean? takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 value",
                    example="(boolean? #t) → #t",
                    suggestion="Provide a single value to test"
                )
            return AIFPLBoolean(isinstance(args[0], AIFPLBoolean))

        elif builtin_name == 'list?':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="List? takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 value",
                    example="(list? (list 1 2 3)) → #t",
                    suggestion="Provide a single value to test"
                )
            return AIFPLBoolean(isinstance(args[0], AIFPLList))

        elif builtin_name == 'alist?':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="Alist? takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 value",
                    example="(alist? my-alist) → #t",
                    suggestion="Provide a single value to test"
                )
            return AIFPLBoolean(isinstance(args[0], AIFPLAlist))

        elif builtin_name == 'function?':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="Function? takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 value",
                    example="(function? (lambda (x) x)) → #t",
                    suggestion="Provide a single value to test"
                )
            return AIFPLBoolean(isinstance(args[0], (AIFPLFunction, AIFPLBuiltinFunction)))

        # Math functions
        elif builtin_name == 'sqrt':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="Sqrt takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 number",
                    example="(sqrt 16) → 4.0",
                    suggestion="Provide a single number"
                )
            if not isinstance(args[0], AIFPLNumber):
                raise AIFPLEvalError(
                    message="Sqrt requires a number",
                    received=f"Argument: {self._format_result(args[0])} ({args[0].type_name()})",
                    expected="Number",
                    example="(sqrt 16) → 4.0",
                    suggestion="Use sqrt only with numbers"
                )
            val = args[0].value
            # Use cmath.sqrt for negative numbers or complex numbers
            if isinstance(val, complex) or (isinstance(val, (int, float)) and val < 0):
                return AIFPLNumber(cmath.sqrt(val))
            # Use math.sqrt for positive real numbers
            return AIFPLNumber(math.sqrt(val))

        elif builtin_name == 'abs':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="Abs takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 number",
                    example="(abs -5) → 5",
                    suggestion="Provide a single number"
                )
            if not isinstance(args[0], AIFPLNumber):
                raise AIFPLEvalError(
                    message="Abs requires a number",
                    received=f"Argument: {self._format_result(args[0])} ({args[0].type_name()})",
                    expected="Number",
                    example="(abs -5) → 5",
                    suggestion="Use abs only with numbers"
                )
            return AIFPLNumber(abs(args[0].value))

        elif builtin_name == 'min':
            if len(args) < 1:
                raise AIFPLEvalError(
                    message="Min requires at least 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="At least 1 number",
                    example="(min 1 5 3) → 1",
                    suggestion="Provide at least one number"
                )
            values = []
            for i, arg in enumerate(args):
                if not isinstance(arg, AIFPLNumber):
                    raise AIFPLEvalError(
                        message="Min requires numbers",
                        received=f"Argument {i+1}: {self._format_result(arg)} ({arg.type_name()})",
                        expected="Number",
                        example="(min 1 5 3) → 1",
                        suggestion="All arguments to min must be numbers"
                    )
                # Check for complex numbers
                if isinstance(arg.value, complex):
                    raise AIFPLEvalError(
                        message="Function 'min' does not support complex numbers",
                        received=f"Argument {i+1}: {self._format_result(arg)}",
                        suggestion="Use only real numbers with min/max"
                    )
                values.append(arg.value)
            return AIFPLNumber(min(values))

        elif builtin_name == 'max':
            if len(args) < 1:
                raise AIFPLEvalError(
                    message="Max requires at least 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="At least 1 number",
                    example="(max 1 5 3) → 5",
                    suggestion="Provide at least one number"
                )
            values = []
            for i, arg in enumerate(args):
                if not isinstance(arg, AIFPLNumber):
                    raise AIFPLEvalError(
                        message="Max requires numbers",
                        received=f"Argument {i+1}: {self._format_result(arg)} ({arg.type_name()})",
                        expected="Number",
                        example="(max 1 5 3) → 5",
                        suggestion="All arguments to max must be numbers"
                    )
                # Check for complex numbers
                if isinstance(arg.value, complex):
                    raise AIFPLEvalError(
                        message="Function 'max' does not support complex numbers",
                        received=f"Argument {i+1}: {self._format_result(arg)}",
                        suggestion="Use only real numbers with min/max"
                    )
                values.append(arg.value)
            return AIFPLNumber(max(values))

        elif builtin_name == 'pow':
            if len(args) != 2:
                raise AIFPLEvalError(
                    message="Pow takes exactly 2 arguments",
                    received=f"Got {len(args)} arguments",
                    expected="2 numbers: base and exponent",
                    example="(pow 2 3) → 8",
                    suggestion="Provide base and exponent"
                )
            if not isinstance(args[0], AIFPLNumber) or not isinstance(args[1], AIFPLNumber):
                raise AIFPLEvalError(
                    message="Pow requires numbers",
                    received=f"Arguments: {self._format_result(args[0])}, {self._format_result(args[1])}",
                    expected="Two numbers",
                    example="(pow 2 3) → 8",
                    suggestion="Both arguments to pow must be numbers"
                )
            return AIFPLNumber(args[0].value ** args[1].value)

        # Trigonometric functions
        elif builtin_name == 'sin':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="Sin takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 number",
                    example="(sin 0) → 0",
                    suggestion="Provide a single number"
                )
            if not isinstance(args[0], AIFPLNumber):
                raise AIFPLEvalError(
                    message="Sin requires a number",
                    received=f"Argument: {self._format_result(args[0])} ({args[0].type_name()})",
                    expected="Number",
                    example="(sin 0) → 0",
                    suggestion="Use sin only with numbers"
                )
            val = args[0].value
            if isinstance(val, complex):
                return AIFPLNumber(cmath.sin(val))
            return AIFPLNumber(math.sin(val))

        elif builtin_name == 'cos':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="Cos takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 number",
                    example="(cos 0) → 1",
                    suggestion="Provide a single number"
                )
            if not isinstance(args[0], AIFPLNumber):
                raise AIFPLEvalError(
                    message="Cos requires a number",
                    received=f"Argument: {self._format_result(args[0])} ({args[0].type_name()})",
                    expected="Number",
                    example="(cos 0) → 1",
                    suggestion="Use cos only with numbers"
                )
            val = args[0].value
            if isinstance(val, complex):
                return AIFPLNumber(cmath.cos(val))
            return AIFPLNumber(math.cos(val))

        elif builtin_name == 'tan':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="Tan takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 number",
                    example="(tan 0) → 0",
                    suggestion="Provide a single number"
                )
            if not isinstance(args[0], AIFPLNumber):
                raise AIFPLEvalError(
                    message="Tan requires a number",
                    received=f"Argument: {self._format_result(args[0])} ({args[0].type_name()})",
                    expected="Number",
                    example="(tan 0) → 0",
                    suggestion="Use tan only with numbers"
                )
            val = args[0].value
            if isinstance(val, complex):
                return AIFPLNumber(cmath.tan(val))
            return AIFPLNumber(math.tan(val))

        # Logarithmic and exponential functions
        elif builtin_name == 'log':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="Log takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 number",
                    example="(log e) → 1",
                    suggestion="Provide a single number"
                )
            if not isinstance(args[0], AIFPLNumber):
                raise AIFPLEvalError(
                    message="Log requires a number",
                    received=f"Argument: {self._format_result(args[0])} ({args[0].type_name()})",
                    expected="Number",
                    example="(log e) → 1",
                    suggestion="Use log only with numbers"
                )
            val = args[0].value
            # Use cmath for negative numbers or complex numbers
            if isinstance(val, complex) or (isinstance(val, (int, float)) and val < 0):
                return AIFPLNumber(cmath.log(val))
            return AIFPLNumber(math.log(val))

        elif builtin_name == 'log10':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="Log10 takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 number",
                    example="(log10 100) → 2",
                    suggestion="Provide a single number"
                )
            if not isinstance(args[0], AIFPLNumber):
                raise AIFPLEvalError(
                    message="Log10 requires a number",
                    received=f"Argument: {self._format_result(args[0])} ({args[0].type_name()})",
                    expected="Number",
                    example="(log10 100) → 2",
                    suggestion="Use log10 only with numbers"
                )
            val = args[0].value
            # Use cmath for negative numbers or complex numbers
            if isinstance(val, complex) or (isinstance(val, (int, float)) and val < 0):
                return AIFPLNumber(cmath.log10(val))
            return AIFPLNumber(math.log10(val))

        elif builtin_name == 'exp':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="Exp takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 number",
                    example="(exp 1) → e",
                    suggestion="Provide a single number"
                )
            if not isinstance(args[0], AIFPLNumber):
                raise AIFPLEvalError(
                    message="Exp requires a number",
                    received=f"Argument: {self._format_result(args[0])} ({args[0].type_name()})",
                    expected="Number",
                    example="(exp 1) → e",
                    suggestion="Use exp only with numbers"
                )
            val = args[0].value
            if isinstance(val, complex):
                return AIFPLNumber(cmath.exp(val))
            return AIFPLNumber(math.exp(val))

        # Rounding functions
        elif builtin_name == 'round':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="Round takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 number",
                    example="(round 3.7) → 4",
                    suggestion="Provide a single number"
                )
            if not isinstance(args[0], AIFPLNumber):
                raise AIFPLEvalError(
                    message="Round requires a number",
                    received=f"Argument: {self._format_result(args[0])} ({args[0].type_name()})",
                    expected="Number",
                    example="(round 3.7) → 4",
                    suggestion="Use round only with numbers"
                )
            val = args[0].value
            # Check if it's complex (can't round complex numbers)
            if isinstance(val, complex):
                # Check if imaginary part is negligible
                if abs(val.imag) > 1e-10:
                    raise AIFPLEvalError(
                        message="Function 'round' does not support complex numbers"
                    )
                val = val.real
            return AIFPLNumber(round(val))

        elif builtin_name == 'floor':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="Floor takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 number",
                    example="(floor 3.7) → 3",
                    suggestion="Provide a single number"
                )
            if not isinstance(args[0], AIFPLNumber):
                raise AIFPLEvalError(
                    message="Floor requires a number",
                    received=f"Argument: {self._format_result(args[0])} ({args[0].type_name()})",
                    expected="Number",
                    example="(floor 3.7) → 3",
                    suggestion="Use floor only with numbers"
                )
            val = args[0].value
            # Check if it's complex (can't floor complex numbers)
            if isinstance(val, complex):
                # Check if imaginary part is negligible
                if abs(val.imag) > 1e-10:
                    raise AIFPLEvalError(
                        message="Function 'floor' does not support complex numbers"
                    )
                val = val.real
            return AIFPLNumber(math.floor(val))

        elif builtin_name == 'ceil':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="Ceil takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 number",
                    example="(ceil 3.2) → 4",
                    suggestion="Provide a single number"
                )
            if not isinstance(args[0], AIFPLNumber):
                raise AIFPLEvalError(
                    message="Ceil requires a number",
                    received=f"Argument: {self._format_result(args[0])} ({args[0].type_name()})",
                    expected="Number",
                    example="(ceil 3.2) → 4",
                    suggestion="Use ceil only with numbers"
                )
            val = args[0].value
            # Check if it's complex (can't ceil complex numbers)
            if isinstance(val, complex):
                # Check if imaginary part is negligible
                if abs(val.imag) > 1e-10:
                    raise AIFPLEvalError(
                        message="Function 'ceil' does not support complex numbers"
                    )
                val = val.real
            return AIFPLNumber(math.ceil(val))

        # Base conversion functions
        elif builtin_name == 'bin':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="Bin takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 integer",
                    example='(bin 255) → "0b11111111"',
                    suggestion="Provide a single integer"
                )
            if not isinstance(args[0], AIFPLNumber):
                raise AIFPLEvalError(
                    message="Bin requires a number",
                    received=f"Argument: {self._format_result(args[0])} ({args[0].type_name()})",
                    expected="Integer",
                    example='(bin 255) → "0b11111111"',
                    suggestion="Use bin only with integers"
                )
            val = args[0].value
            if not isinstance(val, int) and not (isinstance(val, float) and val.is_integer()):
                raise AIFPLEvalError(
                    message="Bin requires an integer",
                    received=f"Value: {val}",
                    expected="Integer",
                    example='(bin 255) → "0b11111111"',
                    suggestion="Provide an integer value"
                )
            return AIFPLString(bin(int(val)))

        elif builtin_name == 'hex':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="Hex takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 integer",
                    example='(hex 255) → "0xff"',
                    suggestion="Provide a single integer"
                )
            if not isinstance(args[0], AIFPLNumber):
                raise AIFPLEvalError(
                    message="Hex requires a number",
                    received=f"Argument: {self._format_result(args[0])} ({args[0].type_name()})",
                    expected="Integer",
                    example='(hex 255) → "0xff"',
                    suggestion="Use hex only with integers"
                )
            val = args[0].value
            if not isinstance(val, int) and not (isinstance(val, float) and val.is_integer()):
                raise AIFPLEvalError(
                    message="Hex requires an integer",
                    received=f"Value: {val}",
                    expected="Integer",
                    example='(hex 255) → "0xff"',
                    suggestion="Provide an integer value"
                )
            return AIFPLString(hex(int(val)))

        elif builtin_name == 'oct':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="Oct takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 integer",
                    example='(oct 64) → "0o100"',
                    suggestion="Provide a single integer"
                )
            if not isinstance(args[0], AIFPLNumber):
                raise AIFPLEvalError(
                    message="Oct requires a number",
                    received=f"Argument: {self._format_result(args[0])} ({args[0].type_name()})",
                    expected="Integer",
                    example='(oct 64) → "0o100"',
                    suggestion="Use oct only with integers"
                )
            val = args[0].value
            if not isinstance(val, int) and not (isinstance(val, float) and val.is_integer()):
                raise AIFPLEvalError(
                    message="Oct requires an integer",
                    received=f"Value: {val}",
                    expected="Integer",
                    example='(oct 64) → "0o100"',
                    suggestion="Provide an integer value"
                )
            return AIFPLString(oct(int(val)))

        # Complex number functions
        elif builtin_name == 'real':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="Real takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 number",
                    example="(real (complex 3 4)) → 3",
                    suggestion="Provide a single number"
                )
            if not isinstance(args[0], AIFPLNumber):
                raise AIFPLEvalError(
                    message="Real requires a number",
                    received=f"Argument: {self._format_result(args[0])} ({args[0].type_name()})",
                    expected="Number",
                    example="(real (complex 3 4)) → 3",
                    suggestion="Use real only with numbers"
                )
            val = args[0].value
            if isinstance(val, complex):
                result = val.real
            else:
                result = val
            # Return as integer if it's a whole number
            if isinstance(result, float) and result.is_integer():
                return AIFPLNumber(int(result))
            return AIFPLNumber(result)

        elif builtin_name == 'imag':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="Imag takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 number",
                    example="(imag (complex 3 4)) → 4",
                    suggestion="Provide a single number"
                )
            if not isinstance(args[0], AIFPLNumber):
                raise AIFPLEvalError(
                    message="Imag requires a number",
                    received=f"Argument: {self._format_result(args[0])} ({args[0].type_name()})",
                    expected="Number",
                    example="(imag (complex 3 4)) → 4",
                    suggestion="Use imag only with numbers"
                )
            val = args[0].value
            if isinstance(val, complex):
                result = val.imag
            else:
                result = 0
            # Return as integer if it's a whole number
            if isinstance(result, float) and result.is_integer():
                return AIFPLNumber(int(result))
            return AIFPLNumber(result)

        elif builtin_name == 'complex':
            if len(args) != 2:
                raise AIFPLEvalError(
                    message="Complex takes exactly 2 arguments",
                    received=f"Got {len(args)} arguments",
                    expected="2 numbers: real and imaginary parts",
                    example="(complex 3 4) → (3+4j)",
                    suggestion="Provide real and imaginary parts"
                )
            if not isinstance(args[0], AIFPLNumber) or not isinstance(args[1], AIFPLNumber):
                if not isinstance(args[0], AIFPLNumber):
                    bad_arg = 0
                else:
                    bad_arg = 1
                raise AIFPLEvalError(
                    message="Complex requires numbers",
                    received=f"Argument {bad_arg+1}: {self._format_result(args[bad_arg])} ({args[bad_arg].type_name()})",
                    expected="Number",
                    example="(complex 3 4) → (3+4j)",
                    suggestion="Both arguments must be numbers"
                )
            real_part = args[0].value
            imag_part = args[1].value
            # Reject complex arguments
            if isinstance(real_part, complex):
                raise AIFPLEvalError(
                    message="complex arguments must be real numbers"
                )
            if isinstance(imag_part, complex):
                raise AIFPLEvalError(
                    message="complex arguments must be real numbers"
                )
            return AIFPLNumber(complex(real_part, imag_part))

        # Higher-order functions
        elif builtin_name == 'range':
            if len(args) < 2 or len(args) > 3:
                raise AIFPLEvalError(
                    message="Range requires 2 or 3 arguments",
                    received=f"Got {len(args)} arguments",
                    expected="2 or 3 numbers: start, end, [step]",
                    example="(range 1 5) → (1 2 3 4) or (range 0 10 2) → (0 2 4 6 8)",
                    suggestion="Provide start, end, and optionally step"
                )
            
            # Check each argument is a number with specific error messages
            if len(args) == 2:
                # 2-argument form: (range start end)
                if not isinstance(args[0], AIFPLNumber):
                    raise AIFPLEvalError(
                        message="Range start must be a number",
                        received=f"Start: {self._format_result(args[0])} ({args[0].type_name()})",
                        expected="Number (integer or float)",
                        example="(range 1 5)",
                        suggestion="Use numeric values for range bounds"
                    )
                if not isinstance(args[1], AIFPLNumber):
                    raise AIFPLEvalError(
                        message="Range end must be a number",
                        received=f"End: {self._format_result(args[1])} ({args[1].type_name()})",
                        expected="Number (integer or float)",
                        example="(range 1 5)",
                        suggestion="Use numeric values for range bounds"
                    )
            else:
                # 3-argument form: (range start end step)
                if not isinstance(args[0], AIFPLNumber):
                    raise AIFPLEvalError(
                        message="Range start must be a number",
                        received=f"Start: {self._format_result(args[0])} ({args[0].type_name()})",
                        expected="Number (integer or float)",
                        example="(range 0 10 2)",
                        suggestion="Use numeric values for range parameters"
                    )
                if not isinstance(args[1], AIFPLNumber):
                    raise AIFPLEvalError(
                        message="Range end must be a number",
                        received=f"End: {self._format_result(args[1])} ({args[1].type_name()})",
                        expected="Number (integer or float)",
                        example="(range 0 10 2)",
                        suggestion="Use numeric values for range parameters"
                    )
                if not isinstance(args[2], AIFPLNumber):
                    raise AIFPLEvalError(
                        message="Range step must be a number",
                        received=f"Step: {self._format_result(args[2])} ({args[2].type_name()})",
                        expected="Number (integer or float)",
                        example="(range 0 10 2)",
                        suggestion="Use numeric values for range parameters"
                    )

            # Validate that all arguments are integers (or floats that are whole numbers)
            # This matches the _ensure_integer behavior in the interpreter
            for i, arg in enumerate(args):
                val = arg.value
                param_name = ["start", "end", "step"][i] if len(args) == 3 else ["start", "end"][i]
                
                if isinstance(val, complex):
                    raise AIFPLEvalError(
                        message="Function 'range' requires integer arguments",
                        received=f"Got: {val} (complex)",
                        expected="Integer number",
                        suggestion="Use whole numbers without decimal points",
                        example="(range 1 5) not (range (complex 1 2) 5)"
                    )
                if isinstance(val, float) and not val.is_integer():
                    raise AIFPLEvalError(
                        message="Function 'range' requires integer arguments",
                        received=f"Got: {val} (float)",
                        expected="Integer number",
                        suggestion="Use whole numbers without decimal points",
                        example="(range 1 5) not (range 1.5 5)"
                    )
                # Validate the value can be converted to int
                try:
                    int(val)
                except (ValueError, OverflowError):
                    raise AIFPLEvalError(
                        message=f"Range {param_name} cannot be converted to integer",
                        received=f"Value: {val}",
                        expected="Valid integer",
                        suggestion="Provide integer values"
                    )

            start = int(args[0].value)
            end = int(args[1].value)
            step = int(args[2].value) if len(args) == 3 else 1

            if step == 0:
                raise AIFPLEvalError(
                    message="Range step cannot be zero",
                    received="Step: 0",
                    expected="Non-zero integer",
                    example="(range 0 10 2) or (range 10 0 -1)",
                    suggestion="Use positive step for ascending range, negative for descending"
                )

            result = []
            if step > 0:
                current = start
                while current < end:
                    result.append(AIFPLNumber(current))
                    current += step
            else:
                current = start
                while current > end:
                    result.append(AIFPLNumber(current))
                    current += step

            return AIFPLList(tuple(result))

        elif builtin_name == 'map':
            if len(args) != 2:
                raise AIFPLEvalError(
                    message="Map function has wrong number of arguments",
                    received=f"Got {len(args)} arguments",
                    expected="Exactly 2 arguments: (map function list)",
                    example="(map (lambda (x) (* x 2)) (list 1 2 3))",
                    suggestion="Map takes a function and a list"
                )
            func = args[0]
            lst = args[1]

            if not isinstance(func, (AIFPLFunction, AIFPLBuiltinFunction)):
                raise AIFPLEvalError(
                    message="Map first argument must be a function",
                    received=f"First argument: {self._format_result(func)} ({func.type_name()})",
                    expected="Function (lambda or builtin)",
                    example="(map (lambda (x) (* x 2)) (list 1 2 3))",
                    suggestion="Provide a function as the first argument"
                )
            if not isinstance(lst, AIFPLList):
                raise AIFPLEvalError(
                    message="Map second argument must be a list",
                    received=f"Second argument: {self._format_result(lst)} ({lst.type_name()})",
                    expected="List of values",
                    example="(map (lambda (x) (* x 2)) (list 1 2 3))",
                    suggestion="Use (list ...) to create a list"
                )

            result = []
            for item in lst.elements:
                # Call function with item
                if isinstance(func, AIFPLFunction):
                    item_result = self._call_bytecode_function(func, [item])
                else:  # AIFPLBuiltinFunction
                    # Call through evaluator
                    if self.evaluator is None:
                        raise AIFPLEvalError("Cannot call builtin without evaluator")
                    item_result = func.native_impl([item], AIFPLEnvironment(), 0)
                result.append(item_result)

            return AIFPLList(tuple(result))

        elif builtin_name == 'filter':
            if len(args) != 2:
                raise AIFPLEvalError(
                    message="Filter function has wrong number of arguments",
                    received=f"Got {len(args)} arguments",
                    expected="Exactly 2 arguments: (filter predicate list)",
                    example="(filter (lambda (x) (> x 0)) (list -1 2 -3 4))",
                    suggestion="Filter takes a predicate function and a list"
                )
            func = args[0]
            lst = args[1]

            if not isinstance(func, (AIFPLFunction, AIFPLBuiltinFunction)):
                raise AIFPLEvalError(
                    message="Filter first argument must be a function",
                    received=f"First argument: {self._format_result(func)} ({func.type_name()})",
                    expected="Function (lambda or builtin)",
                    example="(filter (lambda (x) (> x 0)) (list -1 2 -3 4))",
                    suggestion="Provide a predicate function as the first argument"
                )
            if not isinstance(lst, AIFPLList):
                raise AIFPLEvalError(
                    message="Filter second argument must be a list",
                    received=f"Second argument: {self._format_result(lst)} ({lst.type_name()})",
                    expected="List of values",
                    example="(filter (lambda (x) (> x 0)) (list -1 2 -3 4))",
                    suggestion="Use (list ...) to create a list"
                )

            result = []
            for item in lst.elements:
                # Call function with item
                if isinstance(func, AIFPLFunction):
                    test_result = self._call_bytecode_function(func, [item])
                else:  # AIFPLBuiltinFunction
                    if self.evaluator is None:
                        raise AIFPLEvalError("Cannot call builtin without evaluator")
                    test_result = func.native_impl([item], AIFPLEnvironment(), 0)
                if not isinstance(test_result, AIFPLBoolean):
                    raise AIFPLEvalError(
                        message="Filter predicate must return boolean",
                        received=f"Predicate returned: {self._format_result(test_result)} ({test_result.type_name()})",
                        expected="Boolean value (#t or #f)",
                        example="(filter (lambda (x) (> x 0)) (list -1 2 -3 4))",
                        suggestion="Predicate function should use comparison operators"
                    )
                if test_result.value:
                    result.append(item)

            return AIFPLList(tuple(result))

        elif builtin_name == 'fold':
            if len(args) != 3:
                raise AIFPLEvalError(
                    message="Fold function has wrong number of arguments",
                    received=f"Got {len(args)} arguments",
                    expected="Exactly 3 arguments: (fold function initial list)",
                    example="(fold + 0 (list 1 2 3 4))",
                    suggestion="Fold takes a function, initial value, and list"
                )
            func = args[0]
            init = args[1]
            lst = args[2]

            if not isinstance(func, (AIFPLFunction, AIFPLBuiltinFunction)):
                raise AIFPLEvalError(
                    message="Fold first argument must be a function",
                    received=f"First argument: {self._format_result(func)} ({func.type_name()})",
                    expected="Function (lambda or builtin)",
                    example="(fold + 0 (list 1 2 3 4))",
                    suggestion="Provide a function as the first argument"
                )
            if not isinstance(lst, AIFPLList):
                raise AIFPLEvalError(
                    message="Fold third argument must be a list",
                    received=f"Third argument: {self._format_result(lst)} ({lst.type_name()})",
                    expected="List of values",
                    example="(fold + 0 (list 1 2 3 4))",
                    suggestion="Use (list ...) to create a list"
                )

            accumulator = init
            for item in lst.elements:
                # Call function with accumulator and item
                if isinstance(func, AIFPLFunction):
                    accumulator = self._call_bytecode_function(func, [accumulator, item])
                else:  # AIFPLBuiltinFunction
                    if self.evaluator is None:
                        raise AIFPLEvalError("Cannot call builtin without evaluator")
                    accumulator = func.native_impl([accumulator, item], AIFPLEnvironment(), 0)

            return accumulator

        elif builtin_name == 'find':
            if len(args) != 2:
                raise AIFPLEvalError(
                    message="Find function has wrong number of arguments",
                    received=f"Got {len(args)} arguments",
                    expected="Exactly 2 arguments: (find predicate list)",
                    example="(find (lambda (x) (> x 5)) (list 1 3 7 2))",
                    suggestion="Find takes a predicate function and a list"
                )
            func = args[0]
            lst = args[1]

            if not isinstance(func, (AIFPLFunction, AIFPLBuiltinFunction)):
                raise AIFPLEvalError(
                    message="Find first argument must be a function",
                    received=f"First argument: {self._format_result(func)} ({func.type_name()})",
                    expected="Function (lambda or builtin)",
                    example="(find (lambda (x) (> x 5)) (list 1 3 7 2))",
                    suggestion="Provide a predicate function as the first argument"
                )
            if not isinstance(lst, AIFPLList):
                raise AIFPLEvalError(
                    message="Find second argument must be a list",
                    received=f"Second argument: {self._format_result(lst)} ({lst.type_name()})",
                    expected="List of values",
                    example="(find (lambda (x) (> x 5)) (list 1 3 7 2))",
                    suggestion="Use (list ...) to create a list"
                )

            for item in lst.elements:
                # Call function with item
                if isinstance(func, AIFPLFunction):
                    test_result = self._call_bytecode_function(func, [item])
                else:  # AIFPLBuiltinFunction
                    if self.evaluator is None:
                        raise AIFPLEvalError("Cannot call builtin without evaluator")
                    test_result = func.native_impl([item], AIFPLEnvironment(), 0)
                if not isinstance(test_result, AIFPLBoolean):
                    raise AIFPLEvalError(
                        message="Find predicate must return boolean",
                        received=f"Predicate returned: {self._format_result(test_result)} ({test_result.type_name()})",
                        expected="Boolean value (#t or #f)",
                        example="(find (lambda (x) (> x 5)) (list 1 3 7 2))",
                        suggestion="Predicate function should use comparison operators"
                    )
                if test_result.value:
                    return item

            return AIFPLBoolean(False)

        elif builtin_name == 'any?':
            if len(args) != 2:
                raise AIFPLEvalError(
                    message="Any? function has wrong number of arguments",
                    received=f"Got {len(args)} arguments",
                    expected="Exactly 2 arguments: (any? predicate list)",
                    example="(any? (lambda (x) (> x 5)) (list 1 3 7))",
                    suggestion="Any? takes a predicate function and a list"
                )
            func = args[0]
            lst = args[1]

            if not isinstance(func, (AIFPLFunction, AIFPLBuiltinFunction)):
                raise AIFPLEvalError(
                    message="Any? first argument must be a function",
                    received=f"First argument: {self._format_result(func)} ({func.type_name()})",
                    expected="Function (lambda or builtin)",
                    example="(any? (lambda (x) (> x 5)) (list 1 3 7))",
                    suggestion="Provide a predicate function as the first argument"
                )
            if not isinstance(lst, AIFPLList):
                raise AIFPLEvalError(
                    message="Any? second argument must be a list",
                    received=f"Second argument: {self._format_result(lst)} ({lst.type_name()})",
                    expected="List of values",
                    example="(any? (lambda (x) (> x 5)) (list 1 3 7))",
                    suggestion="Use (list ...) to create a list"
                )

            for item in lst.elements:
                # Call function with item
                if isinstance(func, AIFPLFunction):
                    test_result = self._call_bytecode_function(func, [item])
                else:  # AIFPLBuiltinFunction
                    if self.evaluator is None:
                        raise AIFPLEvalError("Cannot call builtin without evaluator")
                    test_result = func.native_impl([item], AIFPLEnvironment(), 0)
                if not isinstance(test_result, AIFPLBoolean):
                    raise AIFPLEvalError(
                        message="Any? predicate must return boolean",
                        received=f"Predicate returned: {self._format_result(test_result)} ({test_result.type_name()})",
                        expected="Boolean value (#t or #f)",
                        example="(any? (lambda (x) (> x 5)) (list 1 3 7))",
                        suggestion="Predicate function should use comparison operators"
                    )
                if test_result.value:
                    return AIFPLBoolean(True)

            return AIFPLBoolean(False)

        elif builtin_name == 'all?':
            if len(args) != 2:
                raise AIFPLEvalError(
                    message="All? function has wrong number of arguments",
                    received=f"Got {len(args)} arguments",
                    expected="Exactly 2 arguments: (all? predicate list)",
                    example="(all? (lambda (x) (> x 0)) (list 1 3 7))",
                    suggestion="All? takes a predicate function and a list"
                )
            func = args[0]
            lst = args[1]

            if not isinstance(func, (AIFPLFunction, AIFPLBuiltinFunction)):
                raise AIFPLEvalError(
                    message="All? first argument must be a function",
                    received=f"First argument: {self._format_result(func)} ({func.type_name()})",
                    expected="Function (lambda or builtin)",
                    example="(all? (lambda (x) (> x 0)) (list 1 3 7))",
                    suggestion="Provide a predicate function as the first argument"
                )
            if not isinstance(lst, AIFPLList):
                raise AIFPLEvalError(
                    message="All? second argument must be a list",
                    received=f"Second argument: {self._format_result(lst)} ({lst.type_name()})",
                    expected="List of values",
                    example="(all? (lambda (x) (> x 0)) (list 1 3 7))",
                    suggestion="Use (list ...) to create a list"
                )

            for item in lst.elements:
                # Call function with item
                if isinstance(func, AIFPLFunction):
                    test_result = self._call_bytecode_function(func, [item])
                else:  # AIFPLBuiltinFunction
                    if self.evaluator is None:
                        raise AIFPLEvalError("Cannot call builtin without evaluator")
                    test_result = func.native_impl([item], AIFPLEnvironment(), 0)
                if not isinstance(test_result, AIFPLBoolean):
                    raise AIFPLEvalError(
                        message="All? predicate must return boolean",
                        received=f"Predicate returned: {self._format_result(test_result)} ({test_result.type_name()})",
                        expected="Boolean value (#t or #f)",
                        example="(all? (lambda (x) (> x 0)) (list 1 3 7))",
                        suggestion="Predicate function should use comparison operators"
                    )
                if not test_result.value:
                    return AIFPLBoolean(False)

            return AIFPLBoolean(True)

        # String operations
        elif builtin_name == 'string-append':
            result = ""
            for i, arg in enumerate(args):
                if not isinstance(arg, AIFPLString):
                    raise AIFPLEvalError(
                        message="String-append requires strings",
                        received=f"Argument {i+1}: {self._format_result(arg)} ({arg.type_name()})",
                        expected="String",
                        example='(string-append "hello" " " "world") → "hello world"',
                        suggestion="All arguments to string-append must be strings"
                    )
                result += arg.value
            return AIFPLString(result)

        elif builtin_name == 'string-length':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="String-length takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 string",
                    example='(string-length "hello") → 5',
                    suggestion="Provide a single string"
                )
            if not isinstance(args[0], AIFPLString):
                raise AIFPLEvalError(
                    message="String-length requires a string",
                    received=f"Argument: {self._format_result(args[0])} ({args[0].type_name()})",
                    expected="String",
                    example='(string-length "hello") → 5',
                    suggestion="Use string-length only with strings"
                )
            return AIFPLNumber(len(args[0].value))

        elif builtin_name == 'string-upcase':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="String-upcase takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 string",
                    example='(string-upcase "hello") → "HELLO"',
                    suggestion="Provide a single string"
                )
            if not isinstance(args[0], AIFPLString):
                raise AIFPLEvalError(
                    message="String-upcase requires a string",
                    received=f"Argument: {self._format_result(args[0])} ({args[0].type_name()})",
                    expected="String",
                    example='(string-upcase "hello") → "HELLO"',
                    suggestion="Use string-upcase only with strings"
                )
            return AIFPLString(args[0].value.upper())

        elif builtin_name == 'string-downcase':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="String-downcase takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 string",
                    example='(string-downcase "HELLO") → "hello"',
                    suggestion="Provide a single string"
                )
            if not isinstance(args[0], AIFPLString):
                raise AIFPLEvalError(
                    message="String-downcase requires a string",
                    received=f"Argument: {self._format_result(args[0])} ({args[0].type_name()})",
                    expected="String",
                    example='(string-downcase "HELLO") → "hello"',
                    suggestion="Use string-downcase only with strings"
                )
            return AIFPLString(args[0].value.lower())

        elif builtin_name == 'string-trim':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="String-trim takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 string",
                    example='(string-trim "  hello  ") → "hello"',
                    suggestion="Provide a single string"
                )
            if not isinstance(args[0], AIFPLString):
                raise AIFPLEvalError(
                    message="String-trim requires a string",
                    received=f"Argument: {self._format_result(args[0])} ({args[0].type_name()})",
                    expected="String",
                    example='(string-trim "  hello  ") → "hello"',
                    suggestion="Use string-trim only with strings"
                )
            return AIFPLString(args[0].value.strip())

        elif builtin_name == 'string-replace':
            if len(args) != 3:
                raise AIFPLEvalError(
                    message="String-replace takes exactly 3 arguments",
                    received=f"Got {len(args)} arguments",
                    expected="3 strings: string, old, new",
                    example='(string-replace "banana" "a" "o") → "bonono"',
                    suggestion="Provide string, substring to replace, and replacement"
                )
            if not all(isinstance(arg, AIFPLString) for arg in args):
                non_strings = [i for i, arg in enumerate(args) if not isinstance(arg, AIFPLString)]
                raise AIFPLEvalError(
                    message="String-replace requires strings",
                    received=f"Argument {non_strings[0]+1}: {self._format_result(args[non_strings[0]])} ({args[non_strings[0]].type_name()})",
                    expected="String",
                    example='(string-replace "banana" "a" "o") → "bonono"',
                    suggestion="All arguments to string-replace must be strings"
                )
            string = args[0].value
            old = args[1].value
            new = args[2].value
            return AIFPLString(string.replace(old, new))

        elif builtin_name == 'string-split':
            if len(args) != 2:
                raise AIFPLEvalError(
                    message="String-split takes exactly 2 arguments",
                    received=f"Got {len(args)} arguments",
                    expected="2 strings: string and delimiter",
                    example='(string-split "a,b,c" ",") → ("a" "b" "c")',
                    suggestion="Provide a string and a delimiter"
                )
            if not isinstance(args[0], AIFPLString) or not isinstance(args[1], AIFPLString):
                if not isinstance(args[0], AIFPLString):
                    bad_arg = 0
                else:
                    bad_arg = 1
                raise AIFPLEvalError(
                    message="String-split requires strings",
                    received=f"Argument {bad_arg+1}: {self._format_result(args[bad_arg])} ({args[bad_arg].type_name()})",
                    expected="String",
                    example='(string-split "a,b,c" ",") → ("a" "b" "c")',
                    suggestion="Both arguments to string-split must be strings"
                )
            string = args[0].value
            delimiter = args[1].value
            
            # Handle empty delimiter - split into individual characters
            if delimiter == "":
                return AIFPLList(tuple(AIFPLString(char) for char in string))
            
            parts = string.split(delimiter)
            return AIFPLList(tuple(AIFPLString(part) for part in parts))

        elif builtin_name == 'string-join':
            if len(args) != 2:
                raise AIFPLEvalError(
                    message="String-join takes exactly 2 arguments",
                    received=f"Got {len(args)} arguments",
                    expected="2 arguments: list and delimiter",
                    example='(string-join (list "hello" "world") " ") → "hello world"',
                    suggestion="Provide a list of strings and a delimiter"
                )
            if not isinstance(args[0], AIFPLList):
                raise AIFPLEvalError(
                    message="String-join first argument must be a list",
                    received=f"First argument: {self._format_result(args[0])} ({args[0].type_name()})",
                    expected="List of strings",
                    example='(string-join (list "hello" "world") " ") → "hello world"',
                    suggestion="Use a list as the first argument"
                )
            if not isinstance(args[1], AIFPLString):
                raise AIFPLEvalError(
                    message="String-join second argument must be a string",
                    received=f"Second argument: {self._format_result(args[1])} ({args[1].type_name()})",
                    expected="String (delimiter)",
                    example='(string-join (list "hello" "world") " ") → "hello world"',
                    suggestion="Use a string as the delimiter"
                )

            parts = []
            for item in args[0].elements:
                if not isinstance(item, AIFPLString):
                    raise AIFPLEvalError(
                        message="String-join list must contain strings",
                        received=f"List element: {self._format_result(item)} ({item.type_name()})",
                        expected="String",
                        example='(string-join (list "hello" "world") " ") → "hello world"',
                        suggestion="All list elements must be strings"
                    )
                parts.append(item.value)

            delimiter = args[1].value
            return AIFPLString(delimiter.join(parts))

        elif builtin_name == 'string-contains?':
            if len(args) != 2:
                raise AIFPLEvalError(
                    message="String-contains? takes exactly 2 arguments",
                    received=f"Got {len(args)} arguments",
                    expected="2 strings: string and substring",
                    example='(string-contains? "hello" "ell") → #t',
                    suggestion="Provide a string and a substring to search for"
                )
            if not isinstance(args[0], AIFPLString) or not isinstance(args[1], AIFPLString):
                if not isinstance(args[0], AIFPLString):
                    bad_arg = 0
                else:
                    bad_arg = 1
                raise AIFPLEvalError(
                    message="String-contains? requires strings",
                    received=f"Argument {bad_arg+1}: {self._format_result(args[bad_arg])} ({args[bad_arg].type_name()})",
                    expected="String",
                    example='(string-contains? "hello" "ell") → #t',
                    suggestion="Both arguments must be strings"
                )
            return AIFPLBoolean(args[1].value in args[0].value)

        elif builtin_name == 'string-prefix?':
            if len(args) != 2:
                raise AIFPLEvalError(
                    message="String-prefix? takes exactly 2 arguments",
                    received=f"Got {len(args)} arguments",
                    expected="2 strings: string and prefix",
                    example='(string-prefix? "hello" "he") → #t',
                    suggestion="Provide a string and a prefix to test"
                )
            if not isinstance(args[0], AIFPLString) or not isinstance(args[1], AIFPLString):
                if not isinstance(args[0], AIFPLString):
                    bad_arg = 0
                else:
                    bad_arg = 1
                raise AIFPLEvalError(
                    message="String-prefix? requires strings",
                    received=f"Argument {bad_arg+1}: {self._format_result(args[bad_arg])} ({args[bad_arg].type_name()})",
                    expected="String",
                    example='(string-prefix? "hello" "he") → #t',
                    suggestion="Both arguments must be strings"
                )
            return AIFPLBoolean(args[0].value.startswith(args[1].value))

        elif builtin_name == 'string-suffix?':
            if len(args) != 2:
                raise AIFPLEvalError(
                    message="String-suffix? takes exactly 2 arguments",
                    received=f"Got {len(args)} arguments",
                    expected="2 strings: string and suffix",
                    example='(string-suffix? "hello" "lo") → #t',
                    suggestion="Provide a string and a suffix to test"
                )
            if not isinstance(args[0], AIFPLString) or not isinstance(args[1], AIFPLString):
                if not isinstance(args[0], AIFPLString):
                    bad_arg = 0
                else:
                    bad_arg = 1
                raise AIFPLEvalError(
                    message="String-suffix? requires strings",
                    received=f"Argument {bad_arg+1}: {self._format_result(args[bad_arg])} ({args[bad_arg].type_name()})",
                    expected="String",
                    example='(string-suffix? "hello" "lo") → #t',
                    suggestion="Both arguments must be strings"
                )
            return AIFPLBoolean(args[0].value.endswith(args[1].value))

        elif builtin_name == 'string-ref':
            if len(args) != 2:
                raise AIFPLEvalError(
                    message="String-ref takes exactly 2 arguments",
                    received=f"Got {len(args)} arguments",
                    expected="2 arguments: string and index",
                    example='(string-ref "hello" 1) → "e"',
                    suggestion="Provide a string and an index"
                )
            if not isinstance(args[0], AIFPLString):
                raise AIFPLEvalError(
                    message="String-ref first argument must be a string",
                    received=f"Argument 1: {self._format_result(args[0])} ({args[0].type_name()})",
                    expected="String",
                    example='(string-ref "hello" 1) → "e"',
                    suggestion="Use string-ref only with strings"
                )
            if not isinstance(args[1], AIFPLNumber):
                raise AIFPLEvalError(
                    message="String-ref second argument must be a number",
                    received=f"Argument 2: {self._format_result(args[1])} ({args[1].type_name()})",
                    expected="Number (integer index)",
                    example='(string-ref "hello" 1) → "e"',
                    suggestion="Provide a numeric index"
                )
            index = int(args[1].value)
            if index < 0 or index >= len(args[0].value):
                raise AIFPLEvalError(
                    message=f"String index out of range",
                    received=f"Index: {index}, String length: {len(args[0].value)}",
                    expected=f"Index in range 0 to {len(args[0].value) - 1}",
                    example='(string-ref "hello" 1) → "e"',
                    suggestion=f"Use an index between 0 and {len(args[0].value) - 1}"
                )
            return AIFPLString(args[0].value[index])

        elif builtin_name == 'substring':
            if len(args) != 3:
                raise AIFPLEvalError(
                    message="Substring takes exactly 3 arguments",
                    received=f"Got {len(args)} arguments",
                    expected="3 arguments: string, start, end",
                    example='(substring "hello" 1 4) → "ell"',
                    suggestion="Provide a string, start index, and end index"
                )
            if not isinstance(args[0], AIFPLString):
                raise AIFPLEvalError(
                    message="Substring first argument must be a string",
                    received=f"First argument: {self._format_result(args[0])} ({args[0].type_name()})",
                    expected="String",
                    example='(substring "hello" 1 4) → "ell"',
                    suggestion="Use a string as the first argument"
                )
            if not isinstance(args[1], AIFPLNumber) or not isinstance(args[2], AIFPLNumber):
                if not isinstance(args[1], AIFPLNumber):
                    bad_arg = 1
                else:
                    bad_arg = 2
                raise AIFPLEvalError(
                    message="Substring indices must be numbers",
                    received=f"Argument {bad_arg+1}: {self._format_result(args[bad_arg])} ({args[bad_arg].type_name()})",
                    expected="Number (integer index)",
                    example='(substring "hello" 1 4) → "ell"',
                    suggestion="Use numeric indices"
                )
            string = args[0].value
            start = int(args[1].value)
            end = int(args[2].value)
            
            # Validate indices (match interpreter behavior)
            string_len = len(string)
            if start < 0:
                raise AIFPLEvalError(
                    message="substring start index cannot be negative",
                    received=f"Start index: {start}",
                    expected="Non-negative integer",
                    example='(substring "hello" 1 4) → "ell"',
                    suggestion="Use a non-negative start index"
                )
            if end < 0:
                raise AIFPLEvalError(
                    message="substring end index cannot be negative",
                    received=f"End index: {end}",
                    expected="Non-negative integer",
                    example='(substring "hello" 1 4) → "ell"',
                    suggestion="Use a non-negative end index"
                )
            if start > string_len:
                raise AIFPLEvalError(
                    message="substring start index out of range",
                    received=f"Start index: {start}, string length: {string_len}",
                    expected=f"Index between 0 and {string_len}",
                    example='(substring "hello" 1 4) → "ell"',
                    suggestion="Use an index within the string bounds"
                )
            if end > string_len:
                raise AIFPLEvalError(
                    message="substring end index out of range",
                    received=f"End index: {end}, string length: {string_len}",
                    expected=f"Index between 0 and {string_len}",
                    example='(substring "hello" 1 4) → "ell"',
                    suggestion="Use an index within the string bounds"
                )
            if start > end:
                raise AIFPLEvalError(
                    message=f"start index ({start}) cannot be greater than end index ({end})",
                    received=f"Start: {start}, End: {end}",
                    expected="Start index ≤ end index",
                    example='(substring "hello" 1 4) → "ell"',
                    suggestion="Ensure start index is not greater than end index"
                )
            
            return AIFPLString(string[start:end])

        elif builtin_name == 'string->number':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="String->number takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 string",
                    example='(string->number "42") → 42',
                    suggestion="Provide a single string to convert"
                )
            if not isinstance(args[0], AIFPLString):
                raise AIFPLEvalError(
                    message="String->number requires a string",
                    received=f"Argument: {self._format_result(args[0])} ({args[0].type_name()})",
                    expected="String",
                    example='(string->number "42") → 42',
                    suggestion="Use string->number only with strings"
                )
            try:
                # Try to parse as integer first
                if '.' not in args[0].value and 'e' not in args[0].value.lower() and 'j' not in args[0].value.lower():
                    return AIFPLNumber(int(args[0].value))
                
                # Try complex number
                if 'j' in args[0].value.lower():
                    return AIFPLNumber(complex(args[0].value))
                
                # Otherwise float
                else:
                    return AIFPLNumber(float(args[0].value))
            except ValueError as e:
                raise AIFPLEvalError(
                    message=f"Cannot convert string to number: '{args[0].value}'",
                    suggestion="Provide a valid number string",
                    example='(string->number "42") → 42'
                )

        elif builtin_name == 'number->string':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="Number->string takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 number",
                    example='(number->string 42) → "42"',
                    suggestion="Provide a single number to convert"
                )
            if not isinstance(args[0], AIFPLNumber):
                raise AIFPLEvalError(
                    message="Number->string requires a number",
                    received=f"Argument: {self._format_result(args[0])} ({args[0].type_name()})",
                    expected="Number",
                    example='(number->string 42) → "42"',
                    suggestion="Use number->string only with numbers"
                )
            # Format nicely - integers without decimal point
            value = args[0].value
            if isinstance(value, int):
                return AIFPLString(str(value))
            elif isinstance(value, float) and value.is_integer():
                return AIFPLString(str(int(value)))
            else:
                return AIFPLString(str(value))

        # Alist operations
        elif builtin_name == 'alist':
            # Special form: (alist (key1 val1) (key2 val2) ...)
            # Each argument should be a 2-element list
            pairs = []
            for i, arg in enumerate(args):
                if not isinstance(arg, AIFPLList):
                    raise AIFPLEvalError(
                        message=f"Alist pair {i+1} must be a list",
                        received=f"Pair {i+1}: {self._format_result(arg)} ({arg.type_name()})",
                        expected="2-element list: (key value)",
                        example='(alist ("name" "Alice") ("age" 30))',
                        suggestion="Each pair should be a list with key and value"
                    )
                if len(arg.elements) != 2:
                    raise AIFPLEvalError(
                        message=f"Alist pair {i+1} must have exactly 2 elements",
                        received=f"Pair {i+1} has {len(arg.elements)} elements",
                        expected="2 elements: (key value)",
                        example='(alist ("name" "Alice") ("age" 30))',
                        suggestion="Each pair needs exactly one key and one value"
                    )
                key, value = arg.elements
                pairs.append((key, value))
            return AIFPLAlist(tuple(pairs))

        elif builtin_name == 'alist-get':
            if len(args) < 2 or len(args) > 3:
                raise AIFPLEvalError(
                    message="Alist-get requires 2 or 3 arguments",
                    received=f"Got {len(args)} arguments",
                    expected="2 or 3 arguments: alist, key, [default]",
                    example='(alist-get my-alist "name") or (alist-get my-alist "name" "default")',
                    suggestion="Provide alist, key, and optionally a default value"
                )
            alist = args[0]
            key = args[1]
            default = args[2] if len(args) == 3 else None

            if not isinstance(alist, AIFPLAlist):
                raise AIFPLEvalError(
                    message="Alist-get first argument must be an alist",
                    received=f"First argument: {self._format_result(alist)} ({alist.type_name()})",
                    expected="Alist",
                    example='(alist-get my-alist "name")',
                    suggestion="Use alist-get only with alists"
                )

            result = alist.get(key)
            if result is None:
                if default is not None:
                    return default
                else:
                    raise AIFPLEvalError(
                        message="Key not found in alist",
                        received=f"Key: {self._format_result(key)}",
                        expected="Key that exists in the alist",
                        suggestion="Check that the key exists, or provide a default value"
                    )
            return result

        elif builtin_name == 'alist-set':
            if len(args) != 3:
                raise AIFPLEvalError(
                    message="Alist-set takes exactly 3 arguments",
                    received=f"Got {len(args)} arguments",
                    expected="3 arguments: alist, key, value",
                    example='(alist-set my-alist "name" "Bob")',
                    suggestion="Provide alist, key, and value"
                )
            alist = args[0]
            key = args[1]
            value = args[2]

            if not isinstance(alist, AIFPLAlist):
                raise AIFPLEvalError(
                    message="Alist-set first argument must be an alist",
                    received=f"First argument: {self._format_result(alist)} ({alist.type_name()})",
                    expected="Alist",
                    example='(alist-set my-alist "name" "Bob")',
                    suggestion="Use alist-set only with alists"
                )

            return alist.set(key, value)

        elif builtin_name == 'alist-remove':
            if len(args) != 2:
                raise AIFPLEvalError(
                    message="Alist-remove takes exactly 2 arguments",
                    received=f"Got {len(args)} arguments",
                    expected="2 arguments: alist and key",
                    example='(alist-remove my-alist "name")',
                    suggestion="Provide alist and key"
                )
            alist = args[0]
            key = args[1]

            if not isinstance(alist, AIFPLAlist):
                raise AIFPLEvalError(
                    message="Alist-remove first argument must be an alist",
                    received=f"First argument: {self._format_result(alist)} ({alist.type_name()})",
                    expected="Alist",
                    example='(alist-remove my-alist "name")',
                    suggestion="Use alist-remove only with alists"
                )

            return alist.remove(key)

        elif builtin_name == 'alist-has?':
            if len(args) != 2:
                raise AIFPLEvalError(
                    message="Alist-has? takes exactly 2 arguments",
                    received=f"Got {len(args)} arguments",
                    expected="2 arguments: alist and key",
                    example='(alist-has? my-alist "name")',
                    suggestion="Provide alist and key"
                )
            alist = args[0]
            key = args[1]

            if not isinstance(alist, AIFPLAlist):
                raise AIFPLEvalError(
                    message="Alist-has? first argument must be an alist",
                    received=f"First argument: {self._format_result(alist)} ({alist.type_name()})",
                    expected="Alist",
                    example='(alist-has? my-alist "name")',
                    suggestion="Use alist-has? only with alists"
                )

            return AIFPLBoolean(alist.has_key(key))

        elif builtin_name == 'alist-keys':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="Alist-keys takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 alist",
                    example='(alist-keys my-alist)',
                    suggestion="Provide a single alist"
                )
            alist = args[0]

            if not isinstance(alist, AIFPLAlist):
                raise AIFPLEvalError(
                    message="Alist-keys requires an alist",
                    received=f"Argument: {self._format_result(alist)} ({alist.type_name()})",
                    expected="Alist",
                    example='(alist-keys my-alist)',
                    suggestion="Use alist-keys only with alists"
                )

            return AIFPLList(alist.keys())

        elif builtin_name == 'alist-values':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="Alist-values takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 alist",
                    example='(alist-values my-alist)',
                    suggestion="Provide a single alist"
                )
            alist = args[0]

            if not isinstance(alist, AIFPLAlist):
                raise AIFPLEvalError(
                    message="Alist-values requires an alist",
                    received=f"Argument: {self._format_result(alist)} ({alist.type_name()})",
                    expected="Alist",
                    example='(alist-values my-alist)',
                    suggestion="Use alist-values only with alists"
                )

            return AIFPLList(alist.values())

        elif builtin_name == 'alist-merge':
            if len(args) != 2:
                raise AIFPLEvalError(
                    message="Alist-merge takes exactly 2 arguments",
                    received=f"Got {len(args)} arguments",
                    expected="2 alists",
                    example='(alist-merge alist1 alist2)',
                    suggestion="Provide two alists to merge"
                )
            alist1 = args[0]
            alist2 = args[1]

            if not isinstance(alist1, AIFPLAlist) or not isinstance(alist2, AIFPLAlist):
                if not isinstance(alist1, AIFPLAlist):
                    bad_arg = 0
                else:
                    bad_arg = 1
                raise AIFPLEvalError(
                    message="Alist-merge requires two alists",
                    received=f"Argument {bad_arg+1}: {self._format_result(args[bad_arg])} ({args[bad_arg].type_name()})",
                    expected="Alist",
                    example='(alist-merge alist1 alist2)',
                    suggestion="Both arguments must be alists"
                )

            return alist1.merge(alist2)

        elif builtin_name == 'alist?':
            if len(args) != 1:
                raise AIFPLEvalError(
                    message="Alist? takes exactly 1 argument",
                    received=f"Got {len(args)} arguments",
                    expected="1 value",
                    example='(alist? my-alist) → #t',
                    suggestion="Provide a single value to test"
                )
            return AIFPLBoolean(isinstance(args[0], AIFPLAlist))

        else:
            raise AIFPLEvalError(f"Builtin function not implemented: {builtin_name}")
