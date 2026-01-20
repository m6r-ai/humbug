"""AIFPL Virtual Machine - executes bytecode."""

from typing import List, Dict, Any, Optional, Union
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
        
        # Build internal registry of builtin symbols (for higher-order functions)
        # This maps builtin names to their indices in the BUILTIN_TABLE
        from aifpl.aifpl_compiler import AIFPLCompiler
        self.builtin_symbols = set(AIFPLCompiler.BUILTIN_TABLE)
        
        # Create builtin function objects for first-class function support
        self._builtin_functions = self._create_builtin_functions()
    
    def _create_builtin_functions(self) -> Dict[str, AIFPLBuiltinFunction]:
        """Create AIFPLBuiltinFunction objects for all builtins.
        
        This allows builtins to be used as first-class values (e.g., passed to map).
        """
        from aifpl.aifpl_compiler import AIFPLCompiler
        builtins = {}
        
        # Create a builtin function object for each builtin
        # BUILTIN_TABLE is a list, so we enumerate to get indices
        for builtin_index, name in enumerate(AIFPLCompiler.BUILTIN_TABLE):
            # The native_impl is a lambda that calls _call_builtin with the right index
            builtins[name] = AIFPLBuiltinFunction(name, lambda *args, idx=builtin_index: self._call_builtin(idx, list(args)))
        
        return builtins

    def set_globals(self, globals_dict: Dict[str, AIFPLValue]) -> None:
        """Set global variables (constants like pi, e, j) and add builtin functions."""
        self.globals = globals_dict.copy()
        self.globals.update(self._builtin_functions)

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

    def _ensure_integer(self, value: AIFPLValue, function_name: str) -> int:
        """Ensure value is an integer, raise error if not."""
        if not isinstance(value, AIFPLNumber) or not value.is_integer():
            raise AIFPLEvalError(
                f"Function '{function_name}' requires integer arguments, got {value.type_name()}"
            )
        
        # Type narrowing: we know value.value is int here
        assert isinstance(value.value, int), "is_integer() should guarantee int type"
        return value.value

    def _ensure_real_number(self, value: AIFPLValue, function_name: str):
        """Ensure value is a real number (int or float), raise error if complex."""
        if not isinstance(value, AIFPLNumber):
            raise AIFPLEvalError(
                f"Function '{function_name}' requires numeric arguments, got {value.type_name()}"
            )
        
        if isinstance(value.value, complex):
            raise AIFPLEvalError(
                f"Function '{function_name}' does not support complex numbers"
            )
        
        return value.value

    def _ensure_string(self, value: AIFPLValue, function_name: str) -> AIFPLString:
        """Ensure value is a string, raise error if not."""
        if not isinstance(value, AIFPLString):
            raise AIFPLEvalError(
                f"Function '{function_name}' requires string arguments, got {value.type_name()}"
            )
        return value

    def _ensure_list(self, value: AIFPLValue, function_name: str) -> AIFPLList:
        """Ensure value is a list, raise error if not."""
        if not isinstance(value, AIFPLList):
            raise AIFPLEvalError(
                f"Function '{function_name}' requires list arguments, got {value.type_name()}"
            )

        return value

    def _resolve_function(self, value: AIFPLValue, context: str) -> Union[AIFPLFunction, AIFPLBuiltinFunction]:
        """Resolve a value to a function, handling builtin symbols."""
        # If it's already a function, return it
        if isinstance(value, (AIFPLFunction, AIFPLBuiltinFunction)):
            return value

        # If it's a symbol referring to a builtin, create a wrapper
        if isinstance(value, AIFPLSymbol) and value.name in self.builtin_symbols:
            # Create a wrapper that will call the builtin via the VM
            # We use a special AIFPLBuiltinFunction with the VM's evaluator
            if self.evaluator is None:
                raise AIFPLEvalError(f"Cannot resolve builtin '{value.name}' without evaluator")

            # Get the actual builtin from the evaluator
            return self.evaluator._builtin_functions[value.name]

        # Otherwise, it's not a valid function
        # TODO: Fix me!
        raise AIFPLEvalError(
            message="Cannot call non-function value",
#            received=f"Trying to call: {self.format_result(value.name)} ({value.type_name()})",
            expected="Function (builtin or lambda)",
            example="(+ 1 2) calls function +\n(42 1 2) tries to call number 42",
#            suggestion=f"'{value.name}' is not a function - check spelling or define it first"
        )

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
                    raise AIFPLEvalError(
                        message="Cannot call non-function value",
                        received=f"Attempted to call: {self._format_result(func)} ({func.type_name()})",
                        expected="Function (lambda or builtin)",
                        suggestion="Only functions can be called"
                    )

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

    def _call_function_value(self, func: Union[AIFPLFunction, AIFPLBuiltinFunction], args: List[AIFPLValue]) -> AIFPLValue:
        """Call a function value (either user-defined or builtin).
        
        This is a helper for higher-order functions like map, filter, fold, etc.
        """
        if isinstance(func, AIFPLFunction):
            return self._call_bytecode_function(func, args)
        elif isinstance(func, AIFPLBuiltinFunction):
            # Call builtin function by looking up its index
            from aifpl.aifpl_compiler import AIFPLCompiler
            if func.name in AIFPLCompiler.BUILTIN_TABLE:
                builtin_idx = AIFPLCompiler.BUILTIN_TABLE.index(func.name)
                return self._call_builtin(builtin_idx, args)
            else:
                raise AIFPLEvalError(f"Unknown builtin function: {func.name}")
        else:
            raise AIFPLEvalError(f"Expected function, got {func.type_name()}")


    def _call_builtin(self, builtin_index: int, args: List[AIFPLValue]) -> AIFPLValue:
        """Call a builtin function by index.
        
        This method delegates to the unified builtin registry for regular builtins,
        and handles special forms (and, or, map, filter, fold, etc.) separately
        because they require special evaluation semantics.
        """
        from aifpl.aifpl_compiler import AIFPLCompiler
        from aifpl.aifpl_builtins import AIFPLBuiltinRegistry
        
        builtin_name = AIFPLCompiler.BUILTIN_TABLE[builtin_index]
        
        # Special forms that need custom handling (not in the registry)
        # These have special evaluation semantics and can't be handled generically
        
        # Note: 'and' and 'or' are special forms in the evaluator but when called
        # from bytecode, their arguments are already evaluated, so we can handle them simply
        if builtin_name == 'and':
            # All arguments are already evaluated by bytecode
            if not args:
                return AIFPLBoolean(True)
            for i, arg in enumerate(args):
                if not isinstance(arg, AIFPLBoolean):
                    raise AIFPLEvalError(
                        message=f"And operator argument {i+1} must be boolean",
                        received=f"Argument {i+1}: {self._format_result(arg)} ({arg.type_name()})",
                        expected="Boolean value (#t or #f)",
                        example="(and (> x 0) (< x 10))",
                        suggestion="Use comparison or boolean operators to create boolean values"
                    )
                if not arg.value:
                    return AIFPLBoolean(False)
            return AIFPLBoolean(True)
        
        elif builtin_name == 'or':
            # All arguments are already evaluated by bytecode
            if not args:
                return AIFPLBoolean(False)
            for i, arg in enumerate(args):
                if not isinstance(arg, AIFPLBoolean):
                    raise AIFPLEvalError(
                        message=f"Or operator argument {i+1} must be boolean",
                        received=f"Argument {i+1}: {self._format_result(arg)} ({arg.type_name()})",
                        expected="Boolean value (#t or #f)",
                        example="(or (= x 0) (> x 10))",
                        suggestion="Use comparison or boolean operators to create boolean values"
                    )
                if arg.value:
                    return AIFPLBoolean(True)
            return AIFPLBoolean(False)
        
        # Higher-order functions that need special handling
        elif builtin_name == 'map':
            if len(args) != 2:
                raise AIFPLEvalError(
                    message="Map function has wrong number of arguments",
                    received=f"Got {len(args)} arguments",
                    expected="Exactly 2 arguments: (map function list)",
                    example="(map (lambda (x) (* x 2)) (list 1 2 3))",
                    suggestion="Map takes a function and a list"
                )
            
            func_value = args[0]
            list_value = args[1]
            
            if not isinstance(list_value, AIFPLList):
                raise AIFPLEvalError(
                    message="Map second argument must be a list",
                    received=f"Second argument: {self._format_result(list_value)} ({list_value.type_name()})",
                    expected="List of values",
                    example="(map (lambda (x) (* x 2)) (list 1 2 3))",
                    suggestion="Use (list ...) to create a list"
                )
            
            # Resolve function (handles builtin symbols)
            func = self._resolve_function(func_value, "map")
            
            # Apply function to each element
            result_elements = []
            for i, item in enumerate(list_value.elements):
                try:
                    item_result = self._call_function_value(func, [item])
                    result_elements.append(item_result)
                except AIFPLEvalError as e:
                    raise AIFPLEvalError(
                        message=f"Error in map function at element {i+1}",
                        received=f"Element {i+1}: {self._format_result(item)}",
                        context=str(e),
                        suggestion="Check that your function works with all list elements"
                    ) from e
            
            return AIFPLList(tuple(result_elements))
        
        elif builtin_name == 'filter':
            if len(args) != 2:
                raise AIFPLEvalError(
                    message="Filter function has wrong number of arguments",
                    received=f"Got {len(args)} arguments",
                    expected="Exactly 2 arguments: (filter predicate list)",
                    example="(filter (lambda (x) (> x 0)) (list -1 2 -3 4))",
                    suggestion="Filter takes a predicate function and a list"
                )
            
            pred_value = args[0]
            list_value = args[1]
            
            if not isinstance(list_value, AIFPLList):
                raise AIFPLEvalError(
                    message="Filter second argument must be a list",
                    received=f"Second argument: {self._format_result(list_value)} ({list_value.type_name()})",
                    expected="List of values",
                    example="(filter (lambda (x) (> x 0)) (list -1 2 -3 4))",
                    suggestion="Use (list ...) to create a list"
                )
            
            # Resolve function
            pred = self._resolve_function(pred_value, "filter")
            
            # Filter elements
            result_elements = []
            for i, item in enumerate(list_value.elements):
                try:
                    pred_result = self._call_function_value(pred, [item])
                    if not isinstance(pred_result, AIFPLBoolean):
                        raise AIFPLEvalError(
                            message=f"Filter predicate must return boolean at element {i+1}",
                            received=f"Predicate returned: {self._format_result(pred_result)} ({pred_result.type_name()})",
                            expected="Boolean value (#t or #f)",
                            example="(filter (lambda (x) (> x 0)) (list -1 2 -3 4))",
                            suggestion="Predicate function should use comparison operators"
                        )
                    if pred_result.value:
                        result_elements.append(item)
                except AIFPLEvalError as e:
                    raise AIFPLEvalError(
                        message=f"Error in filter predicate at element {i+1}",
                        received=f"Element {i+1}: {self._format_result(item)}",
                        context=str(e),
                        suggestion="Check that your predicate works with all list elements"
                    ) from e
            
            return AIFPLList(tuple(result_elements))
        
        elif builtin_name == 'fold':
            if len(args) != 3:
                raise AIFPLEvalError(
                    message="Fold function has wrong number of arguments",
                    received=f"Got {len(args)} arguments",
                    expected="Exactly 3 arguments: (fold function initial list)",
                    example="(fold + 0 (list 1 2 3 4))",
                    suggestion="Fold takes a function, initial value, and list"
                )
            
            func_value = args[0]
            accumulator = args[1]
            list_value = args[2]
            
            if not isinstance(list_value, AIFPLList):
                raise AIFPLEvalError(
                    message="Fold third argument must be a list",
                    received=f"Third argument: {self._format_result(list_value)} ({list_value.type_name()})",
                    expected="List of values",
                    example="(fold + 0 (list 1 2 3 4))",
                    suggestion="Use (list ...) to create a list"
                )
            
            # Resolve function
            func = self._resolve_function(func_value, "fold")
            
            # Fold over list
            for i, item in enumerate(list_value.elements):
                try:
                    accumulator = self._call_function_value(func, [accumulator, item])
                except AIFPLEvalError as e:
                    raise AIFPLEvalError(
                        message=f"Error in fold function at element {i+1}",
                        received=f"Accumulator: {self._format_result(accumulator)}, Element {i+1}: {self._format_result(item)}",
                        context=str(e),
                        suggestion="Check that your function works with accumulator and all list elements"
                    ) from e
            
            return accumulator
        
        elif builtin_name == 'range':
            if len(args) < 2 or len(args) > 3:
                raise AIFPLEvalError(
                    message="Range function has wrong number of arguments",
                    received=f"Got {len(args)} arguments",
                    expected="2 or 3 arguments: (range start end) or (range start end step)",
                    example="(range 1 5) or (range 0 10 2)",
                    suggestion="Range needs start and end, optionally step"
                )
            
            start_val = args[0]
            end_val = args[1]
            
            if not isinstance(start_val, AIFPLNumber):
                raise AIFPLEvalError(
                    message="Range start must be a number",
                    received=f"Start: {self._format_result(start_val)} ({start_val.type_name()})",
                    expected="Number (integer or float)",
                    example="(range 1 5)",
                    suggestion="Use numeric values for range bounds"
                )
            
            if not isinstance(end_val, AIFPLNumber):
                raise AIFPLEvalError(
                    message="Range end must be a number",
                    received=f"End: {self._format_result(end_val)} ({end_val.type_name()})",
                    expected="Number (integer or float)",
                    example="(range 1 5)",
                    suggestion="Use numeric values for range bounds"
                )
            
            start_int = self._ensure_integer(start_val, "range")
            end_int = self._ensure_integer(end_val, "range")
            
            if len(args) == 3:
                step_val = args[2]
                if not isinstance(step_val, AIFPLNumber):
                    raise AIFPLEvalError(
                        message="Range step must be a number",
                        received=f"Step: {self._format_result(step_val)} ({step_val.type_name()})",
                        expected="Number (integer or float)",
                        example="(range 0 10 2)",
                        suggestion="Use numeric values for range parameters"
                    )
                step_int = self._ensure_integer(step_val, "range")
            else:
                step_int = 1
            
            if step_int == 0:
                raise AIFPLEvalError(
                    message="Range step cannot be zero",
                    received="Step: 0",
                    expected="Non-zero integer",
                    example="(range 0 10 2) or (range 10 0 -1)",
                    suggestion="Use positive step for ascending range, negative for descending"
                )
            
            # Generate range
            range_values = list(range(start_int, end_int, step_int))
            elements = tuple(AIFPLNumber(val) for val in range_values)
            return AIFPLList(elements)
        
        elif builtin_name == 'find':
            if len(args) != 2:
                raise AIFPLEvalError(
                    message="Find function has wrong number of arguments",
                    received=f"Got {len(args)} arguments",
                    expected="Exactly 2 arguments: (find predicate list)",
                    example="(find (lambda (x) (> x 5)) (list 1 2 6 3))",
                    suggestion="Find takes a predicate function and a list"
                )
            
            pred_value = args[0]
            list_value = args[1]
            
            if not isinstance(list_value, AIFPLList):
                raise AIFPLEvalError(
                    message="Find second argument must be a list",
                    received=f"Second argument: {self._format_result(list_value)} ({list_value.type_name()})",
                    expected="List of values",
                    example="(find (lambda (x) (> x 5)) (list 1 2 6 3))",
                    suggestion="Use (list ...) to create a list"
                )
            
            # Resolve function
            pred = self._resolve_function(pred_value, "find")
            
            # Find first matching element
            for i, item in enumerate(list_value.elements):
                try:
                    pred_result = self._call_function_value(pred, [item])
                    if not isinstance(pred_result, AIFPLBoolean):
                        raise AIFPLEvalError(
                            message=f"Find predicate must return boolean at element {i+1}",
                            received=f"Predicate returned: {self._format_result(pred_result)} ({pred_result.type_name()})",
                            expected="Boolean value (#t or #f)",
                            example="(find (lambda (x) (> x 5)) (list 1 2 6 3))",
                            suggestion="Predicate function should use comparison operators"
                        )
                    if pred_result.value:
                        return item
                except AIFPLEvalError as e:
                    raise AIFPLEvalError(
                        message=f"Error in find predicate at element {i+1}",
                        received=f"Element {i+1}: {self._format_result(item)}",
                        context=str(e),
                        suggestion="Check that your predicate works with all list elements"
                    ) from e
            
            return AIFPLBoolean(False)  # Not found
        
        elif builtin_name == 'any?':
            if len(args) != 2:
                raise AIFPLEvalError(
                    message="Any? function has wrong number of arguments",
                    received=f"Got {len(args)} arguments",
                    expected="Exactly 2 arguments: (any? predicate list)",
                    example="(any? (lambda (x) (> x 5)) (list 1 2 6 3))",
                    suggestion="Any? takes a predicate function and a list"
                )
            
            pred_value = args[0]
            list_value = args[1]
            
            if not isinstance(list_value, AIFPLList):
                raise AIFPLEvalError(
                    message="Any? second argument must be a list",
                    received=f"Second argument: {self._format_result(list_value)} ({list_value.type_name()})",
                    expected="List of values",
                    example="(any? (lambda (x) (> x 5)) (list 1 2 6 3))",
                    suggestion="Use (list ...) to create a list"
                )
            
            # Resolve function
            pred = self._resolve_function(pred_value, "any?")
            
            # Check if any element matches
            for i, item in enumerate(list_value.elements):
                try:
                    pred_result = self._call_function_value(pred, [item])
                    if not isinstance(pred_result, AIFPLBoolean):
                        raise AIFPLEvalError(
                            message=f"Any? predicate must return boolean at element {i+1}",
                            received=f"Predicate returned: {self._format_result(pred_result)} ({pred_result.type_name()})",
                            expected="Boolean value (#t or #f)",
                            example="(any? (lambda (x) (> x 5)) (list 1 2 6 3))",
                            suggestion="Predicate function should use comparison operators"
                        )
                    if pred_result.value:
                        return AIFPLBoolean(True)
                except AIFPLEvalError as e:
                    raise AIFPLEvalError(
                        message=f"Error in any? predicate at element {i+1}",
                        received=f"Element {i+1}: {self._format_result(item)}",
                        context=str(e),
                        suggestion="Check that your predicate works with all list elements"
                    ) from e
            
            return AIFPLBoolean(False)
        
        elif builtin_name == 'all?':
            if len(args) != 2:
                raise AIFPLEvalError(
                    message="All? function has wrong number of arguments",
                    received=f"Got {len(args)} arguments",
                    expected="Exactly 2 arguments: (all? predicate list)",
                    example="(all? (lambda (x) (> x 0)) (list 1 2 3))",
                    suggestion="All? takes a predicate function and a list"
                )
            
            pred_value = args[0]
            list_value = args[1]
            
            if not isinstance(list_value, AIFPLList):
                raise AIFPLEvalError(
                    message="All? second argument must be a list",
                    received=f"Second argument: {self._format_result(list_value)} ({list_value.type_name()})",
                    expected="List of values",
                    example="(all? (lambda (x) (> x 0)) (list 1 2 3))",
                    suggestion="Use (list ...) to create a list"
                )
            
            # Resolve function
            pred = self._resolve_function(pred_value, "all?")
            
            # Check if all elements match
            for i, item in enumerate(list_value.elements):
                try:
                    pred_result = self._call_function_value(pred, [item])
                    if not isinstance(pred_result, AIFPLBoolean):
                        raise AIFPLEvalError(
                            message=f"All? predicate must return boolean at element {i+1}",
                            received=f"Predicate returned: {self._format_result(pred_result)} ({pred_result.type_name()})",
                            expected="Boolean value (#t or #f)",
                            example="(all? (lambda (x) (> x 0)) (list 1 2 3))",
                            suggestion="Predicate function should use comparison operators"
                        )
                    if not pred_result.value:
                        return AIFPLBoolean(False)
                except AIFPLEvalError as e:
                    raise AIFPLEvalError(
                        message=f"Error in all? predicate at element {i+1}",
                        received=f"Element {i+1}: {self._format_result(item)}",
                        context=str(e),
                        suggestion="Check that your predicate works with all list elements"
                    ) from e
            
            return AIFPLBoolean(True)
        
        elif builtin_name == 'alist':
            # Alist constructor - arguments are already evaluated pairs
            pairs = []
            for i, arg in enumerate(args):
                if not isinstance(arg, AIFPLList):
                    raise AIFPLEvalError(
                        message=f"Alist pair {i+1} must be a list",
                        received=f"Pair {i+1}: {arg.type_name()}",
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
                
                key = arg.elements[0]
                value = arg.elements[1]
                pairs.append((key, value))
            
            return AIFPLAlist(tuple(pairs))
        
        # All other builtins - delegate to the registry
        else:
            # Create registry if needed (cache it for performance)
            if not hasattr(self, '_builtin_registry'):
                self._builtin_registry = AIFPLBuiltinRegistry()
            
            # Check if this builtin is in the registry
            if self._builtin_registry.has_function(builtin_name):
                # Call through the registry
                env = AIFPLEnvironment()  # Empty environment for builtins
                return self._builtin_registry.call_builtin(builtin_name, args, env, 0)
            else:
                # Unknown builtin
                raise AIFPLEvalError(
                    message=f"Unknown builtin function: {builtin_name}",
                    suggestion="This may be an internal error - please report this issue"
                )
