"""AIFPL Virtual Machine - executes bytecode."""

from typing import List, Dict, Any, cast
from dataclasses import dataclass, field

from aifpl.aifpl_builtins import AIFPLBuiltinRegistry
from aifpl.aifpl_bytecode import CodeObject, Opcode
from aifpl.aifpl_compiler import AIFPLCompiler
from aifpl.aifpl_environment import AIFPLEnvironment
from aifpl.aifpl_error import AIFPLEvalError, ErrorMessageBuilder
from aifpl.aifpl_value import (
    AIFPLValue, AIFPLNumber, AIFPLString, AIFPLBoolean,
    AIFPLList, AIFPLFunction, AIFPLSymbol, AIFPLAList
)


@dataclass
class Frame:
    """
    Execution frame for function calls.

    Each frame has its own locals and instruction pointer.
    """
    code: CodeObject
    ip: int = 0  # Instruction pointer
    locals: List[AIFPLValue | None] = field(init=False)  # Local variables
    closure_env: Any = None  # Closure environment for this frame


    def __post_init__(self) -> None:
        """Initialize locals array based on code object."""
        self.locals = [None] * self.code.local_count


class AIFPLVM:
    """
    Virtual machine for executing AIFPL bytecode.

    Uses a stack-based architecture with lexically-scoped frames.
    """

    def __init__(self) -> None:
        self.stack: List[AIFPLValue] = []
        self.frames: List[Frame] = []
        self.globals: Dict[str, AIFPLValue] = {}
        self.message_builder = ErrorMessageBuilder()

        # Create builtin registry
        self._builtin_registry = AIFPLBuiltinRegistry()

        # Build set of builtin symbols for quick lookup
        self.builtin_symbols = set(AIFPLCompiler.BUILTIN_TABLE)

        # Create builtin function objects for first-class function support (e.g., passed to map)
        self._builtin_functions = self._create_builtin_functions()

        # Build dispatch table for fast opcode execution
        # This is a critical optimization: jump table dispatch is 2-3x faster than if/elif chains
        self._dispatch_table = self._build_dispatch_table()

    def _build_dispatch_table(self) -> List[Any]:
        """
        Build jump table for opcode dispatch.

        This replaces the if/elif chain with direct array indexing,
        significantly improving performance in the hot execution loop.
        """
        table: List[Any] = [None] * 256
        table[Opcode.LOAD_CONST] = self._op_load_const
        table[Opcode.LOAD_TRUE] = self._op_load_true
        table[Opcode.LOAD_FALSE] = self._op_load_false
        table[Opcode.LOAD_EMPTY_LIST] = self._op_load_empty_list
        table[Opcode.LOAD_VAR] = self._op_load_var
        table[Opcode.STORE_VAR] = self._op_store_var
        table[Opcode.LOAD_NAME] = self._op_load_name
        table[Opcode.JUMP] = self._op_jump
        table[Opcode.POP_JUMP_IF_FALSE] = self._op_pop_jump_if_false
        table[Opcode.POP_JUMP_IF_TRUE] = self._op_pop_jump_if_true
        table[Opcode.RAISE_ERROR] = self._op_raise_error
        table[Opcode.MAKE_CLOSURE] = self._op_make_closure
        table[Opcode.CALL_FUNCTION] = self._op_call_function
        table[Opcode.CALL_BUILTIN] = self._op_call_builtin
        table[Opcode.RETURN] = self._op_return
        table[Opcode.PATCH_CLOSURE_SELF] = self._op_patch_closure_self
        table[Opcode.PATCH_CLOSURE_SIBLING] = self._op_patch_closure_sibling
        table[Opcode.MAKE_LIST] = self._op_make_list
        return table

    def _create_builtin_functions(self) -> Dict[str, AIFPLFunction]:
        """
        Create AIFPLFunction objects for all builtins.
        
        Uses the builtin registry to get standard implementations.
        """
        return self._builtin_registry.create_builtin_function_objects()

    def set_globals(self, globals_dict: Dict[str, AIFPLValue], prelude_functions: dict[str, AIFPLFunction] | None = None) -> None:
        """Set global variables (constants like pi, e, j) and add builtin functions."""
        self.globals = globals_dict.copy()
        self.globals.update(self._builtin_functions)
        if prelude_functions:
            self.globals.update(prelude_functions)

    def format_result(self, result: AIFPLValue) -> str:
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
            return f'"{result.value}"'

        if isinstance(result, AIFPLNumber):
            return str(result.value)

        if isinstance(result, AIFPLList):
            # Format list in LISP notation: (element1 element2 ...)
            if result.is_empty():
                return "()"

            formatted_elements = []
            for element in result.elements:
                formatted_elements.append(self.format_result(element))

            return f"({' '.join(formatted_elements)})"

        if isinstance(result, AIFPLAList):
            # Format alist in LISP notation
            if result.is_empty():
                return "(alist)"

            formatted_pairs = []
            for key, value in result.pairs:
                formatted_key = self.format_result(key)
                formatted_value = self.format_result(value)
                formatted_pairs.append(f"({formatted_key} {formatted_value})")

            pairs_str = ' '.join(formatted_pairs)
            return f"(alist {pairs_str})"

        if isinstance(result, AIFPLFunction):
            # Use the describe method which handles both native and user-defined
            if result.is_native:
                return f"<builtin {result.name}>"

            param_str = " ".join(result.parameters)
            return f"<lambda ({param_str})>"

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
        if isinstance(val1, AIFPLNumber):
            return val1.value == cast(AIFPLNumber, val2).value

        if isinstance(val1, AIFPLString):
            return val1.value == cast(AIFPLString, val2).value

        if isinstance(val1, AIFPLBoolean):
            return val1.value == cast(AIFPLBoolean, val2).value

        # Lists: compare element by element
        if isinstance(val1, AIFPLList):
            if len(val1.elements) != len(cast(AIFPLList, val2).elements):
                return False

            return all(self._values_equal(e1, e2) for e1, e2 in zip(val1.elements, cast(AIFPLList, val2).elements))

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

    def _ensure_real_number(self, value: AIFPLValue, function_name: str) -> AIFPLNumber:
        """Ensure value is a real number (int or float), raise error if complex."""
        if not isinstance(value, AIFPLNumber):
            raise AIFPLEvalError(
                f"Function '{function_name}' requires numeric arguments, got {value.type_name()}"
            )

        if isinstance(value.value, complex):
            raise AIFPLEvalError(
                f"Function '{function_name}' does not support complex numbers"
            )

        return value

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

    def _resolve_function(self, value: AIFPLValue) -> AIFPLFunction | None:
        """Resolve a value to a function, handling builtin symbols."""
        # If it's already a function, return it
        if isinstance(value, AIFPLFunction):
            return value

        # If it's a symbol referring to a builtin, return the builtin function object
        if isinstance(value, AIFPLSymbol):
            if value.name in self.builtin_symbols:
                # Get the builtin function from our own registry
                return self._builtin_functions[value.name]

        # Otherwise, it's not a valid function - return None
        # Caller will handle the error with appropriate context
        return None

    def _raise_non_function_error(self, value: AIFPLValue, context: str) -> None:
        """
        Raise a detailed error for attempting to call a non-function value.

        Args:
            value: The value that was attempted to be called
            context: The context where the call was attempted (e.g., "map", "filter")
        """
        # Build suggestion based on whether this is a symbol or not
        if isinstance(value, AIFPLSymbol):
            suggestion = f"'{value.name}' is not a function - check spelling or define it first"
        else:
            suggestion = "Only functions can be called - check that you're using correct syntax"

        raise AIFPLEvalError(
            message="Cannot call non-function value",
            received=f"Trying to call: {self.format_result(value)} ({value.type_name()})",
            context=f"In {context}: first argument must be a function",
            expected="Function (builtin or lambda)",
            example="(+ 1 2) calls function +\\n(42 1 2) tries to call number 42",
            suggestion=suggestion
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

        return "\n".join(lines)

    def _get_current_function_name(self) -> str:
        """
        Get name of currently executing function.

        Returns:
            Function name or '<module>' if at top level
        """
        if not self.frames:
            return "<module>"

        return self.frames[-1].code.name or "<lambda>"

    def execute(self, code: CodeObject) -> AIFPLValue:
        """
        Execute a code object and return the result.

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

    def _execute_frame(self) -> AIFPLValue | None:
        """
        Execute current frame using jump table dispatch.

        This is the hot path - optimized for speed using a dispatch table
        instead of if/elif chains for 2-3x better performance.

        Returns:
            Result value if frame returns, None if continuing execution
        """
        if not self.frames:
            return None

        frame = self.frames[-1]
        code = frame.code
        instructions = code.instructions

        # Cache dispatch table in local variable for faster access
        dispatch = self._dispatch_table

        while frame.ip < len(instructions):
            instr = instructions[frame.ip]
            opcode = instr.opcode

            # Increment IP before executing (so jumps can override)
            frame.ip += 1

            # Jump table dispatch - this is the key optimization!
            # Direct array indexing is much faster than if/elif chain
            handler = dispatch[opcode]
            if handler is None:
                raise AIFPLEvalError(f"Unimplemented opcode: {opcode}")

            # Call the handler - it may return a value for RETURN
            result = handler(frame, code, instr.arg1, instr.arg2)
            if result is not None:
                return result

        # Frame finished without explicit return
        return None

    def _op_load_const(self, _frame: Frame, code: CodeObject, arg1: int, _arg2: int) -> AIFPLValue | None:
        """LOAD_CONST: Push constant from pool onto stack."""
        self.stack.append(code.constants[arg1])
        return None

    def _op_load_true(self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int) -> AIFPLValue | None:
        """LOAD_TRUE: Push boolean true onto stack."""
        self.stack.append(AIFPLBoolean(True))
        return None

    def _op_load_false(self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int) -> AIFPLValue | None:
        """LOAD_FALSE: Push boolean false onto stack."""
        self.stack.append(AIFPLBoolean(False))
        return None

    def _op_load_empty_list(self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int) -> AIFPLValue | None:
        """LOAD_EMPTY_LIST: Push empty list onto stack."""
        self.stack.append(AIFPLList(()))
        return None

    def _op_load_var(self, _frame: Frame, _code: CodeObject, arg1: int, arg2: int) -> AIFPLValue | None:
        """LOAD_VAR: Load variable from frame at depth/index."""
        depth = arg1
        index = arg2

        # Load from frame at depth
        frame_index = len(self.frames) - 1 - depth

        if frame_index < 0 or frame_index >= len(self.frames):
            raise AIFPLEvalError(f"Frame at depth {depth} doesn't exist (have {len(self.frames)} frames)")

        target_frame = self.frames[frame_index]

        if index >= len(target_frame.locals):
            raise AIFPLEvalError(f"Local variable index {index} out of range (frame has {len(target_frame.locals)} locals)")

        value = target_frame.locals[index]
        if value is None:
            raise AIFPLEvalError(f"Uninitialized local variable at depth {depth}, index {index}")

        self.stack.append(value)
        return None

    def _op_store_var(self, _frame: Frame, _code: CodeObject, arg1: int, arg2: int) -> AIFPLValue | None:
        """STORE_VAR: Store top of stack to variable at depth/index."""
        depth = arg1
        index = arg2
        value = self.stack.pop()
        target_frame = self.frames[-(depth + 1)]
        target_frame.locals[index] = value
        return None

    def _op_load_name(self, frame: Frame, code: CodeObject, arg1: int, _arg2: int) -> AIFPLValue | None:
        """LOAD_NAME: Load global variable by name."""
        name = code.names[arg1]

        # First check closure environment (for recursive closures)
        if frame.closure_env and name in frame.closure_env.bindings:
            self.stack.append(frame.closure_env.bindings[name])
            return None

        if name in self.globals:
            self.stack.append(self.globals[name])
            return None

        # Not found - generate helpful error
        available_vars = self._get_available_globals()
        similar = self.message_builder.suggest_similar_functions(name, available_vars, max_suggestions=3)

        suggestion_text = (
            f"Did you mean: {', '.join(similar)}?" if similar
            else "Check spelling or define it in a let binding"
        )

        raise AIFPLEvalError(
            message=f"Undefined variable: '{name}'",
            context=(
                f"Available variables: "
                f"{', '.join(sorted(available_vars)[:10])}{'...' if len(available_vars) > 10 else ''}"
            ),
            suggestion=suggestion_text,
            example=f"(let (({name} some-value)) ...)"
        )

    def _op_jump(self, frame: Frame, _code: CodeObject, arg1: int, _arg2: int) -> AIFPLValue | None:
        """JUMP: Unconditional jump to instruction."""
        frame.ip = arg1
        return None

    def _op_pop_jump_if_false(self, frame: Frame, _code: CodeObject, arg1: int, _arg2: int) -> AIFPLValue | None:
        """POP_JUMP_IF_FALSE: Pop stack, jump if false."""
        condition = self.stack.pop()
        if not isinstance(condition, AIFPLBoolean):
            raise AIFPLEvalError("If condition must be boolean")

        if not condition.value:
            frame.ip = arg1

        return None

    def _op_pop_jump_if_true(self, frame: Frame, _code: CodeObject, arg1: int, _arg2: int) -> AIFPLValue | None:
        """POP_JUMP_IF_TRUE: Pop stack, jump if true."""
        condition = self.stack.pop()
        if not isinstance(condition, AIFPLBoolean):
            raise AIFPLEvalError("If condition must be boolean")

        if condition.value:
            frame.ip = arg1

        return None

    def _op_raise_error(self, _frame: Frame, code: CodeObject, arg1: int, _arg2: int) -> AIFPLValue | None:
        """RAISE_ERROR: Raise error with message from constant pool."""
        error_msg = code.constants[arg1]
        if not isinstance(error_msg, AIFPLString):
            raise AIFPLEvalError("RAISE_ERROR requires a string constant")

        raise AIFPLEvalError(error_msg.value)

    def _op_make_closure(self, _frame: Frame, code: CodeObject, arg1: int, arg2: int) -> AIFPLValue | None:
        """MAKE_CLOSURE: Create closure from code object and captured values."""
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
        closure = AIFPLFunction(
            parameters=tuple(f"param{i}" for i in range(closure_code.param_count)),
            body=None,
            closure_environment=AIFPLEnvironment(bindings=captured_dict),
            name=closure_code.name,
            bytecode=closure_code,
            captured_values=tuple(captured_values)
        )
        self.stack.append(closure)
        return None

    def _op_call_function(self, _frame: Frame, _code: CodeObject, arg1: int, _arg2: int) -> AIFPLValue | None:
        """CALL_FUNCTION: Call function with arguments from stack."""
        arity = arg1

        if len(self.stack) < arity + 1:
            raise AIFPLEvalError(
                message="Stack underflow in CALL_FUNCTION",
                received=f"Stack has {len(self.stack)} items, need {arity + 1}",
                suggestion="This is likely a compiler bug"
            )

        # Get function from under the arguments
        func = self.stack[-(arity + 1)]

        if not isinstance(func, AIFPLFunction):
            raise AIFPLEvalError(
                message="Cannot call non-function value",
                received=f"Attempted to call: {self.format_result(func)} ({func.type_name()})",
                expected="Function (lambda or builtin)",
                suggestion="Only functions can be called"
            )

        # Handle builtin functions
        if func.is_native:
            args = [self.stack.pop() for _ in range(arity)]
            args.reverse()
            self.stack.pop()  # Pop function
            if func.native_impl is None:
                raise AIFPLEvalError(f"Function {func.name} has no native implementation")

            result = func.native_impl(args)
            self.stack.append(result)
            return None

        # Check for tail call optimization
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
            func.bytecode == current_frame.code
        )

        # Remove function from stack
        del self.stack[-(arity + 1)]

        # Tail call optimization
        if is_tail_call and is_self_recursive:
            assert current_frame is not None
            current_frame.ip = 0
            return None  # Continue in same frame

        # Normal call: create new frame
        result = self._call_bytecode_function(func)
        self.stack.append(result)
        return None

    def _op_call_builtin(self, _frame: Frame, _code: CodeObject, arg1: int, arg2: int) -> AIFPLValue | None:
        """CALL_BUILTIN: Call builtin function by index."""
        builtin_index = arg1
        arity = arg2

        # Pop arguments
        args = [self.stack.pop() for _ in range(arity)]
        args.reverse()

        result = self._call_builtin(builtin_index, args)
        self.stack.append(result)
        return None

    def _op_return(self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int) -> AIFPLValue | None:
        """RETURN: Pop frame and return value from stack."""
        self.frames.pop()
        if not self.stack:
            raise AIFPLEvalError("RETURN with empty stack")

        return self.stack.pop()

    def _op_patch_closure_self(self, _frame: Frame, code: CodeObject, arg1: int, arg2: int) -> AIFPLValue | None:
        """PATCH_CLOSURE_SELF: Patch closure to reference itself (for recursion)."""
        name_index = arg1
        var_index = arg2
        var_name = code.names[name_index]

        target_frame = self.frames[-1]

        if var_index >= len(target_frame.locals):
            raise AIFPLEvalError(f"PATCH_CLOSURE_SELF: variable index {var_index} out of range")

        target_closure = target_frame.locals[var_index]

        if not isinstance(target_closure, AIFPLFunction):
            raise AIFPLEvalError("PATCH_CLOSURE_SELF requires a function")

        # Create new environment with self-reference
        new_bindings = {**target_closure.closure_environment.bindings}
        new_env = AIFPLEnvironment(bindings=new_bindings, parent=target_closure.closure_environment.parent)

        # Create patched closure
        patched_closure = AIFPLFunction(
            parameters=target_closure.parameters,
            body=target_closure.body,
            closure_environment=new_env,
            name=target_closure.name,
            bytecode=target_closure.bytecode,
            captured_values=target_closure.captured_values
        )

        # Add self-reference
        new_env.bindings[var_name] = patched_closure

        # Store patched closure back
        target_frame.locals[var_index] = patched_closure
        return None

    def _op_patch_closure_sibling(self, _frame: Frame, code: CodeObject, arg1: int, arg2: int) -> AIFPLValue | None:
        """PATCH_CLOSURE_SIBLING: Patch closure to add sibling reference (for mutual recursion)."""
        closure_var_index = arg1
        const_index = arg2

        # Load patch info from constants
        patch_info = code.constants[const_index]
        if not isinstance(patch_info, AIFPLList) or len(patch_info.elements) != 2:
            raise AIFPLEvalError("PATCH_CLOSURE_SIBLING: invalid patch info")

        elements = patch_info.elements
        if not isinstance(elements[0], AIFPLNumber) or not isinstance(elements[1], AIFPLNumber):
            raise AIFPLEvalError("PATCH_CLOSURE_SIBLING: patch info elements must be numbers")

        if not isinstance(elements[0].value, int) or not isinstance(elements[1].value, int):
            raise AIFPLEvalError("PATCH_CLOSURE_SIBLING: patch info elements must be integers")

        sibling_var_index = int(elements[0].value)
        name_index = int(elements[1].value)
        sibling_name = code.names[name_index]

        target_frame = self.frames[-1]

        # Load the closure to patch
        if closure_var_index >= len(target_frame.locals):
            raise AIFPLEvalError(f"PATCH_CLOSURE_SIBLING: closure index {closure_var_index} out of range")

        target_closure = target_frame.locals[closure_var_index]
        if not isinstance(target_closure, AIFPLFunction):
            raise AIFPLEvalError(f"PATCH_CLOSURE_SIBLING: closure at index {closure_var_index} is not a function")

        # Load the sibling from locals
        if sibling_var_index >= len(target_frame.locals):
            raise AIFPLEvalError(f"PATCH_CLOSURE_SIBLING: sibling index {sibling_var_index} out of range")

        sibling = target_frame.locals[sibling_var_index]

        # Add sibling to closure's environment
        target_closure.closure_environment.bindings[sibling_name] = sibling
        return None

    def _op_make_list(self, _frame: Frame, _code: CodeObject, arg1: int, _arg2: int) -> AIFPLValue | None:
        """MAKE_LIST: Create list from N stack items."""
        n = arg1
        list_elements: List[AIFPLValue] = [self.stack.pop() for _ in range(n)]
        list_elements.reverse()
        self.stack.append(AIFPLList(tuple(list_elements)))
        return None

    def _call_bytecode_function(self, func: AIFPLFunction) -> AIFPLValue:
        """
        Call a bytecode function.

        Arguments are already on the stack. The function prologue will pop them
        and store them in locals.
        """
        code = func.bytecode

        # Create new frame
        new_frame = Frame(code)
        new_frame.closure_env = func.closure_environment

        # Store captured values in locals (after parameters)
        # The lambda compiler puts captured vars after parameters in the local space
        if func.captured_values:
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

    def _call_builtin(self, builtin_index: int, args: List[AIFPLValue]) -> AIFPLValue:
        """Call a builtin function by index.

        This method delegates to the unified builtin registry for regular builtins,
        and handles special forms (and, or, map, filter, fold, etc.) separately
        because they require special evaluation semantics.
        """
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
                        received=f"Argument {i+1}: {self.format_result(arg)} ({arg.type_name()})",
                        expected="Boolean value (#t or #f)",
                        example="(and (> x 0) (< x 10))",
                        suggestion="Use comparison or boolean operators to create boolean values"
                    )

                if not arg.value:
                    return AIFPLBoolean(False)

            return AIFPLBoolean(True)

        if builtin_name == 'or':
            # All arguments are already evaluated by bytecode
            if not args:
                return AIFPLBoolean(False)

            for i, arg in enumerate(args):
                if not isinstance(arg, AIFPLBoolean):
                    raise AIFPLEvalError(
                        message=f"Or operator argument {i+1} must be boolean",
                        received=f"Argument {i+1}: {self.format_result(arg)} ({arg.type_name()})",
                        expected="Boolean value (#t or #f)",
                        example="(or (= x 0) (> x 10))",
                        suggestion="Use comparison or boolean operators to create boolean values"
                    )

                if arg.value:
                    return AIFPLBoolean(True)

            return AIFPLBoolean(False)

        # Check if this builtin is in the registry
        if not self._builtin_registry.has_function(builtin_name):
            # Unknown builtin
            raise AIFPLEvalError(
                message=f"Unknown builtin function: {builtin_name}",
                suggestion="This may be an internal error - please report this issue"
            )

        # Call through the registry
        return self._builtin_registry.call_builtin(builtin_name, args)
