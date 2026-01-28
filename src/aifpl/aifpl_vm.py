"""AIFPL Virtual Machine - executes bytecode."""

from typing import List, Dict, Any
from dataclasses import dataclass, field

from aifpl.aifpl_builtins import AIFPLBuiltinRegistry
from aifpl.aifpl_bytecode import CodeObject, Opcode
from aifpl.aifpl_compiler import AIFPLCompiler
from aifpl.aifpl_environment import AIFPLEnvironment
from aifpl.aifpl_error import AIFPLEvalError, ErrorMessageBuilder
from aifpl.aifpl_value import (
    AIFPLValue, AIFPLInteger, AIFPLFloat, AIFPLComplex,
    AIFPLString, AIFPLBoolean, AIFPLList, AIFPLFunction, AIFPLAList
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
        self._builtin_functions = self._builtin_registry.create_builtin_function_objects()

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
        return table

    def execute(
        self,
        code: CodeObject,
        constants: Dict[str, AIFPLValue],
        prelude_functions: Dict[str, AIFPLFunction] | None = None
    ) -> AIFPLValue:
        """
        Execute a code object and return the result.

        Args:
            code: Compiled code object to execute
            constants: Dictionary of constant values (e.g., pi, e, j)
            prelude_functions: Optional dictionary of prelude functions

        Returns:
            Result value
        """
        self.globals = constants.copy()
        self.globals.update(self._builtin_functions)
        if prelude_functions:
            self.globals.update(prelude_functions)

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

    def _op_load_const(  # pylint: disable=useless-return
        self,
        _frame: Frame,
        code: CodeObject,
        arg1: int,
        _arg2: int
    ) -> AIFPLValue | None:
        """LOAD_CONST: Push constant from pool onto stack."""
        self.stack.append(code.constants[arg1])
        return None

    def _op_load_true(  # pylint: disable=useless-return
        self,
        _frame: Frame,
        _code: CodeObject,
        _arg1: int,
        _arg2: int
    ) -> AIFPLValue | None:
        """LOAD_TRUE: Push boolean true onto stack."""
        self.stack.append(AIFPLBoolean(True))
        return None

    def _op_load_false(  # pylint: disable=useless-return
        self,
        _frame: Frame,
        _code: CodeObject,
        _arg1: int,
        _arg2: int
    ) -> AIFPLValue | None:
        """LOAD_FALSE: Push boolean false onto stack."""
        self.stack.append(AIFPLBoolean(False))
        return None

    def _op_load_empty_list(  # pylint: disable=useless-return
        self,
        _frame: Frame,
        _code: CodeObject,
        _arg1: int,
        _arg2: int
    ) -> AIFPLValue | None:
        """LOAD_EMPTY_LIST: Push empty list onto stack."""
        self.stack.append(AIFPLList(()))
        return None

    def _op_load_var(  # pylint: disable=useless-return
        self,
        _frame: Frame,
        _code: CodeObject,
        arg1: int,
        arg2: int
    ) -> AIFPLValue | None:
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

    def _op_store_var(  # pylint: disable=useless-return
        self,
        _frame: Frame,
        _code: CodeObject,
        arg1: int,
        arg2: int
    ) -> AIFPLValue | None:
        """STORE_VAR: Store top of stack to variable at depth/index."""
        depth = arg1
        index = arg2
        value = self.stack.pop()
        target_frame = self.frames[-(depth + 1)]
        target_frame.locals[index] = value
        return None

    def _op_load_name(  # pylint: disable=useless-return
        self,
        frame: Frame,
        code: CodeObject,
        arg1: int,
        _arg2: int
    ) -> AIFPLValue | None:
        """LOAD_NAME: Load global variable by name."""
        name = code.names[arg1]

        # Check closure environment chain (for recursive closures and nested lambdas)
        # This traverses up the environment chain, allowing nested lambdas to find
        # recursive bindings from outer let scopes
        current_env = frame.closure_env
        while current_env is not None:
            if name in current_env.bindings:
                self.stack.append(current_env.bindings[name])
                return None

            current_env = current_env.parent

        if name in self.globals:
            self.stack.append(self.globals[name])
            return None

        # Not found - generate helpful error
        available_vars = list(self.globals.keys())
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

    def _op_jump(  # pylint: disable=useless-return
        self,
        frame: Frame,
        _code: CodeObject,
        arg1: int,
        _arg2: int
    ) -> AIFPLValue | None:
        """JUMP: Unconditional jump to instruction."""
        frame.ip = arg1
        return None

    def _op_pop_jump_if_false(  # pylint: disable=useless-return
        self,
        frame: Frame,
        _code: CodeObject,
        arg1: int,
        _arg2: int
    ) -> AIFPLValue | None:
        """POP_JUMP_IF_FALSE: Pop stack, jump if false."""
        condition = self.stack.pop()
        if not isinstance(condition, AIFPLBoolean):
            raise AIFPLEvalError("If condition must be boolean")

        if not condition.value:
            frame.ip = arg1

        return None

    def _op_pop_jump_if_true(  # pylint: disable=useless-return
        self,
        frame: Frame,
        _code: CodeObject,
        arg1: int,
        _arg2: int
    ) -> AIFPLValue | None:
        """POP_JUMP_IF_TRUE: Pop stack, jump if true."""
        condition = self.stack.pop()
        if not isinstance(condition, AIFPLBoolean):
            raise AIFPLEvalError("If condition must be boolean")

        if condition.value:
            frame.ip = arg1

        return None

    def _op_raise_error(  # pylint: disable=useless-return
        self,
        _frame: Frame,
        code: CodeObject,
        arg1: int,
        _arg2: int
    ) -> AIFPLValue | None:
        """RAISE_ERROR: Raise error with message from constant pool."""
        error_msg = code.constants[arg1]
        if not isinstance(error_msg, AIFPLString):
            raise AIFPLEvalError("RAISE_ERROR requires a string constant")

        raise AIFPLEvalError(error_msg.value)

    def _op_make_closure(  # pylint: disable=useless-return
        self,
        _frame: Frame,
        code: CodeObject,
        arg1: int,
        arg2: int
    ) -> AIFPLValue | None:
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

        # Create closure with captured environment and parent reference
        # The parent is the current frame's closure environment, which allows
        # nested lambdas to look up recursive bindings from outer let scopes
        current_frame = self.frames[-1] if self.frames else None
        parent_env = current_frame.closure_env if current_frame else None

        closure = AIFPLFunction(
            parameters=tuple(f"param{i}" for i in range(closure_code.param_count)),
            body=None,
            closure_environment=AIFPLEnvironment(bindings=captured_dict, parent=parent_env),
            name=closure_code.name,
            bytecode=closure_code,
            captured_values=tuple(captured_values)
        )
        self.stack.append(closure)
        return None

    def _op_call_function(  # pylint: disable=useless-return
        self,
        _frame: Frame,
        _code: CodeObject,
        arg1: int,
        _arg2: int
    ) -> AIFPLValue | None:
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

        # Check arity for bytecode functions
        expected_arity = func.bytecode.param_count
        if arity != expected_arity:
            func_name = func.name or "<lambda>"
            raise AIFPLEvalError(
                message=f"Function '{func_name}' expects {expected_arity} arguments, got {arity}",
                suggestion=f"Provide exactly {expected_arity} argument{'s' if expected_arity != 1 else ''}"
            )

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

    def _op_call_builtin(  # pylint: disable=useless-return
        self,
        _frame: Frame,
        _code: CodeObject,
        arg1: int,
        arg2: int
    ) -> AIFPLValue | None:
        """CALL_BUILTIN: Call builtin function by index."""
        builtin_index = arg1
        arity = arg2

        # Pop arguments
        args = [self.stack.pop() for _ in range(arity)]
        args.reverse()

        result = self._call_builtin(builtin_index, args)
        self.stack.append(result)
        return None

    def _op_return(
        self,
        _frame: Frame,
        _code: CodeObject,
        _arg1: int,
        _arg2: int
    ) -> AIFPLValue | None:
        """RETURN: Pop frame and return value from stack."""
        self.frames.pop()
        if not self.stack:
            raise AIFPLEvalError("RETURN with empty stack")

        return self.stack.pop()

    def _op_patch_closure_self(  # pylint: disable=useless-return
        self,
        _frame: Frame,
        code: CodeObject,
        arg1: int,
        arg2: int
    ) -> AIFPLValue | None:
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

    def _op_patch_closure_sibling(  # pylint: disable=useless-return
        self,
        _frame: Frame,
        code: CodeObject,
        arg1: int,
        arg2: int
    ) -> AIFPLValue | None:
        """PATCH_CLOSURE_SIBLING: Patch closure to add sibling reference (for mutual recursion)."""
        closure_var_index = arg1
        const_index = arg2

        # Load patch info from constants
        patch_info = code.constants[const_index]
        if not isinstance(patch_info, AIFPLList) or len(patch_info.elements) != 2:
            raise AIFPLEvalError("PATCH_CLOSURE_SIBLING: invalid patch info")

        elements = patch_info.elements
        if not isinstance(elements[0], AIFPLInteger) or not isinstance(elements[1], AIFPLInteger):
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
        """
        Call a builtin function by index.

        This method delegates to the unified builtin registry for regular builtins,
        and handles special forms (and, or, map, filter, fold, etc.) separately
        because they require special evaluation semantics.
        """
        builtin_name = AIFPLCompiler.BUILTIN_TABLE[builtin_index]

        # Check if this builtin is in the registry
        if not self._builtin_registry.has_function(builtin_name):
            # Unknown builtin
            raise AIFPLEvalError(
                message=f"Unknown builtin function: {builtin_name}",
                suggestion="This may be an internal error - please report this issue"
            )

        # Call through the registry
        return self._builtin_registry.call_builtin(builtin_name, args)

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
            escaped_content = self._escape_string_for_lisp(result.value)
            return f'"{escaped_content}"'

        if isinstance(result, (AIFPLInteger, AIFPLFloat)):
            return str(result.value)

        if isinstance(result, AIFPLComplex):
            return str(result.value).strip('()')

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
                formatted_pairs.append(f"(list {formatted_key} {formatted_value})")

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

    def _escape_string_for_lisp(self, s: str) -> str:
        """Escape a string for LISP display format."""
        result = []
        for char in s:
            if char == '"':
                result.append('\\"')

            elif char == '\\':
                result.append('\\\\')

            elif char == '\n':
                result.append('\\n')

            elif char == '\t':
                result.append('\\t')

            elif char == '\r':
                result.append('\\r')

            elif ord(char) < 32:  # Other control characters
                result.append(f'\\u{ord(char):04x}')

            else:
                result.append(char)  # Keep Unicode as-is

        return ''.join(result)
