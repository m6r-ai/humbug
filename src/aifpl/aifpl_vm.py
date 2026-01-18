"""AIFPL Virtual Machine - executes bytecode."""

import math
from typing import List, Dict, Any, Optional, Tuple
from dataclasses import dataclass

from aifpl.aifpl_value import (
    AIFPLValue, AIFPLNumber, AIFPLString, AIFPLBoolean,
    AIFPLList, AIFPLFunction, AIFPLSymbol, AIFPLBuiltinFunction, AIFPLAlist
)
from aifpl.aifpl_bytecode import CodeObject, Opcode
from aifpl.aifpl_error import AIFPLEvalError
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
        """Initialize VM.

        Args:
            evaluator: Reference to AIFPLEvaluator for builtin functions
        """
        self.evaluator = evaluator
        self.stack: List[AIFPLValue] = []
        self.frames: List[Frame] = []
        self.globals: Dict[str, AIFPLValue] = {}

    def set_globals(self, globals_dict: Dict[str, AIFPLValue]) -> None:
        """Set global variables (builtins, constants, etc.)."""
        self.globals = globals_dict.copy()

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

            elif opcode == Opcode.LOAD_LOCAL:
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

            elif opcode == Opcode.STORE_LOCAL:
                depth = arg1
                index = arg2
                value = self.stack.pop()
                # Store to frame at depth
                target_frame = self.frames[-(depth + 1)]
                target_frame.locals[index] = value

            elif opcode == Opcode.LOAD_GLOBAL:
                name = code.names[arg1]

                # First check closure environment (for recursive closures)
                if frame.closure_env and name in frame.closure_env.bindings:
                    self.stack.append(frame.closure_env.bindings[name])
                elif name in self.globals:
                    self.stack.append(self.globals[name])
                else:
                    raise AIFPLEvalError(f"Undefined global variable: '{name}'")

            elif opcode == Opcode.ADD_NN:
                b = self.stack.pop()
                a = self.stack.pop()
                if not isinstance(a, AIFPLNumber) or not isinstance(b, AIFPLNumber):
                    raise AIFPLEvalError("ADD_NN requires numbers")
                self.stack.append(AIFPLNumber(a.value + b.value))

            elif opcode == Opcode.SUB_NN:
                b = self.stack.pop()
                a = self.stack.pop()
                if not isinstance(a, AIFPLNumber) or not isinstance(b, AIFPLNumber):
                    raise AIFPLEvalError("SUB_NN requires numbers")
                self.stack.append(AIFPLNumber(a.value - b.value))

            elif opcode == Opcode.MUL_NN:
                b = self.stack.pop()
                a = self.stack.pop()
                if not isinstance(a, AIFPLNumber) or not isinstance(b, AIFPLNumber):
                    raise AIFPLEvalError("MUL_NN requires numbers")
                self.stack.append(AIFPLNumber(a.value * b.value))

            elif opcode == Opcode.DIV_NN:
                b = self.stack.pop()
                a = self.stack.pop()
                if not isinstance(a, AIFPLNumber) or not isinstance(b, AIFPLNumber):
                    raise AIFPLEvalError("DIV_NN requires numbers")
                if b.value == 0:
                    raise AIFPLEvalError("Division by zero")
                self.stack.append(AIFPLNumber(a.value / b.value))

            elif opcode == Opcode.EQ_NN:
                b = self.stack.pop()
                a = self.stack.pop()
                if not isinstance(a, AIFPLNumber) or not isinstance(b, AIFPLNumber):
                    raise AIFPLEvalError("EQ_NN requires numbers")
                self.stack.append(AIFPLBoolean(a.value == b.value))

            elif opcode == Opcode.LT_NN:
                b = self.stack.pop()
                a = self.stack.pop()
                if not isinstance(a, AIFPLNumber) or not isinstance(b, AIFPLNumber):
                    raise AIFPLEvalError("LT_NN requires numbers")
                self.stack.append(AIFPLBoolean(a.value < b.value))

            elif opcode == Opcode.GT_NN:
                b = self.stack.pop()
                a = self.stack.pop()
                if not isinstance(a, AIFPLNumber) or not isinstance(b, AIFPLNumber):
                    raise AIFPLEvalError("GT_NN requires numbers")
                self.stack.append(AIFPLBoolean(a.value > b.value))

            elif opcode == Opcode.LTE_NN:
                b = self.stack.pop()
                a = self.stack.pop()
                if not isinstance(a, AIFPLNumber) or not isinstance(b, AIFPLNumber):
                    raise AIFPLEvalError("LTE_NN requires numbers")
                self.stack.append(AIFPLBoolean(a.value <= b.value))

            elif opcode == Opcode.GTE_NN:
                b = self.stack.pop()
                a = self.stack.pop()
                if not isinstance(a, AIFPLNumber) or not isinstance(b, AIFPLNumber):
                    raise AIFPLEvalError("GTE_NN requires numbers")
                self.stack.append(AIFPLBoolean(a.value >= b.value))

            elif opcode == Opcode.JUMP:
                frame.ip = arg1

            elif opcode == Opcode.POP_JUMP_IF_FALSE:
                condition = self.stack.pop()
                if not isinstance(condition, AIFPLBoolean):
                    raise AIFPLEvalError("Jump condition must be boolean")
                if not condition.value:
                    frame.ip = arg1

            elif opcode == Opcode.POP_JUMP_IF_TRUE:
                condition = self.stack.pop()
                if not isinstance(condition, AIFPLBoolean):
                    raise AIFPLEvalError("Jump condition must be boolean")
                if condition.value:
                    frame.ip = arg1

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

                if not isinstance(func, AIFPLFunction):
                    raise AIFPLEvalError(f"Cannot call non-function: {type(func).__name__}")

                # Check if function has bytecode
                if hasattr(func, 'bytecode') and func.bytecode is not None:
                    # Call bytecode function
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

                # Load the closure from locals
                closure = frame.locals[var_index]

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
                frame.locals[var_index] = patched_closure

            elif opcode == Opcode.POP:
                self.stack.pop()

            elif opcode == Opcode.MAKE_LIST:
                n = arg1
                elements = [self.stack.pop() for _ in range(n)]
                elements.reverse()
                self.stack.append(AIFPLList(tuple(elements)))

            elif opcode == Opcode.MAKE_FRAME:
                # Create a new frame for let bindings
                # arg1 is the number of local slots
                # Create a dummy code object for the frame
                dummy_code = CodeObject(
                    instructions=[],
                    constants=[],
                    names=[],
                    code_objects=[],
                    local_count=arg1,
                    name="<let>"
                )
                new_frame = Frame(dummy_code)
                self.frames.append(new_frame)

            elif opcode == Opcode.POP_FRAME:
                # Pop the let's frame
                if len(self.frames) > 1:  # Don't pop the main frame
                    self.frames.pop()

            else:
                raise AIFPLEvalError(f"Unimplemented opcode: {opcode.name}")

        # Frame finished without explicit return
        return None

    def _call_bytecode_function(self, func: AIFPLFunction, args: List[AIFPLValue]) -> AIFPLValue:
        """Call a bytecode function."""
        code = func.bytecode

        # Check arity
        if len(args) != code.param_count:
            raise AIFPLEvalError(
                f"Function expects {code.param_count} arguments, got {len(args)}"
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

    def _call_builtin(self, builtin_index: int, args: List[AIFPLValue]) -> AIFPLValue:
        """Call a builtin function by index."""
        from aifpl.aifpl_compiler import AIFPLCompiler

        builtin_name = AIFPLCompiler.BUILTIN_TABLE[builtin_index]

        # Simple arithmetic operations
        if builtin_name == '+':
            total = 0
            for arg in args:
                if not isinstance(arg, AIFPLNumber):
                    raise AIFPLEvalError(f"+ requires numbers, got {type(arg).__name__}")
                total += arg.value
            return AIFPLNumber(total)

        elif builtin_name == '-':
            if not args:
                raise AIFPLEvalError("- requires at least 1 argument")
            if not isinstance(args[0], AIFPLNumber):
                raise AIFPLEvalError(f"- requires numbers, got {type(args[0]).__name__}")

            if len(args) == 1:
                return AIFPLNumber(-args[0].value)

            result = args[0].value
            for arg in args[1:]:
                if not isinstance(arg, AIFPLNumber):
                    raise AIFPLEvalError(f"- requires numbers, got {type(arg).__name__}")
                result -= arg.value
            return AIFPLNumber(result)

        elif builtin_name == '*':
            result = 1
            for arg in args:
                if not isinstance(arg, AIFPLNumber):
                    raise AIFPLEvalError(f"* requires numbers, got {type(arg).__name__}")
                result *= arg.value
            return AIFPLNumber(result)

        elif builtin_name == '/':
            if len(args) < 2:
                raise AIFPLEvalError("/ requires at least 2 arguments")
            if not isinstance(args[0], AIFPLNumber):
                raise AIFPLEvalError(f"/ requires numbers, got {type(args[0]).__name__}")

            result = args[0].value
            for arg in args[1:]:
                if not isinstance(arg, AIFPLNumber):
                    raise AIFPLEvalError(f"/ requires numbers, got {type(arg).__name__}")
                if arg.value == 0:
                    raise AIFPLEvalError("Division by zero")
                result /= arg.value
            return AIFPLNumber(result)

        # Comparisons
        elif builtin_name == '=':
            if len(args) < 2:
                raise AIFPLEvalError("= requires at least 2 arguments")
            first = args[0]
            for arg in args[1:]:
                # Simple equality check (would need more sophisticated comparison)
                if type(first) != type(arg):
                    return AIFPLBoolean(False)
                if isinstance(first, (AIFPLNumber, AIFPLString, AIFPLBoolean)):
                    if first.value != arg.value:
                        return AIFPLBoolean(False)
            return AIFPLBoolean(True)

        elif builtin_name == '<':
            if len(args) < 2:
                raise AIFPLEvalError("< requires at least 2 arguments")
            for i in range(len(args) - 1):
                if not isinstance(args[i], AIFPLNumber) or not isinstance(args[i+1], AIFPLNumber):
                    raise AIFPLEvalError("< requires numbers")
                if not (args[i].value < args[i+1].value):
                    return AIFPLBoolean(False)
            return AIFPLBoolean(True)

        elif builtin_name == '>':
            if len(args) < 2:
                raise AIFPLEvalError("> requires at least 2 arguments")
            for i in range(len(args) - 1):
                if not isinstance(args[i], AIFPLNumber) or not isinstance(args[i+1], AIFPLNumber):
                    raise AIFPLEvalError("> requires numbers")
                if not (args[i].value > args[i+1].value):
                    return AIFPLBoolean(False)
            return AIFPLBoolean(True)

        elif builtin_name == '<=':
            if len(args) < 2:
                raise AIFPLEvalError("<= requires at least 2 arguments")
            for i in range(len(args) - 1):
                if not isinstance(args[i], AIFPLNumber) or not isinstance(args[i+1], AIFPLNumber):
                    raise AIFPLEvalError("<= requires numbers")
                if not (args[i].value <= args[i+1].value):
                    return AIFPLBoolean(False)
            return AIFPLBoolean(True)

        elif builtin_name == '>=':
            if len(args) < 2:
                raise AIFPLEvalError(">= requires at least 2 arguments")
            for i in range(len(args) - 1):
                if not isinstance(args[i], AIFPLNumber) or not isinstance(args[i+1], AIFPLNumber):
                    raise AIFPLEvalError(">= requires numbers")
                if not (args[i].value >= args[i+1].value):
                    return AIFPLBoolean(False)
            return AIFPLBoolean(True)

        # List operations
        elif builtin_name == 'list':
            return AIFPLList(tuple(args))

        elif builtin_name == 'cons':
            if len(args) != 2:
                raise AIFPLEvalError("cons requires exactly 2 arguments")
            head = args[0]
            tail = args[1]
            if not isinstance(tail, AIFPLList):
                raise AIFPLEvalError(f"cons second argument must be a list, got {type(tail).__name__}")
            return AIFPLList((head,) + tail.elements)

        elif builtin_name == 'append':
            result_elements = []
            for arg in args:
                if not isinstance(arg, AIFPLList):
                    raise AIFPLEvalError(f"append requires lists, got {type(arg).__name__}")
                result_elements.extend(arg.elements)
            return AIFPLList(tuple(result_elements))

        elif builtin_name == 'reverse':
            if len(args) != 1:
                raise AIFPLEvalError("reverse requires exactly 1 argument")
            if not isinstance(args[0], AIFPLList):
                raise AIFPLEvalError(f"reverse requires a list, got {type(args[0]).__name__}")
            return AIFPLList(tuple(reversed(args[0].elements)))

        elif builtin_name == 'first':
            if len(args) != 1:
                raise AIFPLEvalError("first requires exactly 1 argument")
            if not isinstance(args[0], AIFPLList):
                raise AIFPLEvalError(f"first requires a list, got {type(args[0]).__name__}")
            if args[0].is_empty():
                raise AIFPLEvalError("first called on empty list")
            return args[0].first()

        elif builtin_name == 'rest':
            if len(args) != 1:
                raise AIFPLEvalError("rest requires exactly 1 argument")
            if not isinstance(args[0], AIFPLList):
                raise AIFPLEvalError(f"rest requires a list, got {type(args[0]).__name__}")
            if args[0].is_empty():
                raise AIFPLEvalError("rest called on empty list")
            return AIFPLList(args[0].elements[1:])

        elif builtin_name == 'last':
            if len(args) != 1:
                raise AIFPLEvalError("last requires exactly 1 argument")
            if not isinstance(args[0], AIFPLList):
                raise AIFPLEvalError(f"last requires a list, got {type(args[0]).__name__}")
            if args[0].is_empty():
                raise AIFPLEvalError("last called on empty list")
            return args[0].elements[-1]

        elif builtin_name == 'length':
            if len(args) != 1:
                raise AIFPLEvalError("length requires exactly 1 argument")
            if not isinstance(args[0], AIFPLList):
                raise AIFPLEvalError(f"length requires a list, got {type(args[0]).__name__}")
            return AIFPLNumber(len(args[0].elements))

        elif builtin_name == 'null?':
            if len(args) != 1:
                raise AIFPLEvalError("null? requires exactly 1 argument")
            if isinstance(args[0], AIFPLList):
                return AIFPLBoolean(args[0].is_empty())
            return AIFPLBoolean(False)

        elif builtin_name == 'member?':
            if len(args) != 2:
                raise AIFPLEvalError("member? requires exactly 2 arguments")
            item = args[0]
            lst = args[1]
            if not isinstance(lst, AIFPLList):
                raise AIFPLEvalError(f"member? second argument must be a list, got {type(lst).__name__}")

            # Simple equality check
            for elem in lst.elements:
                if type(item) == type(elem):
                    if isinstance(item, (AIFPLNumber, AIFPLString, AIFPLBoolean)):
                        if item.value == elem.value:
                            return AIFPLBoolean(True)
            return AIFPLBoolean(False)

        elif builtin_name == 'position':
            if len(args) != 2:
                raise AIFPLEvalError("position requires exactly 2 arguments")
            item = args[0]
            lst = args[1]
            if not isinstance(lst, AIFPLList):
                raise AIFPLEvalError(f"position second argument must be a list, got {type(lst).__name__}")

            for i, elem in enumerate(lst.elements):
                if type(item) == type(elem):
                    if isinstance(item, (AIFPLNumber, AIFPLString, AIFPLBoolean)):
                        if item.value == elem.value:
                            return AIFPLNumber(i)
            return AIFPLBoolean(False)

        elif builtin_name == 'take':
            if len(args) != 2:
                raise AIFPLEvalError("take requires exactly 2 arguments")
            if not isinstance(args[0], AIFPLNumber):
                raise AIFPLEvalError("take first argument must be a number")
            if not isinstance(args[1], AIFPLList):
                raise AIFPLEvalError("take second argument must be a list")
            n = int(args[0].value)
            return AIFPLList(args[1].elements[:n])

        elif builtin_name == 'drop':
            if len(args) != 2:
                raise AIFPLEvalError("drop requires exactly 2 arguments")
            if not isinstance(args[0], AIFPLNumber):
                raise AIFPLEvalError("drop first argument must be a number")
            if not isinstance(args[1], AIFPLList):
                raise AIFPLEvalError("drop second argument must be a list")
            n = int(args[0].value)
            return AIFPLList(args[1].elements[n:])

        elif builtin_name == 'remove':
            if len(args) != 2:
                raise AIFPLEvalError("remove requires exactly 2 arguments")
            item = args[0]
            lst = args[1]
            if not isinstance(lst, AIFPLList):
                raise AIFPLEvalError("remove second argument must be a list")

            result = []
            for elem in lst.elements:
                # Skip matching elements
                if type(item) == type(elem):
                    if isinstance(item, (AIFPLNumber, AIFPLString, AIFPLBoolean)):
                        if item.value == elem.value:
                            continue
                result.append(elem)
            return AIFPLList(tuple(result))

        # Math functions
        elif builtin_name == 'sqrt':
            if len(args) != 1:
                raise AIFPLEvalError("sqrt requires exactly 1 argument")
            if not isinstance(args[0], AIFPLNumber):
                raise AIFPLEvalError("sqrt requires a number")
            return AIFPLNumber(math.sqrt(args[0].value))

        elif builtin_name == 'abs':
            if len(args) != 1:
                raise AIFPLEvalError("abs requires exactly 1 argument")
            if not isinstance(args[0], AIFPLNumber):
                raise AIFPLEvalError("abs requires a number")
            return AIFPLNumber(abs(args[0].value))

        elif builtin_name == 'min':
            if len(args) < 1:
                raise AIFPLEvalError("min requires at least 1 argument")
            values = []
            for arg in args:
                if not isinstance(arg, AIFPLNumber):
                    raise AIFPLEvalError("min requires numbers")
                values.append(arg.value)
            return AIFPLNumber(min(values))

        elif builtin_name == 'max':
            if len(args) < 1:
                raise AIFPLEvalError("max requires at least 1 argument")
            values = []
            for arg in args:
                if not isinstance(arg, AIFPLNumber):
                    raise AIFPLEvalError("max requires numbers")
                values.append(arg.value)
            return AIFPLNumber(max(values))

        elif builtin_name == 'pow':
            if len(args) != 2:
                raise AIFPLEvalError("pow requires exactly 2 arguments")
            if not isinstance(args[0], AIFPLNumber) or not isinstance(args[1], AIFPLNumber):
                raise AIFPLEvalError("pow requires numbers")
            return AIFPLNumber(args[0].value ** args[1].value)

        # Higher-order functions
        elif builtin_name == 'range':
            if len(args) < 2 or len(args) > 3:
                raise AIFPLEvalError("range requires 2 or 3 arguments")
            if not all(isinstance(arg, AIFPLNumber) for arg in args):
                raise AIFPLEvalError("range requires numbers")

            start = int(args[0].value)
            end = int(args[1].value)
            step = int(args[2].value) if len(args) == 3 else 1

            if step == 0:
                raise AIFPLEvalError("range step cannot be zero")

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
                raise AIFPLEvalError("map requires exactly 2 arguments")
            func = args[0]
            lst = args[1]

            if not isinstance(func, (AIFPLFunction, AIFPLBuiltinFunction)):
                raise AIFPLEvalError(f"map first argument must be a function, got {type(func).__name__}")
            if not isinstance(lst, AIFPLList):
                raise AIFPLEvalError(f"map second argument must be a list, got {type(lst).__name__}")

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
                raise AIFPLEvalError("filter requires exactly 2 arguments")
            func = args[0]
            lst = args[1]

            if not isinstance(func, (AIFPLFunction, AIFPLBuiltinFunction)):
                raise AIFPLEvalError(f"filter first argument must be a function, got {type(func).__name__}")
            if not isinstance(lst, AIFPLList):
                raise AIFPLEvalError(f"filter second argument must be a list, got {type(lst).__name__}")

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
                    raise AIFPLEvalError("filter predicate must return boolean")
                if test_result.value:
                    result.append(item)

            return AIFPLList(tuple(result))

        elif builtin_name == 'fold':
            if len(args) != 3:
                raise AIFPLEvalError("fold requires exactly 3 arguments")
            func = args[0]
            init = args[1]
            lst = args[2]

            if not isinstance(func, (AIFPLFunction, AIFPLBuiltinFunction)):
                raise AIFPLEvalError(f"fold first argument must be a function, got {type(func).__name__}")
            if not isinstance(lst, AIFPLList):
                raise AIFPLEvalError(f"fold third argument must be a list, got {type(lst).__name__}")

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

        # String operations
        elif builtin_name == 'string-append':
            result = ""
            for arg in args:
                if not isinstance(arg, AIFPLString):
                    raise AIFPLEvalError(f"string-append requires strings, got {type(arg).__name__}")
                result += arg.value
            return AIFPLString(result)

        elif builtin_name == 'string-length':
            if len(args) != 1:
                raise AIFPLEvalError("string-length requires exactly 1 argument")
            if not isinstance(args[0], AIFPLString):
                raise AIFPLEvalError("string-length requires a string")
            return AIFPLNumber(len(args[0].value))

        elif builtin_name == 'string-upcase':
            if len(args) != 1:
                raise AIFPLEvalError("string-upcase requires exactly 1 argument")
            if not isinstance(args[0], AIFPLString):
                raise AIFPLEvalError("string-upcase requires a string")
            return AIFPLString(args[0].value.upper())

        elif builtin_name == 'string-downcase':
            if len(args) != 1:
                raise AIFPLEvalError("string-downcase requires exactly 1 argument")
            if not isinstance(args[0], AIFPLString):
                raise AIFPLEvalError("string-downcase requires a string")
            return AIFPLString(args[0].value.lower())

        elif builtin_name == 'string-trim':
            if len(args) != 1:
                raise AIFPLEvalError("string-trim requires exactly 1 argument")
            if not isinstance(args[0], AIFPLString):
                raise AIFPLEvalError("string-trim requires a string")
            return AIFPLString(args[0].value.strip())

        elif builtin_name == 'string-replace':
            if len(args) != 3:
                raise AIFPLEvalError("string-replace requires exactly 3 arguments")
            if not all(isinstance(arg, AIFPLString) for arg in args):
                raise AIFPLEvalError("string-replace requires strings")
            string = args[0].value
            old = args[1].value
            new = args[2].value
            return AIFPLString(string.replace(old, new))

        elif builtin_name == 'string-split':
            if len(args) != 2:
                raise AIFPLEvalError("string-split requires exactly 2 arguments")
            if not isinstance(args[0], AIFPLString) or not isinstance(args[1], AIFPLString):
                raise AIFPLEvalError("string-split requires strings")
            string = args[0].value
            delimiter = args[1].value
            parts = string.split(delimiter)
            return AIFPLList(tuple(AIFPLString(part) for part in parts))

        elif builtin_name == 'string-join':
            if len(args) != 2:
                raise AIFPLEvalError("string-join requires exactly 2 arguments")
            if not isinstance(args[0], AIFPLList):
                raise AIFPLEvalError("string-join first argument must be a list")
            if not isinstance(args[1], AIFPLString):
                raise AIFPLEvalError("string-join second argument must be a string")

            parts = []
            for item in args[0].elements:
                if not isinstance(item, AIFPLString):
                    raise AIFPLEvalError("string-join list must contain strings")
                parts.append(item.value)

            delimiter = args[1].value
            return AIFPLString(delimiter.join(parts))

        elif builtin_name == 'string-contains?':
            if len(args) != 2:
                raise AIFPLEvalError("string-contains? requires exactly 2 arguments")
            if not isinstance(args[0], AIFPLString) or not isinstance(args[1], AIFPLString):
                raise AIFPLEvalError("string-contains? requires strings")
            return AIFPLBoolean(args[1].value in args[0].value)

        elif builtin_name == 'string-prefix?':
            if len(args) != 2:
                raise AIFPLEvalError("string-prefix? requires exactly 2 arguments")
            if not isinstance(args[0], AIFPLString) or not isinstance(args[1], AIFPLString):
                raise AIFPLEvalError("string-prefix? requires strings")
            return AIFPLBoolean(args[0].value.startswith(args[1].value))

        elif builtin_name == 'string-suffix?':
            if len(args) != 2:
                raise AIFPLEvalError("string-suffix? requires exactly 2 arguments")
            if not isinstance(args[0], AIFPLString) or not isinstance(args[1], AIFPLString):
                raise AIFPLEvalError("string-suffix? requires strings")
            return AIFPLBoolean(args[0].value.endswith(args[1].value))

        elif builtin_name == 'string-ref':
            if len(args) != 2:
                raise AIFPLEvalError("string-ref requires exactly 2 arguments")
            if not isinstance(args[0], AIFPLString):
                raise AIFPLEvalError("string-ref first argument must be a string")
            if not isinstance(args[1], AIFPLNumber):
                raise AIFPLEvalError("string-ref second argument must be a number")
            index = int(args[1].value)
            if index < 0 or index >= len(args[0].value):
                raise AIFPLEvalError(f"string-ref index out of range: {index}")
            return AIFPLString(args[0].value[index])

        elif builtin_name == 'substring':
            if len(args) != 3:
                raise AIFPLEvalError("substring requires exactly 3 arguments")
            if not isinstance(args[0], AIFPLString):
                raise AIFPLEvalError("substring first argument must be a string")
            if not isinstance(args[1], AIFPLNumber) or not isinstance(args[2], AIFPLNumber):
                raise AIFPLEvalError("substring indices must be numbers")
            string = args[0].value
            start = int(args[1].value)
            end = int(args[2].value)
            return AIFPLString(string[start:end])

        elif builtin_name == 'string->number':
            if len(args) != 1:
                raise AIFPLEvalError("string->number requires exactly 1 argument")
            if not isinstance(args[0], AIFPLString):
                raise AIFPLEvalError("string->number requires a string")
            try:
                # Try to parse as integer first
                if '.' not in args[0].value and 'e' not in args[0].value.lower():
                    return AIFPLNumber(int(args[0].value))
                else:
                    return AIFPLNumber(float(args[0].value))
            except ValueError:
                return AIFPLBoolean(False)  # Return #f on parse failure

        elif builtin_name == 'number->string':
            if len(args) != 1:
                raise AIFPLEvalError("number->string requires exactly 1 argument")
            if not isinstance(args[0], AIFPLNumber):
                raise AIFPLEvalError("number->string requires a number")
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
            for arg in args:
                if not isinstance(arg, AIFPLList):
                    raise AIFPLEvalError(f"alist pairs must be lists, got {type(arg).__name__}")
                if len(arg.elements) != 2:
                    raise AIFPLEvalError(f"alist pairs must have exactly 2 elements, got {len(arg.elements)}")
                key, value = arg.elements
                pairs.append((key, value))
            return AIFPLAlist(tuple(pairs))

        elif builtin_name == 'alist-get':
            if len(args) < 2 or len(args) > 3:
                raise AIFPLEvalError("alist-get requires 2 or 3 arguments")
            alist = args[0]
            key = args[1]
            default = args[2] if len(args) == 3 else None

            if not isinstance(alist, AIFPLAlist):
                raise AIFPLEvalError(f"alist-get first argument must be an alist, got {type(alist).__name__}")

            result = alist.get(key)
            if result is None:
                if default is not None:
                    return default
                else:
                    raise AIFPLEvalError(f"Key not found in alist: {key}")
            return result

        elif builtin_name == 'alist-set':
            if len(args) != 3:
                raise AIFPLEvalError("alist-set requires exactly 3 arguments")
            alist = args[0]
            key = args[1]
            value = args[2]

            if not isinstance(alist, AIFPLAlist):
                raise AIFPLEvalError(f"alist-set first argument must be an alist, got {type(alist).__name__}")

            return alist.set(key, value)

        elif builtin_name == 'alist-remove':
            if len(args) != 2:
                raise AIFPLEvalError("alist-remove requires exactly 2 arguments")
            alist = args[0]
            key = args[1]

            if not isinstance(alist, AIFPLAlist):
                raise AIFPLEvalError(f"alist-remove first argument must be an alist, got {type(alist).__name__}")

            return alist.remove(key)

        elif builtin_name == 'alist-has?':
            if len(args) != 2:
                raise AIFPLEvalError("alist-has? requires exactly 2 arguments")
            alist = args[0]
            key = args[1]

            if not isinstance(alist, AIFPLAlist):
                raise AIFPLEvalError(f"alist-has? first argument must be an alist, got {type(alist).__name__}")

            return AIFPLBoolean(alist.has_key(key))

        elif builtin_name == 'alist-keys':
            if len(args) != 1:
                raise AIFPLEvalError("alist-keys requires exactly 1 argument")
            alist = args[0]

            if not isinstance(alist, AIFPLAlist):
                raise AIFPLEvalError(f"alist-keys requires an alist, got {type(alist).__name__}")

            return AIFPLList(alist.keys())

        elif builtin_name == 'alist-values':
            if len(args) != 1:
                raise AIFPLEvalError("alist-values requires exactly 1 argument")
            alist = args[0]

            if not isinstance(alist, AIFPLAlist):
                raise AIFPLEvalError(f"alist-values requires an alist, got {type(alist).__name__}")

            return AIFPLList(alist.values())

        elif builtin_name == 'alist-merge':
            if len(args) != 2:
                raise AIFPLEvalError("alist-merge requires exactly 2 arguments")
            alist1 = args[0]
            alist2 = args[1]

            if not isinstance(alist1, AIFPLAlist) or not isinstance(alist2, AIFPLAlist):
                raise AIFPLEvalError("alist-merge requires two alists")

            return alist1.merge(alist2)

        elif builtin_name == 'alist?':
            if len(args) != 1:
                raise AIFPLEvalError("alist? requires exactly 1 argument")
            return AIFPLBoolean(isinstance(args[0], AIFPLAlist))

        else:
            raise AIFPLEvalError(f"Builtin function not implemented: {builtin_name}")
