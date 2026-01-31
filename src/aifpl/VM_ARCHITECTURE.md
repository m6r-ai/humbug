# AIFPL Virtual Machine Architecture

## Overview

AIFPL includes a bytecode compiler and virtual machine (VM) for efficient execution of compiled code. The VM uses a stack-based architecture with lexically-scoped frames for function calls.

## Architecture Components

## Bytecode Structure

### CodeObject

A `CodeObject` represents compiled code and contains:

- **`instructions`** - List of bytecode instructions
- **`constants`** - Constant pool (for `LOAD_CONST`)
- **`names`** - Name pool (for `LOAD_NAME`)
- **`code_objects`** - Nested code objects (for closures/lambdas)
- **`free_vars`** - Free variables to capture (for closures)
- **`param_count`** - Number of function parameters
- **`local_count`** - Number of local variables
- **`name`** - Function name (for debugging)

### Instruction

An `Instruction` has:

- **`opcode`** - The operation to perform (from `Opcode` enum)
- **`arg1`** - First argument (default 0)
- **`arg2`** - Second argument (default 0)

Example: `Instruction(Opcode.LOAD_VAR, 2, 3)` → `LOAD_VAR 2 3`

## Opcode Reference

### Constants (4 opcodes)

Load constant values onto the stack.

| Opcode | Args | Description | Example |
|--------|------|-------------|---------|
| `LOAD_CONST` | const_index | Load constant from constant pool | `LOAD_CONST 5` → push constants[5] |
| `LOAD_TRUE` | - | Push boolean true | `LOAD_TRUE` → push #t |
| `LOAD_FALSE` | - | Push boolean false | `LOAD_FALSE` → push #f |
| `LOAD_EMPTY_LIST` | - | Push empty list | `LOAD_EMPTY_LIST` → push () |

### Variables (3 opcodes)

Load and store variables using lexical addressing.

| Opcode | Args | Description | Example |
|--------|------|-------------|---------|
| `LOAD_VAR` | depth, index | Load variable by lexical position | `LOAD_VAR 0 2` → load from current frame, local #2 |
| `STORE_VAR` | depth, index | Store variable by lexical position | `STORE_VAR 1 0` → store to parent frame, local #0 |
| `LOAD_NAME` | name_index | Load by name lookup (globals) | `LOAD_NAME 3` → load names[3] from globals |

**Lexical Addressing**: 
- `depth=0` is the current frame
- `depth=1` is the parent frame
- `depth=2` is the grandparent frame, etc.
- `index` is the position in the frame's locals array

### Control Flow (4 opcodes)

Jump instructions for conditionals and loops.

| Opcode | Args | Description | Example |
|--------|------|-------------|---------|
| `JUMP` | offset | Unconditional jump to instruction | `JUMP 10` → set IP to 10 |
| `JUMP_IF_FALSE` | offset | Pop stack, jump if false | `JUMP_IF_FALSE 15` → if top is #f, jump to 15 |
| `JUMP_IF_TRUE` | offset | Pop stack, jump if true | `JUMP_IF_TRUE 20` → if top is #t, jump to 20 |
| `RAISE_ERROR` | const_index | Raise error with message | `RAISE_ERROR 0` → raise error with constants[0] as message |

### Functions (6 opcodes)

Create, call, and manage functions and closures.

| Opcode | Args | Description | Example |
|--------|------|-------------|---------|
| `MAKE_CLOSURE` | code_index, capture_count | Create closure from code object | `MAKE_CLOSURE 2 3` → create closure from code_objects[2], capturing 3 values from stack |
| `CALL_FUNCTION` | arity | Call function with N arguments | `CALL_FUNCTION 2` → pop 2 args and function, call it |
| `TAIL_CALL_FUNCTION` | arity | Tail call function with N arguments | `TAIL_CALL_FUNCTION 2` → pop 2 args and function, call it |
| `CALL_BUILTIN` | builtin_index, arity | Call builtin function | `CALL_BUILTIN 5 2` → call builtin #5 with 2 args |
| `PATCH_CLOSURE_SELF` | name_index, var_index | Patch closure for self-reference | `PATCH_CLOSURE_SELF 1 0` → add self-reference for recursion |
| `PATCH_CLOSURE_SIBLING` | closure_idx, const_index | Patch closure for mutual recursion | `PATCH_CLOSURE_SIBLING 0 1` → add sibling reference |
| `RETURN` | - | Return from function | `RETURN` → pop frame, return top of stack |

## VM Execution Model

### Stack Machine

The VM uses a value stack for computation:

```python
stack: List[AIFPLValue]  # Operand stack
```

Operations push and pop values from this stack.

### Call Frames

Each function call creates a `Frame`:

```python
@dataclass
class Frame:
    code: CodeObject           # Code being executed
    ip: int                    # Instruction pointer
    locals: List[AIFPLValue]   # Local variables
    closure_env: Any           # Closure environment
```

Frames are stored in a frame stack:

```python
frames: List[Frame]  # Call stack
```

### Execution Loop

The VM executes instructions in the current frame:

1. Fetch instruction at `frame.ip`
2. Increment `frame.ip`
3. Execute the instruction
4. Repeat until `RETURN` or end of code

### Tail Call Optimization

The VM automatically optimizes tail calls:

- Detects when next instruction is `RETURN`
- Reuses current frame instead of creating new one
- Prevents stack overflow in recursive functions

Example:
```lisp
(let ((factorial (lambda (n acc)
                   (if (<= n 1)
                       acc
                       (factorial (- n 1) (* n acc))))))
  (factorial 10000 1))  ; Won't overflow!
```

## Example Compilation

### Source Code

```lisp
(let ((x 5))
  (+ x 3))
```

### Compiled Bytecode

```
CodeObject: <module>
  Parameters: 0
  Locals: 1
  Constants: [5, 3]
  Names: ['+']
  Instructions:
      0: LOAD_CONST 0        ; Push 5
      1: STORE_VAR 0 0       ; Store to local x (depth 0, index 0)
      2: LOAD_NAME 0         ; Load builtin '+'
      3: LOAD_VAR 0 0        ; Load local x
      4: LOAD_CONST 1        ; Push 3
      5: CALL_BUILTIN 0 2    ; Call + with 2 args
      6: RETURN              ; Return result
```

### Execution Trace

```
IP=0: LOAD_CONST 0     stack=[5]
IP=1: STORE_VAR 0 0    stack=[], locals[0]=5
IP=2: LOAD_NAME 0      stack=[<builtin +>]
IP=3: LOAD_VAR 0 0     stack=[<builtin +>, 5]
IP=4: LOAD_CONST 1     stack=[<builtin +>, 5, 3]
IP=5: CALL_BUILTIN 0 2 stack=[8]
IP=6: RETURN           result=8
```

## Closure Implementation

### Free Variables

Closures capture free variables from their lexical scope:

```lisp
(let ((x 10))
  (lambda (y) (+ x y)))  ; Captures 'x'
```

### Compilation

1. Compiler identifies free variables: `['x']`
2. Generates `LOAD_VAR` to push captured values
3. Generates `MAKE_CLOSURE` with capture count
4. Closure stores captured values in its environment

### Bytecode

```
  0: LOAD_CONST 0        ; Push 10
  1: STORE_VAR 0 0       ; Store to x
  2: LOAD_VAR 0 0        ; Load x (to capture)
  3: MAKE_CLOSURE 0 1    ; Create closure, capture 1 value
  4: RETURN
```

### Runtime

When the closure is called:
- New frame is created
- Captured values are stored in frame locals
- Closure can access both parameters and captured values

## Recursive Functions

### Self-Referential Closures

For recursive functions, the compiler uses `PATCH_CLOSURE_SELF`:

```lisp
(let ((factorial (lambda (n)
                   (if (<= n 1)
                       1
                       (* n (factorial (- n 1)))))))
  (factorial 5))
```

The closure is patched to include itself in its environment, enabling recursion.

### Mutual Recursion

For mutually recursive functions, `PATCH_CLOSURE_SIBLING` is used:

```lisp
(let ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
      (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
  (even? 10))
```

Each closure is patched to reference its sibling.

## Builtin Functions

### Builtin Table

Builtins are indexed in `AIFPLCompiler.BUILTIN_TABLE`:

```python
BUILTIN_TABLE = ['+', '-', '*', '/', '=', '<', '>', ...]
```

### Calling Builtins

Two ways to call builtins:

1. **`CALL_BUILTIN`** - Direct call by index
2. **`CALL_FUNCTION`** - Indirect call (builtin as first-class value)

### Special Forms

Some builtins have special evaluation semantics and are handled specially:
- `and`, `or` - Short-circuit evaluation (handled in compiler)
- `map`, `filter`, `fold` - Higher-order functions (handled in VM)
- `range`, `find`, `any?`, `all?` - Special iteration (handled in VM)

## Performance Characteristics

### Bytecode vs. Evaluator

| Operation | Evaluator | Bytecode VM |
|-----------|-----------|-------------|
| Simple arithmetic | ~1x | ~5-10x faster |
| Function calls | ~1x | ~3-5x faster |
| Recursion | Limited by Python stack | Tail-call optimized |
| Closures | Slower | Faster (pre-compiled) |

### Optimization Techniques

1. **Tail Call Optimization** - Reuses frames for tail calls
2. **Constant Folding** - Done at compile time (future)
3. **Lexical Addressing** - Fast variable lookup by position
4. **Builtin Dispatch** - Direct index-based dispatch

## Debugging

### Disassembly

Use `CodeObject.disassemble()` or `repr(code_object)` to view bytecode:

```python
from aifpl.aifpl_compiler import AIFPLCompiler
from aifpl.aifpl_parser import AIFPLParser

code = "(+ 1 2)"
ast = AIFPLParser(code).parse()
bytecode = AIFPLCompiler().compile(ast)
print(bytecode.disassemble())
```

## See Also

- **`aifpl_bytecode.py`** - Opcode definitions and structures
- **`aifpl_compiler.py`** - Compiler implementation
- **`aifpl_vm.py`** - Virtual machine implementation
- **`README.md`** - Language reference and examples
