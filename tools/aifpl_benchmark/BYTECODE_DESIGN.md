# AIFPL Bytecode Compilation Design (Conceptual)

## Current Architecture (Tree-Walking Interpreter)

```python
# Expression: (+ (* 2 3) (- 10 5))

# Parsed AST:
AIFPLList([
    AIFPLSymbol('+'),
    AIFPLList([AIFPLSymbol('*'), AIFPLNumber(2), AIFPLNumber(3)]),
    AIFPLList([AIFPLSymbol('-'), AIFPLNumber(10), AIFPLNumber(5)])
])

# Evaluation (recursive tree walking):
def _evaluate_expression(expr, env, depth):
    tag = expr.type_tag()
    if tag == TYPE_LIST:
        func = _evaluate_expression(expr.first(), env, depth+1)  # Recurse
        args = [_evaluate_expression(arg, env, depth+1) for arg in expr.rest()]  # Recurse
        return _call_function(func, args)
    elif tag == TYPE_SYMBOL:
        return env.lookup(expr.name)
    else:
        return expr

# Problems:
# - Recursive function calls for every node
# - Type checking on every node
# - Environment lookups repeated
# - Function dispatch overhead
```

## Proposed Architecture (Bytecode VM)

### Step 1: Compile to Bytecode

```python
# Expression: (+ (* 2 3) (- 10 5))

# Compiled bytecode:
[
    (LOAD_CONST, 2),        # Stack: [2]
    (LOAD_CONST, 3),        # Stack: [2, 3]
    (CALL_BUILTIN, '*', 2), # Stack: [6]
    (LOAD_CONST, 10),       # Stack: [6, 10]
    (LOAD_CONST, 5),        # Stack: [6, 10, 5]
    (CALL_BUILTIN, '-', 2), # Stack: [6, 5]
    (CALL_BUILTIN, '+', 2), # Stack: [11]
    (RETURN,)               # Return 11
]
```

### Step 2: Execute Bytecode (Stack-Based VM)

```python
def execute(bytecode, env):
    stack = []
    ip = 0  # Instruction pointer
    
    while ip < len(bytecode):
        opcode, *args = bytecode[ip]
        
        if opcode == LOAD_CONST:
            stack.append(AIFPLNumber(args[0]))
        
        elif opcode == LOAD_VAR:
            var_name = args[0]
            stack.append(env.lookup(var_name))
        
        elif opcode == CALL_BUILTIN:
            func_name, arity = args
            func_args = [stack.pop() for _ in range(arity)]
            result = call_builtin(func_name, reversed(func_args))
            stack.append(result)
        
        elif opcode == RETURN:
            return stack.pop()
        
        ip += 1
```

## Bytecode Instruction Set

### Basic Operations
```python
# Constants and Variables
LOAD_CONST   value          # Push constant onto stack
LOAD_VAR     name           # Push variable value onto stack
STORE_VAR    name           # Pop value and store in variable

# Arithmetic (optimized - no function call overhead)
ADD          n              # Pop n values, add them, push result
SUB          n              # Pop n values, subtract, push result
MUL          n              # Pop n values, multiply, push result
DIV          n              # Pop n values, divide, push result

# Comparisons
EQ           n              # Pop n values, test equality, push boolean
LT           n              # Pop n values, test <, push boolean
GT           n              # Pop n values, test >, push boolean

# Control Flow
JUMP         offset         # Unconditional jump
JUMP_IF_FALSE offset        # Jump if top of stack is false
JUMP_IF_TRUE offset         # Jump if top of stack is true

# Functions
MAKE_CLOSURE params body    # Create closure from params and bytecode
CALL_FUNCTION arity         # Pop function and args, call it
CALL_BUILTIN name arity     # Call builtin function directly
TAIL_CALL    arity          # Tail call optimization

# Lists
MAKE_LIST    n              # Pop n values, create list
LIST_FIRST                  # Pop list, push first element
LIST_REST                   # Pop list, push rest

# Alists
MAKE_ALIST   n              # Pop n*2 values (key-value pairs), create alist
ALIST_GET    key            # Pop alist, push value for key

# Stack Manipulation
DUP                         # Duplicate top of stack
POP                         # Discard top of stack
SWAP                        # Swap top two stack values

# Misc
RETURN                      # Return top of stack
```

## Compilation Examples

### Example 1: Simple Arithmetic
```scheme
(+ (* 2 3) (- 10 5))
```

Bytecode:
```python
[
    (LOAD_CONST, 2),
    (LOAD_CONST, 3),
    (MUL, 2),              # Optimized: direct multiply
    (LOAD_CONST, 10),
    (LOAD_CONST, 5),
    (SUB, 2),              # Optimized: direct subtract
    (ADD, 2),              # Optimized: direct add
    (RETURN,)
]
```

### Example 2: Variable Reference
```scheme
(let ((x 5) (y 10))
  (+ x y))
```

Bytecode:
```python
[
    # Bind x = 5
    (LOAD_CONST, 5),
    (STORE_VAR, 'x'),
    
    # Bind y = 10
    (LOAD_CONST, 10),
    (STORE_VAR, 'y'),
    
    # Evaluate (+ x y)
    (LOAD_VAR, 'x'),
    (LOAD_VAR, 'y'),
    (ADD, 2),
    (RETURN,)
]
```

### Example 3: Conditional (if)
```scheme
(if (> x 0)
    (* x 2)
    (- x))
```

Bytecode:
```python
[
    # Evaluate condition (> x 0)
    (LOAD_VAR, 'x'),
    (LOAD_CONST, 0),
    (GT, 2),
    
    # Jump to else if false
    (JUMP_IF_FALSE, 7),     # Jump to instruction 7
    
    # Then branch: (* x 2)
    (LOAD_VAR, 'x'),
    (LOAD_CONST, 2),
    (MUL, 2),
    (JUMP, 10),             # Jump past else
    
    # Else branch: (- x)
    (LOAD_CONST, 0),        # Instruction 7
    (LOAD_VAR, 'x'),
    (SUB, 2),
    
    (RETURN,)               # Instruction 10
]
```

### Example 4: Lambda and Function Call
```scheme
((lambda (x) (* x x)) 5)
```

Bytecode:
```python
[
    # Create closure
    (MAKE_CLOSURE, 
        params=['x'],
        body=[
            (LOAD_VAR, 'x'),
            (LOAD_VAR, 'x'),
            (MUL, 2),
            (RETURN,)
        ]
    ),
    
    # Push argument
    (LOAD_CONST, 5),
    
    # Call function
    (CALL_FUNCTION, 1),
    (RETURN,)
]
```

### Example 5: Map (Higher-Order Function)
```scheme
(map (lambda (x) (* x 2)) (list 1 2 3))
```

Bytecode:
```python
[
    # Create lambda
    (MAKE_CLOSURE,
        params=['x'],
        body=[
            (LOAD_VAR, 'x'),
            (LOAD_CONST, 2),
            (MUL, 2),
            (RETURN,)
        ]
    ),
    
    # Create list
    (LOAD_CONST, 1),
    (LOAD_CONST, 2),
    (LOAD_CONST, 3),
    (MAKE_LIST, 3),
    
    # Call map
    (CALL_BUILTIN, 'map', 2),
    (RETURN,)
]
```

### Example 6: Tail-Recursive Function
```scheme
(let ((sum (lambda (n acc)
             (if (<= n 0)
                 acc
                 (sum (- n 1) (+ acc n))))))
  (sum 100 0))
```

Bytecode for the lambda:
```python
[
    # Condition (<= n 0)
    (LOAD_VAR, 'n'),
    (LOAD_CONST, 0),
    (LE, 2),
    (JUMP_IF_FALSE, 5),
    
    # Then: return acc
    (LOAD_VAR, 'acc'),
    (RETURN,),
    
    # Else: tail call (sum (- n 1) (+ acc n))
    (LOAD_VAR, 'sum'),      # Instruction 5
    (LOAD_VAR, 'n'),
    (LOAD_CONST, 1),
    (SUB, 2),
    (LOAD_VAR, 'acc'),
    (LOAD_VAR, 'n'),
    (ADD, 2),
    (TAIL_CALL, 2),         # Optimized: reuse stack frame
]
```

## Implementation Structure

```
src/aifpl/
├── aifpl_compiler.py          # NEW: Compile AST to bytecode
│   ├── AIFPLCompiler
│   │   ├── compile(ast) -> bytecode
│   │   ├── _compile_expression(expr, env_info)
│   │   ├── _compile_function_call(func, args)
│   │   ├── _compile_if(condition, then, else)
│   │   ├── _compile_let(bindings, body)
│   │   └── _compile_lambda(params, body)
│   └── optimize_bytecode(bytecode) -> bytecode
│
├── aifpl_bytecode.py          # NEW: Bytecode definitions
│   ├── Opcode (enum)
│   ├── Instruction (dataclass)
│   └── BytecodeProgram (dataclass)
│
├── aifpl_vm.py                # NEW: Virtual machine
│   ├── AIFPLVM
│   │   ├── execute(bytecode, env)
│   │   ├── _execute_instruction(instr)
│   │   └── _call_function(func, args)
│   └── Stack management
│
└── aifpl.py                   # MODIFIED: Add compilation mode
    └── evaluate(expr, compile=True)
```

## Key Optimizations in Bytecode

### 1. **Constant Folding**
```python
# Source: (+ 2 3)
# Naive: LOAD_CONST 2, LOAD_CONST 3, ADD
# Optimized: LOAD_CONST 5
```

### 2. **Inline Built-in Operations**
```python
# Source: (+ x y)
# Interpreter: Lookup '+', check type, call function
# Bytecode: LOAD_VAR x, LOAD_VAR y, ADD (direct operation)
```

### 3. **Tail Call Optimization (Built-in)**
```python
# TAIL_CALL instruction reuses stack frame
# No stack growth for tail recursion
```

### 4. **Environment Optimization**
```python
# Compile-time: Analyze variable scopes
# Runtime: Use array indexing instead of dict lookup

# Instead of: env.lookup('x')
# Use: env.get_at_depth(2, 0)  # 2 levels up, index 0
```

### 5. **Specialized Instructions**
```python
# Instead of generic CALL_BUILTIN for everything
# Use specialized: ADD, SUB, MUL, DIV, etc.
# Avoids function call overhead
```

## Performance Expectations

### Current (Optimized Interpreter)
```
Map (100 elements):     0.416ms
Filter (100 elements):  0.416ms
Fold (100 elements):    0.129ms
```

### With Bytecode Compilation
```
Map (100 elements):     ~0.050ms (8x faster)
Filter (100 elements):  ~0.050ms (8x faster)
Fold (100 elements):    ~0.020ms (6x faster)
```

**Why the improvement:**
- No tree walking (flat bytecode array)
- No type checking per node (pre-compiled)
- Direct operations (ADD vs function call to '+')
- Optimized variable access
- Better CPU cache usage (linear bytecode vs tree traversal)

## Implementation Effort

### Phase 1: Basic Compiler (1 week)
- Compile arithmetic, variables, if
- Basic VM with stack
- No optimizations yet

### Phase 2: Functions (1 week)
- Lambda compilation
- Function calls
- Closures

### Phase 3: Higher-Order Functions (3-4 days)
- Map, filter, fold
- List operations

### Phase 4: Optimizations (1 week)
- Constant folding
- Tail call optimization
- Specialized instructions

### Phase 5: Testing & Integration (3-4 days)
- Comprehensive tests
- Benchmark comparison
- Fallback to interpreter for edge cases

**Total: 3-4 weeks for full implementation**

## Compatibility Strategy

```python
class AIFPL:
    def __init__(self, use_bytecode=True, fallback_to_interpreter=True):
        self.use_bytecode = use_bytecode
        self.fallback = fallback_to_interpreter
        self.compiler = AIFPLCompiler()
        self.vm = AIFPLVM()
        self.interpreter = AIFPLEvaluator()
    
    def evaluate(self, expression: str):
        parsed = self.parse(expression)
        
        if self.use_bytecode:
            try:
                bytecode = self.compiler.compile(parsed)
                return self.vm.execute(bytecode)
            except CompilationError as e:
                if self.fallback:
                    # Fall back to interpreter for unsupported features
                    return self.interpreter.evaluate(parsed)
                raise
        else:
            return self.interpreter.evaluate(parsed)
```

## Alternative: Hybrid Approach

**Compile hot paths only:**

```python
class AIFPL:
    def __init__(self):
        self.execution_counts = {}
        self.compiled_cache = {}
        self.hot_threshold = 10
    
    def evaluate(self, expression: str):
        parsed = self.parse(expression)
        expr_hash = hash(str(parsed))
        
        # Track execution count
        self.execution_counts[expr_hash] = self.execution_counts.get(expr_hash, 0) + 1
        
        # Compile if hot
        if self.execution_counts[expr_hash] >= self.hot_threshold:
            if expr_hash not in self.compiled_cache:
                self.compiled_cache[expr_hash] = self.compiler.compile(parsed)
            return self.vm.execute(self.compiled_cache[expr_hash])
        
        # Otherwise interpret
        return self.interpreter.evaluate(parsed)
```

This gives you:
- No compilation overhead for one-shot evaluations (AI tool use case)
- Automatic optimization for repeated evaluations (benchmark/production use case)

## Conclusion

Bytecode compilation is a significant undertaking but provides:
- **5-10x speedup** across the board
- **Better than persistent data structures** for most workloads
- **Enables future JIT compilation** if needed

The hybrid approach might be ideal:
- Keeps fast interpreter for AI tool use (one-shot evaluations)
- Automatically optimizes hot paths for production workloads
- Best of both worlds

Would you want to pursue this in the future, or are the current optimizations sufficient?
