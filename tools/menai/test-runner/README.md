# Menai Test Runner

A test runner for Menai modules. Test files use the `_test.menai` suffix and
export a structured tree of named test thunks. The runner discovers, executes,
and reports them with full isolation between tests.

## Running tests

```bash
# Run all tests under a directory (recursive)
python tools/menai/test-runner/test-run.py menai_modules/

# Run a single test file
python tools/menai/test-runner/test-run.py menai_modules/json_parser_test.menai

# Show passing tests as well as failures
python tools/menai/test-runner/test-run.py menai_modules/ --verbose

# Filter by name (case-insensitive substring match on full path)
python tools/menai/test-runner/test-run.py menai_modules/ --filter "parse-string"
```

The runner exits with code 0 if all tests pass, non-zero if any fail.

## Writing tests

A test file is a `.menai` module that imports `menai_test` and exports a dict
with a `"tests"` key containing a node-list:

```menai
(let ((mymod  (import "my_module"))
      (t      (import "menai_test")))
  (let ((my-fn        (dict-get mymod "my-fn"))
        (assert-equal (dict-get t     "assert-equal")))
    (dict
      (list "tests" (list

        (list "group name" (list
          (list "test name" (lambda () (assert-equal (my-fn 1) 2)))
          (list "another"   (lambda () (assert-equal (my-fn 0) 0)))
        ))

      )))))
```

### Node structure

The `"tests"` value is a **node-list** — a list of nodes, where each node is a
two-element list `(name thing)`:

- **Leaf**: `thing` is a zero-argument lambda (the test thunk)
- **Branch**: `thing` is another node-list (a named group)

Nesting is arbitrary. Branch names appear in the output path separated by ` > `.

### Isolation

Each leaf thunk runs in a **fresh VM invocation**. A runtime error (type error,
assertion failure, etc.) in one test does not affect any other. This is the
mechanism that makes `assert-equal` safe to use — the VM terminates on the
first failure and the runner catches it at the Python level.

### assert-equal

`assert-equal` compares two values for structural equality across all Menai
types (boolean, integer, float, complex, string, symbol, none, list, dict,
set). On mismatch it raises with a message showing the expected and actual
values:

```text
assert-equal failed
  expected: 42
  actual:   43
```

On a runtime error (type error, etc.) the error message from the VM is shown
directly — no special handling is needed.

## Test support module

The runner prepends its own directory to the Menai module path, making
`(import "menai_test")` available to all test files. This module is **only**
available when running under the test runner — it is not part of the standard
module library.

Exports:

| Name            | Description                                              |
|-----------------|----------------------------------------------------------|
| `assert-equal`  | Raises if two values are not structurally equal          |
| `test-find`     | Internal — used by the runner to locate leaf thunks      |

## Output format

```
menai_modules/json_parser_test.menai
  ✓  objects > empty object
  ✓  objects > single key
  ✗  strings > escapes > unicode
       Error: assert-equal failed
         expected: "A"
         actual:   "a"
  3/4 FAILED

============================================================
Total: 3/4 passed, 1 FAILED
```
