# AIFPL Reformatting Guide: One Closing Paren Per Line

## Problem

LLMs (and humans!) struggle to count closing parentheses when they're clustered together:

```aifpl
;; BAD: 11 closing parens in a row - very error-prone!
(let ((apply-all (lambda (lines pos remaining)
  (if (null? remaining)
      (list "success" lines)
      (let ((result (apply-change lines pos (first remaining))))
        (let ((status (first result)))
          (if (= status "ok")
              (let ((new-lines (first (rest result)))
                    (new-pos (first (rest (rest result)))))
                (apply-all new-lines new-pos (rest remaining)))
              (list "error" (first (rest result)) (first (rest (rest result)))))))))))
  (apply-all file-lines start-pos changes))))
                                    ^^^^^^^^^^^ - How many is that? Hard to count!
```

## Solution: One Closing Paren Per Line

### Formatting Rules

1. **Complex forms** (`let`, `lambda`, `match`, `if` with nested structure): Put closing `)` on its own line
2. **Simple expressions** (arithmetic, function calls): Closing `)` can be on same line
3. **Maximum**: No more than 2 closing `)` on the same line
4. **Comments**: Add `;` comments after `)` to indicate what's being closed
5. **Indentation**: Align `)` with the construct it closes (or slightly indented)

### Example: Before and After

#### BEFORE (Hard to Count)
```aifpl
(let ((apply-all (lambda (lines pos remaining)
  (if (null? remaining)
      (list "success" lines)
      (let ((result (apply-change lines pos (first remaining))))
        (let ((status (first result)))
          (if (= status "ok")
              (let ((new-lines (first (rest result)))
                    (new-pos (first (rest (rest result)))))
                (apply-all new-lines new-pos (rest remaining)))
              (list "error" (first (rest result)) (first (rest (rest result)))))))))))
  (apply-all file-lines start-pos changes))))
```

**Problem**: 11 closing parens in a row at the end!

#### AFTER (Easy to Count)
```aifpl
(let ((apply-all (lambda (lines pos remaining)
  (if (null? remaining)
      (list "success" lines)
      (let ((result (apply-change lines pos (first remaining))))
        (let ((status (first result)))
          (if (= status "ok")
              (let ((new-lines (first (rest result)))
                    (new-pos (first (rest (rest result))))
                   )
                (apply-all new-lines new-pos (rest remaining))
              ) ; end let new-lines/new-pos
              (list "error" (first (rest result)) (first (rest (rest result))))
          ) ; end if = ok
        ) ; end let status
      ) ; end let result
  ) ; end if null?
) ; end lambda apply-all
) ; end apply-all binding
) ; end let apply-all
  (apply-all file-lines start-pos changes)
) ; end let body
```

**Benefits**:
- ✅ Each `)` on its own line (except simple expressions)
- ✅ Comments show what each `)` closes
- ✅ Easy to count: 7 lines with single `)`, 0 lines with multiple
- ✅ Clear structure through indentation
- ✅ Trivial to verify correctness

## Detailed Examples

### Simple Expressions (Can Cuddle)

```aifpl
;; OK: Simple arithmetic
(+ 1 2 3)

;; OK: Simple function call
(string-trim line)

;; OK: Two closes for nested simple expressions
(first (rest items))
```

### Complex Expressions (Separate)

```aifpl
;; GOOD: Lambda with closing paren separated
(lambda (x y)
  (+ x y)
) ; end lambda

;; GOOD: Let with proper separation
(let ((x 5)
      (y 10)
     )
  (+ x y)
) ; end let

;; GOOD: Nested lets with clear structure
(let ((outer 100))
  (let ((inner 200))
    (+ outer inner)
  ) ; end inner let
) ; end outer let
```

### Binding Lists

For `let` binding lists, close the individual bindings on same line, but separate the list close:

```aifpl
;; GOOD: Bindings closed on same line, list closed separately
(let ((x 5)
      (y 10)
      (z 15)
     )
  (+ x y z)
)

;; ALSO GOOD: Multi-line bindings
(let ((x (+ 1 2 3))
      (y (let ((a 5))
           (* a 2)
         ) ; end inner let
      )
     )
  (+ x y)
)
```

### Match Expressions

```aifpl
;; GOOD: Match with separated closes
(match value
  (42 "found")
  (_ "other")
) ; end match
```

## Benefits for LLMs

### Before Reformatting
- LLM sees: `)))))))))))` 
- LLM thinks: "Is that 10 or 11 parens? Let me count... 1, 2, 3... wait, start over..."
- Result: ❌ Frequently miscounts

### After Reformatting
- LLM sees:
  ```
  ) ; end let new-lines
  ) ; end if status
  ) ; end let result
  ) ; end if null
  ) ; end lambda
  ) ; end binding
  ) ; end let
  ```
- LLM thinks: "7 lines with closing parens, each clearly labeled"
- Result: ✅ Easy to verify

## Migration Strategy

### Step 1: Reformat Existing Code

Use the reformatted version as template:
- `patch_library_reformatted.aifpl` - fully reformatted version

### Step 2: Adopt in New Code

When writing new AIFPL code:
1. Start with proper indentation
2. Put complex form closes on separate lines
3. Add comments to closing parens
4. Keep simple expressions compact

### Step 3: Update Documentation

Add formatting guidelines to:
- AIFPL README
- Code examples
- LLM prompts

## Formatting Checklist

When writing or reviewing AIFPL code:

- [ ] No more than 2 closing `)` on any line
- [ ] Complex forms (`let`, `lambda`, `match`) have `)` on separate line
- [ ] Each closing `)` has a comment (for nested code)
- [ ] Indentation clearly shows structure
- [ ] Binding lists close separately from their containing form
- [ ] Simple expressions can stay compact

## Tools

### Future Enhancements

Consider building:
1. **Auto-formatter**: Reformats AIFPL code automatically
2. **Linter**: Warns about paren clustering
3. **Syntax highlighter**: Shows matching parens clearly
4. **Editor integration**: Auto-formats on save

## Summary

The "one closing paren per line" rule:
- ✅ **Solves the LLM counting problem**
- ✅ **Minimal syntax change** (just formatting)
- ✅ **Backward compatible** (old code still valid)
- ✅ **Human-readable** (easier for everyone)
- ✅ **Easy to adopt** (clear rules)
- ✅ **Tool-friendly** (can auto-format)

This is the **simplest solution** that actually fixes the problem.
