# AIFPL Formatting Quick Reference

## The Golden Rule

**Complex forms get separate closing parens. Simple expressions stay compact.**

## Quick Decision Tree

```
Is this a let, lambda, or match?
├─ YES → Closing ) on separate line with comment
└─ NO → Is it a simple expression (arithmetic, function call)?
   ├─ YES → Closing ) can be on same line
   └─ NO → Multiple levels of nesting?
      ├─ YES → Separate closing ) with comments
      └─ NO → Use judgment (prefer clarity)
```

## Rules at a Glance

| Rule | Example | ✅/❌ |
|------|---------|-------|
| 1. Complex forms: separate close | `(let (...) body)` | ✅ |
| 2. Simple expressions: compact OK | `(+ 1 2)` | ✅ |
| 3. Max 2 `)` per line | `(first (rest items))` | ✅ |
| 4. Add comments to closes | `) ; end let` | ✅ |
| 5. Indent to show structure | Proper indentation | ✅ |

## Examples

### ✅ GOOD: Let Expression
```aifpl
(let ((x 5)
      (y 10)
     )
  (+ x y)
) ; end let
```

### ❌ BAD: Cuddled Closes
```aifpl
(let ((x 5)
      (y 10))
  (+ x y))
```

### ✅ GOOD: Lambda
```aifpl
(lambda (x y)
  (* x y)
) ; end lambda
```

### ✅ GOOD: Nested Lets
```aifpl
(let ((outer 100))
  (let ((inner 200))
    (+ outer inner)
  ) ; end inner let
) ; end outer let
```

### ✅ GOOD: Simple Expressions
```aifpl
(+ 1 2 3)
(first (rest items))
(string-trim line)
```

### ✅ GOOD: Match
```aifpl
(match value
  (42 "found")
  (_ "other")
) ; end match
```

## Common Patterns

### Pattern 1: Let with Multiple Bindings
```aifpl
(let ((var1 value1)
      (var2 value2)
      (var3 value3)
     )
  body-expression
) ; end let
```

### Pattern 2: Nested Function Calls
```aifpl
(let ((result (process-data
                (filter predicate items)
              ) ; end process-data
      ) ; end result binding
     )
  result
) ; end let
```

### Pattern 3: If Inside Let
```aifpl
(let ((value (compute-value)))
  (if (valid? value)
      (process value)
      (error "Invalid")
  ) ; end if
) ; end let
```

### Pattern 4: Lambda with Let Body
```aifpl
(lambda (x y)
  (let ((sum (+ x y)))
    (* sum 2)
  ) ; end let
) ; end lambda
```

### Pattern 5: Recursive Function
```aifpl
(let ((factorial (lambda (n acc)
  (if (<= n 1)
      acc
      (factorial (- n 1) (* n acc))
  ) ; end if
) ; end lambda factorial
) ; end factorial binding
) ; end let
  (factorial 5 1)
) ; end let body
```

## Indentation Guide

```aifpl
(form                      ; Opening
  content                  ; Indented 2 spaces
  more-content             ; Same level
) ; end form               ; Aligned with opening or indented
```

## Comment Conventions

| What to Say | Example |
|-------------|---------|
| End of let | `) ; end let` |
| End of lambda | `) ; end lambda` |
| End of binding | `) ; end binding` |
| End of let body | `) ; end let body` |
| End of if | `) ; end if` |
| End of match | `) ; end match` |
| Named let | `) ; end let variable-name` |

## Before You Commit: Checklist

- [ ] No line has more than 2 closing `)`
- [ ] All `let` forms have `)` on separate line
- [ ] All `lambda` forms have `)` on separate line
- [ ] All `match` forms have `)` on separate line
- [ ] Each closing `)` has a comment (for nested code)
- [ ] Indentation shows structure clearly
- [ ] Simple expressions stay compact

## Red Flags 🚩

These indicate formatting problems:

- 🚩 Three or more `)` in a row: `)))`
- 🚩 Five or more `)` in a row: `)))))`
- 🚩 Ten or more `)` in a row: `)))))))))))` ← **VERY BAD**
- 🚩 Closing `)` with no comment in deeply nested code
- 🚩 Inconsistent indentation

## Quick Fixes

### Fix 1: Split the Closes
```aifpl
;; Before
(let ((x 5)) (+ x 10))

;; After
(let ((x 5))
  (+ x 10)
)
```

### Fix 2: Add Comments
```aifpl
;; Before
(let ((x (let ((y 5)) y))) x)

;; After
(let ((x (let ((y 5))
           y
         ) ; end inner let
      ) ; end x binding
     )
  x
) ; end outer let
```

### Fix 3: Proper Indentation
```aifpl
;; Before
(let ((x 5)
(y 10))
(+ x y))

;; After
(let ((x 5)
      (y 10)
     )
  (+ x y)
)
```

## When in Doubt

**Ask yourself**: "Can I count the closing parens without using my finger?"

- ✅ **YES** → Good formatting
- ❌ **NO** → Needs reformatting

## Remember

The goal is **clarity**, not perfection. When in doubt:
1. Separate complex closes
2. Add comments
3. Use proper indentation
4. Keep simple things simple

## Tools (Future)

Planned tools to help:
- Auto-formatter
- Linter (warns about paren clusters)
- Syntax highlighter
- Editor integration

## More Information

- `REFORMATTING_GUIDE.md` - Complete guide
- `BEFORE_AFTER_COMPARISON.md` - Detailed examples
- `REFORMATTING_SUMMARY.md` - Overview and statistics
