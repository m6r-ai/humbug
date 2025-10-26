from aifpl import AIFPL, AIFPLParseError

aifpl = AIFPL()

# Test case: missing paren but more code after
expr = "(let ((x 5) (+ x 2))"

print(f"Expression: {expr}")
print(f"Positions:  {''.join(str(i % 10) for i in range(len(expr)))}")
print()

try:
    aifpl.evaluate(expr)
except AIFPLParseError as e:
    print(str(e))
