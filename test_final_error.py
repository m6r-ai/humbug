from aifpl import AIFPL, AIFPLParseError

aifpl = AIFPL()

# The test case from our discussion
expr = "(let ((x 5)) (+ x 2"

print("="*70)
print("Expression with missing parens:")
print("="*70)
print(expr)
print()
print("Character positions:")
print(''.join(str(i % 10) for i in range(len(expr))))
print()
print("="*70)
print("Error Message:")
print("="*70)

try:
    aifpl.evaluate(expr)
except AIFPLParseError as e:
    print(str(e))
