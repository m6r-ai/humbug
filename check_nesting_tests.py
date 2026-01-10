#!/usr/bin/env python3
"""Check what nesting tests exist."""

import re

with open('tests/dmarkdown/test_markdown_ast_builder.py') as f:
    content = f.read()

# Find all test functions
test_pattern = r'def (test_\w+)\(.*?\):\s*"""(.*?)"""'
tests = re.findall(test_pattern, content, re.DOTALL)

print(f"Total tests found: {len(tests)}\n")

# Categorize tests
categories = {
    'blockquote': [],
    'nested_list': [],
    'code_block': [],
    'table': [],
    'nesting': []
}

for test_name, docstring in tests:
    doc_lower = docstring.lower()
    name_lower = test_name.lower()
    
    if 'blockquote' in name_lower or 'blockquote' in doc_lower:
        categories['blockquote'].append((test_name, docstring.strip()))
    if 'nest' in name_lower or 'nest' in doc_lower:
        categories['nesting'].append((test_name, docstring.strip()))
    if 'code' in name_lower and ('block' in name_lower or 'block' in doc_lower):
        categories['code_block'].append((test_name, docstring.strip()))
    if 'table' in name_lower or 'table' in doc_lower:
        categories['table'].append((test_name, docstring.strip()))

print("=" * 80)
print("BLOCKQUOTE TESTS:")
print("=" * 80)
for name, doc in categories['blockquote']:
    print(f"  {name}: {doc}")

print("\n" + "=" * 80)
print("NESTING TESTS:")
print("=" * 80)
for name, doc in categories['nesting']:
    print(f"  {name}: {doc}")

print("\n" + "=" * 80)
print("CODE BLOCK TESTS:")
print("=" * 80)
for name, doc in categories['code_block']:
    print(f"  {name}: {doc}")

print("\n" + "=" * 80)
print("TABLE TESTS:")
print("=" * 80)
for name, doc in categories['table']:
    print(f"  {name}: {doc}")

# Now check for specific nesting scenarios
print("\n" + "=" * 80)
print("MISSING TEST SCENARIOS:")
print("=" * 80)

missing = []

# Check for code block in list item
if not any('code' in name.lower() and 'list' in name.lower() for name, _ in tests):
    missing.append("Code block nested in list item")

# Check for table in list item  
if not any('table' in name.lower() and 'list' in name.lower() for name, _ in tests):
    missing.append("Table nested in list item")

# Check for blockquote with table
if not any('blockquote' in name.lower() and 'table' in name.lower() for name, _ in tests):
    missing.append("Blockquote with table")

# Check for complex mixed nesting
if not any('blockquote' in name.lower() and 'list' in name.lower() and 'code' in name.lower() for name, _ in tests):
    missing.append("Complex nesting (blockquote + list + code)")

for item in missing:
    print(f"  ⚠️  {item}")

if not missing:
    print("  ✅ All major nesting scenarios appear to be covered!")
