The part of the code that triggers the use of the text fixture files is in the main test file (`test_markdown_parser.py`), specifically this parameterized test:

```python
@pytest.mark.parametrize("markdown_path,expected_json_path", find_test_files())
def test_parse_fixture_files(self, markdown_path, expected_json_path):
    """Test parsing markdown files against expected JSON outputs."""
    is_match, diff = parse_and_compare(markdown_path, expected_json_path)
    assert is_match, f"AST mismatch for {os.path.basename(markdown_path)}:\n{diff}"
```

This test works in the following way:

1. The `find_test_files()` function (from `test_utils.py`) scans the fixtures directory for all markdown files (.md) that have corresponding JSON files (.json).

2. `pytest.mark.parametrize` creates a separate test case for each pair of files found.

3. For each test case, `parse_and_compare()` is called, which:
   - Loads the markdown file
   - Parses it with the MarkdownParser
   - Serializes the resulting AST
   - Loads the expected JSON file
   - Compares the actual AST with the expected one

4. If the ASTs don't match, the test fails with a detailed diff showing the differences.

This approach allows you to add new test cases simply by creating new pairs of .md and .json files in the fixtures directory, without modifying any code. The test runner will automatically discover and execute tests for all fixture files.