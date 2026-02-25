"""Test string-index function."""

import pytest
from aifpl.aifpl import AIFPL
from aifpl.aifpl_error import AIFPLEvalError

def test_string_index_basic():
    """Test basic string-index usage."""
    aifpl = AIFPL()
    
    # Found at start
    assert aifpl.evaluate_and_format('(string-index "hello" "h")') == "0"
    
    # Found in middle
    assert aifpl.evaluate_and_format('(string-index "hello" "ll")') == "2"
    
    # Found at end
    assert aifpl.evaluate_and_format('(string-index "hello" "o")') == "4"
    
    # Not found
    assert aifpl.evaluate_and_format('(string-index "hello" "z")') == "#f"
    
    # Empty substring
    assert aifpl.evaluate_and_format('(string-index "hello" "")') == "0"
    
    # Empty string
    assert aifpl.evaluate_and_format('(string-index "" "a")') == "#f"
    assert aifpl.evaluate_and_format('(string-index "" "")') == "0"

def test_string_index_types():
    """Test type checking for string-index."""
    aifpl = AIFPL()
    
    with pytest.raises(AIFPLEvalError) as excinfo:
        aifpl.evaluate_and_format('(string-index 1 "hello")')
    assert "requires string arguments" in str(excinfo.value)
    
    with pytest.raises(AIFPLEvalError) as excinfo:
        aifpl.evaluate_and_format('(string-index "hello" 1)')
    assert "requires string arguments" in str(excinfo.value)

def test_string_index_unicode():
    """Test string-index with unicode characters."""
    aifpl = AIFPL()
    
    assert aifpl.evaluate_and_format('(string-index "hello world 🌍" "🌍")') == "12"
    assert aifpl.evaluate_and_format('(string-index "hello world 🌍" "world")') == "6"
