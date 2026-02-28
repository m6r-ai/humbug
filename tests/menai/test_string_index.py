"""Test string-index function."""

import pytest
from menai.menai import Menai
from menai.menai_error import MenaiEvalError

def test_string_index_basic():
    """Test basic string-index usage."""
    menai = Menai()
    
    # Found at start
    assert menai.evaluate_and_format('(string-index "hello" "h")') == "0"
    
    # Found in middle
    assert menai.evaluate_and_format('(string-index "hello" "ll")') == "2"
    
    # Found at end
    assert menai.evaluate_and_format('(string-index "hello" "o")') == "4"
    
    # Not found
    assert menai.evaluate_and_format('(string-index "hello" "z")') == "#none"
    
    # Empty substring
    assert menai.evaluate_and_format('(string-index "hello" "")') == "0"
    
    # Empty string
    assert menai.evaluate_and_format('(string-index "" "a")') == "#none"
    assert menai.evaluate_and_format('(string-index "" "")') == "0"

def test_string_index_types():
    """Test type checking for string-index."""
    menai = Menai()
    
    with pytest.raises(MenaiEvalError) as excinfo:
        menai.evaluate_and_format('(string-index 1 "hello")')
    assert "requires string arguments" in str(excinfo.value)
    
    with pytest.raises(MenaiEvalError) as excinfo:
        menai.evaluate_and_format('(string-index "hello" 1)')
    assert "requires string arguments" in str(excinfo.value)

def test_string_index_unicode():
    """Test string-index with unicode characters."""
    menai = Menai()
    
    assert menai.evaluate_and_format('(string-index "hello world 🌍" "🌍")') == "12"
    assert menai.evaluate_and_format('(string-index "hello world 🌍" "world")') == "6"
