"""Enhanced error classes with detailed context for AIFPL."""

from dataclasses import dataclass
from typing import Optional
import difflib

from aifpl.aifpl_error import AIFPLError, AIFPLTokenError, AIFPLParseError, AIFPLEvalError


@dataclass
class ErrorContext:
    """Context information for detailed errors."""
    line: Optional[int] = None
    column: Optional[int] = None
    expression: Optional[str] = None
    position: Optional[int] = None


@dataclass 
class AIFPLDetailedError(AIFPLError):
    """Enhanced error with structured context information."""
    message: str
    context: Optional[str] = None
    expected: Optional[str] = None  
    received: Optional[str] = None
    example: Optional[str] = None
    suggestion: Optional[str] = None
    error_context: Optional[ErrorContext] = None
    
    def __str__(self) -> str:
        """Format detailed error message."""
        parts = [f"Error: {self.message}"]
        
        # Add position information if available
        if self.error_context and self.error_context.line is not None:
            parts.append(f"Line {self.error_context.line}, Column {self.error_context.column}")
        elif self.error_context and self.error_context.position is not None:
            parts.append(f"Position {self.error_context.position}")
            
        # Add received/expected information
        if self.received:
            parts.append(f"Received: {self.received}")
        if self.expected:
            parts.append(f"Expected: {self.expected}")
            
        # Add context
        if self.context:
            parts.append(f"Context: {self.context}")
            
        # Add suggestion
        if self.suggestion:
            parts.append(f"Suggestion: {self.suggestion}")
            
        # Add example
        if self.example:
            parts.append(f"Example: {self.example}")
            
        return "\n".join(parts)


class AIFPLDetailedTokenError(AIFPLDetailedError, AIFPLTokenError):
    """Detailed tokenization error."""
    pass


class AIFPLDetailedParseError(AIFPLDetailedError, AIFPLParseError):
    """Detailed parsing error."""
    pass


class AIFPLDetailedEvalError(AIFPLDetailedError, AIFPLEvalError):
    """Detailed evaluation error."""
    pass


class ErrorMessageBuilder:
    """Helper class for building detailed error messages."""
    
    @staticmethod
    def position_to_line_column(expression: str, position: int) -> tuple[int, int]:
        """Convert character position to line/column numbers."""
        if position < 0 or position > len(expression):
            return 1, 1
            
        lines = expression[:position].split('\n')
        line = len(lines)
        column = len(lines[-1]) + 1 if lines else 1
        return line, column
    
    @staticmethod
    def get_expression_context(expression: str, position: int, context_size: int = 20) -> str:
        """Get context around error position."""
        start = max(0, position - context_size)
        end = min(len(expression), position + context_size)
        
        before = expression[start:position]
        at_pos = expression[position] if position < len(expression) else ""
        after = expression[position+1:end]
        
        # Add markers to highlight the error position
        return f"...{before}→{at_pos}←{after}..."
    
    @staticmethod
    def suggest_similar_functions(target: str, available_functions: list[str], max_suggestions: int = 3) -> list[str]:
        """Suggest similar function names using fuzzy matching."""
        if not target or not available_functions:
            return []
            
        # Use difflib for similarity matching
        matches = difflib.get_close_matches(target, available_functions, n=max_suggestions, cutoff=0.6)
        return matches
    
    @staticmethod
    def create_function_example(func_name: str) -> str:
        """Create usage example for common functions."""
        examples = {
            # Arithmetic
            '+': "(+ 1 2 3) → 6",
            '-': "(- 10 3) → 7",
            '*': "(* 2 3 4) → 24", 
            '/': "(/ 12 3) → 4",
            '//': "(// 7 3) → 2",
            '%': "(% 7 3) → 1",
            '**': "(** 2 3) → 8",
            
            # Comparison
            '=': "(= 1 1 1) → #t",
            '!=': "(!= 1 2) → #t", 
            '<': "(< 1 2 3) → #t",
            '>': "(> 3 2 1) → #t",
            '<=': "(<= 1 1 2) → #t",
            '>=': "(>= 3 2 2) → #t",
            
            # Boolean
            'and': "(and #t #t #f) → #f",
            'or': "(or #f #t) → #t",
            'not': "(not #t) → #f",
            'if': "(if (> 5 3) \"yes\" \"no\") → \"yes\"",
            
            # Lists
            'list': "(list 1 2 3) → (1 2 3)",
            'cons': "(cons 1 (list 2 3)) → (1 2 3)",
            'first': "(first (list 1 2 3)) → 1",
            'rest': "(rest (list 1 2 3)) → (2 3)",
            'length': "(length (list 1 2 3)) → 3",
            'append': "(append (list 1 2) (list 3 4)) → (1 2 3 4)",
            
            # Strings  
            'string-append': "(string-append \"hello\" \" \" \"world\") → \"hello world\"",
            'string-length': "(string-length \"hello\") → 5",
            'substring': "(substring \"hello\" 1 4) → \"ell\"",
            
            # Higher-order
            'map': "(map (lambda (x) (* x 2)) (list 1 2 3)) → (2 4 6)",
            'filter': "(filter (lambda (x) (> x 0)) (list -1 2 -3 4)) → (2 4)",
            'fold': "(fold + 0 (list 1 2 3 4)) → 10",
            
            # Math
            'sin': "(sin (* pi 0.5)) → 1.0",
            'sqrt': "(sqrt 16) → 4.0",
            'abs': "(abs -5) → 5",
            'round': "(round 3.7) → 4",
            
            # Let and Lambda
            'let': "(let ((x 5) (y 10)) (+ x y)) → 15",
            'lambda': "((lambda (x) (* x x)) 5) → 25"
        }
        
        return examples.get(func_name, f"({func_name} ...)")
    
    @staticmethod
    def get_common_mistakes_suggestion(error_type: str, context: str = "") -> Optional[str]:
        """Get suggestions for common mistakes."""
        suggestions = {
            "let_binding": "Remember: let bindings need double parentheses: (let ((var val)) body)",
            "lambda_params": "Lambda parameters should be symbols: (lambda (x y) body)",
            "quote_syntax": "Use quote or ': (quote expr) or 'expr",
            "function_call": "Function calls: (function-name arg1 arg2 ...)",
            "boolean_condition": "Conditions must be boolean: use =, <, >, and, or, not",
            "list_access": "Lists are 0-indexed: (list-ref my-list 0) gets first element",
            "string_index": "Strings are 0-indexed: (string-ref \"hello\" 0) gets 'h'"
        }
        
        return suggestions.get(error_type)