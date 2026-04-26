"""Tests for Scheme keyword (special form) tokenization."""

from syntax.scheme.scheme_lexer import SchemeLexer
from syntax.lexer import TokenType


ALL_SPECIAL_FORMS = [
    'let-syntax', 'letrec-syntax', 'syntax-rules', 'define-syntax',
    'else', '=>', 'define', 'unquote-splicing', 'unquote',
    'quote', 'lambda', 'if', 'set!', 'begin', 'cond', 'and', 'or', 'case',
    'let', 'let*', 'letrec', 'do', 'delay', 'quasiquote',
]


class TestSchemeKeywords:
    """Tests for Scheme keyword (special form) tokenization."""

    def _lex(self, source: str) -> list:
        lexer = SchemeLexer()
        lexer.lex(None, source)
        return list(lexer._tokens)

    def test_keyword_define(self):
        """'define' is recognized as a KEYWORD."""
        tokens = self._lex("define")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == "define"

    def test_keyword_lambda(self):
        """'lambda' is recognized as a KEYWORD."""
        tokens = self._lex("lambda")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == "lambda"

    def test_keyword_if(self):
        """'if' is recognized as a KEYWORD."""
        tokens = self._lex("if")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == "if"

    def test_keyword_set_bang(self):
        """'set!' is recognized as a KEYWORD."""
        tokens = self._lex("set!")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == "set!"

    def test_keyword_begin(self):
        """'begin' is recognized as a KEYWORD."""
        tokens = self._lex("begin")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == "begin"

    def test_keyword_cond(self):
        """'cond' is recognized as a KEYWORD."""
        tokens = self._lex("cond")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == "cond"

    def test_keyword_and(self):
        """'and' is recognized as a KEYWORD."""
        tokens = self._lex("and")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == "and"

    def test_keyword_or(self):
        """'or' is recognized as a KEYWORD."""
        tokens = self._lex("or")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == "or"

    def test_keyword_case(self):
        """'case' is recognized as a KEYWORD."""
        tokens = self._lex("case")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == "case"

    def test_keyword_let(self):
        """'let' is recognized as a KEYWORD."""
        tokens = self._lex("let")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == "let"

    def test_keyword_let_star(self):
        """'let*' is recognized as a KEYWORD."""
        tokens = self._lex("let*")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == "let*"

    def test_keyword_letrec(self):
        """'letrec' is recognized as a KEYWORD."""
        tokens = self._lex("letrec")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == "letrec"

    def test_keyword_do(self):
        """'do' is recognized as a KEYWORD."""
        tokens = self._lex("do")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == "do"

    def test_keyword_delay(self):
        """'delay' is recognized as a KEYWORD."""
        tokens = self._lex("delay")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == "delay"

    def test_keyword_quasiquote(self):
        """'quasiquote' is recognized as a KEYWORD."""
        tokens = self._lex("quasiquote")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == "quasiquote"

    def test_keyword_quote(self):
        """'quote' is recognized as a KEYWORD."""
        tokens = self._lex("quote")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == "quote"

    def test_keyword_unquote(self):
        """'unquote' is recognized as a KEYWORD."""
        tokens = self._lex("unquote")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == "unquote"

    def test_keyword_unquote_splicing(self):
        """'unquote-splicing' is recognized as a KEYWORD."""
        tokens = self._lex("unquote-splicing")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == "unquote-splicing"

    def test_keyword_else(self):
        """'else' is recognized as a KEYWORD."""
        tokens = self._lex("else")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == "else"

    def test_keyword_arrow(self):
        """'=>' is recognized as a KEYWORD."""
        tokens = self._lex("=>")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == "=>"

    def test_keyword_define_syntax(self):
        """'define-syntax' is recognized as a KEYWORD."""
        tokens = self._lex("define-syntax")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == "define-syntax"

    def test_keyword_let_syntax(self):
        """'let-syntax' is recognized as a KEYWORD."""
        tokens = self._lex("let-syntax")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == "let-syntax"

    def test_keyword_letrec_syntax(self):
        """'letrec-syntax' is recognized as a KEYWORD."""
        tokens = self._lex("letrec-syntax")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == "letrec-syntax"

    def test_keyword_syntax_rules(self):
        """'syntax-rules' is recognized as a KEYWORD."""
        tokens = self._lex("syntax-rules")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == "syntax-rules"

    def test_all_special_forms_recognized(self):
        """All R5RS special forms are recognized as KEYWORD tokens."""
        for form in ALL_SPECIAL_FORMS:
            tokens = self._lex(form)
            assert len(tokens) == 1, f"Expected 1 token for '{form}', got {len(tokens)}"
            assert tokens[0].type == TokenType.KEYWORD, f"Expected KEYWORD for '{form}', got {tokens[0].type}"

    def test_keywords_case_insensitive_upper(self):
        """Keywords are matched case-insensitively; 'DEFINE' is a KEYWORD."""
        tokens = self._lex("DEFINE")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == "DEFINE", "Original case must be preserved"

    def test_keywords_case_insensitive_mixed(self):
        """Keywords are matched case-insensitively; 'Lambda' is a KEYWORD."""
        tokens = self._lex("Lambda")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == "Lambda"

    def test_keywords_case_insensitive_if(self):
        """'IF' is recognized as a KEYWORD."""
        tokens = self._lex("IF")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD

    def test_keywords_case_insensitive_let_star(self):
        """'LET*' is recognized as a KEYWORD."""
        tokens = self._lex("LET*")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.KEYWORD

    def test_keyword_in_expression(self):
        """A keyword inside a list expression is correctly identified."""
        tokens = self._lex("(define x 42)")
        kw_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        assert len(kw_tokens) == 1
        assert kw_tokens[0].value == "define"

    def test_keyword_start_position(self):
        """The start position of a keyword is correctly recorded."""
        tokens = self._lex("  lambda")
        kw_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        assert len(kw_tokens) == 1
        assert kw_tokens[0].start == 2, f"Expected start=2, got {kw_tokens[0].start}"

    def test_non_keyword_define_prefix(self):
        """'defined' is not a keyword — it is an IDENTIFIER."""
        tokens = self._lex("defined")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.IDENTIFIER

    def test_non_keyword_let_prefix(self):
        """'letter' is not a keyword — it is an IDENTIFIER."""
        tokens = self._lex("letter")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.IDENTIFIER

    def test_non_keyword_if_prefix(self):
        """'iffy' is not a keyword — it is an IDENTIFIER."""
        tokens = self._lex("iffy")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.IDENTIFIER

    def test_non_keyword_and_prefix(self):
        """'android' is not a keyword — it is an IDENTIFIER."""
        tokens = self._lex("android")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.IDENTIFIER

    def test_non_keyword_or_prefix(self):
        """'order' is not a keyword — it is an IDENTIFIER."""
        tokens = self._lex("order")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.IDENTIFIER

    def test_multiple_keywords_in_expression(self):
        """Multiple keywords in a nested expression are all correctly identified."""
        tokens = self._lex("(if (and x y) (or a b) z)")
        kw_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        assert len(kw_tokens) == 3
        assert kw_tokens[0].value == "if"
        assert kw_tokens[1].value == "and"
        assert kw_tokens[2].value == "or"

    def test_keyword_positions_are_ordered(self):
        """Keyword start positions are in ascending order."""
        tokens = self._lex("(if (and x y) a b)")
        kw_tokens = [t for t in tokens if t.type == TokenType.KEYWORD]
        starts = [t.start for t in kw_tokens]
        assert starts == sorted(starts), f"Keyword positions not ordered: {starts}"
