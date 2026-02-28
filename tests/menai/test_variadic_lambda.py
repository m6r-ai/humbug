"""Tests for variadic lambda (list-rest parameter) support.

Covers the dot-rest syntax: (lambda (a b . rest) body)
and the pure-variadic form: (lambda (. args) body).

The rest parameter receives all excess arguments as an MenaiList,
enabling user-defined variadic functions in pure Menai bytecode.
"""

import pytest

from menai import MenaiEvalError


class TestVariadicLambdaBasic:
    """Basic rest-parameter behaviour."""

    def test_pure_variadic_collects_all_args(self, menai):
        """(lambda (. args) args) returns all arguments as a list."""
        assert menai.evaluate_and_format('((lambda (. args) args) 1 2 3)') == '(1 2 3)'

    def test_pure_variadic_single_arg(self, menai):
        """Single argument is wrapped in a list."""
        assert menai.evaluate_and_format('((lambda (. args) args) 42)') == '(42)'

    def test_pure_variadic_zero_args(self, menai):
        """Zero arguments produces an empty list."""
        assert menai.evaluate_and_format('((lambda (. args) args))') == '()'

    def test_mixed_fixed_and_rest(self, menai):
        """Fixed parameters are bound normally; rest collects the remainder."""
        assert menai.evaluate_and_format(
            '((lambda (x . rest) (list-prepend rest x)) 10 20 30)'
        ) == '(10 20 30)'

    def test_mixed_two_fixed_and_rest(self, menai):
        """Two fixed params plus a rest parameter."""
        assert menai.evaluate_and_format(
            '((lambda (a b . rest) (list a b rest)) 1 2 3 4 5)'
        ) == '(1 2 (3 4 5))'

    def test_rest_empty_when_no_excess_args(self, menai):
        """Rest parameter is an empty list when no excess arguments are supplied."""
        assert menai.evaluate_and_format(
            '((lambda (x . rest) rest) 42)'
        ) == '()'

    def test_rest_empty_with_two_fixed(self, menai):
        """Rest is empty when exactly the fixed-arity args are supplied."""
        assert menai.evaluate_and_format(
            '((lambda (a b . rest) rest) 1 2)'
        ) == '()'

    def test_rest_contains_mixed_types(self, menai):
        """Rest parameter can hold values of different types."""
        assert menai.evaluate_and_format(
            '((lambda (. args) args) 1 "hello" #t)'
        ) == '(1 "hello" #t)'

    def test_rest_parameter_is_a_list(self, menai):
        """The rest parameter is a proper list (list? returns #t)."""
        assert menai.evaluate_and_format(
            '((lambda (. args) (list? args)) 1 2 3)'
        ) == '#t'

    def test_rest_parameter_empty_is_a_list(self, menai):
        """The rest parameter is a list even when empty."""
        assert menai.evaluate_and_format(
            '((lambda (. args) (list? args)))'
        ) == '#t'


class TestVariadicLambdaArithmetic:
    """Variadic functions implementing arithmetic operations."""

    def test_variadic_sum_zero_args(self, menai):
        """Variadic sum with zero arguments returns identity 0."""
        expr = '''
        (let ((my-sum (lambda (. args)
                        (fold-list integer+ 0 args))))
          (my-sum))
        '''
        assert menai.evaluate_and_format(expr) == '0'

    def test_variadic_sum_one_arg(self, menai):
        """Variadic sum with one argument returns that argument."""
        expr = '''
        (let ((my-sum (lambda (. args)
                        (fold-list integer+ 0 args))))
          (my-sum 7))
        '''
        assert menai.evaluate_and_format(expr) == '7'

    def test_variadic_sum_multiple_args(self, menai):
        """Variadic sum with multiple arguments folds correctly."""
        expr = '''
        (let ((my-sum (lambda (. args)
                        (fold-list integer+ 0 args))))
          (my-sum 1 2 3 4 5))
        '''
        assert menai.evaluate_and_format(expr) == '15'

    def test_variadic_product(self, menai):
        """Variadic product using fold."""
        expr = '''
        (let ((my-product (lambda (. args)
                            (fold-list integer* 1 args))))
          (my-product 2 3 4))
        '''
        assert menai.evaluate_and_format(expr) == '24'

    def test_variadic_min(self, menai):
        """Variadic minimum using letrec loop."""
        expr = '''
        (let ((my-min (lambda (first . rest)
                        (fold-list (lambda (acc x) (if (integer<? x acc) x acc))
                                   first
                                   rest))))
          (my-min 5 3 8 1 4))
        '''
        assert menai.evaluate_and_format(expr) == '1'

    def test_variadic_max(self, menai):
        """Variadic maximum using fold."""
        expr = '''
        (let ((my-max (lambda (first . rest)
                        (fold-list (lambda (acc x) (if (integer>? x acc) x acc))
                                   first
                                   rest))))
          (my-max 5 3 8 1 4))
        '''
        assert menai.evaluate_and_format(expr) == '8'


class TestVariadicLambdaListOps:
    """Variadic functions implementing list operations."""

    def test_variadic_list_constructor(self, menai):
        """Variadic function that collects args into a list (mirrors builtin list)."""
        expr = '((lambda (. args) args) 1 2 3 4)'
        assert menai.evaluate_and_format(expr) == '(1 2 3 4)'

    def test_variadic_append(self, menai):
        """Variadic list-concat using fold."""
        expr = '''
        (let ((my-list-concat (lambda (. lists)
                           (fold-list list-concat (list) lists))))
          (my-list-concat (list 1 2) (list 3 4) (list 5)))
        '''
        assert menai.evaluate_and_format(expr) == '(1 2 3 4 5)'

    def test_variadic_string_join(self, menai):
        """Variadic string concatenation using fold."""
        expr = '''
        (let ((my-concat (lambda (. strs)
                           (fold-list string-concat "" strs))))
          (my-concat "hello" " " "world"))
        '''
        assert menai.evaluate_and_format(expr) == '"hello world"'

    def test_variadic_length(self, menai):
        """Counting the number of variadic arguments using length."""
        expr = '((lambda (. args) (list-length args)) 10 20 30 40)'
        assert menai.evaluate_and_format(expr) == '4'


class TestVariadicLambdaHigherOrder:
    """Variadic lambdas used with higher-order functions."""

    def test_variadic_passed_to_map(self, menai):
        """A variadic function used as the mapped function (single-arg call)."""
        # map calls (f element) with exactly 1 arg, so rest is empty
        expr = '''
        (let ((wrap (lambda (x . rest) (list-prepend rest x))))
          (map-list wrap (list 1 2 3)))
        '''
        assert menai.evaluate_and_format(expr) == '((1) (2) (3))'

    def test_variadic_passed_to_fold(self, menai):
        """A variadic function used as the fold combiner (two-arg call)."""
        # fold calls (f acc element) with exactly 2 args
        expr = '''
        (let ((my-add (lambda (a . rest)
                        (fold-list integer+ a rest))))
          (fold-list my-add 0 (list 1 2 3 4)))
        '''
        assert menai.evaluate_and_format(expr) == '10'

    def test_variadic_returned_from_function(self, menai):
        """A factory function that returns a variadic closure."""
        expr = '''
        (let ((make-adder (lambda (base)
                            (lambda (. args)
                              (fold-list integer+ base args)))))
          (let ((add-from-10 (make-adder 10)))
            (add-from-10 1 2 3)))
        '''
        assert menai.evaluate_and_format(expr) == '16'

    def test_variadic_in_letrec(self, menai):
        """Variadic function defined in letrec can be recursive."""
        expr = '''
        (letrec ((sum-all (lambda (. args)
                            (letrec ((loop (lambda (lst acc)
                                             (if (list-null? lst) acc
                                                 (loop (list-rest lst) (integer+ acc (list-first lst)))))))
                              (loop args 0)))))
          (sum-all 10 20 30))
        '''
        assert menai.evaluate_and_format(expr) == '60'

    def test_variadic_stored_in_list(self, menai):
        """Variadic functions stored in a list and retrieved by index."""
        expr = '''
        (let ((ops (list (lambda (. args) (fold-list integer+ 0 args))
                         (lambda (. args) (fold-list integer* 1 args)))))
          (list ((list-first ops) 1 2 3)
                ((list-ref ops 1) 2 3 4)))
        '''
        assert menai.evaluate_and_format(expr) == '(6 24)'


class TestVariadicLambdaTailCalls:
    """Variadic lambdas with tail-call optimisation."""

    def test_variadic_with_tail_recursive_helper(self, menai):
        """Variadic entry point delegating to a tail-recursive helper."""
        expr = '''
        (let ((sum-all (lambda (. args)
                         (letrec ((loop (lambda (lst acc)
                                          (if (list-null? lst) acc
                                              (loop (list-rest lst) (integer+ acc (list-first lst)))))))
                           (loop args 0)))))
          (sum-all 1 2 3 4 5 6 7 8 9 10))
        '''
        assert menai.evaluate_and_format(expr) == '55'

    def test_variadic_deep_recursion_via_helper(self, menai):
        """Large number of args processed by tail-recursive helper (tests TCO)."""
        # Build (sum-all 1 2 ... 100) dynamically via range + apply-style fold
        expr = '''
        (let ((sum-all (lambda (. args)
                         (letrec ((loop (lambda (lst acc)
                                          (if (list-null? lst) acc
                                              (loop (list-rest lst) (integer+ acc (list-first lst)))))))
                           (loop args 0)))))
          (fold-list (lambda (f x) (f x))
                     sum-all
                     (list (lambda (f) (f 1 2 3 4 5 6 7 8 9 10
                                          11 12 13 14 15 16 17 18 19 20)))))
        '''
        # The above is awkward - just call directly with 20 args
        expr2 = '''
        (let ((sum-all (lambda (. args)
                         (letrec ((loop (lambda (lst acc)
                                          (if (list-null? lst) acc
                                              (loop (list-rest lst) (integer+ acc (list-first lst)))))))
                           (loop args 0)))))
          (sum-all 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
        '''
        assert menai.evaluate_and_format(expr2) == '210'


class TestVariadicLambdaErrors:
    """Error cases for variadic lambda definitions and calls."""

    def test_too_few_args_for_fixed_prefix(self, menai):
        """Calling a variadic function with fewer args than the fixed prefix raises an error."""
        with pytest.raises(MenaiEvalError, match=r"at least 2 argument"):
            menai.evaluate('((lambda (a b . rest) rest) 1)')

    def test_too_few_args_single_fixed(self, menai):
        """One fixed param + rest: calling with zero args raises an error."""
        with pytest.raises(MenaiEvalError, match=r"at least 1 argument"):
            menai.evaluate('((lambda (x . rest) x))')

    def test_multiple_dots_rejected(self, menai):
        """Two dots in the parameter list is a syntax error."""
        with pytest.raises(MenaiEvalError, match=r"more than one dot"):
            menai.evaluate('(lambda (a . b . c) a)')

    def test_dot_not_second_to_last_rejected(self, menai):
        """Dot with more than one element following it is a syntax error."""
        with pytest.raises(MenaiEvalError, match=r"[Rr]est parameter"):
            menai.evaluate('(lambda (a . b c) a)')

    def test_dot_at_end_rejected(self, menai):
        """Dot as the last element (no rest name) is a syntax error."""
        with pytest.raises(MenaiEvalError, match=r"[Rr]est parameter"):
            menai.evaluate('(lambda (a b .) a)')

    def test_dot_only_rejected(self, menai):
        """A parameter list containing only a dot is a syntax error."""
        with pytest.raises(MenaiEvalError, match=r"[Rr]est parameter"):
            menai.evaluate('(lambda (.) a)')

    def test_duplicate_rest_param_name_rejected(self, menai):
        """Rest parameter name clashing with a fixed param is an error."""
        with pytest.raises(MenaiEvalError, match=r"[Uu]nique|[Dd]uplicate"):
            menai.evaluate('(lambda (x . x) x)')


class TestVariadicLambdaClosures:
    """Variadic lambdas capturing free variables."""

    def test_variadic_captures_outer_variable(self, menai):
        """Rest-parameter lambda can close over an outer binding."""
        expr = '''
        (let ((base 100))
          ((lambda (. args) (fold-list integer+ base args)) 1 2 3))
        '''
        assert menai.evaluate_and_format(expr) == '106'

    def test_variadic_closure_in_let(self, menai):
        """Variadic closure stored in a let binding captures correctly."""
        expr = '''
        (let ((scale 3))
          (let ((scale-and-sum (lambda (. args)
                                 (fold-list integer+ 0 (map-list (lambda (x) (integer* x scale)) args)))))
            (scale-and-sum 1 2 3 4)))
        '''
        # (1+2+3+4)*3 = 30
        assert menai.evaluate_and_format(expr) == '30'

    def test_variadic_nested_closure(self, menai):
        """Variadic lambda nested inside another lambda captures correctly."""
        expr = '''
        (let ((make-accumulator (lambda (init)
                                  (lambda (. args)
                                    (fold-list integer+ init args)))))
          (let ((acc (make-accumulator 1000)))
            (acc 1 2 3)))
        '''
        assert menai.evaluate_and_format(expr) == '1006'


class TestVariadicLambdaDescribe:
    """The describe/format of variadic functions reflects the rest parameter."""

    def test_function_is_variadic(self, menai):
        """function? returns #t for a variadic lambda."""
        assert menai.evaluate_and_format(
            '(function? (lambda (. args) args))'
        ) == '#t'

    def test_variadic_function_is_callable(self, menai):
        """A variadic function stored in a variable is callable."""
        expr = '''
        (let ((f (lambda (x . rest) (list-prepend rest x))))
          (f 1 2 3))
        '''
        assert menai.evaluate_and_format(expr) == '(1 2 3)'
