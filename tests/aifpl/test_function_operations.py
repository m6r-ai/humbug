"""Tests for apply, function-min-arity, function-variadic?, function-accepts?,
function=?, and function!=?."""

import pytest
from aifpl import AIFPL


@pytest.fixture
def aifpl():
    return AIFPL()


# ---------------------------------------------------------------------------
# apply — basic dispatch
# ---------------------------------------------------------------------------

class TestApplyBasic:
    def test_apply_fixed_arity(self, aifpl):
        assert aifpl.evaluate("(apply integer-abs (list -5))") == 5

    def test_apply_binary_builtin(self, aifpl):
        assert aifpl.evaluate("(apply integer+ (list 3 4))") == 7

    def test_apply_variadic_builtin_zero_args(self, aifpl):
        # integer+ with 0 args returns identity 0
        assert aifpl.evaluate("(apply integer+ (list))") == 0

    def test_apply_variadic_builtin_many_args(self, aifpl):
        assert aifpl.evaluate("(apply integer+ (list 1 2 3 4 5))") == 15

    def test_apply_user_lambda(self, aifpl):
        assert aifpl.evaluate(
            "(let ((f (lambda (x y) (integer* x y)))) (apply f (list 6 7)))"
        ) == 42

    def test_apply_variadic_lambda(self, aifpl):
        assert aifpl.evaluate(
            "(let ((f (lambda (. args) (list-length args)))) (apply f (list 1 2 3)))"
        ) == 3

    def test_apply_variadic_lambda_empty(self, aifpl):
        assert aifpl.evaluate(
            "(let ((f (lambda (. args) (list-length args)))) (apply f (list)))"
        ) == 0

    def test_apply_mixed_variadic_lambda(self, aifpl):
        # (lambda (x . rest) ...) called with 3 args via apply
        assert aifpl.evaluate(
            "(let ((f (lambda (x . rest) (integer+ x (list-length rest))))) "
            "  (apply f (list 10 1 2 3)))"
        ) == 13

    def test_apply_string_concat(self, aifpl):
        assert aifpl.evaluate(
            '(apply string-concat (list "hello" " " "world"))'
        ) == "hello world"

    def test_apply_list_builtin(self, aifpl):
        assert aifpl.evaluate("(apply list (list 1 2 3))") == [1, 2, 3]

    def test_apply_single_arg(self, aifpl):
        assert aifpl.evaluate(
            "(let ((f (lambda (x) (integer* x x)))) (apply f (list 9)))"
        ) == 81

    def test_apply_returns_correct_value(self, aifpl):
        assert aifpl.evaluate("(apply integer-max (list 3 7 2 9 1))") == 9


# ---------------------------------------------------------------------------
# apply — tail call (TAIL_APPLY)
# ---------------------------------------------------------------------------

class TestApplyTailCall:
    def test_apply_in_tail_position(self, aifpl):
        # The apply is the tail expression of the outer lambda
        # Use let* so g can close over f
        assert aifpl.evaluate(
            "(let* ((f (lambda (x y) (integer+ x y)))"
            "       (g (lambda (args) (apply f args))))"
            "  (g (list 10 20)))"
        ) == 30

    def test_apply_tail_recursive_via_apply(self, aifpl):
        # Deep recursion using apply in tail position — would overflow without TCO
        result = aifpl.evaluate("""
            (letrec ((sum (lambda (n acc)
                            (if (integer=? n 0)
                                acc
                                (apply sum (list (integer- n 1) (integer+ acc n)))))))
              (sum 10000 0))
        """)
        assert result == 50005000

    def test_apply_tail_position_in_if(self, aifpl):
        assert aifpl.evaluate(
            "(let ((add (lambda (x y) (integer+ x y))))"
            "  (if #t (apply add (list 3 4)) 0))"
        ) == 7


# ---------------------------------------------------------------------------
# apply — error cases
# ---------------------------------------------------------------------------

class TestApplyErrors:
    def test_apply_non_function_first_arg(self, aifpl):
        with pytest.raises(Exception, match="apply"):
            aifpl.evaluate("(apply 42 (list 1 2))")

    def test_apply_non_list_second_arg(self, aifpl):
        with pytest.raises(Exception, match="apply"):
            aifpl.evaluate("(apply integer+ 42)")

    def test_apply_arity_mismatch_fixed(self, aifpl):
        with pytest.raises(Exception):
            aifpl.evaluate(
                "(let ((f (lambda (x y) x))) (apply f (list 1)))"
            )

    def test_apply_too_few_for_variadic_fixed_params(self, aifpl):
        with pytest.raises(Exception):
            # (lambda (x . rest) ...) needs at least 1 arg
            aifpl.evaluate(
                "(let ((f (lambda (x . rest) x))) (apply f (list)))"
            )


# ---------------------------------------------------------------------------
# function-min-arity
# ---------------------------------------------------------------------------

class TestFunctionMinArity:
    def test_zero_param_lambda(self, aifpl):
        assert aifpl.evaluate(
            "(function-min-arity (lambda () 42))"
        ) == 0

    def test_one_param_lambda(self, aifpl):
        assert aifpl.evaluate(
            "(function-min-arity (lambda (x) x))"
        ) == 1

    def test_two_param_lambda(self, aifpl):
        assert aifpl.evaluate(
            "(function-min-arity (lambda (x y) x))"
        ) == 2

    def test_fully_variadic_lambda(self, aifpl):
        # (lambda (. args) ...) — minimum is 0
        assert aifpl.evaluate(
            "(function-min-arity (lambda (. args) args))"
        ) == 0

    def test_variadic_one_fixed(self, aifpl):
        # (lambda (x . rest) ...) — minimum is 1
        assert aifpl.evaluate(
            "(function-min-arity (lambda (x . rest) x))"
        ) == 1

    def test_variadic_two_fixed(self, aifpl):
        assert aifpl.evaluate(
            "(function-min-arity (lambda (x y . rest) x))"
        ) == 2

    def test_fixed_arity_builtin(self, aifpl):
        assert aifpl.evaluate("(function-min-arity integer-abs)") == 1

    def test_variadic_builtin(self, aifpl):
        # integer+ accepts 0 or more
        assert aifpl.evaluate("(function-min-arity integer+)") == 0

    def test_returns_integer(self, aifpl):
        result = aifpl.evaluate("(integer? (function-min-arity integer-abs))")
        assert result is True

    def test_non_function_raises(self, aifpl):
        with pytest.raises(Exception, match="function-min-arity"):
            aifpl.evaluate("(function-min-arity 42)")


# ---------------------------------------------------------------------------
# function-variadic?
# ---------------------------------------------------------------------------

class TestFunctionVariadicP:
    def test_fixed_lambda_is_not_variadic(self, aifpl):
        assert aifpl.evaluate(
            "(function-variadic? (lambda (x y) x))"
        ) is False

    def test_zero_param_lambda_is_not_variadic(self, aifpl):
        assert aifpl.evaluate(
            "(function-variadic? (lambda () 42))"
        ) is False

    def test_fully_variadic_lambda(self, aifpl):
        assert aifpl.evaluate(
            "(function-variadic? (lambda (. args) args))"
        ) is True

    def test_mixed_variadic_lambda(self, aifpl):
        assert aifpl.evaluate(
            "(function-variadic? (lambda (x . rest) x))"
        ) is True

    def test_fixed_builtin_not_variadic(self, aifpl):
        assert aifpl.evaluate("(function-variadic? integer-abs)") is False

    def test_variadic_builtin(self, aifpl):
        assert aifpl.evaluate("(function-variadic? integer+)") is True

    def test_returns_boolean(self, aifpl):
        result = aifpl.evaluate("(boolean? (function-variadic? integer+))")
        assert result is True

    def test_non_function_raises(self, aifpl):
        with pytest.raises(Exception, match="function-variadic"):
            aifpl.evaluate('(function-variadic? "hello")')


# ---------------------------------------------------------------------------
# function-accepts?
# ---------------------------------------------------------------------------

class TestFunctionAcceptsP:
    def test_fixed_exact_arity(self, aifpl):
        assert aifpl.evaluate(
            "(function-accepts? (lambda (x y) x) 2)"
        ) is True

    def test_fixed_wrong_arity(self, aifpl):
        assert aifpl.evaluate(
            "(function-accepts? (lambda (x y) x) 1)"
        ) is False

    def test_fixed_zero_arity(self, aifpl):
        assert aifpl.evaluate(
            "(function-accepts? (lambda () 42) 0)"
        ) is True

    def test_fixed_zero_arity_wrong(self, aifpl):
        assert aifpl.evaluate(
            "(function-accepts? (lambda () 42) 1)"
        ) is False

    def test_variadic_at_minimum(self, aifpl):
        # (lambda (x . rest) ...) accepts 1 or more
        assert aifpl.evaluate(
            "(function-accepts? (lambda (x . rest) x) 1)"
        ) is True

    def test_variadic_above_minimum(self, aifpl):
        assert aifpl.evaluate(
            "(function-accepts? (lambda (x . rest) x) 5)"
        ) is True

    def test_variadic_below_minimum(self, aifpl):
        assert aifpl.evaluate(
            "(function-accepts? (lambda (x . rest) x) 0)"
        ) is False

    def test_fully_variadic_accepts_zero(self, aifpl):
        assert aifpl.evaluate(
            "(function-accepts? (lambda (. args) args) 0)"
        ) is True

    def test_fully_variadic_accepts_many(self, aifpl):
        assert aifpl.evaluate(
            "(function-accepts? (lambda (. args) args) 100)"
        ) is True

    def test_builtin_fixed(self, aifpl):
        assert aifpl.evaluate("(function-accepts? integer-abs 1)") is True
        assert aifpl.evaluate("(function-accepts? integer-abs 2)") is False

    def test_builtin_variadic(self, aifpl):
        assert aifpl.evaluate("(function-accepts? integer+ 0)") is True
        assert aifpl.evaluate("(function-accepts? integer+ 10)") is True

    def test_returns_boolean(self, aifpl):
        result = aifpl.evaluate(
            "(boolean? (function-accepts? integer-abs 1))"
        )
        assert result is True

    def test_non_function_raises(self, aifpl):
        with pytest.raises(Exception, match="function-accepts"):
            aifpl.evaluate("(function-accepts? 42 1)")

    def test_non_integer_n_raises(self, aifpl):
        with pytest.raises(Exception, match="function-accepts"):
            aifpl.evaluate('(function-accepts? integer+ "two")')


# ---------------------------------------------------------------------------
# function=? and function!=?
# ---------------------------------------------------------------------------

class TestFunctionEquality:
    def test_same_builtin_equal(self, aifpl):
        assert aifpl.evaluate("(function=? integer+ integer+)") is True

    def test_different_builtins_not_equal(self, aifpl):
        assert aifpl.evaluate("(function=? integer+ integer*)") is False

    def test_same_lambda_binding_equal(self, aifpl):
        assert aifpl.evaluate(
            "(let ((f (lambda (x) x))) (function=? f f))"
        ) is True

    def test_different_lambda_instances_not_equal(self, aifpl):
        # Two separately created lambdas with identical bodies are distinct objects
        assert aifpl.evaluate(
            "(let ((f (lambda (x) x)) (g (lambda (x) x))) (function=? f g))"
        ) is False

    def test_neq_same_builtin(self, aifpl):
        assert aifpl.evaluate("(function!=? integer+ integer+)") is False

    def test_neq_different_builtins(self, aifpl):
        assert aifpl.evaluate("(function!=? integer+ integer*)") is True

    def test_neq_same_lambda(self, aifpl):
        assert aifpl.evaluate(
            "(let ((f (lambda (x) x))) (function!=? f f))"
        ) is False

    def test_neq_different_lambdas(self, aifpl):
        assert aifpl.evaluate(
            "(let ((f (lambda (x) x)) (g (lambda (x) x))) (function!=? f g))"
        ) is True

    def test_function_eq_returns_boolean(self, aifpl):
        result = aifpl.evaluate("(boolean? (function=? integer+ integer+))")
        assert result is True

    def test_function_eq_non_function_first_raises(self, aifpl):
        with pytest.raises(Exception, match="function=\\?"):
            aifpl.evaluate("(function=? 42 integer+)")

    def test_function_eq_non_function_second_raises(self, aifpl):
        with pytest.raises(Exception, match="function=\\?"):
            aifpl.evaluate("(function=? integer+ 42)")

    def test_function_neq_non_function_raises(self, aifpl):
        with pytest.raises(Exception, match="function!=\\?"):
            aifpl.evaluate("(function!=? 42 integer+)")


# ---------------------------------------------------------------------------
# Integration: apply + function-accepts? together
# ---------------------------------------------------------------------------

class TestApplyWithIntrospection:
    def test_safe_dispatch_correct_arity(self, aifpl):
        result = aifpl.evaluate("""
            (let ((dispatch (lambda (f args)
                              (if (function-accepts? f (list-length args))
                                  (apply f args)
                                  (error "arity mismatch")))))
              (dispatch integer+ (list 1 2 3)))
        """)
        assert result == 6

    def test_safe_dispatch_wrong_arity_raises(self, aifpl):
        with pytest.raises(Exception):
            aifpl.evaluate("""
                (let ((dispatch (lambda (f args)
                                  (if (function-accepts? f (list-length args))
                                      (apply f args)
                                      (error "arity mismatch")))))
                  (dispatch integer-abs (list 1 2)))
            """)

    def test_apply_with_map(self, aifpl):
        # Use apply to call a binary function on each pair in a list of pairs
        result = aifpl.evaluate("""
            (let ((pairs (list (list 1 2) (list 3 4) (list 5 6))))
              (map (lambda (pair) (apply integer+ pair)) pairs))
        """)
        assert result == [3, 7, 11]

    def test_function_eq_used_as_predicate(self, aifpl):
        # Deduplicate a list of function references by identity
        result = aifpl.evaluate("""
            (let ((f integer+)
                  (g integer*)
                  (fns (list integer+ integer* integer+ integer-abs integer*)))
              (letrec ((dedup (lambda (lst seen acc)
                                (if (list-null? lst)
                                    (list-reverse acc)
                                    (if (any? (lambda (s) (function=? s (list-first lst))) seen)
                                        (dedup (list-rest lst) seen acc)
                                        (dedup (list-rest lst)
                                               (list-prepend seen (list-first lst))
                                               (list-prepend acc (list-first lst))))))))
                (list-length (dedup fns (list) (list)))))
        """)
        assert result == 3  # integer+, integer*, integer-abs

    def test_min_arity_drives_apply(self, aifpl):
        # Build an argument list of the right length and apply
        result = aifpl.evaluate("""
            (let ((f (lambda (x y z) (integer+ x (integer+ y z))))
                  (n (function-min-arity (lambda (x y z) (integer+ x (integer+ y z))))))
              (apply f (range 1 (integer+ n 1))))
        """)
        assert result == 6  # 1+2+3
