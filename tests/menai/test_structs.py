"""Tests for struct support in Menai."""

import pytest

from menai import Menai, MenaiEvalError


@pytest.fixture
def menai():
    """Create a fresh Menai instance for each test."""
    return Menai()


# ---------------------------------------------------------------------------
# 1. Basic construction and field access
# ---------------------------------------------------------------------------

class TestStructConstruction:
    """Test basic struct construction."""

    def test_zero_field_struct(self, menai):
        """A struct with no fields constructs and displays correctly."""
        result = menai.evaluate_and_format('(let ((Unit (struct ()))) (Unit))')
        assert result == '(Unit)'

    def test_single_field_struct(self, menai):
        """A struct with one field constructs and displays correctly."""
        result = menai.evaluate_and_format('(let ((Box (struct (value)))) (Box 42))')
        assert result == '(Box 42)'

    def test_multi_field_struct(self, menai):
        """A struct with multiple fields constructs and displays correctly."""
        result = menai.evaluate_and_format(
            '(let ((Point (struct (x y)))) (Point 3 4))'
        )
        assert result == '(Point 3 4)'

    def test_three_field_struct(self, menai):
        """A struct with three fields constructs correctly."""
        result = menai.evaluate_and_format(
            '(let ((RGB (struct (r g b)))) (RGB 255 128 0))'
        )
        assert result == '(RGB 255 128 0)'

    def test_positional_field_ordering(self, menai):
        """Fields are stored in the order given to the constructor."""
        result = menai.evaluate_and_format(
            '(let ((Pair (struct (first second)))) (Pair "hello" "world"))'
        )
        assert result == '(Pair "hello" "world")'

    def test_struct_with_mixed_field_types(self, menai):
        """A struct can hold fields of different types."""
        result = menai.evaluate_and_format(
            '(let ((Person (struct (name age)))) (Person "Alice" 30))'
        )
        assert result == '(Person "Alice" 30)'


# ---------------------------------------------------------------------------
# 2. Field access (struct-get)
# ---------------------------------------------------------------------------

class TestStructGet:
    """Test struct-get field access."""

    def test_get_first_field(self, menai):
        """struct-get retrieves the first field correctly."""
        result = menai.evaluate_and_format(
            "(let ((Point (struct (x y))) (p (Point 3 4))) (struct-get p 'x))"
        )
        assert result == '3'

    def test_get_second_field(self, menai):
        """struct-get retrieves the second field correctly."""
        result = menai.evaluate_and_format(
            "(let ((Point (struct (x y))) (p (Point 3 4))) (struct-get p 'y))"
        )
        assert result == '4'

    def test_get_each_field_of_three_field_struct(self, menai):
        """struct-get retrieves each field of a three-field struct."""
        expr = '''
        (let ((RGB (struct (r g b)))
              (c (RGB 10 20 30)))
          (list (struct-get c 'r) (struct-get c 'g) (struct-get c 'b)))
        '''
        assert menai.evaluate_and_format(expr) == '(10 20 30)'

    def test_get_string_field(self, menai):
        """struct-get works on string-valued fields."""
        result = menai.evaluate_and_format(
            "(let ((Person (struct (name age))) (p (Person \"Alice\" 30))) (struct-get p 'name))"
        )
        assert result == '"Alice"'

    def test_get_unknown_field_raises_error(self, menai):
        """struct-get raises an error for an unknown field name."""
        with pytest.raises(MenaiEvalError, match="has no field"):
            menai.evaluate(
                "(let ((Point (struct (x y))) (p (Point 1 2))) (struct-get p 'z))"
            )

    def test_get_on_non_struct_raises_error(self, menai):
        """struct-get raises an error when called on a non-struct."""
        with pytest.raises(MenaiEvalError, match="struct-get"):
            menai.evaluate("(struct-get 42 'x)")

    def test_get_on_list_raises_error(self, menai):
        """struct-get raises an error when called on a list."""
        with pytest.raises(MenaiEvalError, match="struct-get"):
            menai.evaluate("(struct-get (list 1 2) 'x)")


# ---------------------------------------------------------------------------
# 3. Functional update (struct-set)
# ---------------------------------------------------------------------------

class TestStructSet:
    """Test struct-set functional update."""

    def test_update_first_field(self, menai):
        """struct-set updates the first field and returns a new struct."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (p (Point 1 2)))
          (struct-set p 'x 99))
        ''')
        assert result == '(Point 99 2)'

    def test_update_last_field(self, menai):
        """struct-set updates the last field and returns a new struct."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (p (Point 1 2)))
          (struct-set p 'y 99))
        ''')
        assert result == '(Point 1 99)'

    def test_update_middle_field(self, menai):
        """struct-set updates a middle field correctly."""
        result = menai.evaluate_and_format('''
        (let ((RGB (struct (r g b)))
              (c (RGB 10 20 30)))
          (struct-set c 'g 99))
        ''')
        assert result == '(RGB 10 99 30)'

    def test_original_unchanged_after_set(self, menai):
        """struct-set is a functional update — the original struct is unchanged."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (p (Point 1 2)))
          (let ((p2 (struct-set p 'x 99)))
            (list (struct-get p 'x) (struct-get p2 'x))))
        ''')
        assert result == '(1 99)'

    def test_set_unknown_field_raises_error(self, menai):
        """struct-set raises an error for an unknown field name."""
        with pytest.raises(MenaiEvalError, match="has no field"):
            menai.evaluate(
                "(let ((Point (struct (x y))) (p (Point 1 2))) (struct-set p 'z 99))"
            )

    def test_set_on_non_struct_raises_error(self, menai):
        """struct-set raises an error when called on a non-struct."""
        with pytest.raises(MenaiEvalError, match="struct-set"):
            menai.evaluate("(struct-set 42 'x 1)")


# ---------------------------------------------------------------------------
# 4. Type predicates
# ---------------------------------------------------------------------------

class TestStructPredicates:
    """Test struct? and struct-type? predicates."""

    def test_struct_predicate_true_for_instance(self, menai):
        """(struct? p) returns #t for a struct instance."""
        result = menai.evaluate_and_format(
            '(let ((Point (struct (x y))) (p (Point 1 2))) (struct? p))'
        )
        assert result == '#t'

    def test_struct_predicate_false_for_integer(self, menai):
        """(struct? 42) returns #f for a non-struct."""
        assert menai.evaluate_and_format('(struct? 42)') == '#f'

    def test_struct_predicate_false_for_list(self, menai):
        """(struct? (list 1 2)) returns #f for a list."""
        assert menai.evaluate_and_format('(struct? (list 1 2))') == '#f'

    def test_struct_predicate_false_for_string(self, menai):
        """(struct? "hello") returns #f for a string."""
        assert menai.evaluate_and_format('(struct? "hello")') == '#f'

    def test_struct_type_predicate_true_for_matching_type(self, menai):
        """(struct-type? Point p) returns #t when p is a Point."""
        result = menai.evaluate_and_format(
            '(let ((Point (struct (x y))) (p (Point 1 2))) (struct-type? Point p))'
        )
        assert result == '#t'

    def test_struct_type_predicate_false_for_different_struct_type(self, menai):
        """(struct-type? Point v) returns #f when v is a Vec (nominal typing)."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (Vec (struct (x y)))
              (v (Vec 1 2)))
          (struct-type? Point v))
        ''')
        assert result == '#f'

    def test_struct_type_predicate_false_for_non_struct(self, menai):
        """(struct-type? Point 42) returns #f for a non-struct value."""
        result = menai.evaluate_and_format(
            '(let ((Point (struct (x y)))) (struct-type? Point 42))'
        )
        assert result == '#f'

    def test_struct_type_predicate_false_for_struct_type_value(self, menai):
        """(struct-type? Point Point) returns #f — Point is the type, not an instance."""
        result = menai.evaluate_and_format(
            '(let ((Point (struct (x y)))) (struct-type? Point Point))'
        )
        assert result == '#f'


# ---------------------------------------------------------------------------
# 5. Equality
# ---------------------------------------------------------------------------

class TestStructEquality:
    """Test struct=? and struct!=? equality predicates."""

    def test_equal_structs_same_type_same_fields(self, menai):
        """(struct=? p1 p2) is #t when type and all fields match."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (p1 (Point 1 2))
              (p2 (Point 1 2)))
          (struct=? p1 p2))
        ''')
        assert result == '#t'

    def test_unequal_structs_same_type_different_fields(self, menai):
        """(struct=? p1 p2) is #f when fields differ."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (p1 (Point 1 2))
              (p2 (Point 3 4)))
          (struct=? p1 p2))
        ''')
        assert result == '#f'

    def test_unequal_structs_different_types_same_field_values(self, menai):
        """(struct=? p v) is #f even when field values are the same but types differ."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (Vec (struct (x y)))
              (p (Point 1 2))
              (v (Vec 1 2)))
          (struct=? p v))
        ''')
        assert result == '#f'

    def test_neq_mirrors_eq_true(self, menai):
        """(struct!=? p1 p2) is #f when structs are equal."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (p1 (Point 1 2))
              (p2 (Point 1 2)))
          (struct!=? p1 p2))
        ''')
        assert result == '#f'

    def test_neq_mirrors_eq_false(self, menai):
        """(struct!=? p1 p2) is #t when structs differ."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (p1 (Point 1 2))
              (p2 (Point 3 4)))
          (struct!=? p1 p2))
        ''')
        assert result == '#t'

    def test_neq_different_types(self, menai):
        """(struct!=? p v) is #t when types differ even if field values match."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (Vec (struct (x y)))
              (p (Point 1 2))
              (v (Vec 1 2)))
          (struct!=? p v))
        ''')
        assert result == '#t'

    def test_struct_eq_on_non_struct_raises_error(self, menai):
        """struct=? raises an error when called on a non-struct."""
        with pytest.raises(MenaiEvalError, match="struct=\\?"):
            menai.evaluate(
                "(let ((Point (struct (x y))) (p (Point 1 2))) (struct=? p 42))"
            )


# ---------------------------------------------------------------------------
# 6. Introspection
# ---------------------------------------------------------------------------

class TestStructIntrospection:
    """Test struct-type-name, struct-fields, and struct-type."""

    def test_struct_type_name(self, menai):
        """(struct-type-name Point) returns the string \"Point\"."""
        result = menai.evaluate_and_format(
            '(let ((Point (struct (x y)))) (struct-type-name Point))'
        )
        assert result == '"Point"'

    def test_struct_fields_returns_symbols(self, menai):
        """(struct-fields Point) returns a list of field name symbols."""
        result = menai.evaluate_and_format(
            '(let ((Point (struct (x y)))) (struct-fields Point))'
        )
        assert result == '(x y)'

    def test_struct_fields_zero_field_struct(self, menai):
        """(struct-fields Unit) returns an empty list for a zero-field struct."""
        result = menai.evaluate_and_format(
            '(let ((Unit (struct ()))) (struct-fields Unit))'
        )
        assert result == '()'

    def test_struct_type_returns_type_value(self, menai):
        """(struct-type p) returns the struct type value of an instance."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (p (Point 1 2)))
          (struct? (struct-type p)))
        ''')
        # struct-type returns a struct-type value, not a struct instance
        assert result == '#f'

    def test_struct_type_name_roundtrip(self, menai):
        """(struct-type-name (struct-type p)) round-trips to the type name."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (p (Point 3 4)))
          (struct-type-name (struct-type p)))
        ''')
        assert result == '"Point"'

    def test_struct_type_identity(self, menai):
        """(struct-type p) returns the same type as the binding."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (p (Point 1 2)))
          (struct-type? Point p))
        ''')
        assert result == '#t'

    def test_struct_type_name_on_non_struct_type_raises_error(self, menai):
        """struct-type-name raises an error when called on a non-struct-type."""
        with pytest.raises(MenaiEvalError, match="struct-type-name"):
            menai.evaluate("(struct-type-name 42)")

    def test_struct_fields_on_non_struct_type_raises_error(self, menai):
        """struct-fields raises an error when called on a non-struct-type."""
        with pytest.raises(MenaiEvalError, match="struct-fields"):
            menai.evaluate("(struct-fields 42)")

    def test_struct_type_on_non_struct_raises_error(self, menai):
        """struct-type raises an error when called on a non-struct."""
        with pytest.raises(MenaiEvalError, match="struct-type"):
            menai.evaluate("(struct-type 42)")


# ---------------------------------------------------------------------------
# 7. Pattern matching
# ---------------------------------------------------------------------------

class TestStructPatternMatching:
    """Test struct pattern matching via match."""

    def test_basic_destructuring(self, menai):
        """Pattern (Point a b) binds fields to variables correctly."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (p (Point 3 4)))
          (match p
            ((Point a b) (integer+ a b))
            (_ 0)))
        ''')
        assert result == '7'

    def test_destructuring_uses_field_order(self, menai):
        """Destructuring binds variables in field definition order."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (p (Point 10 20)))
          (match p
            ((Point a b) (list a b))
            (_ (list))))
        ''')
        assert result == '(10 20)'

    def test_wrong_type_struct_falls_through(self, menai):
        """A struct of the wrong type falls through to the next pattern."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (Vec (struct (x y)))
              (v (Vec 1 2)))
          (match v
            ((Point a b) "point")
            ((Vec a b) "vec")
            (_ "other")))
        ''')
        assert result == '"vec"'

    def test_wildcard_still_works_alongside_struct_patterns(self, menai):
        """Wildcard _ still matches when no struct pattern applies."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (p (Point 1 2)))
          (match 42
            ((Point a b) "point")
            (_ "other")))
        ''')
        assert result == '"other"'

    def test_nested_struct_patterns(self, menai):
        """Nested struct patterns destructure inner structs correctly."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (Line (struct (start end)))
              (line (Line (Point 0 0) (Point 3 4))))
          (match line
            ((Line (Point x1 y1) (Point x2 y2))
             (list x1 y1 x2 y2))
            (_ (list))))
        ''')
        assert result == '(0 0 3 4)'

    def test_zero_field_struct_pattern(self, menai):
        """A zero-field struct can be matched by its pattern."""
        result = menai.evaluate_and_format('''
        (let ((Unit (struct ()))
              (u (Unit)))
          (match u
            ((Unit) "unit!")
            (_ "other")))
        ''')
        assert result == '"unit!"'

    def test_struct_pattern_in_function(self, menai):
        """Struct patterns work correctly inside a lambda."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y))))
          (let ((distance-sq (lambda (p)
                  (match p
                    ((Point x y) (integer+ (integer* x x) (integer* y y)))
                    (_ 0)))))
            (distance-sq (Point 3 4))))
        ''')
        assert result == '25'


# ---------------------------------------------------------------------------
# 8. Hashability
# ---------------------------------------------------------------------------

class TestStructHashability:
    """Test struct use in sets and as dict keys."""

    def test_struct_with_scalar_fields_usable_in_set(self, menai):
        """A struct with scalar fields can be added to a set."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (p (Point 1 2)))
          (set-length (set-add (set) p)))
        ''')
        assert result == '1'

    def test_two_equal_structs_deduplicated_in_set(self, menai):
        """Two equal structs are treated as the same set element."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (p1 (Point 1 2))
              (p2 (Point 1 2)))
          (set-length (set-add (set-add (set) p1) p2)))
        ''')
        assert result == '1'

    def test_two_different_structs_both_in_set(self, menai):
        """Two distinct structs both appear in the set."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (p1 (Point 1 2))
              (p2 (Point 3 4)))
          (set-length (set-add (set-add (set) p1) p2)))
        ''')
        assert result == '2'

    def test_struct_with_scalar_fields_usable_as_dict_key(self, menai):
        """A struct with scalar fields can be used as a dict key."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (p (Point 1 2)))
          (let ((d (dict (list p "origin"))))
            (dict-get d p #none)))
        ''')
        assert result == '"origin"'

    def test_struct_with_list_field_raises_error_in_set(self, menai):
        """A struct containing a list field raises an error when used in a set."""
        with pytest.raises(MenaiEvalError):
            menai.evaluate('''
            (let ((Container (struct (items)))
                  (c (Container (list 1 2 3))))
              (set-add (set) c))
            ''')

    def test_struct_with_list_field_raises_error_as_dict_key(self, menai):
        """A struct containing a list field raises an error when used as a dict key."""
        with pytest.raises(MenaiEvalError):
            menai.evaluate('''
            (let ((Container (struct (items)))
                  (c (Container (list 1 2 3))))
              (dict (list c "value")))
            ''')


# ---------------------------------------------------------------------------
# 9. Scoping in let and let*
# ---------------------------------------------------------------------------

class TestStructScoping:
    """Test that struct types scope correctly in let and let*."""

    def test_struct_in_let_usable_in_body(self, menai):
        """A struct type defined in let is usable in the body."""
        result = menai.evaluate_and_format(
            '(let ((Point (struct (x y)))) (Point 1 2))'
        )
        assert result == '(Point 1 2)'

    def test_struct_in_let_star_usable_in_body(self, menai):
        """A struct type defined in let* is usable in the body."""
        result = menai.evaluate_and_format(
            '(let* ((Point (struct (x y)))) (Point 1 2))'
        )
        assert result == '(Point 1 2)'

    def test_struct_in_let_star_usable_in_subsequent_binding(self, menai):
        """A struct type defined in let* is usable in a later binding."""
        result = menai.evaluate_and_format('''
        (let* ((Point (struct (x y)))
               (origin (Point 0 0)))
          (struct-get origin 'x))
        ''')
        assert result == '0'

    def test_multiple_structs_in_let_star(self, menai):
        """Multiple struct types defined in the same let* are all usable."""
        result = menai.evaluate_and_format('''
        (let* ((Point (struct (x y)))
               (Color (struct (r g b)))
               (p (Point 1 2))
               (c (Color 255 0 128)))
          (list (struct-get p 'x) (struct-get c 'r)))
        ''')
        assert result == '(1 255)'

    def test_multiple_structs_in_let(self, menai):
        """Multiple struct types defined in the same let are all usable."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (Vec (struct (dx dy))))
          (list (Point 1 2) (Vec 3 4)))
        ''')
        assert result == '((Point 1 2) (Vec 3 4))'

    def test_struct_in_let_usable_in_pattern_match(self, menai):
        """A struct type defined in let is usable in a match pattern in the body."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (p (Point 5 6)))
          (match p
            ((Point a b) (integer+ a b))
            (_ 0)))
        ''')
        assert result == '11'


# ---------------------------------------------------------------------------
# 10. Module export
# ---------------------------------------------------------------------------

class TestStructModuleExport:
    """Test exporting and importing struct types via modules."""

    def test_struct_type_exported_and_constructor_usable(self, tmp_path):
        """A struct type exported from a module can be constructed in the importer."""
        (tmp_path / "shapes.menai").write_text(
            '(let ((Point (struct (x y)))) (dict (list "Point" Point)))'
        )
        m = Menai(module_path=[str(tmp_path)])
        result = m.evaluate_and_format('''
        (let ((shapes (import "shapes")))
          (let ((Point (dict-get shapes "Point")))
            (Point 3 4)))
        ''')
        assert result == '(Point 3 4)'

    def test_struct_get_on_imported_struct(self, tmp_path):
        """struct-get works on instances of an imported struct type."""
        (tmp_path / "shapes.menai").write_text(
            '(let ((Point (struct (x y)))) (dict (list "Point" Point)))'
        )
        m = Menai(module_path=[str(tmp_path)])
        result = m.evaluate_and_format('''
        (let ((shapes (import "shapes")))
          (let ((Point (dict-get shapes "Point"))
                (p (let ((Point (dict-get shapes "Point"))) (Point 7 8))))
            (struct-get p 'x)))
        ''')
        assert result == '7'

    def test_pattern_matching_on_imported_struct(self, tmp_path):
        """Pattern matching works on instances of an imported struct type."""
        (tmp_path / "shapes.menai").write_text(
            '(let ((Point (struct (x y)))) (dict (list "Point" Point)))'
        )
        m = Menai(module_path=[str(tmp_path)])
        result = m.evaluate_and_format('''
        (let ((shapes (import "shapes")))
          (let ((Point (dict-get shapes "Point"))
                (p (let ((Point (dict-get shapes "Point"))) (Point 3 4))))
            (match p
              ((Point a b) (integer+ a b))
              (_ 0))))
        ''')
        assert result == '7'


    def test_struct_defined_in_letrec_module(self, tmp_path):
        """A struct defined in a letrec module body is exported and usable."""
        (tmp_path / "shapes.menai").write_text("""
(letrec ((Point (struct (x y)))
         (make-point (lambda (a b) (Point a b)))
         (point-x (lambda (p) (match p ((Point x _) x))))
         (point-y (lambda (p) (match p ((Point _ y) y)))))
  (dict
    (list "Point" Point)
    (list "make-point" make-point)
    (list "point-x" point-x)
    (list "point-y" point-y)))
""")
        m = Menai(module_path=[str(tmp_path)])
        result = m.evaluate("""
(let ((shapes (import "shapes")))
  (let ((make-point (dict-get shapes "make-point"))
        (point-x    (dict-get shapes "point-x"))
        (point-y    (dict-get shapes "point-y")))
    (let ((p (make-point 3 4)))
      (integer+ (point-x p) (point-y p)))))
""")
        assert result == 7

    def test_struct_constructor_from_letrec_module(self, tmp_path):
        """The exported struct type itself can be used as a constructor by the importer."""
        (tmp_path / "shapes.menai").write_text("""
(letrec ((Point (struct (x y))))
  (dict (list "Point" Point)))
""")
        m = Menai(module_path=[str(tmp_path)])
        result = m.evaluate_and_format("""
(let ((shapes (import "shapes")))
  (let ((Point (dict-get shapes "Point")))
    (Point 5 6)))
""")
        assert result == '(Point 5 6)'

    def test_pattern_matching_on_struct_from_letrec_module(self, tmp_path):
        """Pattern matching works on instances of a struct type exported from a letrec module."""
        (tmp_path / "shapes.menai").write_text("""
(letrec ((Point (struct (x y))))
  (dict (list "Point" Point)))
""")
        m = Menai(module_path=[str(tmp_path)])
        result = m.evaluate("""
(let ((shapes (import "shapes")))
  (let ((Point (dict-get shapes "Point")))
    (let ((p (Point 10 20)))
      (match p
        ((Point a b) (integer+ a b))
        (_ 0)))))
""")
        assert result == 30


# ---------------------------------------------------------------------------
# 11. Error cases
# ---------------------------------------------------------------------------

class TestStructErrors:
    """Test error cases for struct operations."""

    def test_wrong_arity_to_constructor_too_few(self, menai):
        """Calling a struct constructor with too few arguments raises an error."""
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(let ((Point (struct (x y)))) (Point 1))')

    def test_wrong_arity_to_constructor_too_many(self, menai):
        """Calling a struct constructor with too many arguments raises an error."""
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('(let ((Point (struct (x y)))) (Point 1 2 3))')

    def test_struct_in_letrec_works(self, menai):
        """Struct definitions are permitted in letrec and are hoisted to let."""
        result = menai.evaluate('(letrec ((Point (struct (x y)))) (Point 1 2))')
        assert str(result) != ''

    def test_struct_in_letrec_with_sibling_lambdas(self, menai):
        """Struct defined in letrec is visible to sibling lambda bindings and the body."""
        result = menai.evaluate(
            '(letrec ((Point (struct (x y)))'
            '         (make (lambda (a b) (Point a b)))'
            '         (get-x (lambda (p) (match p ((Point x _) x)))))'
            '  (get-x (make 42 99)))'
        )
        assert str(result) == '42'

    def test_struct_outside_let_raises_error(self, menai):
        """Using (struct ...) outside a let/let* binding position raises an error."""
        with pytest.raises(MenaiEvalError, match="[Ss]truct"):
            menai.evaluate('(struct (x y))')

    def test_struct_get_on_non_struct_raises_error(self, menai):
        """struct-get raises an error when the first argument is not a struct."""
        with pytest.raises(MenaiEvalError, match="struct-get"):
            menai.evaluate("(struct-get 42 'x)")

    def test_struct_set_on_non_struct_raises_error(self, menai):
        """struct-set raises an error when the first argument is not a struct."""
        with pytest.raises(MenaiEvalError, match="struct-set"):
            menai.evaluate("(struct-set 42 'x 1)")

    def test_struct_get_unknown_field_raises_error(self, menai):
        """struct-get raises an error for an unknown field name."""
        with pytest.raises(MenaiEvalError, match="has no field"):
            menai.evaluate(
                "(let ((Point (struct (x y))) (p (Point 1 2))) (struct-get p 'z))"
            )

    def test_struct_set_unknown_field_raises_error(self, menai):
        """struct-set raises an error for an unknown field name."""
        with pytest.raises(MenaiEvalError, match="has no field"):
            menai.evaluate(
                "(let ((Point (struct (x y))) (p (Point 1 2))) (struct-set p 'z 99))"
            )

    def test_struct_eq_on_non_struct_first_arg_raises_error(self, menai):
        """struct=? raises an error when the first argument is not a struct."""
        with pytest.raises(MenaiEvalError, match="struct=\\?"):
            menai.evaluate(
                "(let ((Point (struct (x y))) (p (Point 1 2))) (struct=? 42 p))"
            )

    def test_struct_eq_on_non_struct_second_arg_raises_error(self, menai):
        """struct=? raises an error when the second argument is not a struct."""
        with pytest.raises(MenaiEvalError, match="struct=\\?"):
            menai.evaluate(
                "(let ((Point (struct (x y))) (p (Point 1 2))) (struct=? p 42))"
            )


# ---------------------------------------------------------------------------
# 12. First-class use of struct operations
# ---------------------------------------------------------------------------

class TestStructFirstClass:
    """Test struct operations used as first-class functions."""

    def test_struct_predicate_as_first_class_filter(self, menai):
        """struct? used as a first-class predicate with filter-list."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (items (list (Point 1 2) 42 (Point 3 4) "hello")))
          (filter-list struct? items))
        ''')
        assert result == '((Point 1 2) (Point 3 4))'

    def test_struct_predicate_stored_in_variable(self, menai):
        """struct? can be stored in a variable and called indirectly."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (p (Point 1 2))
              (pred struct?))
          (pred p))
        ''')
        assert result == '#t'

    def test_struct_type_predicate_as_first_class(self, menai):
        """struct-type? used as a first-class function."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (p (Point 1 2))
              (check struct-type?))
          (check Point p))
        ''')
        assert result == '#t'

    def test_struct_get_as_first_class(self, menai):
        """struct-get used as a first-class function passed to map-list."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (points (list (Point 1 2) (Point 3 4) (Point 5 6))))
          (map-list (lambda (p) (struct-get p 'x)) points))
        ''')
        assert result == '(1 3 5)'

    def test_struct_get_stored_in_variable(self, menai):
        """struct-get stored in a variable and called indirectly."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (p (Point 10 20))
              (getter struct-get))
          (getter p 'y))
        ''')
        assert result == '20'

    def test_struct_set_as_first_class(self, menai):
        """struct-set used as a first-class function."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (p (Point 1 2))
              (updater struct-set))
          (updater p 'x 99))
        ''')
        assert result == '(Point 99 2)'

    def test_struct_set_stored_in_variable(self, menai):
        """struct-set stored in a variable and used to update multiple structs."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (points (list (Point 1 2) (Point 3 4)))
              (updater struct-set))
          (map-list (lambda (p) (updater p 'x 0)) points))
        ''')
        assert result == '((Point 0 2) (Point 0 4))'

    def test_struct_eq_as_first_class(self, menai):
        """struct=? used as a first-class equality function."""
        result = menai.evaluate_and_format('''
        (let* ((Point (struct (x y)))
               (p1 (Point 1 2))
               (p2 (Point 1 2))
               (eq struct=?))
          (eq p1 p2))
        ''')
        assert result == '#t'

    def test_struct_neq_as_first_class(self, menai):
        """struct!=? used as a first-class inequality function."""
        result = menai.evaluate_and_format('''
        (let* ((Point (struct (x y)))
               (p1 (Point 1 2))
               (p2 (Point 3 4))
               (neq struct!=?))
          (neq p1 p2))
        ''')
        assert result == '#t'

    def test_struct_type_as_first_class(self, menai):
        """struct-type used as a first-class function."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (p (Point 1 2))
              (get-type struct-type))
          (struct-type-name (get-type p)))
        ''')
        assert result == '"Point"'

    def test_struct_type_name_as_first_class(self, menai):
        """struct-type-name used as a first-class function."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (Vec (struct (dx dy)))
              (name-of struct-type-name))
          (list (name-of Point) (name-of Vec)))
        ''')
        assert result == '("Point" "Vec")'

    def test_struct_fields_as_first_class(self, menai):
        """struct-fields used as a first-class function."""
        result = menai.evaluate_and_format('''
        (let ((Point (struct (x y)))
              (get-fields struct-fields))
          (get-fields Point))
        ''')
        assert result == '(x y)'

    def test_struct_operations_passed_to_higher_order_function(self, menai):
        """Struct operations can be passed as arguments to user-defined higher-order functions."""
        result = menai.evaluate_and_format('''
        (let* ((Point (struct (x y)))
               (apply-to (lambda (f a b) (f a b))))
          (apply-to struct=? (Point 1 2) (Point 1 2)))
        ''')
        assert result == '#t'


# ---------------------------------------------------------------------------
# 13. Dynamic struct type construction (struct type as runtime value)
# ---------------------------------------------------------------------------

class TestStructDynamicConstruction:
    """Test calling a struct type that is a runtime value rather than a statically-known name.

    When the compiler cannot see that a value is a struct type at compile time
    (e.g. it was retrieved from a dict, list, or returned from a lambda), it
    emits a CALL instruction rather than MAKE_STRUCT.  The VM must handle
    MenaiStructType in CALL and TAIL_CALL position.
    """

    def test_constructor_retrieved_from_dict(self, menai):
        """A struct type stored in a dict and retrieved at runtime can construct instances."""
        result = menai.evaluate_and_format('''
        (let* ((point (struct (x y)))
               (d     (dict (list "point" point)))
               (ctor  (dict-get d "point")))
          (ctor 3 4))
        ''')
        assert result == '(point 3 4)'

    def test_constructor_retrieved_from_list(self, menai):
        """A struct type stored in a list and retrieved at runtime can construct instances."""
        result = menai.evaluate_and_format('''
        (let* ((point (struct (x y)))
               (lst   (list point))
               (ctor  (list-first lst)))
          (ctor 5 6))
        ''')
        assert result == '(point 5 6)'

    def test_constructor_returned_from_lambda(self, menai):
        """A struct type returned from a lambda call can construct instances."""
        result = menai.evaluate_and_format('''
        (let* ((point    (struct (x y)))
               (get-ctor (lambda () point))
               (ctor     (get-ctor)))
          (ctor 7 8))
        ''')
        assert result == '(point 7 8)'

    def test_field_access_on_dynamically_constructed_instance(self, menai):
        """struct-get works on an instance built via a dynamically-called constructor."""
        result = menai.evaluate_and_format('''
        (let* ((point (struct (x y)))
               (d     (dict (list "point" point)))
               (ctor  (dict-get d "point"))
               (p     (ctor 10 20)))
          (struct-get p 'y))
        ''')
        assert result == '20'

    def test_type_predicate_on_dynamically_constructed_instance(self, menai):
        """struct-type? works correctly on an instance built via a dynamically-called constructor."""
        result = menai.evaluate_and_format('''
        (let* ((point (struct (x y)))
               (d     (dict (list "point" point)))
               (ctor  (dict-get d "point"))
               (p     (ctor 1 2)))
          (struct-type? point p))
        ''')
        assert result == '#t'

    def test_constructor_in_tail_position(self, menai):
        """A dynamically-retrieved struct constructor called in tail position works correctly."""
        result = menai.evaluate_and_format('''
        (let* ((point (struct (x y)))
               (d     (dict (list "point" point)))
               (make  (lambda (a b)
                        (let ((ctor (dict-get d "point")))
                          (ctor a b)))))
          (make 11 22))
        ''')
        assert result == '(point 11 22)'

    def test_constructor_used_with_map_list(self, menai):
        """A dynamically-retrieved struct constructor can be passed to map-list."""
        result = menai.evaluate_and_format('''
        (let* ((point (struct (x y)))
               (d     (dict (list "point" point)))
               (ctor  (dict-get d "point"))
               (pairs (list (list 1 2) (list 3 4) (list 5 6))))
          (map-list (lambda (pair) (ctor (list-ref pair 0) (list-ref pair 1))) pairs))
        ''')
        assert result == '((point 1 2) (point 3 4) (point 5 6))'

    def test_wrong_arity_dynamic_constructor_too_few(self, menai):
        """Calling a dynamically-retrieved struct constructor with too few args raises an error."""
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('''
            (let* ((point (struct (x y)))
                   (d     (dict (list "point" point)))
                   (ctor  (dict-get d "point")))
              (ctor 1))
            ''')

    def test_wrong_arity_dynamic_constructor_too_many(self, menai):
        """Calling a dynamically-retrieved struct constructor with too many args raises an error."""
        with pytest.raises(MenaiEvalError, match="wrong number of arguments"):
            menai.evaluate('''
            (let* ((point (struct (x y)))
                   (d     (dict (list "point" point)))
                   (ctor  (dict-get d "point")))
              (ctor 1 2 3))
            ''')
