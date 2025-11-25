"""Tests for diff exceptions."""

import pytest

from diff.diff_exceptions import (
    DiffError,
    DiffParseError,
    DiffMatchError,
    DiffApplicationError,
    DiffValidationError
)


class TestDiffError:
    """Test base DiffError exception."""

    def test_create_simple_error(self):
        """Test creating a simple error without details."""
        error = DiffError("Something went wrong")
        assert str(error) == "Something went wrong"
        assert error.error_details is None

    def test_create_error_with_details(self):
        """Test creating an error with details."""
        details = {
            'phase': 'test',
            'line': 42,
            'reason': 'test reason'
        }
        error = DiffError("Error occurred", error_details=details)

        assert str(error) == "Error occurred"
        assert error.error_details is not None
        assert error.error_details['phase'] == 'test'
        assert error.error_details['line'] == 42
        assert error.error_details['reason'] == 'test reason'

    def test_error_is_exception(self):
        """Test that DiffError is an Exception."""
        error = DiffError("test")
        assert isinstance(error, Exception)

    def test_raise_and_catch(self):
        """Test raising and catching DiffError."""
        with pytest.raises(DiffError) as exc_info:
            raise DiffError("test error")

        assert "test error" in str(exc_info.value)

    def test_error_details_can_be_none(self):
        """Test that error_details can be explicitly None."""
        error = DiffError("test", error_details=None)
        assert error.error_details is None


class TestDiffParseError:
    """Test DiffParseError exception."""

    def test_inherits_from_diff_error(self):
        """Test that DiffParseError inherits from DiffError."""
        error = DiffParseError("parse error")
        assert isinstance(error, DiffError)
        assert isinstance(error, Exception)

    def test_create_parse_error(self):
        """Test creating a parse error."""
        error = DiffParseError("Invalid hunk header")
        assert "Invalid hunk header" in str(error)

    def test_parse_error_with_details(self):
        """Test parse error with detailed information."""
        details = {
            'line_number': 5,
            'line_content': '@@ invalid @@',
            'expected_format': '@@ -old_start,old_count +new_start,new_count @@'
        }
        error = DiffParseError("Invalid format", error_details=details)

        assert error.error_details['line_number'] == 5
        assert '@@ invalid @@' in error.error_details['line_content']

    def test_raise_and_catch_parse_error(self):
        """Test raising and catching DiffParseError."""
        with pytest.raises(DiffParseError) as exc_info:
            raise DiffParseError("parse failed")

        assert "parse failed" in str(exc_info.value)

    def test_catch_as_base_error(self):
        """Test that DiffParseError can be caught as DiffError."""
        with pytest.raises(DiffError):
            raise DiffParseError("parse failed")


class TestDiffMatchError:
    """Test DiffMatchError exception."""

    def test_inherits_from_diff_error(self):
        """Test that DiffMatchError inherits from DiffError."""
        error = DiffMatchError("match error")
        assert isinstance(error, DiffError)
        assert isinstance(error, Exception)

    def test_create_match_error(self):
        """Test creating a match error."""
        error = DiffMatchError("Could not locate hunk")
        assert "Could not locate hunk" in str(error)

    def test_match_error_with_details(self):
        """Test match error with detailed information."""
        details = {
            'failed_hunk': 2,
            'expected_location': 100,
            'best_match': {
                'location': 105,
                'confidence': 0.6
            },
            'expected_context': ['line 1', 'line 2'],
            'actual_context': ['line 1', 'different line']
        }
        error = DiffMatchError("Low confidence match", error_details=details)

        assert error.error_details['failed_hunk'] == 2
        assert error.error_details['best_match']['confidence'] == 0.6

    def test_raise_and_catch_match_error(self):
        """Test raising and catching DiffMatchError."""
        with pytest.raises(DiffMatchError) as exc_info:
            raise DiffMatchError("match failed")

        assert "match failed" in str(exc_info.value)


class TestDiffApplicationError:
    """Test DiffApplicationError exception."""

    def test_inherits_from_diff_error(self):
        """Test that DiffApplicationError inherits from DiffError."""
        error = DiffApplicationError("application error")
        assert isinstance(error, DiffError)
        assert isinstance(error, Exception)

    def test_create_application_error(self):
        """Test creating an application error."""
        error = DiffApplicationError("Failed to apply hunk")
        assert "Failed to apply hunk" in str(error)

    def test_application_error_with_details(self):
        """Test application error with detailed information."""
        details = {
            'phase': 'application',
            'hunk_number': 3,
            'location': 50,
            'reason': 'Document modification failed'
        }
        error = DiffApplicationError("Application failed", error_details=details)

        assert error.error_details['phase'] == 'application'
        assert error.error_details['hunk_number'] == 3

    def test_raise_and_catch_application_error(self):
        """Test raising and catching DiffApplicationError."""
        with pytest.raises(DiffApplicationError) as exc_info:
            raise DiffApplicationError("application failed")

        assert "application failed" in str(exc_info.value)


class TestDiffValidationError:
    """Test DiffValidationError exception."""

    def test_inherits_from_diff_error(self):
        """Test that DiffValidationError inherits from DiffError."""
        error = DiffValidationError("validation error")
        assert isinstance(error, DiffError)
        assert isinstance(error, Exception)

    def test_create_validation_error(self):
        """Test creating a validation error."""
        error = DiffValidationError("Overlapping hunks detected")
        assert "Overlapping hunks" in str(error)

    def test_validation_error_with_details(self):
        """Test validation error with detailed information."""
        details = {
            'phase': 'validation',
            'reason': 'Overlapping hunks',
            'hunk1_range': [10, 15],
            'hunk2_range': [14, 20],
            'suggestion': 'Regenerate diff with non-overlapping changes'
        }
        error = DiffValidationError("Validation failed", error_details=details)

        assert error.error_details['reason'] == 'Overlapping hunks'
        assert error.error_details['hunk1_range'] == [10, 15]
        assert 'suggestion' in error.error_details

    def test_raise_and_catch_validation_error(self):
        """Test raising and catching DiffValidationError."""
        with pytest.raises(DiffValidationError) as exc_info:
            raise DiffValidationError("validation failed")

        assert "validation failed" in str(exc_info.value)


class TestExceptionHierarchy:
    """Test exception hierarchy and relationships."""

    def test_all_inherit_from_diff_error(self):
        """Test that all exceptions inherit from DiffError."""
        assert issubclass(DiffParseError, DiffError)
        assert issubclass(DiffMatchError, DiffError)
        assert issubclass(DiffApplicationError, DiffError)
        assert issubclass(DiffValidationError, DiffError)

    def test_all_inherit_from_exception(self):
        """Test that all exceptions inherit from Exception."""
        assert issubclass(DiffError, Exception)
        assert issubclass(DiffParseError, Exception)
        assert issubclass(DiffMatchError, Exception)
        assert issubclass(DiffApplicationError, Exception)
        assert issubclass(DiffValidationError, Exception)

    def test_catch_all_as_diff_error(self):
        """Test that all specific errors can be caught as DiffError."""
        exceptions = [
            DiffParseError("parse"),
            DiffMatchError("match"),
            DiffApplicationError("application"),
            DiffValidationError("validation")
        ]

        for exc in exceptions:
            with pytest.raises(DiffError):
                raise exc

    def test_specific_catch_doesnt_catch_others(self):
        """Test that specific error types don't catch each other."""
        # DiffParseError doesn't catch DiffMatchError
        with pytest.raises(DiffMatchError):
            try:
                raise DiffMatchError("match error")
            except DiffParseError:
                pytest.fail("Should not catch DiffMatchError as DiffParseError")

        # DiffMatchError doesn't catch DiffApplicationError
        with pytest.raises(DiffApplicationError):
            try:
                raise DiffApplicationError("application error")
            except DiffMatchError:
                pytest.fail("Should not catch DiffApplicationError as DiffMatchError")

    def test_error_details_preserved_through_hierarchy(self):
        """Test that error_details work for all exception types."""
        details = {'test': 'value'}

        errors = [
            DiffError("test", error_details=details),
            DiffParseError("test", error_details=details),
            DiffMatchError("test", error_details=details),
            DiffApplicationError("test", error_details=details),
            DiffValidationError("test", error_details=details)
        ]

        for error in errors:
            assert error.error_details == details
            assert error.error_details['test'] == 'value'
