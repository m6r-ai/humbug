"""Tests for fuzzy_match_score, the Quick Switcher's ranking function."""

from desktop.quick_switcher.quick_switcher_match import fuzzy_match_score


class TestFuzzyMatchScore:
    def test_empty_query_matches_everything_with_zero_score(self):
        assert fuzzy_match_score("", "anything.py") == 0

    def test_non_subsequence_does_not_match(self):
        assert fuzzy_match_score("xyz", "quick_switcher.py") is None

    def test_out_of_order_characters_do_not_match(self):
        assert fuzzy_match_score("wq", "quick_switcher.py") is None

    def test_case_insensitive_subsequence_matches(self):
        assert fuzzy_match_score("QSW", "quick_switcher.py") is not None

    def test_word_boundary_match_scores_higher_than_mid_word(self):
        boundary_score = fuzzy_match_score("s", "quick_switcher.py")  # 's' right after '_'
        mid_word_score = fuzzy_match_score("t", "quick_switcher.py")  # 't' mid-word
        assert boundary_score is not None
        assert mid_word_score is not None
        assert boundary_score > mid_word_score

    def test_consecutive_run_scores_higher_than_scattered_match(self):
        consecutive = fuzzy_match_score("qui", "quick_switcher.py")
        scattered = fuzzy_match_score("qch", "quick_switcher.py")
        assert consecutive is not None
        assert scattered is not None
        assert consecutive > scattered

    def test_prefix_match_outranks_a_match_found_deeper_in_the_text(self):
        prefix = fuzzy_match_score("qui", "quick_switcher.py")
        deeper = fuzzy_match_score("qui", "requickened.py")
        assert prefix is not None
        assert deeper is not None
        assert prefix > deeper
