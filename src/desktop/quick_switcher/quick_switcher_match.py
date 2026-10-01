"""Fuzzy subsequence scoring used to rank Quick Switcher candidates."""

_WORD_BOUNDARY_CHARS = "/\\ _-."


def fuzzy_match_score(query: str, text: str) -> int | None:
    """
    Score how well the characters of `query` appear, in order, within `text`.

    Matching is case-insensitive and a subsequence match: every character of
    `query` must appear in `text` in the same relative order, though not
    necessarily consecutively. Returns None if no such match exists.

    Higher scores rank better matches first: consecutive runs of matched
    characters and matches starting right after a path separator, space,
    underscore, hyphen, or dot score higher than characters scattered deep
    inside a word, so "qs" ranks "quick_switcher.py" above "sequesteredfile".
    """
    if not query:
        return 0

    lowered_query = query.casefold()
    lowered_text = text.casefold()

    score = 0
    search_from = 0
    run_length = 0
    for character in lowered_query:
        found_at = lowered_text.find(character, search_from)
        if found_at == -1:
            return None

        if found_at == search_from:
            run_length += 1
            score += 4 + run_length

        else:
            run_length = 0
            score -= min(found_at - search_from, 5)

        if found_at == 0 or lowered_text[found_at - 1] in _WORD_BOUNDARY_CHARS:
            score += 8

        search_from = found_at + 1

    return score
