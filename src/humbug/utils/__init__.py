"""Utility functions for the Humbug application."""

def sanitize_input(text: str) -> str:
    """Strip control characters from input text, preserving newlines."""
    return ''.join(char for char in text if char == '\n' or (ord(char) >= 32 and ord(char) != 127))

def printable_len(s: str) -> int:
    """
    Calculate the printable length of a string by ignoring ANSI escape codes.

    Parameters:
    - s (str): The input string containing ANSI escape codes.

    Returns:
    - int: The length of the string in terms of printable characters.
    """
    length = 0
    i = 0
    n = len(s)

    while i < n:
        if s[i] == '\x1b':  # ESC character
            if i + 1 < n and s[i + 1] == '[':
                # Start of ANSI escape sequence
                i += 2  # Skip ESC and '['

                # Skip until the end of the escape sequence (letter)
                while i < n and not ('@' <= s[i] <= '~'):
                    i += 1

                if i < n and ('@' <= s[i] <= '~'):
                    i += 1  # Skip the final byte of the escape sequence
            else:
                # ESC not followed by '[', count as printable
                length += 1
                i += 1
        else:
            # Regular printable character
            length += 1
            i += 1

    return length
