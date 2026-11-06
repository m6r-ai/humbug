"""
Matching of paths against a list of glob entries.

This is the rule behind the filesystem tool's file access lists and behind what the
change scanner leaves out of a mindspace.  Both callers use this package rather than
each other, so the two cannot say different things about what an entry matches.

The entries are the user's: glob patterns, one per line, in the terms stated on
:class:`path_matching.path_patterns.PathPatterns`.
"""

from path_matching.path_patterns import PathPatterns

__all__ = ['PathPatterns']
