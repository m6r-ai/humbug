"""Classification of an authorization request, used by the approval policy."""

from enum import Enum, auto


class AIToolApprovalKind(Enum):
    """
    The kind of change a tool is asking permission to make.

    A tool states what a request is, not whether the user may be spared seeing it:
    the policy that decides that reads this classification alongside its own
    conditions.  The distinction is a property of the change rather than of the tool,
    on the same terms as the persistent/ephemeral dividing line in ADR-0007: a change
    is reversible when the file change journal records it, so a request whose only
    effect is such a change is a candidate for a standing approval, and any other
    request is not.
    """

    STANDARD = auto()
    """A request a human must decide, whatever standing approvals are in force."""

    MINDSPACE_FILE_CHANGE = auto()
    """A change the file change journal records, so one it can reverse."""
