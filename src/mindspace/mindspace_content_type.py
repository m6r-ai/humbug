from enum import Enum, auto


class MindspaceContentType(Enum):
    """
    Classification of mindspace file content for search and indexing purposes.

    This is distinct from the Qt sidebar view types — it describes what a file
    contains, not how it is displayed.
    """
    CONVERSATIONS = auto()
    FILES = auto()
