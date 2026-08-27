from enum import Enum, auto


class MindspaceContentType(Enum):
    """
    Classification of mindspace file content for search and indexing purposes.
    """
    CONVERSATIONS = auto()
    FILES = auto()
