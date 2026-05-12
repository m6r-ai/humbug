from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any


@dataclass
class PDFObjectRef:
    """A reference to an indirect PDF object."""

    obj_num: int
    gen_num: int

    def __hash__(self) -> int:
        return hash((self.obj_num, self.gen_num))


@dataclass
class PDFStream:
    """A decoded PDF stream object."""

    data: bytes
    attrs: dict[str, Any]


@dataclass
class PDFXRefEntry:
    """One entry from a PDF cross-reference table."""

    offset: int
    gen_num: int
    in_use: bool


@dataclass
class PDFDocument:
    """Top-level container for a parsed PDF document."""

    objects: dict[int, Any] = field(default_factory=dict)
    xref: dict[int, PDFXRefEntry] = field(default_factory=dict)
    trailer: dict[str, Any] = field(default_factory=dict)

    def get_object(self, ref: PDFObjectRef) -> Any:
        """Resolve an indirect object reference, following chains up to a maximum depth."""
        max_depth = 16
        seen: set[int] = set()

        current: Any = ref
        for _ in range(max_depth):
            if not isinstance(current, PDFObjectRef):
                return current

            if current.obj_num in seen:
                return None

            seen.add(current.obj_num)
            current = self.objects.get(current.obj_num)

        return None
