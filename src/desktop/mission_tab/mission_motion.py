"""Whether Mission Control animates, shared by everything it draws."""


class MissionMotion:
    """
    Holds the user's choice to reduce motion in Mission Control.

    With motion reduced nothing glides, fades, pulses, spins, flies or types itself
    out: everything is drawn in its final state straight away.  The choice is saved
    by the mission service; this only lets every drawn part read it.
    """

    _instance: "MissionMotion | None" = None

    def __new__(cls) -> "MissionMotion":
        """Create or return the singleton instance."""
        if cls._instance is None:
            cls._instance = super().__new__(cls)

        return cls._instance

    def __init__(self) -> None:
        """Initialise if not already done."""
        if not hasattr(self, "_initialized"):
            self._reduced = False
            self._initialized = True

    def reduced(self) -> bool:
        """Return whether motion is reduced."""
        return self._reduced

    def set_reduced(self, reduced: bool) -> None:
        """
        Choose whether motion is reduced.

        Args:
            reduced: True to draw everything still
        """
        self._reduced = reduced
