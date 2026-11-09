"""Owner of the Mission Control service for the open mindspace."""

from mindspace.mindspace import Mindspace
from mission import MissionService


class MissionServiceHolder:
    """
    Keeps one MissionService for the mindspace that is open.

    Every Mission Control tab shares the service, so a tab moved between
    columns or reopened sees the same activity.  A service for a different
    mindspace is stopped before a new one is created.
    """

    _instance: "MissionServiceHolder | None" = None

    def __new__(cls) -> "MissionServiceHolder":
        """Create or return the singleton instance."""
        if cls._instance is None:
            cls._instance = super().__new__(cls)

        return cls._instance

    def __init__(self) -> None:
        """Initialise if not already done."""
        if not hasattr(self, "_initialized"):
            self._service: MissionService | None = None
            self._initialized = True

    def service_for(self, mindspace: Mindspace) -> MissionService:
        """
        Return the running service for a mindspace, starting it if needed.

        Args:
            mindspace: The open mindspace

        Returns:
            The service
        """
        if self._service is not None and self._service.mindspace_path() != mindspace.mindspace_path():
            self._service.stop()
            self._service = None

        if self._service is None:
            self._service = MissionService(mindspace)
            self._service.start()

        return self._service

    def current_service(self) -> MissionService | None:
        """Return the running service, if a mindspace's missions are being followed."""
        return self._service

    def release(self) -> None:
        """Stop the service once no Mission Control tab needs it."""
        if self._service is not None:
            self._service.stop()
            self._service = None
