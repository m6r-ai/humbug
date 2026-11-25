"""Tells the user when a mission needs them or has finished, while they are looking elsewhere."""

from collections.abc import Callable

from PySide6.QtCore import QEvent, QObject, QTimer
from PySide6.QtGui import QIcon
from PySide6.QtWidgets import QApplication, QSystemTrayIcon, QWidget

from desktop.language.language_manager import LanguageManager
from mission import MissionService
from mission.mission_alerts import MissionAlert, MissionAlertKind, MissionAlertWatcher


# Changes come in bursts while a mission works, so they are looked at together.
_CHECK_DELAY_MS = 400

_MESSAGE_DURATION_MS = 8000


class MissionNotifier(QObject):
    """
    Raises Mission Control's alerts outside it.

    Every alert marks Mission Control as updated.  While the window is in the background an
    alert also asks for the user's attention, by bouncing the dock icon or flashing the
    taskbar, and shows a system notification where the platform offers them; clicking it
    opens the mission.  The notification icon is only shown while an alert is unseen.
    """

    def __init__(
        self,
        window: QWidget,
        show_mission: Callable[[str], None],
        mark_updated: Callable[[], None]
    ) -> None:
        """
        Initialise the notifier for a main window.

        Args:
            window: The main window, whose being in the background decides how loudly to alert
            show_mission: Opens Mission Control on a mission
            mark_updated: Marks Mission Control as having something new
        """
        super().__init__(window)
        self._window = window
        self._show_mission = show_mission
        self._mark_updated = mark_updated
        self._service: MissionService | None = None
        self._watcher = MissionAlertWatcher()
        self._tray: QSystemTrayIcon | None = None
        self._clicked_mission_id = ""
        self._check_timer = QTimer(self)
        self._check_timer.setSingleShot(True)
        self._check_timer.timeout.connect(self._check)
        window.installEventFilter(self)

    def follow(self, service: MissionService | None) -> None:
        """
        Watch a service's missions, or stop watching when given None.

        What is already waiting when watching begins is not announced.
        """
        if service is self._service:
            return

        if self._service is not None:
            self._service.remove_listener(self._on_changed)

        self._service = service
        self._watcher = MissionAlertWatcher()
        self._check_timer.stop()
        if service is not None:
            service.add_listener(self._on_changed)
            self._watcher.changes(service)

    def eventFilter(self, watched: QObject, event: QEvent) -> bool:
        """Put the notification icon away once the user is back in the window."""
        if watched is self._window and event.type() == QEvent.Type.WindowActivate and self._tray is not None:
            self._tray.hide()

        return super().eventFilter(watched, event)

    def _on_changed(self) -> None:
        """Look for news shortly, once the current burst of changes is over."""
        if not self._check_timer.isActive():
            self._check_timer.start(_CHECK_DELAY_MS)

    def _check(self) -> None:
        """Raise whatever has newly happened."""
        if self._service is None:
            return

        alerts = self._watcher.changes(self._service)
        if not alerts:
            return

        self._mark_updated()
        if self._window.isActiveWindow():
            return

        QApplication.alert(self._window)
        title, message = self._describe(alerts[-1])
        if len(alerts) > 1:
            message = f"{message}  (+{len(alerts) - 1})"

        self._notify(alerts[-1].mission_id, title, message)

    def _describe(self, alert: MissionAlert) -> tuple[str, str]:
        """Return the title and message that tell the user about an alert."""
        strings = LanguageManager().strings()
        title = {
            MissionAlertKind.DECISION: strings.mission_notify_decision,
            MissionAlertKind.COMPLETED: strings.mission_notify_completed,
            MissionAlertKind.REPLIED: strings.mission_notify_replied,
            MissionAlertKind.FAILED: strings.mission_notify_failed,
            MissionAlertKind.STUCK: strings.mission_notify_stuck,
        }[alert.kind]
        message = f"{alert.mission_title}: {alert.detail}" if alert.detail else alert.mission_title
        return title, message

    def _notify(self, mission_id: str, title: str, message: str) -> None:
        """Show a system notification, where the platform has them."""
        if not QSystemTrayIcon.isSystemTrayAvailable() or not QSystemTrayIcon.supportsMessages():
            return

        if self._tray is None:
            icon = self._window.windowIcon()
            if icon.isNull():
                icon = QApplication.windowIcon()

            if icon.isNull():
                return

            self._tray = QSystemTrayIcon(QIcon(icon), self)
            self._tray.messageClicked.connect(self._on_message_clicked)
            self._tray.activated.connect(self._on_message_clicked)

        self._clicked_mission_id = mission_id
        self._tray.show()
        self._tray.showMessage(title, message, QSystemTrayIcon.MessageIcon.Information, _MESSAGE_DURATION_MS)

    def _on_message_clicked(self, *_args: object) -> None:
        """Bring the window forward on the mission the notification was about."""
        if self._window.isMinimized():
            self._window.showNormal()

        self._window.raise_()
        self._window.activateWindow()
        if self._clicked_mission_id:
            self._show_mission(self._clicked_mission_id)
