"""Controller for the Humbug onboarding product tour."""

import logging
from typing import TYPE_CHECKING

from PySide6.QtCore import QEvent, QObject

from desktop.language.language_manager import LanguageManager
from desktop.tour.tour_overlay import TourOverlay
from desktop.tour.tour_step import TourStep
from desktop.tour.tour_steps import build_tour_steps
from desktop.user.onboarding_tour_status import OnboardingTourStatus
from desktop.user.user_manager import UserError, UserManager

if TYPE_CHECKING:
    from desktop.main_window import MainWindow

CURRENT_TOUR_VERSION = 1


class TourController(QObject):
    """
    Drives the onboarding product tour.

    Owns the current step index, resolves each step's spotlight target,
    persists tour completion state via UserManager, and creates or tears
    down the presentational TourOverlay as the tour starts and finishes.
    """

    def __init__(self, main_window: "MainWindow", steps: list[TourStep] | None = None) -> None:
        super().__init__(main_window)
        self._logger = logging.getLogger("TourController")
        self._main_window = main_window
        self._user_manager = UserManager()
        self._language_manager = LanguageManager()
        self._steps = steps if steps is not None else build_tour_steps()
        self._index = 0
        self._overlay: TourOverlay | None = None

    def maybe_start_on_launch(self) -> None:
        """Start the tour automatically if this user has not seen the current tour version."""
        settings = self._user_manager.settings()
        if settings.onboarding_tour_version >= CURRENT_TOUR_VERSION:
            return

        self.start()

    def start(self) -> None:
        """Start (or restart) the tour from the first step."""
        self._index = 0

        if self._overlay is None:
            central_widget = self._main_window.centralWidget()
            self._overlay = TourOverlay(central_widget)
            self._overlay.setGeometry(central_widget.rect())
            self._overlay.next_requested.connect(self._on_next)
            self._overlay.back_requested.connect(self._on_back)
            self._overlay.skip_requested.connect(self._on_skip)
            self._main_window.installEventFilter(self)

        self._show_current_step()

    def eventFilter(self, watched: QObject, event: QEvent) -> bool:
        if self._overlay is not None and watched is self._main_window and event.type() == QEvent.Type.Resize:
            self._overlay.setGeometry(self._main_window.centralWidget().rect())
            self._overlay.refresh_layout()

        return super().eventFilter(watched, event)

    def _show_current_step(self) -> None:
        """Render the overlay for the step at the current index."""
        assert self._overlay is not None
        strings = self._language_manager.strings()
        step = self._steps[self._index]
        is_last = self._index == len(self._steps) - 1

        self._overlay.show_step(
            title=step.title(strings),
            description=step.description(strings),
            step_label=strings.tour_step_of.format(self._index + 1, len(self._steps)),
            back_label=strings.tour_back_button,
            skip_label=strings.tour_skip_button,
            next_label=strings.tour_finish_button if is_last else strings.tour_next_button,
            can_go_back=self._index > 0,
            spotlight_target=step.resolve_target(self._main_window),
        )

    def _on_next(self) -> None:
        """Advance to the next step, or complete the tour if this was the last one."""
        if self._index >= len(self._steps) - 1:
            self._finish(OnboardingTourStatus.COMPLETED)
            return

        self._index += 1
        self._show_current_step()

    def _on_back(self) -> None:
        """Return to the previous step, if there is one."""
        if self._index == 0:
            return

        self._index -= 1
        self._show_current_step()

    def _on_skip(self) -> None:
        """End the tour early at the user's request."""
        self._finish(OnboardingTourStatus.SKIPPED)

    def _finish(self, status: OnboardingTourStatus) -> None:
        """Fade out and tear down the overlay, and persist the tour's final status."""
        overlay = self._overlay
        self._overlay = None
        if overlay is not None:
            self._main_window.removeEventFilter(self)
            overlay.fade_out(lambda: self._cleanup_overlay(overlay))

        settings = self._user_manager.settings()
        settings.onboarding_tour_status = status
        settings.onboarding_tour_version = CURRENT_TOUR_VERSION
        try:
            self._user_manager.update_settings(settings)

        except UserError as e:
            self._logger.warning("Failed to persist onboarding tour state: %s", str(e))

    def _cleanup_overlay(self, overlay: TourOverlay) -> None:
        """Hide and destroy an overlay once its fade-out animation has finished."""
        overlay.hide()
        overlay.deleteLater()
