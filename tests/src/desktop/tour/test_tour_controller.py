"""Tests for the onboarding tour controller (step advancement and persistence)."""
# pylint: disable=protected-access, missing-class-docstring, missing-function-docstring
# pylint: disable=redefined-outer-name, unused-argument

import pytest
from PySide6.QtWidgets import QMainWindow, QWidget

from desktop.tour.tour_controller import CURRENT_TOUR_VERSION, TourController
from desktop.tour.tour_step import TourStep
from desktop.user.onboarding_tour_status import OnboardingTourStatus
from desktop.user.user_settings import UserSettings


class _FakeUserManager:
    """Stand-in for the UserManager singleton that never touches disk."""

    def __init__(self) -> None:
        self._settings = UserSettings.create_default()
        self.update_calls: list[UserSettings] = []

    def settings(self) -> UserSettings:
        return self._settings

    def update_settings(self, settings: UserSettings) -> None:
        self.update_calls.append(settings)
        self._settings = settings


def make_steps(count=3):
    return [
        TourStep(
            title=lambda strings, i=i: f"title {i}",
            description=lambda strings, i=i: f"description {i}",
            resolve_target=lambda _main_window: None,
        )
        for i in range(count)
    ]


@pytest.fixture
def fake_user_manager(monkeypatch):
    fake = _FakeUserManager()
    monkeypatch.setattr("desktop.tour.tour_controller.UserManager", lambda: fake)
    return fake


@pytest.fixture
def main_window(qapp):
    window = QMainWindow()
    window.setCentralWidget(QWidget())
    window.resize(800, 600)
    yield window
    window.deleteLater()
    qapp.processEvents()


class TestStartAndAdvance:
    def test_start_shows_first_step(self, fake_user_manager, main_window):
        controller = TourController(main_window, steps=make_steps())
        controller.start()
        assert controller._overlay is not None
        assert controller._overlay._title_label.text() == "title 0"

    def test_next_advances_step(self, fake_user_manager, main_window, settle):
        controller = TourController(main_window, steps=make_steps())
        controller.start()
        controller._on_next()
        settle(800)
        assert controller._overlay._title_label.text() == "title 1"

    def test_back_returns_to_previous_step(self, fake_user_manager, main_window, settle):
        controller = TourController(main_window, steps=make_steps())
        controller.start()
        controller._on_next()
        settle(800)
        controller._on_back()
        settle(800)
        assert controller._overlay._title_label.text() == "title 0"

    def test_back_on_first_step_is_a_no_op(self, fake_user_manager, main_window):
        controller = TourController(main_window, steps=make_steps())
        controller.start()
        controller._on_back()
        assert controller._overlay._title_label.text() == "title 0"

    def test_next_on_last_step_completes_and_removes_overlay(self, fake_user_manager, main_window):
        controller = TourController(main_window, steps=make_steps(count=2))
        controller.start()
        controller._on_next()
        controller._on_next()
        assert controller._overlay is None
        assert fake_user_manager.settings().onboarding_tour_status == OnboardingTourStatus.COMPLETED
        assert fake_user_manager.settings().onboarding_tour_version == CURRENT_TOUR_VERSION


class TestSkip:
    def test_skip_removes_overlay_and_persists_skipped_status(self, fake_user_manager, main_window):
        controller = TourController(main_window, steps=make_steps())
        controller.start()
        controller._on_skip()
        assert controller._overlay is None
        assert fake_user_manager.settings().onboarding_tour_status == OnboardingTourStatus.SKIPPED
        assert fake_user_manager.settings().onboarding_tour_version == CURRENT_TOUR_VERSION


class TestAutoStartOnLaunch:
    def test_starts_when_never_seen(self, fake_user_manager, main_window):
        fake_user_manager.settings().onboarding_tour_version = 0
        controller = TourController(main_window, steps=make_steps())
        controller.maybe_start_on_launch()
        assert controller._overlay is not None

    def test_does_not_start_when_already_seen(self, fake_user_manager, main_window):
        fake_user_manager.settings().onboarding_tour_version = CURRENT_TOUR_VERSION
        controller = TourController(main_window, steps=make_steps())
        controller.maybe_start_on_launch()
        assert controller._overlay is None


class TestRestart:
    def test_take_a_tour_restarts_from_first_step(self, fake_user_manager, main_window):
        controller = TourController(main_window, steps=make_steps())
        controller.start()
        controller._on_next()
        controller._on_skip()
        assert controller._overlay is None

        controller.start()
        assert controller._overlay is not None
        assert controller._overlay._title_label.text() == "title 0"
