"""Definition of a single step of the onboarding product tour."""

from collections.abc import Callable
from dataclasses import dataclass
from typing import TYPE_CHECKING

from PySide6.QtWidgets import QWidget

from desktop.language.language_strings import LanguageStrings

if TYPE_CHECKING:
    from desktop.main_window import MainWindow


@dataclass
class TourStep:
    """
    Describes one step of the onboarding tour.

    resolve_target is called each time the step is shown and returns the
    widget to spotlight, or None to show the step as a centred card with
    no spotlight (used for the welcome step, and as a graceful fallback
    when the intended target cannot be found).
    """
    title: Callable[[LanguageStrings], str]
    description: Callable[[LanguageStrings], str]
    resolve_target: Callable[["MainWindow"], QWidget | None]
