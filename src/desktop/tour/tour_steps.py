"""Centrally configurable list of onboarding tour steps for Humbug."""

from typing import TYPE_CHECKING

from PySide6.QtWidgets import QWidget

from desktop.conversation_tab.conversation_tab import ConversationTab
from desktop.tour.tour_step import TourStep

if TYPE_CHECKING:
    from desktop.main_window import MainWindow


def _resolve_start_here(main_window: "MainWindow") -> QWidget | None:
    """Spotlight the mindspace picker button, the first thing a new user should open."""
    return main_window.sidebar_manager().header_widget()


def _resolve_open_conversation(main_window: "MainWindow") -> QWidget | None:
    """Spotlight the Conversations rail button, used to open or start a conversation."""
    return main_window.sidebar_manager().panel_button("conversations")


def _resolve_workspace(main_window: "MainWindow") -> QWidget | None:
    """Spotlight the tab area where conversations and other tabs are shown."""
    return main_window.tab_manager()


def _resolve_key_action(main_window: "MainWindow") -> QWidget | None:
    """Spotlight the message submit button, falling back to the workspace if no conversation is open."""
    tab = main_window.tab_manager().get_current_tab()
    if isinstance(tab, ConversationTab):
        submit_button = tab.conversation_widget().submit_button()
        if submit_button is not None:
            return submit_button

    return main_window.tab_manager()


def _resolve_output(main_window: "MainWindow") -> QWidget | None:
    """Spotlight the conversation history area, falling back to the workspace if no conversation is open."""
    tab = main_window.tab_manager().get_current_tab()
    if isinstance(tab, ConversationTab):
        return tab.conversation_widget().scroll_area()

    return main_window.tab_manager()


def _resolve_navigation(main_window: "MainWindow") -> QWidget | None:
    """Spotlight the sidebar icon rail used to switch between panels."""
    return main_window.sidebar_manager().rail_widget()


def _resolve_help(main_window: "MainWindow") -> QWidget | None:
    """Spotlight the settings button, mentioned alongside the tour replay entry point."""
    return main_window.sidebar_manager().settings_button()


def build_tour_steps() -> list[TourStep]:
    """Build the ordered list of onboarding tour steps."""
    return [
        TourStep(
            title=lambda strings: strings.tour_welcome_title,
            description=lambda strings: strings.tour_welcome_description,
            resolve_target=lambda _main_window: None,
        ),
        TourStep(
            title=lambda strings: strings.tour_start_here_title,
            description=lambda strings: strings.tour_start_here_description,
            resolve_target=_resolve_start_here,
        ),
        TourStep(
            title=lambda strings: strings.tour_open_conversation_title,
            description=lambda strings: strings.tour_open_conversation_description,
            resolve_target=_resolve_open_conversation,
        ),
        TourStep(
            title=lambda strings: strings.tour_workspace_title,
            description=lambda strings: strings.tour_workspace_description,
            resolve_target=_resolve_workspace,
        ),
        TourStep(
            title=lambda strings: strings.tour_key_action_title,
            description=lambda strings: strings.tour_key_action_description,
            resolve_target=_resolve_key_action,
        ),
        TourStep(
            title=lambda strings: strings.tour_output_title,
            description=lambda strings: strings.tour_output_description,
            resolve_target=_resolve_output,
        ),
        TourStep(
            title=lambda strings: strings.tour_navigation_title,
            description=lambda strings: strings.tour_navigation_description,
            resolve_target=_resolve_navigation,
        ),
        TourStep(
            title=lambda strings: strings.tour_help_title,
            description=lambda strings: strings.tour_help_description,
            resolve_target=_resolve_help,
        ),
    ]
