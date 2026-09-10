"""Onboarding product tour status enumeration."""

from enum import Enum


class OnboardingTourStatus(Enum):
    """Enumeration of the states the onboarding product tour can be in."""
    NOT_STARTED = "not_started"
    SKIPPED = "skipped"
    COMPLETED = "completed"
