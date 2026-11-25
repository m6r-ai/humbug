"""Tests for remembering whether the guide to Mission Control has been put away."""

import json
import os
import tempfile

from desktop.user.user_settings import UserSettings


def test_guide_is_shown_until_dismissed():
    """A new user has not dismissed the guide."""
    assert not UserSettings.create_default().mission_guide_dismissed


def test_dismissal_survives_save_and_load():
    """Putting the guide away is kept between runs."""
    settings = UserSettings.create_default()
    settings.mission_guide_dismissed = True
    with tempfile.TemporaryDirectory() as tmp_dir:
        path = os.path.join(tmp_dir, "user-settings.json")
        settings.save(path)
        assert UserSettings.load(path).mission_guide_dismissed


def test_malformed_value_shows_the_guide():
    """A value that is not a yes or no is read as not dismissed."""
    with tempfile.TemporaryDirectory() as tmp_dir:
        path = os.path.join(tmp_dir, "user-settings.json")
        UserSettings.create_default().save(path)
        with open(path, encoding="utf-8") as handle:
            data = json.load(handle)

        data["missionGuideDismissed"] = "yes"
        with open(path, "w", encoding="utf-8") as handle:
            json.dump(data, handle)

        assert not UserSettings.load(path).mission_guide_dismissed
