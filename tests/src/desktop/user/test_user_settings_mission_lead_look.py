"""Tests for remembering how mission leads look."""

import json
import os
import tempfile

from desktop.user.user_settings import UserSettings


def test_leads_start_with_the_classic_look():
    """A new user's mission leads have the classic look."""
    assert UserSettings.create_default().mission_lead_look == "classic"


def test_chosen_look_survives_save_and_load():
    """The look chosen is kept between runs."""
    settings = UserSettings.create_default()
    settings.mission_lead_look = "rounded"
    with tempfile.TemporaryDirectory() as tmp_dir:
        path = os.path.join(tmp_dir, "user-settings.json")
        settings.save(path)
        assert UserSettings.load(path).mission_lead_look == "rounded"


def test_unknown_look_falls_back_to_classic():
    """A look that is not one of the two is read as the classic look."""
    with tempfile.TemporaryDirectory() as tmp_dir:
        path = os.path.join(tmp_dir, "user-settings.json")
        UserSettings.create_default().save(path)
        with open(path, encoding="utf-8") as handle:
            data = json.load(handle)

        data["missionLeadLook"] = "sparkly"
        with open(path, "w", encoding="utf-8") as handle:
            json.dump(data, handle)

        assert UserSettings.load(path).mission_lead_look == "classic"
