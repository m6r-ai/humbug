"""Tests for the user settings revision counter used to detect external changes."""

import json
import os
import tempfile

from desktop.user.user_settings import UserSettings


class TestRevisionDefaults:
    def test_default_settings_start_at_revision_zero(self):
        settings = UserSettings.create_default()
        assert settings.revision == 0

    def test_file_without_revision_loads_as_zero(self):
        with tempfile.TemporaryDirectory() as tmp_dir:
            path = os.path.join(tmp_dir, "user-settings.json")
            with open(path, "w", encoding="utf-8") as f:
                json.dump({}, f)

            loaded = UserSettings.load(path)

        assert loaded.revision == 0


class TestRevisionIncrements:
    def test_each_save_increments_the_revision(self):
        settings = UserSettings.create_default()

        with tempfile.TemporaryDirectory() as tmp_dir:
            path = os.path.join(tmp_dir, "user-settings.json")
            settings.save(path)
            assert settings.revision == 1

            settings.save(path)
            assert settings.revision == 2

            loaded = UserSettings.load(path)

        assert loaded.revision == 2

    def test_revision_survives_save_and_load(self):
        settings = UserSettings.create_default()

        with tempfile.TemporaryDirectory() as tmp_dir:
            path = os.path.join(tmp_dir, "user-settings.json")
            settings.save(path)
            first = UserSettings.load(path)
            first_revision = first.revision

            first.save(path)
            second = UserSettings.load(path)

        assert second.revision == first_revision + 1


class TestMalformedRevision:
    def test_negative_revision_falls_back_to_zero(self):
        with tempfile.TemporaryDirectory() as tmp_dir:
            path = os.path.join(tmp_dir, "user-settings.json")
            with open(path, "w", encoding="utf-8") as f:
                json.dump({"revision": -5}, f)

            loaded = UserSettings.load(path)

        assert loaded.revision == 0

    def test_non_integer_revision_falls_back_to_zero(self):
        with tempfile.TemporaryDirectory() as tmp_dir:
            path = os.path.join(tmp_dir, "user-settings.json")
            with open(path, "w", encoding="utf-8") as f:
                json.dump({"revision": "not-a-number"}, f)

            loaded = UserSettings.load(path)

        assert loaded.revision == 0

    def test_boolean_revision_falls_back_to_zero(self):
        with tempfile.TemporaryDirectory() as tmp_dir:
            path = os.path.join(tmp_dir, "user-settings.json")
            with open(path, "w", encoding="utf-8") as f:
                json.dump({"revision": True}, f)

            loaded = UserSettings.load(path)

        assert loaded.revision == 0
