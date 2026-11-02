"""Tests for the UserManager settings mutation and reload behaviour."""

import json
import os
import tempfile

import pytest

from desktop.user.user_manager import UserError, UserManager
from desktop.user.user_settings import UserSettings


@pytest.fixture
def user_manager(tmp_path, monkeypatch):
    """A UserManager instance isolated to a temporary home directory."""
    monkeypatch.setattr(UserManager, "_instance", None)
    monkeypatch.setenv("HOME", str(tmp_path))
    monkeypatch.delenv("ANTHROPIC_API_KEY", raising=False)

    manager = UserManager()
    yield manager

    monkeypatch.setattr(UserManager, "_instance", None)


class TestUpdateSettingsFields:
    def test_applies_a_single_field(self, user_manager):
        user_manager.update_settings_fields(check_for_updates=False)

        assert user_manager.settings().check_for_updates is False

    def test_applies_multiple_fields(self, user_manager):
        user_manager.update_settings_fields(
            onboarding_tour_status=user_manager.settings().onboarding_tour_status,
            onboarding_tour_version=7
        )

        assert user_manager.settings().onboarding_tour_version == 7

    def test_persists_the_change_to_disk(self, user_manager):
        user_manager.update_settings_fields(check_for_updates=False)

        settings_path = os.path.expanduser("~/.humbug/user-settings.json")
        with open(settings_path, encoding="utf-8") as f:
            data = json.load(f)

        assert data["checkForUpdates"] is False

    def test_rejects_an_unknown_field(self, user_manager):
        with pytest.raises(UserError):
            user_manager.update_settings_fields(not_a_real_setting=1)

    def test_emits_settings_changed(self, user_manager):
        received = []
        user_manager.settings_changed.connect(lambda: received.append(True))

        user_manager.update_settings_fields(check_for_updates=False)

        assert received == [True]


class TestConcurrentInstanceUpdates:
    """Tests that a field update preserves changes written by another instance."""

    def test_a_field_changed_elsewhere_is_preserved(self, user_manager):
        """A field another instance changed survives this instance's update."""
        settings_path = os.path.expanduser("~/.humbug/user-settings.json")

        elsewhere = UserSettings.load(settings_path)
        elsewhere.check_for_updates = False
        elsewhere.save(settings_path)

        user_manager.update_settings_fields(onboarding_tour_version=7)

        stored = UserSettings.load(settings_path)
        assert stored.onboarding_tour_version == 7
        assert stored.check_for_updates is False

    def test_the_in_memory_settings_adopt_the_merged_state(self, user_manager):
        """After an update this instance sees the other instance's change too."""
        settings_path = os.path.expanduser("~/.humbug/user-settings.json")

        elsewhere = UserSettings.load(settings_path)
        elsewhere.check_for_updates = False
        elsewhere.save(settings_path)

        user_manager.update_settings_fields(onboarding_tour_version=7)

        assert user_manager.settings().check_for_updates is False
        assert user_manager.settings().onboarding_tour_version == 7

    def test_an_update_still_advances_the_revision(self, user_manager):
        """A merged update is ordered after the write it merged with."""
        settings_path = os.path.expanduser("~/.humbug/user-settings.json")

        elsewhere = UserSettings.load(settings_path)
        elsewhere.check_for_updates = False
        elsewhere.save(settings_path)

        user_manager.update_settings_fields(onboarding_tour_version=7)

        assert UserSettings.load(settings_path).revision > elsewhere.revision


class TestRevisionOrdering:
    def test_saving_increments_the_revision(self, user_manager):
        before = user_manager.settings().revision
        user_manager.update_settings_fields(check_for_updates=False)

        assert user_manager.settings().revision == before + 1

    def test_adopts_a_higher_on_disk_revision_before_writing(self, user_manager):
        settings_path = os.path.expanduser("~/.humbug/user-settings.json")

        # Simulate another instance having written a newer revision.
        on_disk = UserSettings.load(settings_path)
        on_disk.revision = 50
        on_disk.save(settings_path)
        written_revision = UserSettings.load(settings_path).revision

        user_manager.update_settings_fields(check_for_updates=False)

        assert user_manager.settings().revision == written_revision + 1


class TestReloadIfChanged:
    def test_returns_false_when_the_revision_is_not_newer(self, user_manager):
        assert user_manager.reload_if_changed() is False

    def test_returns_false_when_the_file_does_not_exist(self, user_manager):
        os.remove(os.path.expanduser("~/.humbug/user-settings.json"))

        assert user_manager.reload_if_changed() is False

    def test_loads_a_newer_revision_from_disk(self, user_manager):
        settings_path = os.path.expanduser("~/.humbug/user-settings.json")

        on_disk = UserSettings.load(settings_path)
        on_disk.check_for_updates = False
        on_disk.save(settings_path)

        assert user_manager.reload_if_changed() is True
        assert user_manager.settings().check_for_updates is False

    def test_emits_settings_changed_on_reload(self, user_manager):
        settings_path = os.path.expanduser("~/.humbug/user-settings.json")

        on_disk = UserSettings.load(settings_path)
        on_disk.save(settings_path)

        received = []
        user_manager.settings_changed.connect(lambda: received.append(True))

        user_manager.reload_if_changed()

        assert received == [True]

    def test_does_not_emit_when_nothing_changed(self, user_manager):
        received = []
        user_manager.settings_changed.connect(lambda: received.append(True))

        user_manager.reload_if_changed()

        assert received == []
