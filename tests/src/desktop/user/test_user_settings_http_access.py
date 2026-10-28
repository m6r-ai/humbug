"""Tests for persisting HTTP access settings in UserSettings."""

import json
import os
import tempfile

from desktop.user.user_settings import UserSettings
from http_ai_tool.http_access_settings import HttpAccessSettings


def _round_trip(settings: UserSettings) -> UserSettings:
    """Save settings to a temporary file and load them back."""
    with tempfile.TemporaryDirectory() as tmp:
        path = os.path.join(tmp, "settings.json")
        settings.save(path)
        return UserSettings.load(path)


class TestHttpAccessSettingsDefaults:
    """Tests for the default HTTP access values."""

    def test_defaults_enable_auto_approval(self) -> None:
        """A new settings object should have auto-approval enabled."""
        assert UserSettings.create_default().allow_external_http_access is True

    def test_defaults_use_empty_allowlist(self) -> None:
        """A new settings object should start with an empty HTTP allowlist."""
        assert UserSettings.create_default().external_http_allowlist == []

    def test_defaults_use_default_denylist(self) -> None:
        """A new settings object should start with the default HTTP denylist."""
        settings = UserSettings.create_default()
        assert settings.external_http_denylist == HttpAccessSettings.get_default_denylist()


class TestHttpAccessSettingsRoundTrip:
    """Tests for saving and loading HTTP access settings."""

    def test_allowlist_round_trips(self) -> None:
        """A saved allowlist should load back unchanged."""
        settings = UserSettings.create_default()
        settings.external_http_allowlist = ["api.example.com", "https://other.com:8443"]

        loaded = _round_trip(settings)
        assert loaded.external_http_allowlist == ["api.example.com", "https://other.com:8443"]

    def test_denylist_round_trips(self) -> None:
        """A saved denylist should load back unchanged."""
        settings = UserSettings.create_default()
        settings.external_http_denylist = ["localhost", "10.0.0.0/8"]

        loaded = _round_trip(settings)
        assert loaded.external_http_denylist == ["localhost", "10.0.0.0/8"]

    def test_disabled_auto_approval_round_trips(self) -> None:
        """A disabled auto-approval switch should load back disabled."""
        settings = UserSettings.create_default()
        settings.allow_external_http_access = False

        loaded = _round_trip(settings)
        assert loaded.allow_external_http_access is False

    def test_saved_keys_use_expected_names(self) -> None:
        """The settings file should use the documented JSON key names."""
        settings = UserSettings.create_default()
        settings.external_http_allowlist = ["example.com"]

        with tempfile.TemporaryDirectory() as tmp:
            path = os.path.join(tmp, "settings.json")
            settings.save(path)
            with open(path, "r", encoding="utf-8") as f:
                data = json.load(f)

        assert data["allowExternalHttpAccess"] is True
        assert data["externalHttpAllowlist"] == ["example.com"]
        assert data["externalHttpDenylist"] == HttpAccessSettings.get_default_denylist()


class TestHttpAccessSettingsValidation:
    """Tests for validating malformed HTTP access settings."""

    def _load_with(self, overrides: dict) -> UserSettings:
        """Load settings from a file containing the given overrides."""
        settings = UserSettings.create_default()
        with tempfile.TemporaryDirectory() as tmp:
            path = os.path.join(tmp, "settings.json")
            settings.save(path)
            with open(path, "r", encoding="utf-8") as f:
                data = json.load(f)

            data.update(overrides)
            with open(path, "w", encoding="utf-8") as f:
                json.dump(data, f)

            return UserSettings.load(path)

    def test_non_bool_switch_falls_back_to_default(self) -> None:
        """A non-boolean switch value should fall back to the default."""
        loaded = self._load_with({"allowExternalHttpAccess": "yes"})
        assert loaded.allow_external_http_access is True

    def test_non_list_allowlist_falls_back_to_default(self) -> None:
        """A non-list allowlist should fall back to the default."""
        loaded = self._load_with({"externalHttpAllowlist": "example.com"})
        assert loaded.external_http_allowlist == HttpAccessSettings.get_default_allowlist()

    def test_allowlist_with_non_string_entries_falls_back_to_default(self) -> None:
        """An allowlist containing non-string entries should fall back to the default."""
        loaded = self._load_with({"externalHttpAllowlist": ["example.com", 42]})
        assert loaded.external_http_allowlist == HttpAccessSettings.get_default_allowlist()

    def test_empty_denylist_falls_back_to_default(self) -> None:
        """An emptied denylist should be restored to the default so protection cannot be lost."""
        loaded = self._load_with({"externalHttpDenylist": []})
        assert loaded.external_http_denylist == HttpAccessSettings.get_default_denylist()

    def test_non_list_denylist_falls_back_to_default(self) -> None:
        """A non-list denylist should fall back to the default."""
        loaded = self._load_with({"externalHttpDenylist": 5})
        assert loaded.external_http_denylist == HttpAccessSettings.get_default_denylist()
