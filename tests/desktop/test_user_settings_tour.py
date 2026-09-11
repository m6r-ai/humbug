"""Tests for onboarding tour state persistence on UserSettings."""

import json
import os
import tempfile

from desktop.user.onboarding_tour_status import OnboardingTourStatus
from desktop.user.user_settings import UserSettings


class TestDefaults:
    def test_default_settings_have_not_started_tour(self):
        settings = UserSettings.create_default()
        assert settings.onboarding_tour_status == OnboardingTourStatus.NOT_STARTED
        assert settings.onboarding_tour_version == 0


class TestRoundTrip:
    def test_completed_status_and_version_survive_save_and_load(self):
        settings = UserSettings.create_default()
        settings.onboarding_tour_status = OnboardingTourStatus.COMPLETED
        settings.onboarding_tour_version = 1

        with tempfile.TemporaryDirectory() as tmp_dir:
            path = os.path.join(tmp_dir, "user-settings.json")
            settings.save(path)
            loaded = UserSettings.load(path)

        assert loaded.onboarding_tour_status == OnboardingTourStatus.COMPLETED
        assert loaded.onboarding_tour_version == 1

    def test_skipped_status_survives_save_and_load(self):
        settings = UserSettings.create_default()
        settings.onboarding_tour_status = OnboardingTourStatus.SKIPPED
        settings.onboarding_tour_version = 1

        with tempfile.TemporaryDirectory() as tmp_dir:
            path = os.path.join(tmp_dir, "user-settings.json")
            settings.save(path)
            loaded = UserSettings.load(path)

        assert loaded.onboarding_tour_status == OnboardingTourStatus.SKIPPED


class TestMalformedData:
    def test_invalid_status_string_falls_back_to_not_started(self):
        with tempfile.TemporaryDirectory() as tmp_dir:
            path = os.path.join(tmp_dir, "user-settings.json")
            with open(path, "w", encoding="utf-8") as f:
                json.dump({"onboardingTourStatus": "NOT_A_REAL_STATUS"}, f)

            loaded = UserSettings.load(path)

        assert loaded.onboarding_tour_status == OnboardingTourStatus.NOT_STARTED

    def test_missing_fields_fall_back_to_defaults(self):
        with tempfile.TemporaryDirectory() as tmp_dir:
            path = os.path.join(tmp_dir, "user-settings.json")
            with open(path, "w", encoding="utf-8") as f:
                json.dump({}, f)

            loaded = UserSettings.load(path)

        assert loaded.onboarding_tour_status == OnboardingTourStatus.NOT_STARTED
        assert loaded.onboarding_tour_version == 0
