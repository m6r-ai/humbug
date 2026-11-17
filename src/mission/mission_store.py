"""Storage of mission records inside a mindspace."""

import json
import logging
import os
import tempfile

from mission.mission import Mission
from mission.mission_error import MissionError


_PREFERENCES_FILE = "preferences.json"


class MissionStore:
    """
    Reads and writes mission records under `.humbug/missions/`.

    Each mission is one JSON file named by its identifier.  Files are replaced
    atomically so a crash mid-write never leaves a truncated record.
    """

    def __init__(self, missions_dir: str) -> None:
        """
        Initialise the store.

        Args:
            missions_dir: Absolute path of the directory holding mission records
        """
        self._missions_dir = missions_dir
        self._logger = logging.getLogger("MissionStore")

    def _record_path(self, mission_id: str) -> str:
        """Return the path of the record for a mission."""
        return os.path.join(self._missions_dir, f"{mission_id}.json")

    def load_all(self) -> list[Mission]:
        """
        Load every readable mission record, oldest first.

        Unreadable records are logged and skipped so one damaged file does not
        hide the others.

        Returns:
            The missions found
        """
        if not os.path.isdir(self._missions_dir):
            return []

        missions: list[Mission] = []
        for name in sorted(os.listdir(self._missions_dir)):
            if not name.endswith(".json") or name == _PREFERENCES_FILE:
                continue

            path = os.path.join(self._missions_dir, name)
            try:
                with open(path, "r", encoding="utf-8") as f:
                    data = json.load(f)

                if not isinstance(data, dict):
                    raise ValueError("Mission record is not an object")

                missions.append(Mission.from_dict(data))

            except (OSError, ValueError) as e:
                self._logger.warning("Skipping unreadable mission record '%s': %s", path, e)

        missions.sort(key=lambda m: m.created_at)
        return missions

    def save(self, mission: Mission) -> None:
        """
        Write a mission record, replacing any previous version atomically.

        Args:
            mission: The mission to write

        Raises:
            MissionError: If the record cannot be written
        """
        self._write_json(f"{mission.mission_id}.json", mission.to_dict())

    def _write_json(self, name: str, data: dict) -> None:
        """
        Write a JSON file in the missions directory, replacing any previous version atomically.

        Args:
            name: File name within the missions directory
            data: Content to write

        Raises:
            MissionError: If the file cannot be written
        """
        try:
            os.makedirs(self._missions_dir, exist_ok=True)
            fd, temp_path = tempfile.mkstemp(dir=self._missions_dir, prefix=".mission-", suffix=".tmp")
            try:
                with os.fdopen(fd, "w", encoding="utf-8") as f:
                    json.dump(data, f, indent=2)

                os.replace(temp_path, os.path.join(self._missions_dir, name))

            except BaseException:
                if os.path.exists(temp_path):
                    os.remove(temp_path)

                raise

        except OSError as e:
            raise MissionError(f"Failed to save mission: {e}") from e

    def _load_preferences(self) -> dict:
        """Return the saved preferences, or none if they cannot be read."""
        try:
            with open(os.path.join(self._missions_dir, _PREFERENCES_FILE), "r", encoding="utf-8") as f:
                data = json.load(f)

        except (OSError, ValueError):
            return {}

        return data if isinstance(data, dict) else {}

    def load_resume_automatically(self) -> bool:
        """
        Return whether interrupted missions should be resumed when Humbug opens.

        Returns:
            The saved preference, or False if none has been saved
        """
        return self._load_preferences().get("resumeAutomatically") is True

    def save_resume_automatically(self, enabled: bool) -> None:
        """
        Save whether interrupted missions should be resumed when Humbug opens.

        Args:
            enabled: The preference

        Raises:
            MissionError: If the preference cannot be written
        """
        self._write_json(_PREFERENCES_FILE, {**self._load_preferences(), "resumeAutomatically": enabled})

    def load_reduce_motion(self) -> bool:
        """
        Return whether Mission Control should draw everything still.

        Returns:
            The saved preference, or False if none has been saved
        """
        return self._load_preferences().get("reduceMotion") is True

    def save_reduce_motion(self, reduced: bool) -> None:
        """
        Save whether Mission Control should draw everything still.

        Args:
            reduced: The preference

        Raises:
            MissionError: If the preference cannot be written
        """
        self._write_json(_PREFERENCES_FILE, {**self._load_preferences(), "reduceMotion": reduced})

    def delete(self, mission_id: str) -> None:
        """
        Remove a mission record.  Missing records are ignored.

        Args:
            mission_id: Identifier of the mission to remove

        Raises:
            MissionError: If the record exists but cannot be removed
        """
        try:
            os.remove(self._record_path(mission_id))

        except FileNotFoundError:
            pass

        except OSError as e:
            raise MissionError(f"Failed to remove mission: {e}") from e
