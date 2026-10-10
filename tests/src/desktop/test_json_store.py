"""Tests for concurrency-safe JSON file storage."""

import json
import os
import threading

from desktop import json_store


class TestReadJson:
    def test_missing_file_reads_as_none(self, tmp_path):
        """A file that does not exist reads as None."""
        assert json_store.read_json(str(tmp_path / "absent.json")) is None

    def test_malformed_file_reads_as_none(self, tmp_path):
        """A file that is not valid JSON reads as None."""
        path = tmp_path / "bad.json"
        path.write_text("{not json", encoding="utf-8")

        assert json_store.read_json(str(path)) is None

    def test_non_object_reads_as_none(self, tmp_path):
        """JSON that is not an object reads as None."""
        path = tmp_path / "list.json"
        path.write_text("[1, 2, 3]", encoding="utf-8")

        assert json_store.read_json(str(path)) is None

    def test_object_round_trips(self, tmp_path):
        """A stored object reads back with the same contents."""
        path = str(tmp_path / "data.json")
        json_store.write_json(path, {"a": 1, "b": ["x"]})

        assert json_store.read_json(path) == {"a": 1, "b": ["x"]}


class TestWriteJson:
    def test_write_creates_missing_directories(self, tmp_path):
        """Writing creates any missing parent directories."""
        path = str(tmp_path / "nested" / "deeper" / "data.json")
        json_store.write_json(path, {"a": 1})

        assert json_store.read_json(path) == {"a": 1}

    def test_write_applies_requested_permissions(self, tmp_path):
        """A requested file mode is applied to the stored file."""
        path = str(tmp_path / "secret.json")
        json_store.write_json(path, {"key": "value"}, mode=0o600)

        assert os.stat(path).st_mode & 0o777 == 0o600

    def test_write_leaves_no_temporary_files(self, tmp_path):
        """Writing leaves only the target file behind."""
        path = str(tmp_path / "data.json")
        json_store.write_json(path, {"a": 1})

        assert os.listdir(tmp_path) == ["data.json"]

    def test_concurrent_writers_do_not_disturb_each_other(self, tmp_path):
        """Simultaneous writes each complete, leaving one of them stored intact."""
        path = str(tmp_path / "data.json")
        start = threading.Barrier(8)
        failures: list[BaseException] = []

        def _write(value: int) -> None:
            start.wait()
            try:
                json_store.write_json(path, {"writer": value})

            except BaseException as e:  # pylint: disable=broad-exception-caught
                failures.append(e)

        threads = [threading.Thread(target=_write, args=(i,)) for i in range(8)]
        for thread in threads:
            thread.start()

        for thread in threads:
            thread.join()

        assert not failures
        stored = json_store.read_json(path)
        assert stored is not None
        assert stored["writer"] in range(8)
        assert os.listdir(tmp_path) == ["data.json"]

    def test_existing_contents_survive_a_failed_write(self, tmp_path):
        """A write that fails leaves the previous contents intact."""
        path = str(tmp_path / "data.json")
        json_store.write_json(path, {"a": 1})

        try:
            json_store.write_json(path, {"bad": {1, 2}})

        except TypeError:
            pass

        assert json_store.read_json(path) == {"a": 1}


class TestUpdateJson:
    def test_update_receives_current_contents(self, tmp_path):
        """The mutate function is given the contents currently stored."""
        path = str(tmp_path / "data.json")
        json_store.write_json(path, {"count": 1})

        seen = {}

        def _mutate(current):
            seen.update(current)
            return {"count": current["count"] + 1}

        json_store.update_json(path, _mutate)

        assert seen == {"count": 1}
        assert json_store.read_json(path) == {"count": 2}

    def test_update_of_missing_file_starts_empty(self, tmp_path):
        """Updating a file that does not exist starts from an empty object."""
        path = str(tmp_path / "data.json")

        json_store.update_json(path, lambda current: {"created": not current})

        assert json_store.read_json(path) == {"created": True}

    def test_concurrent_updates_are_not_lost(self, tmp_path):
        """Updates made from several threads at once all reach the stored file."""
        path = str(tmp_path / "data.json")
        json_store.write_json(path, {"entries": []})
        start = threading.Barrier(8)

        def _append(value: int) -> None:
            start.wait()
            json_store.update_json(
                path, lambda current: {"entries": current.get("entries", []) + [value]}
            )

        threads = [threading.Thread(target=_append, args=(i,)) for i in range(8)]
        for thread in threads:
            thread.start()

        for thread in threads:
            thread.join()

        stored = json_store.read_json(path)
        assert stored is not None
        assert sorted(stored["entries"]) == list(range(8))


class TestAcquire:
    def test_claim_is_exclusive(self, tmp_path):
        """A second claim on the same path is refused while the first is held."""
        path = str(tmp_path / "resource")

        first = json_store.acquire(path)
        assert first is not None

        try:
            assert json_store.acquire(path) is None

        finally:
            first.close()

    def test_claim_can_be_retaken_after_release(self, tmp_path):
        """Releasing a claim allows it to be taken again."""
        path = str(tmp_path / "resource")

        first = json_store.acquire(path)
        assert first is not None
        first.close()

        second = json_store.acquire(path)
        assert second is not None
        second.close()

    def test_separate_paths_do_not_conflict(self, tmp_path):
        """Claims on different paths are independent."""
        first = json_store.acquire(str(tmp_path / "one"))
        second = json_store.acquire(str(tmp_path / "two"))

        assert first is not None
        assert second is not None
        first.close()
        second.close()

    def test_a_held_claim_does_not_block_reading_the_file(self, tmp_path):
        """Holding a claim leaves the guarded file readable."""
        path = str(tmp_path / "resource")
        json_store.write_json(path, {"a": 1})

        claim = json_store.acquire(path)
        assert claim is not None

        try:
            assert json_store.read_json(path) == {"a": 1}

        finally:
            claim.close()


class TestInteroperability:
    def test_written_files_are_plain_json(self, tmp_path):
        """Stored files can be read by anything that understands JSON."""
        path = str(tmp_path / "data.json")
        json_store.write_json(path, {"a": 1})

        with open(path, encoding="utf-8") as f:
            assert json.load(f) == {"a": 1}
