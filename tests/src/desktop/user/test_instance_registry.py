"""Tests for the registry of running Humbug instances."""

import json
import os
import tempfile

from desktop.user.instance_registry import InstanceRegistry


def write_record(tmp_dir: str, pid: int, mindspace_path: str, start_time: float) -> None:
    """Write a registry record directly, standing in for another instance."""
    instances_dir = os.path.join(tmp_dir, "instances")
    os.makedirs(instances_dir, exist_ok=True)
    with open(os.path.join(instances_dir, f"{pid}.json"), "w", encoding="utf-8") as f:
        json.dump({
            "pid": pid,
            "start_time": start_time,
            "mindspace_path": mindspace_path,
            "launched_at": 0.0,
        }, f)


def find_dead_pid() -> int:
    """Return a process id that is not currently in use."""
    pid = 999999
    while True:
        try:
            os.kill(pid, 0)

        except ProcessLookupError:
            return pid

        except OSError:
            return pid

        pid -= 1


class TestRegisterAndUnregister:
    def test_register_writes_a_record_for_this_process(self):
        with tempfile.TemporaryDirectory() as tmp_dir:
            registry = InstanceRegistry(tmp_dir)
            registry.register("/some/mindspace")

            path = os.path.join(tmp_dir, "instances", f"{os.getpid()}.json")
            assert os.path.exists(path)

            with open(path, encoding="utf-8") as f:
                data = json.load(f)

            assert data["pid"] == os.getpid()
            assert data["mindspace_path"] == "/some/mindspace"

    def test_unregister_removes_the_record(self):
        with tempfile.TemporaryDirectory() as tmp_dir:
            registry = InstanceRegistry(tmp_dir)
            registry.register("/some/mindspace")
            registry.unregister()

            path = os.path.join(tmp_dir, "instances", f"{os.getpid()}.json")
            assert not os.path.exists(path)

    def test_unregister_is_safe_when_not_registered(self):
        with tempfile.TemporaryDirectory() as tmp_dir:
            registry = InstanceRegistry(tmp_dir)
            registry.unregister()

    def test_register_replaces_a_previous_record(self):
        with tempfile.TemporaryDirectory() as tmp_dir:
            registry = InstanceRegistry(tmp_dir)
            registry.register("/first")
            registry.register("/second")

            path = os.path.join(tmp_dir, "instances", f"{os.getpid()}.json")
            with open(path, encoding="utf-8") as f:
                data = json.load(f)

            assert data["mindspace_path"] == "/second"


class TestFindLiveInstance:
    def test_returns_none_when_registry_directory_does_not_exist(self):
        with tempfile.TemporaryDirectory() as tmp_dir:
            registry = InstanceRegistry(tmp_dir)
            assert registry.find_live_instance("/some/mindspace") is None

    def test_ignores_this_process_own_record(self):
        with tempfile.TemporaryDirectory() as tmp_dir:
            registry = InstanceRegistry(tmp_dir)
            registry.register("/some/mindspace")

            assert registry.find_live_instance("/some/mindspace") is None

    def test_returns_none_for_an_empty_mindspace_path(self):
        with tempfile.TemporaryDirectory() as tmp_dir:
            registry = InstanceRegistry(tmp_dir)
            assert registry.find_live_instance("") is None

    def test_finds_a_live_instance_with_the_target_mindspace(self):
        with tempfile.TemporaryDirectory() as tmp_dir:
            registry = InstanceRegistry(tmp_dir)
            other_pid = os.getpid() + 1
            write_record(tmp_dir, other_pid, "/target/mindspace", 0.0)

            found = registry.find_live_instance("/target/mindspace")

        assert found is not None
        assert found.pid == other_pid

    def test_ignores_a_live_instance_with_a_different_mindspace(self):
        with tempfile.TemporaryDirectory() as tmp_dir:
            registry = InstanceRegistry(tmp_dir)
            other_pid = os.getpid() + 1
            write_record(tmp_dir, other_pid, "/other/mindspace", 0.0)

            found = registry.find_live_instance("/target/mindspace")

        assert found is None

    def test_removes_a_stale_record_for_a_dead_process(self):
        with tempfile.TemporaryDirectory() as tmp_dir:
            registry = InstanceRegistry(tmp_dir)
            dead_pid = find_dead_pid()
            write_record(tmp_dir, dead_pid, "/target/mindspace", 0.0)

            found = registry.find_live_instance("/target/mindspace")

            path = os.path.join(tmp_dir, "instances", f"{dead_pid}.json")
            assert found is None
            assert not os.path.exists(path)

    def test_ignores_a_malformed_record(self):
        with tempfile.TemporaryDirectory() as tmp_dir:
            registry = InstanceRegistry(tmp_dir)
            instances_dir = os.path.join(tmp_dir, "instances")
            os.makedirs(instances_dir, exist_ok=True)
            with open(os.path.join(instances_dir, "12345.json"), "w", encoding="utf-8") as f:
                f.write("not json at all")

            assert registry.find_live_instance("/target/mindspace") is None

    def test_ignores_a_record_missing_required_fields(self):
        with tempfile.TemporaryDirectory() as tmp_dir:
            registry = InstanceRegistry(tmp_dir)
            instances_dir = os.path.join(tmp_dir, "instances")
            os.makedirs(instances_dir, exist_ok=True)
            with open(os.path.join(instances_dir, "12345.json"), "w", encoding="utf-8") as f:
                json.dump({"pid": 12345}, f)

            assert registry.find_live_instance("/target/mindspace") is None

    def test_ignores_non_json_files_in_the_registry_directory(self):
        with tempfile.TemporaryDirectory() as tmp_dir:
            registry = InstanceRegistry(tmp_dir)
            instances_dir = os.path.join(tmp_dir, "instances")
            os.makedirs(instances_dir, exist_ok=True)
            with open(os.path.join(instances_dir, "README.txt"), "w", encoding="utf-8") as f:
                f.write("ignore me")

            assert registry.find_live_instance("/target/mindspace") is None


class TestIsOpenElsewhere:
    def test_default_constructor_uses_the_standard_humbug_directory(self, monkeypatch, tmp_path):
        monkeypatch.setenv("HOME", str(tmp_path))

        registry = InstanceRegistry()
        registry.register("/target/mindspace")

        expected = os.path.join(str(tmp_path), ".humbug", "instances", f"{os.getpid()}.json")
        assert os.path.exists(expected)
        registry.unregister()

    def test_returns_false_when_no_other_instance_has_it(self):
        with tempfile.TemporaryDirectory() as tmp_dir:
            registry = InstanceRegistry(tmp_dir)
            assert registry.is_open_elsewhere("/target/mindspace") is False

    def test_returns_true_when_a_live_instance_has_it(self):
        with tempfile.TemporaryDirectory() as tmp_dir:
            registry = InstanceRegistry(tmp_dir)
            other_pid = os.getpid() + 1
            write_record(tmp_dir, other_pid, "/target/mindspace", 0.0)

            assert registry.is_open_elsewhere("/target/mindspace") is True

    def test_returns_false_for_the_current_instance_own_mindspace(self):
        with tempfile.TemporaryDirectory() as tmp_dir:
            registry = InstanceRegistry(tmp_dir)
            registry.register("/target/mindspace")

            assert registry.is_open_elsewhere("/target/mindspace") is False
