"""Tests for building the launch context used to start a new Humbug instance."""

import sys

from desktop.user.launch_context import LaunchContext


class TestCurrent:
    def test_development_launch_reruns_the_desktop_module(self, monkeypatch):
        monkeypatch.delattr(sys, "frozen", raising=False)

        context = LaunchContext.current()

        assert context.executable == sys.executable
        assert context.argv == ["-m", "desktop"]

    def test_frozen_launch_reruns_the_application_binary(self, monkeypatch):
        monkeypatch.setattr(sys, "frozen", True, raising=False)

        context = LaunchContext.current()

        assert context.executable == sys.executable
        assert context.argv == []

    def test_working_directory_is_the_current_directory(self, monkeypatch):
        monkeypatch.delattr(sys, "frozen", raising=False)

        context = LaunchContext.current()

        assert context.working_directory


class TestSpawnArgumentConstruction:
    def test_spawn_passes_the_mindspace_argument(self, monkeypatch):
        captured = {}

        class _FakeProcess:
            pid = 4242

        def _fake_popen(argv, **kwargs):
            captured["argv"] = argv
            captured["kwargs"] = kwargs
            return _FakeProcess()

        monkeypatch.setattr("desktop.user.launch_context.subprocess.Popen", _fake_popen)

        context = LaunchContext(executable="/usr/bin/python", argv=["-m", "desktop"], working_directory="/tmp")
        pid = context.spawn("/target/mindspace")

        assert pid == 4242
        assert captured["argv"] == ["/usr/bin/python", "-m", "desktop", "--mindspace", "/target/mindspace"]
        assert captured["kwargs"]["cwd"] == "/tmp"

    def test_spawn_does_not_pass_a_modified_environment(self, monkeypatch):
        captured = {}

        class _FakeProcess:
            pid = 4242

        def _fake_popen(argv, **kwargs):
            captured["kwargs"] = kwargs
            return _FakeProcess()

        monkeypatch.setattr("desktop.user.launch_context.subprocess.Popen", _fake_popen)

        context = LaunchContext(executable="/usr/bin/python", argv=["-m", "desktop"], working_directory="/tmp")
        context.spawn("/target/mindspace")

        assert "env" not in captured["kwargs"]
