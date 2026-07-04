"""Tests for per-repo identity and remote-URL editing."""

import os
import shutil
import subprocess

import pytest

from git import (
    get_identity,
    list_remotes,
    set_identity,
    set_remote_url,
)


pytestmark = pytest.mark.skipif(
    shutil.which("git") is None, reason="git executable not available"
)


def _git(cwd, *args):
    subprocess.run(["git"] + list(args), cwd=cwd, check=True, capture_output=True, text=True)


@pytest.fixture
def repo(tmp_path):
    root = str(tmp_path / "repo")
    os.makedirs(root)
    _git(root, "init")
    return root


def test_set_and_get_identity(repo):
    set_identity(repo, "Office User", "office@corp.com")
    name, email = get_identity(repo)
    assert name == "Office User"
    assert email == "office@corp.com"


def test_identity_empty_when_unset(repo):
    # A fresh repo with no local identity: values come back empty (or global);
    # at minimum the call must not raise.
    name, email = get_identity(repo)
    assert isinstance(name, str) and isinstance(email, str)


def test_identity_is_repo_local(repo, tmp_path):
    set_identity(repo, "Personal Me", "me@home.com")
    # A second repo is unaffected by the first's local identity.
    other = str(tmp_path / "other")
    os.makedirs(other)
    _git(other, "init")
    set_identity(other, "Work Me", "me@work.com")

    assert get_identity(repo) == ("Personal Me", "me@home.com")
    assert get_identity(other) == ("Work Me", "me@work.com")


def test_set_remote_url(repo):
    _git(repo, "remote", "add", "origin", "https://old.example.com/r.git")
    set_remote_url(repo, "origin", "git@github-work:org/r.git")
    remotes = list_remotes(repo)
    assert remotes[0].url == "git@github-work:org/r.git"


def test_set_remote_url_empty_raises(repo):
    _git(repo, "remote", "add", "origin", "https://x/r.git")
    with pytest.raises(ValueError):
        set_remote_url(repo, "origin", "  ")
