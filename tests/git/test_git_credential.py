"""Tests for storing an access token via git's credential helper.

These tests configure a repo-local ``store`` helper pointing at a temp file so
nothing touches the real OS keychain.
"""

import os
import shutil
import subprocess

import pytest

from git import set_credential


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
    return root, str(tmp_path / "creds")


def _use_store_helper(root, creds_file):
    # Repo-local store helper -> set_credential sees a configured helper and
    # writes the token to *creds_file* instead of the keychain.
    _git(root, "config", "credential.helper", f"store --file={creds_file}")


def test_stores_token_for_https_remote(repo):
    root, creds = repo
    _git(root, "remote", "add", "origin", "https://github.com/org/proj.git")
    _use_store_helper(root, creds)

    set_credential(root, "octocat", "ghp_SECRET123")

    assert os.path.exists(creds)
    with open(creds, encoding="utf-8") as f:
        contents = f.read()

    assert "ghp_SECRET123" in contents
    assert "octocat" in contents
    assert "github.com" in contents
    # Path-keyed (useHttpPath) so accounts on the same host stay separate.
    assert _git_local(root, "credential.useHttpPath") == "true"


def _git_local(root, key):
    out = subprocess.run(
        ["git", "config", "--local", key], cwd=root, capture_output=True, text=True
    )
    return out.stdout.strip()


def test_rejects_ssh_remote(repo):
    root, creds = repo
    _git(root, "remote", "add", "origin", "git@github.com:org/proj.git")
    _use_store_helper(root, creds)

    with pytest.raises(ValueError):
        set_credential(root, "octocat", "ghp_SECRET123")


def test_rejects_when_no_remote(repo):
    root, creds = repo
    _use_store_helper(root, creds)
    with pytest.raises(ValueError):
        set_credential(root, "octocat", "ghp_SECRET123")


def test_rejects_empty_token(repo):
    root, creds = repo
    _git(root, "remote", "add", "origin", "https://github.com/org/proj.git")
    _use_store_helper(root, creds)
    with pytest.raises(ValueError):
        set_credential(root, "octocat", "   ")
