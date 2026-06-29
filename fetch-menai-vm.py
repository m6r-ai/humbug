#!/usr/bin/env python3
"""
Fetch the pre-built Menai C VM binary for the current platform and Python version.

Downloads the appropriate artifact from GitHub Actions and places it in
src/menai/ so that the C VM is available without a local C compiler.

Usage:
    python fetch-menai-vm.py

The script detects the current platform and Python version, downloads the
matching artifact from the latest successful run of the build workflow on
main, and installs it into src/menai/.
"""

import io
import json
import os
import platform
import sys
import sysconfig
import urllib.request
import zipfile


def detect_label() -> str:
    """Determine the artifact label for the current platform and Python version."""
    py_version = f"py{sys.version_info.major}{sys.version_info.minor}"

    machine = platform.machine().lower()
    if machine in ("arm64", "aarch64"):
        arch = "arm64"

    elif machine in ("x86_64", "amd64"):
        arch = "x86_64"

    else:
        print(f"Unsupported architecture: {machine}")
        sys.exit(1)

    os_name = sys.platform
    if os_name == "linux":
        os_label = "linux"

    elif os_name == "darwin":
        os_label = "macos"

    elif os_name == "win32":
        os_label = "windows"

    else:
        print(f"Unsupported OS: {os_name}")
        sys.exit(1)

    label = f"{os_label}-{arch}-{py_version}"
    return label


def fetch_artifact(label: str) -> bytes:
    """Download the artifact zip from GitHub Actions and return the binary content."""
    owner_repo = os.environ.get("GITHUB_REPOSITORY", "m6r-ai/humbug")
    api_base = f"https://api.github.com/repos/{owner_repo}"

    # Find the latest successful workflow run on main.
    url = (
        f"{api_base}/actions/workflows/build-menai-vm.yml/runs"
        f"?branch=main&status=success&per_page=1"
    )
    req = urllib.request.Request(url, headers={
        "Accept": "application/vnd.github+json",
        "X-GitHub-Api-Version": "2022-11-28",
    })

    # Add auth token if available (for private repos or rate limits).
    token = os.environ.get("GITHUB_TOKEN")
    if token:
        req.add_header("Authorization", f"Bearer {token}")

    print(f"Fetching latest workflow run from {url}...")
    with urllib.request.urlopen(req) as resp:
        data = json.loads(resp.read())

    if not data.get("workflow_runs"):
        print("No successful workflow runs found on main.")
        sys.exit(1)

    run = data["workflow_runs"][0]
    run_id = run["id"]
    print(f"Latest run: #{run_id} ({run['created_at']})")

    # List artifacts for this run and find the matching label.
    url = f"{api_base}/actions/runs/{run_id}/artifacts"
    req = urllib.request.Request(url, headers={
        "Accept": "application/vnd.github+json",
        "X-GitHub-Api-Version": "2022-11-28",
    })
    if token:
        req.add_header("Authorization", f"Bearer {token}")

    with urllib.request.urlopen(req) as resp:
        data = json.loads(resp.read())

    target_name = f"menai-vm-{label}"
    artifact = None
    for a in data.get("artifacts", []):
        if a["name"] == target_name:
            artifact = a
            break

    if not artifact:
        print(f"No artifact found for label '{target_name}'.")
        print("Available artifacts:")
        for a in data.get("artifacts", []):
            print(f"  {a['name']}")
        sys.exit(1)

    # Download the artifact zip.
    archive_url = artifact["archive_download_url"]
    req = urllib.request.Request(archive_url, headers={
        "Accept": "application/vnd.github+json",
        "X-GitHub-Api-Version": "2022-11-28",
    })
    if token:
        req.add_header("Authorization", f"Bearer {token}")

    print(f"Downloading {target_name}...")
    with urllib.request.urlopen(req) as resp:
        zip_data = resp.read()

    # Extract the binary from the zip.
    with zipfile.ZipFile(io.BytesIO(zip_data)) as zf:
        names = zf.namelist()
        if not names:
            print("Artifact zip is empty.")
            sys.exit(1)

        return zf.read(names[0])


def main() -> None:
    label = detect_label()
    print(f"Platform: {label}")

    ext_suffix = sysconfig.get_config_var("EXT_SUFFIX")
    expected_filename = f"menai_vm_c{ext_suffix}"
    dest_dir = os.path.join("src", "menai")
    dest_path = os.path.join(dest_dir, expected_filename)

    print(f"Target: {dest_path}")

    binary = fetch_artifact(label)

    os.makedirs(dest_dir, exist_ok=True)
    with open(dest_path, "wb") as f:
        f.write(binary)

    print(f"Installed: {dest_path} ({len(binary)} bytes)")

    # Verify it loads.
    print("Verifying import...")
    sys.path.insert(0, "src")
    try:
        # Force reimport in case a stale version is cached.
        if "menai.menai_vm_c" in sys.modules:
            del sys.modules["menai.menai_vm_c"]
        from menai.menai_vm_c import execute  # noqa: F401
        print("C VM loaded successfully.")

    except ImportError as e:
        print(f"ERROR: C VM failed to load: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()