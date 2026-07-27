#!/usr/bin/env python3
"""
Fetch the pre-built Menai C VM binary for the current platform and Python version.

Downloads the appropriate binary from the GitHub Release and places it in
the installed menai package's vm/ directory so that the C VM is available
without a local C compiler.

No authentication required — release assets are publicly downloadable.

Usage:
    python fetch-menai-vm.py

The script detects the current platform and Python version, downloads the
matching binary from the latest release in the menai repository, and installs
it into the menai package's vm/ directory.
"""

import importlib.util
import os
import platform
import sys
import sysconfig
import urllib.request


OWNER_REPO = "m6r-ai/menai"
RELEASE_TAG = "menai-vm-latest"
DOWNLOAD_BASE = (
    f"https://github.com/{OWNER_REPO}/releases/download/{RELEASE_TAG}"
)


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

    return f"{os_label}-{arch}-{py_version}"


def resolve_menai_vm_dir() -> str:
    """Find the installed menai package's vm/ directory."""
    spec = importlib.util.find_spec("menai")
    if spec is None or spec.origin is None:
        print("ERROR: menai package is not installed.")
        print("Install it with: pip install -e ../menai")
        sys.exit(1)

    menai_init = spec.origin
    menai_pkg_dir = os.path.dirname(menai_init)
    vm_dir = os.path.join(menai_pkg_dir, "vm")

    if not os.path.isdir(vm_dir):
        print(f"ERROR: vm/ directory not found at {vm_dir}")
        sys.exit(1)

    return vm_dir


def fetch_binary(label: str) -> bytes:
    """Download the binary from the GitHub Release and return its content."""
    url = f"{DOWNLOAD_BASE}/{label}.bin"
    print(f"Downloading from {url}...")
    try:
        with urllib.request.urlopen(url) as resp:
            return resp.read()

    except OSError as e:
        # urllib raises HTTPError (a subclass of URLError, which is OSError)
        print(f"ERROR: Download failed: {e}")
        print(f"\nThis usually means either:")
        print(f"  1. The binary for your platform ({label}) hasn't been built yet.")
        print(f"  2. You're not connected to the internet.")
        print(f"\nYou can still build the C VM locally with:")
        print(f"  cd ../menai && python setup.py build_ext --inplace")
        sys.exit(1)


def main() -> None:
    label = detect_label()
    print(f"Platform: {label}")

    vm_dir = resolve_menai_vm_dir()

    ext_suffix = sysconfig.get_config_var("EXT_SUFFIX")
    expected_filename = f"menai_vm_c{ext_suffix}"
    dest_path = os.path.join(vm_dir, expected_filename)

    print(f"Target: {dest_path}")

    binary = fetch_binary(label)

    with open(dest_path, "wb") as f:
        f.write(binary)

    print(f"Installed: {dest_path} ({len(binary)} bytes)")

    # Verify it loads.
    print("Verifying import...")
    try:
        # Force reimport in case a stale version is cached.
        if "menai.vm.menai_vm_c" in sys.modules:
            del sys.modules["menai.vm.menai_vm_c"]

        from menai.vm.menai_vm_c import execute  # noqa: F401
        print("C VM loaded successfully.")

    except ImportError as e:
        print(f"ERROR: C VM failed to load: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
