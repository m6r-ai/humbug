#!/usr/bin/env bash
#
# Bootstrap a Humbug developer environment: creates the venv, installs
# Humbug + dev dependencies, clones/installs the sibling Menai repo, and
# fetches the prebuilt Menai C VM binary.
#
# Usage: ./setup-dev.sh
# Safe to re-run: existing venv/menai checkout are reused, not recreated.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

VENV_DIR="venv"
MENAI_DIR="../menai"

if command -v python3 >/dev/null 2>&1; then
    PYTHON="python3"
else
    PYTHON="python"
fi

echo "==> Python virtual environment"
if [ -d "$VENV_DIR" ]; then
    echo "    $VENV_DIR already exists, reusing it"
else
    "$PYTHON" -m venv "$VENV_DIR"
fi

# Windows venvs (including under Git Bash) use Scripts/ instead of bin/.
if [ -d "$VENV_DIR/Scripts" ]; then
    VENV_BIN="$VENV_DIR/Scripts"
else
    VENV_BIN="$VENV_DIR/bin"
fi

# shellcheck disable=SC1091
source "$VENV_BIN/activate"

pip install --upgrade pip

echo "==> Menai language engine"
# Cloned and installed in the same pip invocation as Humbug below: humbug's
# pyproject.toml requires "menai", which isn't on PyPI, so both editable
# installs must be resolved together in one command.
if [ -d "$MENAI_DIR" ]; then
    echo "    $MENAI_DIR already exists, reusing it"
else
    git clone https://github.com/m6r-ai/menai.git "$MENAI_DIR"
fi
echo "==> Installing Menai + Humbug (runtime + dev dependencies)"
pip install -e "$MENAI_DIR" -e ".[dev]"

echo "==> Fetching prebuilt Menai C VM binary"
if ! python fetch-menai-vm.py; then
    echo "    No prebuilt binary available for this platform."
    echo "    Falling back to building from source (requires a C compiler):"
    (cd "$MENAI_DIR" && python setup.py build_ext --inplace)
fi

echo
echo "Setup complete. Activate the environment with:"
echo "    source $VENV_BIN/activate"
echo "Then launch Humbug with:"
echo "    python -m desktop"
