#!/usr/bin/env bash
#
# Restart Humbug: stop any running instance, then launch a fresh one from this
# checkout — fully detached, so it isn't tied to this terminal.
#
# Usage:  ./restart.sh
#
set -euo pipefail

# Run from the repository root (the directory this script lives in).
cd "$(dirname "$0")"

VENV_PYTHON="venv/bin/python"

# Match the running app by its launch command line.  The leading "[-]" is a
# regex character class for "-": it both stops pgrep from reading "-m" as an
# option and stops this script from matching its own command line.
PATTERN="[-]m desktop"

if [[ ! -x "$VENV_PYTHON" ]]; then
    echo "error: $VENV_PYTHON not found. Create the venv and run: pip install -e \".[dev]\"" >&2
    exit 1
fi

# Stop the running instance (gracefully, then force if it lingers).
pids=$(pgrep -f "$PATTERN" || true)
if [[ -n "$pids" ]]; then
    echo "Stopping running Humbug (PIDs: $(echo "$pids" | tr '\n' ' '))…"
    # shellcheck disable=SC2086
    kill $pids 2>/dev/null || true

    for _ in $(seq 1 20); do
        pgrep -f "$PATTERN" >/dev/null || break
        sleep 0.25
    done

    pids=$(pgrep -f "$PATTERN" || true)
    if [[ -n "$pids" ]]; then
        echo "Force-killing (PIDs: $(echo "$pids" | tr '\n' ' '))…"
        # shellcheck disable=SC2086
        kill -9 $pids 2>/dev/null || true
        sleep 0.5
    fi
else
    echo "No running Humbug found."
fi

# Launch a fresh instance fully detached: the subshell backgrounds it and exits,
# so the app is re-parented away from this terminal (no terminal stays attached,
# and closing this terminal won't kill it).
echo "Starting Humbug…"
( PYTHONPATH=src nohup "$VENV_PYTHON" -m desktop >/dev/null 2>&1 & )

sleep 1
if ! pgrep -f "$PATTERN" >/dev/null; then
    echo "warning: Humbug does not appear to have started — run '$VENV_PYTHON -m desktop' to see the error." >&2
    exit 1
fi
echo "Humbug started."

# The app logs to ~/.humbug/logs/<timestamp>.log; follow the newest one.  The app
# is detached, so Ctrl+C here only stops the tail — it does not stop Humbug.
LOG_DIR="$HOME/.humbug/logs"
latest_log=""
for _ in $(seq 1 10); do
    latest_log=$(ls -t "$LOG_DIR"/*.log 2>/dev/null | head -1 || true)
    [[ -n "$latest_log" ]] && break
    sleep 0.25
done

if [[ -n "$latest_log" ]]; then
    echo "Following $latest_log  (Ctrl+C to stop tailing; Humbug keeps running)"
    echo "------------------------------------------------------------"
    exec tail -n 20 -f "$latest_log"
else
    echo "No log file found yet under $LOG_DIR."
fi
