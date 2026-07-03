#!/bin/sh
set -e

# Build a signed/notarized DMG from the PyInstaller .app bundle.
#
# Output:
#   dist/Humbug-v<version>-macos-<arch>.dmg

# Get version from version.py
VERSION=$(python -c "from src.desktop.version import CURRENT_VERSION; print(CURRENT_VERSION)")
if [ -z "$VERSION" ]; then
    echo "ERROR: Could not determine version."
    exit 1
fi

# Determine architecture
ARCH=$(uname -m)
case "$ARCH" in
    x86_64|amd64)
        DMG_ARCH="x86_64"
        ;;
    aarch64|arm64)
        DMG_ARCH="arm64"
        ;;
    *)
        echo "ERROR: Unsupported architecture: $ARCH"
        exit 1
        ;;
esac

OUTPUT="dist/Humbug-v${VERSION}-macos-${DMG_ARCH}.dmg"

# If the DMG already exists, delete it.
test -f "$OUTPUT" && rm "$OUTPUT"

create-dmg \
  --volname "Humbug" \
  --window-pos 200 120 \
  --window-size 600 300 \
  --icon-size 100 \
  --icon "Humbug.app" 175 120 \
  --hide-extension "Humbug.app" \
  --app-drop-link 425 120 \
  "$OUTPUT" \
  "dist/Humbug.app"
