#!/bin/sh

# Build a Humbug AppImage from PyInstaller output.
#
# Prerequisites:
#   - PyInstaller has already been run (dist/Humbug/ must exist)
#   - appimagetool is available on PATH or downloaded by this script
#
# Usage:
#   ./build-appimage.sh
#
# Output:
#   dist/Humbug-x86_64.AppImage  (or -aarch64.AppImage on ARM)

set -e

DIST_DIR="dist"
APP_DIR="Humbug.AppDir"
BUILD_DIR="$DIST_DIR/Humbug"

if [ ! -d "$BUILD_DIR" ]; then
    echo "ERROR: $BUILD_DIR not found. Run PyInstaller first."
    exit 1
fi

# Clean up any previous AppDir
rm -rf "$DIST_DIR/$APP_DIR"
mkdir -p "$DIST_DIR/$APP_DIR"

# Copy PyInstaller output into AppDir/usr/bin
mkdir -p "$DIST_DIR/$APP_DIR/usr/bin"
cp -r "$BUILD_DIR"/* "$DIST_DIR/$APP_DIR/usr/bin/"

# Create the AppRun launcher
cat > "$DIST_DIR/$APP_DIR/AppRun" << 'APPRUN'
#!/bin/sh
SELF=$(readlink -f "$0")
HERE=$(dirname "$SELF")
exec "${HERE}/usr/bin/Humbug" "$@"
APPRUN
chmod +x "$DIST_DIR/$APP_DIR/AppRun"

# Create .desktop file
mkdir -p "$DIST_DIR/$APP_DIR/usr/share/applications"
cat > "$DIST_DIR/$APP_DIR/humbug.desktop" << 'DESKTOP'
[Desktop Entry]
Type=Application
Name=Humbug
Comment=Operating system for human-AI collaboration
Exec=Humbug
Icon=humbug
Terminal=false
Categories=Development;
DESKTOP

# Also place a copy in the standard applications directory
cp "$DIST_DIR/$APP_DIR/humbug.desktop" "$DIST_DIR/$APP_DIR/usr/share/applications/humbug.desktop"

# Install icon
mkdir -p "$DIST_DIR/$APP_DIR/usr/share/icons/hicolor/256x256/apps"
cp resources/icons/app-icon.png "$DIST_DIR/$APP_DIR/usr/share/icons/hicolor/256x256/apps/humbug.png"
cp resources/icons/app-icon.png "$DIST_DIR/$APP_DIR/humbug.png"

# Determine architecture suffix
ARCH=$(uname -m)
case "$ARCH" in
    x86_64|amd64)
        APPIMAGE_ARCH="x86_64"
        ;;
    aarch64|arm64)
        APPIMAGE_ARCH="aarch64"
        ;;
    *)
        APPIMAGE_ARCH="$ARCH"
        ;;
esac

# Get or download appimagetool
if command -v appimagetool >/dev/null 2>&1; then
    AIM=appimagetool
else
    AIM="$DIST_DIR/appimagetool"
    if [ ! -f "$AIM" ]; then
        echo "Downloading appimagetool..."
        APPIMAGE_RELEASE_URL="https://github.com/AppImage/appimagetool/releases/download/continuous/appimagetool-${APPIMAGE_ARCH}.AppImage"
        curl -fsSL -o "$AIM" "$APPIMAGE_RELEASE_URL"
        chmod +x "$AIM"
        # Workaround for FUSE issues on some CI runners
        export APPIMAGE_EXTRACT_AND_RUN=1
    fi
fi

# Build the AppImage
OUTPUT="$DIST_DIR/Humbug-${APPIMAGE_ARCH}.AppImage"
echo "Building $OUTPUT ..."
"$AIM" "$DIST_DIR/$APP_DIR" "$OUTPUT"

echo "Done: $OUTPUT"
