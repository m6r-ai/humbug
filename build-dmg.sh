#!/bin/sh

# If the DMG already exists, delete it.
test -f "dist/Humbug.dmg" && rm "dist/Humbug.dmg"

create-dmg \
  --volname "Humbug" \
  --window-pos 200 120 \
  --window-size 600 300 \
  --icon-size 100 \
  --icon "Humbug.app" 175 120 \
  --hide-extension "Humbug.app" \
  --app-drop-link 425 120 \
  "dist/Humbug.dmg" \
  "dist/Humbug.app"
