.DEFAULT_GOAL := test

#
# Detect the operating system.
#
UNAME := $(shell uname -s)

#
# Python interpreter — uses the venv if present, otherwise system python3.
#
PYTHON := $(shell test -f venv/bin/python && echo venv/bin/python || echo python3)

#
# Run the full test suite.
#
.PHONY: test

test:
	$(PYTHON) -m pytest tests/

#
# Fetch the pre-built Menai C VM binary (if not already present).
#
.PHONY: fetch-vm

fetch-vm:
	$(PYTHON) fetch-menai-vm.py

#
# Build the macOS application bundle and DMG.
#
.PHONY: app

app:
	$(PYTHON) -m PyInstaller humbug.spec
	./build-dmg.sh
