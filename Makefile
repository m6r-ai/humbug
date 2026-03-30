.DEFAULT_GOAL := build

#
# Detect the operating system.
#
UNAME := $(shell uname -s)

#
# Python interpreter — uses the venv if present, otherwise system python3.
#
PYTHON := $(shell test -f venv/bin/python && echo venv/bin/python || echo python3)

#
# Extension source files.
#
C_SOURCES := \
	src/menai/menai_value_c.c \
	src/menai/menai_vm_c.c

#
# Derive the expected .so names from the Python ABI tag.
#
EXT_SUFFIX := $(shell $(PYTHON) -c "import sysconfig; print(sysconfig.get_config_var('EXT_SUFFIX'))")

SO_FILES := \
	$(patsubst src/menai/%.c, src/menai/%$(EXT_SUFFIX), $(C_SOURCES))

#
# Build all extensions in-place.
#
.PHONY: build

build: $(SO_FILES)

$(SO_FILES): $(C_SOURCES) src/menai/menai_value_c.h
	$(PYTHON) setup.py build_ext --inplace
	@mkdir -p build && touch $@

#
# Run the full test suite.
#
.PHONY: test

test:
	$(PYTHON) -m pytest tests/

#
# Run only the Menai tests.
#
.PHONY: test-menai

test-menai:
	$(PYTHON) -m pytest tests/menai/

#
# Build the macOS application bundle and DMG.
#
.PHONY: app

app: build
	$(PYTHON) -m PyInstaller humbug.spec
	./build-dmg.sh

#
# Remove the compiled .so files (reverts to pure-Python fallback).
#
.PHONY: clean

clean:
	rm -f $(SO_FILES)

#
# Remove all build artefacts.
#
.PHONY: realclean

realclean: clean
	rm -rf build/temp.* build/lib.*
	find src -name "*.pyc" -delete
	find src -name "__pycache__" -type d -exec rm -rf {} +
