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
# Extension entry point — the single .c file that defines the Python module.
#
SO_CORE_SOURCES := src/menai/menai_vm_c.c

#
# All C source and header files in the menai VM directory — any change to any
# of them triggers a rebuild.
#
C_SOURCES := $(wildcard src/menai/menai_vm_*.[ch])

#
# Derive the expected .so name from the Python ABI tag.
#
EXT_SUFFIX := $(shell $(PYTHON) -c "import sysconfig; print(sysconfig.get_config_var('EXT_SUFFIX'))")

SO_FILES := \
	$(patsubst src/menai/%.c, src/menai/%$(EXT_SUFFIX), $(SO_CORE_SOURCES))

#
# Build all extensions in-place.
#
.PHONY: build

build: $(SO_FILES)

$(SO_FILES): $(C_SOURCES)
	@rm -f $(SO_FILES)
	@rm -rf build/temp.* build/lib.*
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
