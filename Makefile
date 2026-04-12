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
SO_CORE_SOURCES := \
	src/menai/menai_vm_c.c

#
# Derive the expected .so names from the Python ABI tag.
#
EXT_SUFFIX := $(shell $(PYTHON) -c "import sysconfig; print(sysconfig.get_config_var('EXT_SUFFIX'))")

SO_FILES := \
	$(patsubst src/menai/%.c, src/menai/%$(EXT_SUFFIX), $(SO_CORE_SOURCES))

C_SOURCES := \
	src/menai/menai_vm_c.c \
	src/menai/menai_vm_string.c \
	src/menai/menai_vm_string.h \
	src/menai/menai_vm_string_tables.h \
	src/menai/menai_vm_function.c \
	src/menai/menai_vm_function.h \
	src/menai/menai_vm_symbol.c \
	src/menai/menai_vm_symbol.h \
	src/menai/menai_vm_list.c \
	src/menai/menai_vm_list.h \
	src/menai/menai_vm_set.c \
	src/menai/menai_vm_set.h \
	src/menai/menai_vm_complex.c \
	src/menai/menai_vm_complex.h \
	src/menai/menai_vm_float.c \
	src/menai/menai_vm_float.h \
	src/menai/menai_vm_integer.c \
	src/menai/menai_vm_integer.h \
	src/menai/menai_vm_boolean.c \
	src/menai/menai_vm_boolean.h \
	src/menai/menai_vm_value.c \
	src/menai/menai_vm_value.h \
	src/menai/menai_vm_none.c \
	src/menai/menai_vm_none.h

#
# Build all extensions in-place.
#
.PHONY: build

build: $(SO_FILES)

$(SO_FILES): $(C_SOURCES)
	@rm -f $(SO_FILES)
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
