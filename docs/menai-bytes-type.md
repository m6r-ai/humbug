# Menai Bytes Type — Design Document

## Motivation

Menai currently handles only text (Unicode strings). Many binary file formats — zip
archives, PNG images, PDFs, protobuf messages, ELF binaries — require reading and
manipulating raw byte sequences. Today these formats can only be processed by shelling
out to external tools via the terminal, which hurts portability and composability.

Adding a `bytes` type to Menai enables pure-Menai library implementations of binary
format parsers and builders. This keeps format logic in the language layer where it is
reusable, composable, and testable, rather than fragmenting it across external tool
dependencies.

## Architecture

The bytes type follows a clean separation of concerns:

- **Menai** handles pure computation on bytes — parsing, building, transforming. No
  side effects, no I/O, no permission model. This is consistent with Menai's existing
  design philosophy.
- **The I/O framework** (a separate concern, not part of this proposal) handles reading
  bytes from and writing bytes to the filesystem, network, and other sources. It manages
  human permission and approval for side effects. This is the seam layer that feeds bytes
  into Menai and writes bytes back out.

Menai never touches the filesystem. It receives bytes values, computes on them, and
returns bytes values. The I/O framework handles the boundary with the outside world.

## Type: bytes

A new runtime value type representing an immutable sequence of bytes (octets, 0–255).
Values are immutable, like all Menai values. The type follows existing Menai conventions:
explicit per-type operators (`bytes-ref`, `bytes-slice`, `bytes-length`), strict runtime
typing, no implicit coercion.

### Naming

The type is called `bytes`, not `blob`, because:

- It says exactly what it is — a sequence of bytes.
- No SQL baggage ("BLOB" implies opaque, unstructured data — the opposite of what we
  want, since the whole point is to parse and manipulate binary data).
- Consistent with mainstream languages (Python `bytes`, Rust `[u8]`/`Bytes`, C# `byte[]`).

### No new literal syntax

Bytes values are constructed via VM opcodes that fold to constants at compile
time by the existing constant folder:

```menai
(string->bytes "hello")           ; UTF-8 encoded bytes
(string-hex->bytes "504b0304")    ; hex-encoded bytes
```

No `#hex"..."` or `#utf8"..."` literal syntax is needed. The constant folder already
produces optimal encodings when VM opcodes are called with literal arguments.
Adding literal syntax would provide only cosmetic brevity with no semantic or runtime
benefit, and would add parsing complexity and potential confusion about when to use
literals vs VM opcodes.

This is consistent with Menai's existing philosophy of explicit, unambiguous function
names over syntactic sugar.

### Strings are Unicode, not byte sequences

Menai strings are sequences of Unicode codepoints, not byte sequences. There is no
"Latin-1 string encoding" — that concept is meaningless when strings are already fully
decoded Unicode. The bridge between strings and bytes is UTF-8 encode/decode plus hex
representation, all implemented as VM opcodes. If you need specific byte values that
don't correspond to a Unicode string, use hex.

## VM Opcodes vs Prelude Functions

Operations are divided into two categories:

- **VM opcodes** — implemented in the C VM for correctness (require access to the
  underlying byte storage) or performance (hot paths in binary format parsing).
- **Prelude functions** — implemented in Menai as compositions of VM opcodes. These
  include all higher-order operations and trivial compositions.

The distinction follows the existing pattern: opcode-backed builtins are registered in
`BUILTIN_OPCODE_ARITIES` in `menai_builtin_registry.py`; prelude functions are Menai
lambdas in `_PRELUDE_SOURCE` in `menai.py`.

### Why the line is drawn where it is

Multi-byte reads and writes are VM opcodes because each one is a single native memory
operation. In Menai, `bytes-read-u32-le` would be 4 `bytes-ref` calls, 3 bit shifts,
3 bit-ors, and 7 intermediate integer allocations. In a format parser reading hundreds
of fields, that overhead dominates. The compiler's constant folder cannot help when
offsets are runtime variables.

LEB128 encode/decode is a VM opcode because the continuation-bit loop with per-iteration
`bytes-ref` + bit ops + integer boxing is significant when parsing formats with many
varint fields (e.g., protobuf messages).

Higher-order operations (`map-bytes`, `filter-bytes`, `fold-bytes`, `zip-bytes`) are
prelude functions because they must call back into user-provided Menai lambdas per
element — the VM cannot eliminate that function call. The iteration itself is Menai
code built on `bytes-ref` and `bytes-append-u8`.

## VM Opcodes

### Type foundation (4)

These cannot exist without native support — they require access to the runtime type
tag or internal byte storage.

```menai
(bytes? x)                    ; type predicate
(bytes)                       ; construct empty bytes
(bytes-length b)              ; length field access
(bytes=? b1 b2)               ; raw byte comparison
```

### Single-byte bridge (2)

The irreducible primitives — all multi-byte operations and prelude functions build on
these.

```menai
(bytes-ref b offset)          ; read one byte → integer (0-255)
(bytes-append-u8 b value)     ; append one byte (value must be 0-255)
```

### Construction from list (1)

Could be a fold of `bytes-append-u8`, but that's O(n) allocations for what should be a
single allocation. Named `list->bytes` to mirror `bytes->list` — the inverse operation.

```menai
(list->bytes lst)             ; construct bytes from list of integers (0-255 each)
```

### Slicing (1)

Essential for parsing. If the VM implements bytes with structural sharing (offset +
length into a backing buffer), slicing is O(1). This cannot be done in Menai because
Menai code cannot create internal pointers into byte storage. Follows the same pattern
as `string-slice`: a VM opcode with arity 3 (bytes, start, end), a prelude wrapper for
the optional-end case, and a desugarer computed-default that synthesises
`($bytes-length b)` when the end is omitted.

```menai
(bytes-slice b start end)     ; sub-bytes from start (inclusive) to end (exclusive)
(bytes-slice b start)         ; sub-bytes from start to end
```

### String and hex conversions (5)

The bridge between Unicode strings and bytes. UTF-8 encode/decode involves multi-byte
sequence handling that is non-trivial but expressible in Menai. However, these are
common enough and the performance difference significant enough to warrant native
implementation. Hex conversions are VM opcodes rather than prelude functions so that
the constant folder can fold them when called with literal arguments — this is the
de facto bytes literal mechanism (see Constant Folding).

```menai
(string->bytes s)             ; UTF-8 encode
(bytes->string b)             ; UTF-8 decode (strict — error on invalid sequences)
(bytes->list b)               ; list of integers (0-255 each)
(bytes->string-hex b)         ; hex encode → lowercase hex string
(string-hex->bytes s)         ; hex decode (error on invalid hex string)
```

### Inequality (1)

Consistency with `string!=?`. Avoids a `boolean-not` wrapper on every inequality check.
Can be simplified later if the optimizer learns to collapse it.

```menai
(bytes!=? b1 b2)              ; negation of bytes=?
```

### Multi-byte reads (17)

Each is a single native memory read. In Menai, these would be compositions of
`bytes-ref` + bit shifts + bit ors with intermediate integer allocations — too expensive
in hot parsing loops.

```menai
; Unsigned, no endianness (single byte)
(bytes-read-u8 b offset)

; Unsigned, little-endian
(bytes-read-u16-le b offset)
(bytes-read-u24-le b offset)
(bytes-read-u32-le b offset)
(bytes-read-u64-le b offset)

; Unsigned, big-endian
(bytes-read-u16-be b offset)
(bytes-read-u24-be b offset)
(bytes-read-u32-be b offset)
(bytes-read-u64-be b offset)

; Signed (two's complement), little-endian
(bytes-read-i8 b offset)
(bytes-read-i16-le b offset)
(bytes-read-i24-le b offset)
(bytes-read-i32-le b offset)
(bytes-read-i64-le b offset)

; Signed (two's complement), big-endian
(bytes-read-i16-be b offset)
(bytes-read-i24-be b offset)
(bytes-read-i32-be b offset)
(bytes-read-i64-be b offset)
```

`u8` and `i8` have no endianness variant (single byte). All return integers. Out-of-bounds
reads raise a runtime error.

### Multi-byte append (18)

Each is a single native multi-byte write + one allocation. In Menai, these would be
multiple `bytes-append-u8` calls with intermediate bytes allocations.

```menai
; Unsigned
(bytes-append-u8 b value)
(bytes-append-u16-le b value)
(bytes-append-u16-be b value)
(bytes-append-u24-le b value)
(bytes-append-u24-be b value)
(bytes-append-u32-le b value)
(bytes-append-u32-be b value)
(bytes-append-u64-le b value)
(bytes-append-u64-be b value)

; Signed
(bytes-append-i8 b value)
(bytes-append-i16-le b value)
(bytes-append-i16-be b value)
(bytes-append-i24-le b value)
(bytes-append-i24-be b value)
(bytes-append-i32-le b value)
(bytes-append-i32-be b value)
(bytes-append-i64-le b value)
(bytes-append-i64-be b value)
```

All return new bytes (immutability). Values out of range for the width raise a runtime
error (e.g., passing 300 to `bytes-append-u8`).

### Multi-byte write (18)

Patching at a specific offset — used for writing CRC fields, size fields, and offset
fields after computing the full content. Same performance reasoning as append.

```menai
; Unsigned
(bytes-write-u8 b offset value)
(bytes-write-u16-le b offset value)
(bytes-write-u16-be b offset value)
(bytes-write-u24-le b offset value)
(bytes-write-u24-be b offset value)
(bytes-write-u32-le b offset value)
(bytes-write-u32-be b offset value)
(bytes-write-u64-le b offset value)
(bytes-write-u64-be b offset value)

; Signed
(bytes-write-i8 b offset value)
(bytes-write-i16-le b offset value)
(bytes-write-i16-be b offset value)
(bytes-write-i24-le b offset value)
(bytes-write-i24-be b offset value)
(bytes-write-i32-le b offset value)
(bytes-write-i32-be b offset value)
(bytes-write-i64-le b offset value)
(bytes-write-i64-be b offset value)
```

Returns new bytes (immutability). Out-of-bounds writes raise a runtime error.

### Variable-length integers (4)

LEB128 encode/decode involves continuation-bit loops. The byte-level mechanism is
non-trivial (continuation bit handling, sign extension for sleb128) and cannot be
expressed as a simple composition of other bytes operations.

```menai
(bytes-read-uleb128 b offset)     ; → (value next-offset)
(bytes-append-uleb128 b value)

(bytes-read-sleb128 b offset)     ; → (value next-offset)
(bytes-append-sleb128 b value)
```

Read functions return a two-element list `(value next-offset)` so the caller knows where
the next field begins. This is the standard pattern for walking variable-length formats.

ZigZag encoding (used by protobuf sint32/sint64 and Avro) is NOT included as a VM opcode
or bytes operation. It is a pure integer transform — `(integer-bit-xor (integer-bit-shift-left n 1) (integer-bit-shift-right n 63))` for encode, and a similar one-liner for
decode. A protobuf library can define it in two lines of Menai.

### Search (2)

Hot paths in format parsing — scanning for magic numbers, delimiters, boundaries.
Native implementation can use optimized search (memchr, Boyer-Moore, etc.).

```menai
(bytes-index needle haystack)    ; offset of first occurrence of needle, or #none
(bytes-index-int byte b)          ; offset of first byte equal to byte, or #none
```

`bytes-index-int` is the single-byte scan case — finding null terminators, newlines,
delimiters. Common enough in binary format parsing to warrant its own opcode rather
than constructing a single-byte bytes value for `bytes-index`.

### Concatenation (1)

Critical for construction. If implemented as a fold of `bytes-append-u8` or pairwise
`bytes-concat`, the result is O(n²) allocation. Native can precompute total length,
allocate once, and memcpy.

Variadic calls are handled the same way as `string-concat` and `list-concat`: the
desugarer fold-reduces `(bytes-concat b1 b2 b3 ...)` into nested binary
`($bytes-concat ($bytes-concat b1 b2) b3) ...` calls, and the prelude provides a
zero-arg identity wrapper that returns empty bytes. The VM opcode itself is binary
(arity 2).

```menai
(bytes-concat b1 b2 ...)         ; concatenated bytes
(bytes-concat)                   ; empty bytes (zero-arg identity, prelude wrapper)
```

### Lexicographic comparison (4)

Native `memcmp` is dramatically faster than a Menai loop of `bytes-ref` + `integer<?` +
`bytes=?` on remainders. Useful for sorting, binary search over byte sequences.

```menai
(bytes<? b1 b2)
(bytes>? b1 b2)
(bytes<=? b1 b2)
(bytes>=? b1 b2)
```

### VM opcode summary

| Category | Count |
|----------|-------|
| Type foundation | 4 |
| Single-byte bridge | 2 |
| Construction from list | 1 |
| Slicing | 1 |
| String and hex conversions | 5 |
| Inequality | 1 |
| Multi-byte reads | 17 |
| Multi-byte append | 18 |
| Multi-byte write | 18 |
| Variable-length integers | 4 |
| Search | 2 |
| Concatenation | 1 |
| Lexicographic comparison | 4 |
| **Total** | **~78** |

## Prelude Functions

### Higher-order operations (4)

Implemented in Menai as compositions of `bytes-ref`, `bytes-length`, `bytes-append-u8`,
and `bytes->list`. The VM cannot eliminate the per-element callback into user-provided
lambdas, so there is no performance benefit to making these native.

```menai
(map-bytes (lambda (byte) (integer-bit-xor byte 0xFF)) b)
(filter-bytes (lambda (byte) (integer<? byte 128)) b)
(fold-bytes (lambda (acc byte) (integer+ acc byte)) 0 b)
(zip-bytes b1 b2)               ; → list of (byte byte) pairs
```

### Convenience predicates (3)

Trivial compositions with no performance concern.

```menai
(bytes-empty? b)                ; (integer=? (bytes-length b) 0)
(bytes-prefix? b prefix)         ; (bytes=? (bytes-slice b 0 (bytes-length prefix)) prefix)
(bytes-suffix? b suffix)         ; (bytes=? (bytes-slice b (integer- (bytes-length b) (bytes-length suffix))) suffix)
```

### Splitting (2)

Loops over `bytes-index` + `bytes-slice`.

```menai
(bytes-split b delimiter)        ; list of bytes segments
(bytes-split-int b byte)         ; split on single byte value (e.g. null delimiter)
```

### Deliberately excluded

The following were considered and rejected:

- **`bytes-contains?`** — no precedent (Menai has no `string-contains?`), trivial
  composition: `(not (none? (bytes-index needle haystack)))`.
- **`bytes-empty?` as a VM opcode** — just `(integer=? (bytes-length b) 0)`, same
  internal field check.
- **ZigZag varint functions** — pure integer transform, two lines of Menai using
  existing bitwise ops. Belongs in format-specific libraries (e.g., a protobuf module),
  not the core bytes API.
- **Latin-1 string encoding** — meaningless when Menai strings are already Unicode. The
  only use case (lossless byte-to-char mapping for legacy zip filenames) is a
  library-level concern, not a core type operation.
- **Base64 literals** — never authored by hand; useful for transport, not for source
  code. Runtime conversion (`string-base64->bytes`) can be added as a prelude function
  if needed.
- **New literal syntax** (`#hex"..."`, `#utf8"..."`) — the constant folder already
  produces optimal encodings from `string->bytes` and `string-hex->bytes` VM opcodes
  when called with literal arguments. New syntax adds parsing complexity with no
  semantic or runtime benefit.

## Implementation Plan

### Phase 1: Core VM support

1. Add `MenaiBytes` value type to the C VM (`menai_vm_c.h` and a new `menai_vm_bytes.c`).
   The C struct stores an immutable byte buffer with structural sharing for O(1) slicing
   (offset + length into a backing buffer, following the same pattern as `MenaiList`).
   Add a `MENAITYPE_BYTES` type tag. Add bytes support to `menai_value_hash` and
   `menai_value_equal` so bytes can be used as dict keys.
2. Add a thin `MenaiBytes` wrapper to `menai_value.py` for the slow-world bridge
   (Python fallback and `to_python` / `describe` / `to_hashable_key` interop).
3. Register all VM opcodes in `BUILTIN_OPCODE_ARITIES` in `menai_builtin_registry.py`.
4. Implement opcodes in the C VM:
   - Type foundation, single-byte bridge, construction, slicing, string and hex
     conversions, inequality.
   - Multi-byte reads, appends, writes.
   - Variable-length integers (LEB128).
   - Search, concatenation, lexicographic comparison.
5. Add `bytes?` type predicate support to the semantic analyzer and desugarer (type
   guard for `match` on bytes literals, computed-default for `bytes-slice`).

### Phase 2: Prelude functions

1. Add prelude implementations to `_PRELUDE_SOURCE` in `menai.py`:
   - `map-bytes`, `filter-bytes`, `fold-bytes`, `zip-bytes`
   - `bytes-empty?`, `bytes-prefix?`, `bytes-suffix?`
   - `bytes-split`, `bytes-split-int`
2. Add `bytes-concat` to the fold-reducible variadic list in the desugarer (alongside
   `string-concat`, `list-concat`).

### Phase 3: Constant folding

1. Add a new `MenaiASTBytes` AST node class to `menai_ast.py` (carrying raw byte data
   as a Python `bytes` object) so the constant folder can emit bytes constants.
2. Extend `MenaiASTConstantFolder` to fold `string->bytes` and `string-hex->bytes`
   calls with literal arguments into `MenaiASTBytes` nodes.
3. This serves as the bytes literal mechanism — no new literal syntax needed.
4. Folding is placed before tests so that test code can use `string-hex->bytes` and
   `string->bytes` with literals without runtime decode overhead.

### Phase 4: Tests

1. Unit tests for every VM opcode (type checking, correctness, bounds checking, error
   conditions).
2. Unit tests for every prelude function.
3. Integration tests demonstrating real format parsing (e.g., reading a zip local file
   header, parsing a PNG signature chunk).

### Phase 5: I/O framework integration (separate concern)

1. Extend the I/O framework (filesystem tool, editor tool, etc.) to read files as bytes
   and pass them to Menai as `MenaiBytes` values.
2. Extend `transform_file` and `editor.transform` to accept bytes input and produce bytes
   output, in addition to the existing text-based `input-text` / `input-lines` model.
3. This is the permission boundary — Menai stays pure, the I/O framework handles human
   approval for reading and writing binary files.

## Implementation Status

### Completed

- **C VM value type**: `MenaiBytes` struct in `menai_vm_c.h` with structural sharing for O(1)
  slicing (owner/view pattern mirroring `MenaiList`). Implementation in `menai_vm_bytes.c`.
  Type tag `MENAITYPE_BYTES` (0x000e). Hash, equality, compare, dealloc all wired into the
  C VM dispatch (`menai_vm_value.c`, `menai_vm_hashtable.c`).
- **Bridge layer**: `menai_convert_value` (slow→fast) and `menai_value_to_slow_value`
  (fast→slow) handle bytes in `menai_vm_bridge.c`.
- **Python value type**: `MenaiBytes` in `menai_value.py` with `to_hashable_key` support
  for dict keys.
- **AST node**: `MenaiASTBytes` in `menai_ast.py`.
- **Bytecode opcodes**: 40+ opcodes registered in `menai_bytecode.py` (range 400–477)
  and `BUILTIN_OPCODE_MAP`.
- **Builtin registry**: All arities in `BUILTIN_FUNCTION_ARITIES` in
  `menai_builtin_registry.py`.
- **Desugarer**: `bytes-slice` computed-default, `bytes-concat` fold-reducible variadic,
  `bytes=?`/`bytes!=?` strict equality, `bytes<?`/`bytes>?`/`bytes<=?`/`bytes>=?`
  comparison chains, `MenaiASTBytes` in match pattern handling.
- **Constant folder**: `string->bytes` and `string-hex->bytes` fold to `MenaiASTBytes`.
- **IR builder**: `MenaiASTBytes` added to the constant literal tuple.
- **Bytecode builder**: `MenaiBytes` added to constant pool dedup key.
- **Prelude**: All bytes builtins as prelude lambdas in `menai.py`.
- **C VM opcode handlers**: All implemented in `menai_vm_c.c` — type predicate, equality,
  inequality, length, ref, append-u8, list→bytes, slice, string↔bytes (UTF-8), hex
  conversions, bytes→list, concat, index, index-int, lexicographic comparisons, and all
  18 multi-byte reads (u8/i8 + u16/u24/u32/u64 LE/BE + i16/i24/i32/i64 LE/BE) via a
  `BYTES_READ_MULTI` macro with sign extension.
- **Integer helpers**: `integer_to_long` and `integer_to_ssize_t` added to `menai_vm_c.c`
  for extracting C integers from `MenaiInteger` in opcode handlers.
- **Build**: `menai_vm_bytes.c` added to `setup.py`.
- **Tests**: `tests/menai/test_bytes.py` with 196 test cases covering all implemented
  opcodes. 192 passing, 4 failing (see Known Issues).
  opcodes. All 196 passing.

### Not Yet Implemented

- Multi-byte **append** opcodes (18): `bytes-append-u16-le`, etc.
- Multi-byte **write** opcodes (18): `bytes-write-u16-le`, etc.
- LEB128 encode/decode (4): `bytes-read-uleb128`, `bytes-append-uleb128`,
  `bytes-read-sleb128`, `bytes-append-sleb128`.
- Prelude functions: `map-bytes`, `filter-bytes`, `fold-bytes`, `zip-bytes`,
  `bytes-empty?`, `bytes-prefix?`, `bytes-suffix?`, `bytes-split`, `bytes-split-int`.
- Phase 5: I/O framework integration.

## Example: Parsing a Zip Local File Header

This demonstrates how the bytes type enables pure-Menai binary format parsing:

```menai
(let ((parse-local-file-header
        (lambda (b)
          (if (bytes!=? (bytes-slice b 0 4) (string-hex->bytes "504b0304"))
              (error "not a zip local file header")
              (dict
                "signature"         (bytes-slice b 0 4)
                "version-needed"    (bytes-read-u16-le b 4)
                "flags"             (bytes-read-u16-le b 6)
                "compression"       (bytes-read-u16-le b 8)
                "mod-time"          (bytes-read-u16-le b 10)
                "mod-date"          (bytes-read-u16-le b 12)
                "crc32"             (bytes-read-u32-le b 14)
                "compressed-size"   (bytes-read-u32-le b 18)
                "uncompressed-size" (bytes-read-u32-le b 22)
                "filename-length"   (bytes-read-u16-le b 26)
                "extra-length"      (bytes-read-u16-le b 28))))))
  (parse-local-file-header some-bytes))
```

## Design Decisions

### Why `bytes` not `blob`

"Blob" carries SQL baggage (Binary Large OBject) and implies opaque, unstructured data.
The whole point is to parse and manipulate binary data, so the type name should reflect
that. "Bytes" is accurate, conventional (Python, Rust, C#, Java), and consistent with
Menai's noun-based type names (`list`, `string`, `dict`, `set`).

### Why no literal syntax

The constant folder already produces optimal encodings when `string->bytes` and
`string-hex->bytes` VM opcodes are called with literal arguments.
`string-hex->bytes "504b0304"` folds to the same bytes
value that `#hex"504b0304"` would produce. New literal syntax adds parsing complexity,
documentation burden, and potential confusion with no semantic or runtime benefit.

### Why `bytes-read-u8` when we have `bytes-ref`

Redundant in semantics (both read a single byte), but keeping `bytes-read-u8` makes
code that reads mixed-width fields read uniformly:

```menai
(dict
  "version"    (bytes-read-u8 b 0)
  "flags"      (bytes-read-u16-le b 1)
  "entry-count" (bytes-read-u32-le b 3))
```

versus the jarring mix:

```menai
(dict
  "version"    (bytes-ref b 0)          ; different function name
  "flags"      (bytes-read-u16-le b 1)   ; for the same conceptual operation
  "entry-count" (bytes-read-u32-le b 3))
```

### Why u24 variants

24-bit integers appear in real formats: MP3 frame headers, FLAC stream info, various
embedded format fields. They are awkward to emulate with u16 + u8 reads due to
endianness. Including them avoids error-prone manual composition.

### Why LEB128 is a VM opcode but ZigZag is not

LEB128's byte-level mechanism (continuation bits, 7-data-bits-per-byte, sign extension)
is genuinely non-trivial and cannot be expressed as a simple composition of other bytes
operations. ZigZag is a pure integer transform — two lines of existing bitwise ops. A
format library that needs ZigZag + LEB128 can compose them trivially.

### Why the return value of `bytes-read-uleb128` is a list

`(value next-offset)` — a two-element list. The caller destructures with `match`:

```menai
(match (bytes-read-uleb128 b offset)
  ((value next-offset)
   ...))
```

This is idiomatic Menai and doesn't require defining a struct type for a simple pair.

### Why hex conversions are VM opcodes, not prelude functions

`bytes->string-hex` and `string-hex->bytes` could be prelude functions composed from
`integer->string` / `string->integer` with radix 16. However, making them VM opcodes
allows the constant folder to fold them when called with literal arguments — this is
the de facto bytes literal mechanism. The constant folder operates on `$`-prefixed
primitives (VM opcodes), not prelude function names. Promoting hex conversions to
opcodes keeps the folder's architecture unchanged.

### Why `bytes-slice` follows the `string-slice` pattern

`bytes-slice` has a VM opcode (arity 3: bytes, start, end) with a prelude wrapper for
the optional-end case and a desugarer computed-default completion. This mirrors
`string-slice` and `list-slice` exactly: `(bytes-slice b start)` desugars to
`(let ((#:t b)) ($bytes-slice #:t start ($bytes-length #:t)))`.

### Why `bytes-concat` follows the `string-concat` pattern

The VM opcode is binary (arity 2). Variadic calls are fold-reduced by the desugarer
into nested binary calls, and the prelude provides a zero-arg identity wrapper. This
is the same pattern as `string-concat` and `list-concat`.

### Why bytes can be dict keys

Bytes values are hashable and can be used as dict keys. This is useful for lookup
tables keyed by magic numbers, file signatures, or other fixed byte sequences. The
hash is based on the logical byte content, not the internal representation, so two
`MenaiBytes` values with the same bytes are equal regardless of whether one is a
slice view and the other is a standalone copy.

### Display format

`(describe some-bytes)` produces a hex representation: `#bytes"504b0304"`. Values
longer than 64 bytes are truncated with `...` after the first 64 bytes (128 hex
characters). This is a display format, not literal syntax.