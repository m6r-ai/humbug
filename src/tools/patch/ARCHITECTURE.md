# AIFPL Patcher - Architecture Diagram

## System Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                                                                         │
│                          AIFPL PATCHER SYSTEM                           │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────────┐
│                         USER INTERACTION LAYER                          │
│                         (aifpl_patcher.py)                              │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  • Command-line argument parsing                                       │
│  • User input validation                                               │
│  • Colored terminal output                                             │
│  • Progress and status messages                                        │
│  • Error reporting                                                     │
│                                                                         │
└────────────────┬────────────────────────────────────┬──────────────────┘
                 │                                    │
                 ▼                                    ▼
┌────────────────────────────────┐   ┌──────────────────────────────────┐
│   FILE I/O OPERATIONS          │   │   BACKUP MANAGEMENT              │
│                                │   │                                  │
│  • Read source files           │   │  • Create .bak files             │
│  • Write patched files         │   │  • Verify backups                │
│  • Handle encodings (UTF-8)    │   │  • Restore on error              │
│  • Preserve file attributes    │   │                                  │
└────────────────┬───────────────┘   └──────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                      DIFF PARSING LAYER                                 │
│                   (unified_diff_parser.py)                              │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  INPUT: Unified diff text                                              │
│  ┌────────────────────────────────────────────────┐                   │
│  │ --- a/file.py                                  │                   │
│  │ +++ b/file.py                                  │                   │
│  │ @@ -10,7 +10,8 @@                              │                   │
│  │  context line                                  │                   │
│  │ -old line                                      │                   │
│  │ +new line                                      │                   │
│  └────────────────────────────────────────────────┘                   │
│                                                                         │
│  PROCESSING:                                                           │
│  • Parse file headers (---, +++)                                      │
│  • Extract target filename                                            │
│  • Parse hunk headers (@@ ... @@)                                     │
│  • Identify change types (context, delete, insert)                   │
│  • Handle git-style paths (a/, b/)                                   │
│                                                                         │
│  OUTPUT: Structured hunks                                              │
│  ┌────────────────────────────────────────────────┐                   │
│  │ [                                              │                   │
│  │   {                                            │                   │
│  │     'start_line': 9,                           │                   │
│  │     'old_count': 7,                            │                   │
│  │     'new_count': 8,                            │                   │
│  │     'changes': [                               │                   │
│  │       ('context', 'context line'),             │                   │
│  │       ('delete', 'old line'),                  │                   │
│  │       ('insert', 'new line')                   │                   │
│  │     ]                                          │                   │
│  │   }                                            │                   │
│  │ ]                                              │                   │
│  └────────────────────────────────────────────────┘                   │
│                                                                         │
└────────────────┬────────────────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                    DATA CONVERSION LAYER                                │
│                   (aifpl_patch_bridge.py)                               │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  Python Data → AIFPL Expressions                                       │
│  ┌────────────────────────────────────────────────┐                   │
│  │ Python:                                        │                   │
│  │   ['line 1', 'line 2', 'line 3']               │                   │
│  │                                                │                   │
│  │ AIFPL:                                         │                   │
│  │   (list "line 1" "line 2" "line 3")            │                   │
│  └────────────────────────────────────────────────┘                   │
│                                                                         │
│  ┌────────────────────────────────────────────────┐                   │
│  │ Python:                                        │                   │
│  │   {                                            │                   │
│  │     'start_line': 9,                           │                   │
│  │     'changes': [('delete', 'old')]             │                   │
│  │   }                                            │                   │
│  │                                                │                   │
│  │ AIFPL:                                         │                   │
│  │   (list                                        │                   │
│  │     (list "start-line" 9)                      │                   │
│  │     (list "changes"                            │                   │
│  │       (list (list "delete" "old"))))           │                   │
│  └────────────────────────────────────────────────┘                   │
│                                                                         │
│  • String escaping and quoting                                        │
│  • List structure conversion                                          │
│  • Hunk structure conversion                                          │
│  • Result parsing and interpretation                                  │
│                                                                         │
└────────────────┬────────────────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                      AIFPL EXECUTION LAYER                              │
│                   (aifpl_patch_library.aifpl)                           │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  PURE FUNCTIONAL PATCH LOGIC                                           │
│                                                                         │
│  ┌─────────────────────────────────────────────────────────────────┐  │
│  │ 1. LINE MATCHING                                                │  │
│  │    • Normalize lines (trim whitespace)                          │  │
│  │    • Compare for exact match                                    │  │
│  │    • Calculate similarity score (0.0 to 1.0)                    │  │
│  │    • Handle empty lines                                         │  │
│  └─────────────────────────────────────────────────────────────────┘  │
│                                                                         │
│  ┌─────────────────────────────────────────────────────────────────┐  │
│  │ 2. CONTEXT EXTRACTION                                           │  │
│  │    • Extract context lines from hunk                            │  │
│  │    • Count leading context                                      │  │
│  │    • Get context texts                                          │  │
│  └─────────────────────────────────────────────────────────────────┘  │
│                                                                         │
│  ┌─────────────────────────────────────────────────────────────────┐  │
│  │ 3. FUZZY MATCHING                                               │  │
│  │    • Define search range (±fuzz_range)                          │  │
│  │    • Score each position:                                       │  │
│  │      ┌──────────────────────────────────────────────┐           │  │
│  │      │ for each candidate position:                 │           │  │
│  │      │   extract file segment                       │           │  │
│  │      │   compare with context lines                 │           │  │
│  │      │   calculate average similarity               │           │  │
│  │      │   store (position, score)                    │           │  │
│  │      └──────────────────────────────────────────────┘           │  │
│  │    • Select position with highest score                         │  │
│  │    • Require score > 0.5 threshold                              │  │
│  └─────────────────────────────────────────────────────────────────┘  │
│                                                                         │
│  ┌─────────────────────────────────────────────────────────────────┐  │
│  │ 4. HUNK APPLICATION                                             │  │
│  │    • For each change:                                           │  │
│  │      ┌──────────────────────────────────────────────┐           │  │
│  │      │ context: verify line matches, advance        │           │  │
│  │      │ delete:  verify and remove line              │           │  │
│  │      │ insert:  add line at position                │           │  │
│  │      └──────────────────────────────────────────────┘           │  │
│  │    • Return modified lines or error                             │  │
│  └─────────────────────────────────────────────────────────────────┘  │
│                                                                         │
│  ┌─────────────────────────────────────────────────────────────────┐  │
│  │ 5. VALIDATION (Dry-Run)                                         │  │
│  │    • Apply patch without modifying                              │  │
│  │    • Report success/failure for each hunk                       │  │
│  │    • Provide detailed error messages                            │  │
│  └─────────────────────────────────────────────────────────────────┘  │
│                                                                         │
│  EXPORTED FUNCTIONS:                                                   │
│  • patch-apply: Apply patch to file lines                             │
│  • patch-validate: Validate patch applicability                       │
│  • find-best-match: Find best location for hunk                       │
│  • score-match-at-position: Score a specific position                 │
│                                                                         │
└────────────────┬────────────────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                       RESULT PROCESSING                                 │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  AIFPL Result → Python Data                                            │
│  ┌────────────────────────────────────────────────┐                   │
│  │ AIFPL:                                         │                   │
│  │   ("success" ("line 1" "line 2" "line 3"))     │                   │
│  │                                                │                   │
│  │ Python:                                        │                   │
│  │   (True, ['line 1', 'line 2', 'line 3'])       │                   │
│  └────────────────────────────────────────────────┘                   │
│                                                                         │
│  ┌────────────────────────────────────────────────┐                   │
│  │ AIFPL:                                         │                   │
│  │   ("error" "Context mismatch" 42)              │                   │
│  │                                                │                   │
│  │ Python:                                        │                   │
│  │   (False, "Context mismatch (at line 42)")     │                   │
│  └────────────────────────────────────────────────┘                   │
│                                                                         │
└────────────────┬────────────────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                         OUTPUT LAYER                                    │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  SUCCESS PATH:                                                         │
│  • Write patched content to file                                      │
│  • Preserve original file attributes                                  │
│  • Display success message                                            │
│  • Show backup location (if created)                                  │
│                                                                         │
│  ERROR PATH:                                                           │
│  • Display detailed error message                                     │
│  • Show which hunk failed and why                                     │
│  • Suggest fixes (increase fuzz, check context)                       │
│  • Leave original file unchanged                                      │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

## Data Flow

### Validation Flow (Dry-Run)

```
User Command
    │
    ├─> Read source file
    ├─> Parse diff file
    ├─> Convert to AIFPL
    │
    └─> AIFPL: patch-validate
            │
            ├─> For each hunk:
            │   ├─> Find best match location
            │   ├─> Score context similarity
            │   └─> Report valid/invalid
            │
            └─> Return validation results
                    │
                    └─> Display to user
```

### Application Flow

```
User Command (--apply)
    │
    ├─> Validate patch first
    │       │
    │       └─> If validation fails: STOP
    │
    ├─> Create backup (if --backup)
    │
    └─> AIFPL: patch-apply
            │
            ├─> For each hunk:
            │   ├─> Find best match location
            │   ├─> Apply changes:
            │   │   ├─> Context: verify
            │   │   ├─> Delete: remove line
            │   │   └─> Insert: add line
            │   └─> Update file lines
            │
            └─> Return patched lines
                    │
                    ├─> Write to file
                    └─> Display success
```

## Component Interaction

```
┌──────────────┐
│     CLI      │  User interface, colored output
└──────┬───────┘
       │
       ├─────> ┌──────────────┐
       │       │  Diff Parser │  Parse unified diff format
       │       └──────┬───────┘
       │              │
       └─────> ┌──────┴───────┐
               │    Bridge    │  Convert Python ↔ AIFPL
               └──────┬───────┘
                      │
                      ├─────> ┌──────────────┐
                      │       │ AIFPL Engine │  Core patch logic
                      │       └──────────────┘
                      │
                      └─────> Parse results back to Python
```

## Key Design Principles

### 1. Separation of Concerns
- **CLI**: User interaction only
- **Parser**: Diff format only
- **Bridge**: Data conversion only
- **AIFPL**: Pure logic only

### 2. Pure Functional Core
- **AIFPL**: No side effects
- **Immutable data**: No state corruption
- **Testable**: Logic independent of I/O

### 3. Safety First
- **Validate before apply**: Dry-run default
- **Backup support**: Optional safety net
- **Atomic writes**: All-or-nothing
- **Error handling**: Comprehensive checks

### 4. User-Friendly
- **Colored output**: Visual feedback
- **Detailed errors**: Actionable messages
- **Verbose mode**: Debugging support
- **Clear documentation**: Multiple guides

## Performance Characteristics

### Time Complexity
```
Parsing:        O(n)       n = diff lines
Fuzzy Search:   O(m×f×c)   m = hunks, f = fuzz, c = context
Application:    O(h×l)     h = hunks, l = file lines
```

### Space Complexity
```
File Storage:   O(l)       l = file lines
Patch Storage:  O(h×c)     h = hunks, c = changes per hunk
AIFPL Stack:    O(d)       d = recursion depth
```

## Extensibility Points

### Adding New Matching Algorithms
→ Modify `aifpl_patch_library.aifpl`
→ Add new scoring functions
→ Export through function list

### Adding New Diff Formats
→ Create new parser in Python
→ Convert to same hunk structure
→ Bridge handles the rest

### Adding New CLI Features
→ Extend `aifpl_patcher.py`
→ Add argument parser options
→ Implement in CLI class

## Security Boundaries

```
┌─────────────────────────────────────────┐
│          User Input (Untrusted)         │
└────────────────┬────────────────────────┘
                 │
                 ├─> Validation
                 ├─> Path sanitization
                 ├─> Bounds checking
                 │
                 ▼
┌─────────────────────────────────────────┐
│        AIFPL Sandbox (Trusted)          │
│  • No I/O operations                    │
│  • No system calls                      │
│  • Bounded recursion                    │
│  • Memory limits                        │
└─────────────────────────────────────────┘
```

---

**Architecture**: Clean, modular, extensible
**Principles**: Pure functional core, separation of concerns, safety first
**Performance**: Efficient for typical patches, scalable for large files
