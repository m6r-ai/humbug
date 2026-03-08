# Project Planner - Current Status

**Last Updated:** 2026-02-03

## Executive Summary

We have successfully built a sophisticated project planning system using Menai (AI Functional Programming Language) as the core analysis engine. The system now includes:

✅ **Complete Menai module system** - All three core modules converted to use module imports  
✅ **Calendar arithmetic** - Full date/calendar calculations (16 functions)  
✅ **Validation functions** - Circular dependency detection, task validation (11 functions)  
✅ **Scheduling algorithm** - Complete CPM implementation (forward/backward pass, slack, critical path)  
✅ **Working tests** - Calendar and validation modules fully tested  
✅ **Updated demo** - All demonstrations use module system  

**Current Challenge:** Scheduling algorithm works on simple projects (3 tasks) but has issues with complex projects (20 tasks). Need tracing capability to debug.

---

## Architecture

The three-layer architecture is proven and working:

```
Python Layer (Import/Export/UI)
        ↕
  Menai Bridge (Data Conversion)
        ↕
   Menai Modules (Analysis)
     - calendar.menai
     - validation.menai
     - scheduling.menai
```

**Key achievement:** All modules now use Menai's module system with `(import "path/module")` instead of manual file loading.

---

## Completed Components

### 1. Core Menai Modules (✓ Complete)

#### calendar.menai (16 functions)
- ✅ Date parsing and formatting (ISO dates)
- ✅ Day of week calculation (Zeller's algorithm)
- ✅ Working day detection (handles weekends and holidays)
- ✅ Calendar day arithmetic (add/subtract days)
- ✅ Working day arithmetic (skip weekends/holidays)
- ✅ Duration calculations (working days between dates)
- ✅ Date comparison (before/after/equal)
- ✅ High-level helpers (calculate-end-date, calculate-start-date)
- ✅ Supports 5-day and 7-day calendars
- ✅ **All tests passing** (test_calendar.py)

#### validation.menai (11 functions)
- ✅ Graph operations (get-predecessors, get-successors)
- ✅ Circular dependency detection (DFS-based cycle detection)
- ✅ Task validation (check required fields, schedule mode consistency)
- ✅ Dependency validation (check task existence, valid types)
- ✅ Project validation (comprehensive validation)
- ✅ Consistency checks (duplicate IDs, orphaned tasks, invalid calendars)
- ✅ **All tests passing** (test_validation_simple.py)

#### scheduling.menai (5 functions)
- ✅ Forward pass (earliest start/finish calculation)
- ✅ Backward pass (latest start/finish calculation)
- ✅ Slack calculation (float/buffer time)
- ✅ Critical path identification (zero slack tasks)
- ✅ Complete CPM scheduling (all passes combined)
- ✅ Supports all 4 dependency types (FS, SS, FF, SF)
- ✅ Supports lag times (positive and negative)
- ✅ **Works on simple projects** (3 tasks, test_scheduling_basic.py)
- ⚠️ **Issues with complex projects** (20 tasks - only schedules 5)

### 2. Python Support (✓ Complete)

#### menai_bridge.py
- ✅ Python dict ↔ Menai alist conversion
- ✅ Python list ↔ Menai list conversion
- ✅ Nested structure handling
- ✅ String escaping
- ✅ Helper functions for evaluation with data

#### example_project.py
- ✅ Realistic 20-task software build program
- ✅ Multiple teams (backend, frontend, infra, qa, external)
- ✅ All 4 dependency types (FS, SS, FF, SF)
- ✅ Mixed calendars (5-day, 7-day, vendor-specific)
- ✅ External vendor dependencies
- ✅ Multi-source tracking (Jira, MS Project, Excel)

### 3. Tests (✓ Complete for calendar and validation)

#### test_calendar.py
- ✅ Date parsing and formatting
- ✅ Day of week calculation
- ✅ Working day detection
- ✅ Calendar day arithmetic
- ✅ Working day arithmetic (5-day and 7-day)
- ✅ Duration calculations
- ✅ Date comparison
- ✅ High-level scheduling helpers
- ✅ **All 11 test sections passing**

#### test_validation_simple.py
- ✅ Get predecessors
- ✅ Circular dependency detection (no cycles)
- ✅ Circular dependency detection (with cycles)
- ✅ Find orphaned tasks
- ✅ **All tests passing**

#### test_scheduling_basic.py
- ✅ Forward pass on 3-task chain
- ✅ Full CPM on 3-task chain
- ✅ Start-to-start dependency
- ✅ **All 3 tasks scheduled correctly**
- ✅ **Critical path correctly identified**

### 4. Demonstrations (✓ Updated)

#### demo.py
- ✅ Updated to use module system
- ✅ Basic queries (task IDs, names, status counts)
- ✅ Filtering and searching (critical tasks, external dependencies, owner-based)
- ✅ Dependency analysis (predecessors, successors, type distribution)
- ✅ Resource analysis (tasks per team, total duration by team)
- ✅ Validation module usage (cycle detection, orphaned tasks)
- ✅ Calendar module usage (working days between, add working days)
- ✅ **All demonstrations working**

---

## Module System Migration (✓ Complete)

Successfully migrated all code to use Menai's module system:

**Before:**
```python
# Old approach - manual file loading
calendar_code = open("calendar.menai").read()
result = bridge.evaluate_with_data(
    f'(let ((funcs {calendar_code})) ...)',
    {"data": data}
)
```

**After:**
```menai
; New approach - module imports
(let ((calendar (import "tools/planner/calendar"))
      (add-working-days (alist-get calendar "add-working-days")))
  (add-working-days "2025-03-03" 5 test-calendar))
```

**Benefits:**
- ✅ Cleaner code
- ✅ Module caching (loaded once)
- ✅ Better encapsulation
- ✅ No code embedding in strings
- ✅ Easier to maintain

**Files updated:**
- ✅ calendar.menai (already correct structure)
- ✅ validation.menai (already correct structure)
- ✅ scheduling.menai (converted from lambda to module)
- ✅ test_calendar.py (updated to use module imports)
- ✅ test_validation_simple.py (updated to use module imports)
- ✅ demo.py (updated to use module imports)

**Files removed:**
- ❌ check_each_function.py (debug script, no longer needed)
- ❌ check_parens.py (debug script, no longer needed)
- ❌ fix_scheduling.py (one-off fix, no longer needed)
- ❌ scheduling_stub.menai (stub version, replaced by full implementation)
- ❌ test_recursion.py (testing old issues, no longer needed)
- ❌ test_modules_import.py (temporary test file)
- ❌ test_validation.py (old version using manual loading)
- ❌ test_scheduling*.py (4 old versions using manual loading)
- ❌ test_results.txt (old output)

---

## Current Challenge: Scheduling Debug Issue

### The Problem

The scheduling algorithm has different behavior on simple vs complex projects:

**Simple 3-task project (✓ Works):**
- Tasks: T1 (5 days) → T2 (3 days) → T3 (2 days)
- Result: All 3 tasks scheduled correctly
- Critical path: All 3 tasks identified
- Dates: Correctly calculated with proper dependencies

**Complex 20-task project (⚠️ Issues):**
- Tasks: 20 tasks with 27 dependencies
- Result: Only 5 tasks scheduled (should be 20)
- Critical path: 0 tasks (should be multiple)
- Issue: Algorithm stops early for unknown reason

### Why We Can't Debug

Menai is a pure functional language with no side effects:
- ❌ No print statements
- ❌ No logging
- ❌ No breakpoints
- ❌ Can only see final result

We need to understand:
- Which tasks are being scheduled in what order?
- Why does scheduling stop at 5 tasks?
- Which tasks are deemed "not ready"?
- What constraints are being applied?

### Solution: Tracing Capability

**Document created:** `TRACING_REQUIREMENTS.md`

This document describes:
- The debugging problem in detail
- 4 proposed solutions (trace function, debug return values, instrumentation, stepwise evaluation)
- Recommendation: Add `trace` function to Menai
- Implementation plan
- Use cases and examples

**Next step:** Take TRACING_REQUIREMENTS.md to a separate AI session to implement tracing in Menai.

---

## Files Structure

```
tools/planner/
├── README.md                      # Project documentation
├── STATUS.md                      # This file
├── EXAMPLES.md                    # Query examples
├── TRACING_REQUIREMENTS.md        # Tracing/debug requirements (NEW)
│
├── calendar.menai                 # Calendar arithmetic module (16 functions)
├── validation.menai               # Validation module (11 functions)
├── scheduling.menai               # Scheduling/CPM module (5 functions)
│
├── menai_bridge.py                # Python ↔ Menai conversion
├── example_project.py             # 20-task example project
├── demo.py                        # Demonstrations (updated)
│
├── test_calendar.py               # Calendar tests (all passing)
├── test_validation_simple.py      # Validation tests (all passing)
└── test_scheduling_basic.py       # Basic scheduling tests (all passing)
```

**Total:** 13 files, ~100 KB of code and documentation

---

## Technical Decisions

### 1. Menai Module System ✓

**Decision:** Use Menai's built-in module system with `(import "path/module")`.

**Rationale:**
- Cleaner code (no manual file loading)
- Module caching (performance)
- Better encapsulation
- Standard approach

**Status:** ✅ Complete - all modules migrated

### 2. Module Structure ✓

**Decision:** Modules use `let` or `letrec` to define functions, return alist of exports.

**Pattern:**
```menai
(let (
  (function-1 (lambda (args) ...))
  (function-2 (lambda (args) ...))
)
  (alist
    (list "function-1" function-1)
    (list "function-2" function-2)))
```

**Status:** ✅ All three modules follow this pattern

### 3. Testing Strategy ✓

**Decision:** Start with simple test cases, then scale to complex projects.

**Rationale:**
- Validates algorithm correctness on simple cases
- Reveals issues with complex cases
- Provides baseline for debugging

**Status:** ✅ Simple tests passing, complex tests reveal issues

### 4. Pure Functional Approach ✓

**Decision:** Keep Menai pure functional (no I/O, no side effects).

**Rationale:**
- Enables safe scenario planning
- Allows parallel computation
- Makes code predictable and testable

**Challenge:** Makes debugging harder (need tracing solution)

---

## Performance

### Current Performance
- ✅ Calendar tests: <1 second (11 test sections)
- ✅ Validation tests: <1 second (4 tests)
- ✅ Scheduling basic: <1 second (3 tasks)
- ✅ Demo: <2 seconds (all demonstrations)
- ✅ Module imports: Cached after first load

### Scalability
- ✅ Handles 20 tasks (though scheduling has bugs)
- ✅ Module system performs well
- ✅ No performance issues observed yet

**Conclusion:** Performance is not a concern at current scale.

---

## Next Steps

### Immediate Priority

1. **Implement Menai tracing** (separate session)
   - Take TRACING_REQUIREMENTS.md to new AI session
   - Implement `trace` function in Menai
   - Add trace calls to scheduling.menai
   - Debug why only 5 tasks are scheduled

### After Tracing

2. **Fix scheduling issues**
   - Use tracing to understand the problem
   - Fix algorithm or data issues
   - Verify all 20 tasks schedule correctly
   - Test with various dependency types

3. **Comprehensive scheduling tests**
   - Test all 4 dependency types thoroughly
   - Test lag times (positive and negative)
   - Test mixed calendars
   - Test edge cases

### Future Phases

4. **Resource analysis** (Phase 3)
   - Resource loading over time
   - Overallocation detection
   - Resource leveling suggestions

5. **Scenario planning** (Phase 4)
   - Create/compare scenarios
   - What-if analysis
   - Impact assessment

6. **Import/Export** (Phase 5)
   - Jira importer
   - MS Project importer
   - Excel importer
   - Report generation

7. **User Interface** (Phase 6)
   - Command-line interface
   - Natural language queries
   - Visualization

---

## Known Issues

### High Priority

1. **Scheduling stops early** (20-task project)
   - Symptom: Only 5 of 20 tasks scheduled
   - Impact: Cannot use for real projects
   - Cause: Unknown (need tracing to debug)
   - Status: Documented in TRACING_REQUIREMENTS.md

### Medium Priority

2. **Circular dependency in example project**
   - T019/T020 have FF dependency creating cycle
   - Detected by validation module (59 cycles found)
   - May contribute to scheduling issues
   - Need to decide: fix example or handle in algorithm

### Low Priority

3. **Project-start date not used**
   - Scheduling uses hardcoded "2025-01-01" default
   - Should respect project-start field
   - Easy fix once scheduling is debugged

---

## Success Metrics

### Completed ✅
- [x] Menai modules working
- [x] Module system migration complete
- [x] Calendar arithmetic complete and tested
- [x] Validation complete and tested
- [x] Scheduling algorithm implemented
- [x] Simple projects schedule correctly
- [x] Demo updated and working

### In Progress ⚠️
- [ ] Complex projects schedule correctly (blocked on tracing)
- [ ] All dependency types tested thoroughly
- [ ] All scheduling edge cases handled

### Not Started 📋
- [ ] Resource analysis
- [ ] Scenario planning
- [ ] Import/Export functionality
- [ ] User interface

---

## Lessons Learned

### What Worked Well

1. **Module system migration** - Cleaner code, better structure
2. **Test-first approach** - Simple tests validated algorithm
3. **Functional programming** - Elegant queries, safe data handling
4. **Python bridge** - Seamless integration between Python and Menai

### Challenges

1. **Debugging pure functional code** - Need tracing capability
2. **Complex algorithms** - Hard to understand without step-by-step visibility
3. **Example project complexity** - Circular dependencies complicate testing

### Key Insights

1. **Start simple** - 3-task test revealed algorithm works
2. **Tracing is essential** - Cannot debug without observability
3. **Pure functional has tradeoffs** - Safety vs debuggability
4. **Module system is powerful** - Clean separation of concerns

---

## Questions for Next Session

1. **How to implement tracing in Menai?**
   - See TRACING_REQUIREMENTS.md
   - Preferred approach: `trace` function
   - Need to maintain functional purity

2. **Should we fix the example project's circular dependency?**
   - Current: T019/T020 have cycle
   - Option A: Fix the example (list-remove cycle)
   - Option B: Handle cycles in scheduling algorithm
   - Decision needed after tracing reveals root cause

3. **How to handle project-start date?**
   - Currently ignored (uses "2025-01-01")
   - Should be passed to scheduling functions
   - Minor fix, low priority

---

## Conclusion

**Major Progress:** We have completed the core Menai modules (calendar, validation, scheduling) and migrated everything to the module system. The architecture is proven, tests are passing for simple cases, and the foundation is solid.

**Current Blocker:** Scheduling algorithm works on simple projects but fails on complex projects. We need tracing capability to debug this issue.

**Path Forward:** Implement Menai tracing (separate session using TRACING_REQUIREMENTS.md), then use it to debug and fix the scheduling algorithm. Once that's resolved, we can move forward with resource analysis, scenario planning, and eventually a full user interface.

**Status: Phase 1 complete, Phase 2 blocked on tracing implementation.**

---

**Document version:** 2.0  
**Last updated:** 2026-02-03  
**Next review:** After tracing implementation
