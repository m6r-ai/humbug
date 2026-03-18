#!/usr/bin/env python3
"""
Test calendar arithmetic functions using module system.

Tests date parsing, working day calculations, and calendar-aware scheduling.
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from menai import Menai


def print_test(name: str):
    """Print test section header."""
    print(f"\n{'='*80}")
    print(f" {name}")
    print('='*80)


def test_with_calendar(menai, expr, calendar=None):
    """Helper to evaluate expression with calendar module and optional calendar data."""
    if calendar:
        # Build a calendar struct using the constructor exported from the module
        working_days = " ".join(f'"{d}"' for d in sorted(calendar["working-days"]))
        holidays = " ".join(f'"{h}"' for h in sorted(calendar.get("holidays", set())))
        cal_id = calendar["id"]
        cal_name = calendar.get("name", cal_id)
        cal_type = calendar["type"]
        full_expr = f'''(let* ((calendar-mod (import "tools/planner/calendar"))
                              (make-calendar (dict-get calendar-mod "calendar"))
                              (calendar (make-calendar "{cal_id}" "{cal_name}" "{cal_type}"
                                                       (set {working_days}) (set {holidays}))))
                          {expr})'''
    else:
        full_expr = f'''(let ((calendar-mod (import "tools/planner/calendar")))
                          {expr})'''
    return menai.evaluate(full_expr)


def main():
    """Run calendar function tests."""
    
    print_test("CALENDAR ARITHMETIC TESTS")
    
    menai = Menai()
    
    # Define test calendar (5-day week)
    test_calendar = {
        "id": "test-5day",
        "type": "5-day",
        "working-days": {"mon", "tue", "wed", "thu", "fri"},
        "holidays": {"2025-12-25", "2025-12-26", "2026-01-01"}
    }
    
    # Define 7-day calendar
    calendar_7day = {
        "id": "test-7day",
        "type": "7-day",
        "working-days": {"mon", "tue", "wed", "thu", "fri", "sat", "sun"},
        "holidays": set()
    }
    
    # === TEST 1: Date parsing and formatting ===
    print_test("TEST 1: Date Parsing and Formatting")
    
    test_date = "2025-03-15"
    result = test_with_calendar(menai, f'''
        (let ((parse (dict-get calendar-mod "parse-date"))
              (format (dict-get calendar-mod "format-date")))
          (let ((parsed (parse "{test_date}")))
            (list parsed (format parsed))))
    ''')
    print(f"  Parse '{test_date}': {result[0]}")
    print(f"  Format back: {result[1]}")
    assert result[1] == test_date, "Round-trip failed!"
    print("  ✓ Round-trip successful")
    
    # === TEST 2: Day of week ===
    print_test("TEST 2: Day of Week Calculation")
    
    test_dates = [
        ("2025-01-01", "wed"),
        ("2025-12-25", "thu"),
        ("2025-03-17", "mon"),
        ("2025-06-15", "sun"),
    ]
    
    for date, expected_day in test_dates:
        result = test_with_calendar(menai, f'''
            (let ((dow-func (dict-get calendar-mod "day-of-week"))
                  (name-func (dict-get calendar-mod "day-name")))
              (name-func (dow-func "{date}")))
        ''')
        status = "✓" if result == expected_day else "✗"
        print(f"  {status} {date} = {result} (expected {expected_day})")
        assert result == expected_day, f"Day of week mismatch for {date}"
    
    # === TEST 3: Working day check ===
    print_test("TEST 3: Working Day Detection")
    
    test_cases = [
        ("2025-03-17", True, "Monday"),
        ("2025-03-22", False, "Saturday"),
        ("2025-03-23", False, "Sunday"),
        ("2025-12-25", False, "Christmas (holiday)"),
        ("2025-03-18", True, "Tuesday"),
    ]
    
    for date, expected, description in test_cases:
        result = test_with_calendar(menai, f'''
            (let ((is-working (dict-get calendar-mod "is-working-day?")))
              (is-working "{date}" calendar))
        ''', test_calendar)
        status = "✓" if result == expected else "✗"
        print(f"  {status} {date} ({description}): {result}")
        assert result == expected, f"Working day check failed for {date}"
    
    # === TEST 4: Add calendar days ===
    print_test("TEST 4: Add Calendar Days")
    
    start = "2025-03-15"
    test_cases = [
        (1, "2025-03-16"),
        (10, "2025-03-25"),
        (20, "2025-04-04"),
        (-5, "2025-03-10"),
    ]
    
    for days, expected in test_cases:
        result = test_with_calendar(menai, f'''
            (let ((add-days (dict-get calendar-mod "add-calendar-days")))
              (add-days "{start}" {days}))
        ''')
        status = "✓" if result == expected else "✗"
        print(f"  {status} {start} + {days} days = {result} (expected {expected})")
        assert result == expected, f"Calendar day addition failed"
    
    # === TEST 5: Add working days (5-day calendar) ===
    print_test("TEST 5: Add Working Days (5-day week)")
    
    start = "2025-03-17"
    test_cases = [
        (1, "2025-03-18", "Mon + 1 = Tue"),
        (5, "2025-03-24", "Mon + 5 = Mon (skip weekend)"),
        (10, "2025-03-31", "Mon + 10 = Mon (skip 2 weekends)"),
    ]
    
    for days, expected, description in test_cases:
        result = test_with_calendar(menai, f'''
            (let ((add-working (dict-get calendar-mod "add-working-days")))
              (add-working "{start}" {days} calendar))
        ''', test_calendar)
        status = "✓" if result == expected else "✗"
        print(f"  {status} {description}: {result} (expected {expected})")
        assert result == expected, f"Working day addition failed: {description}"
    
    # === TEST 6: Add working days (7-day calendar) ===
    print_test("TEST 6: Add Working Days (7-day week)")
    
    start = "2025-03-17"
    test_cases = [
        (1, "2025-03-18", "Every day is working"),
        (7, "2025-03-24", "7 days = 1 week"),
        (10, "2025-03-27", "10 days"),
    ]
    
    for days, expected, description in test_cases:
        result = test_with_calendar(menai, f'''
            (let ((add-working (dict-get calendar-mod "add-working-days")))
              (add-working "{start}" {days} calendar))
        ''', calendar_7day)
        status = "✓" if result == expected else "✗"
        print(f"  {status} {description}: {result} (expected {expected})")
        assert result == expected, f"Working day addition failed: {description}"
    
    # === TEST 7: Working days between dates ===
    print_test("TEST 7: Count Working Days Between Dates")
    
    test_cases = [
        ("2025-03-17", "2025-03-18", 1, "Mon to Tue = 1 day"),
        ("2025-03-17", "2025-03-24", 5, "Mon to Mon = 5 days (skip weekend)"),
        ("2025-03-17", "2025-03-31", 10, "Mon to Mon = 10 days (skip 2 weekends)"),
        ("2025-03-21", "2025-03-24", 1, "Fri to Mon = 1 day (skip weekend)"),
    ]
    
    for start, end, expected, description in test_cases:
        result = test_with_calendar(menai, f'''
            (let ((count-working (dict-get calendar-mod "working-days-between")))
              (count-working "{start}" "{end}" calendar))
        ''', test_calendar)
        status = "✓" if result == expected else "✗"
        print(f"  {status} {description}: {result} (expected {expected})")
        assert result == expected, f"Working days count failed: {description}"
    
    # === TEST 8: Date comparison ===
    print_test("TEST 8: Date Comparison")
    
    test_cases = [
        ("2025-03-15", "2025-03-20", -1, "before"),
        ("2025-03-20", "2025-03-15", 1, "after"),
        ("2025-03-15", "2025-03-15", 0, "equal"),
    ]
    
    for date1, date2, expected, description in test_cases:
        result = test_with_calendar(menai, f'''
            (let ((compare (dict-get calendar-mod "compare-dates")))
              (compare "{date1}" "{date2}"))
        ''')
        status = "✓" if result == expected else "✗"
        print(f"  {status} {date1} {description} {date2}: {result}")
        assert result == expected, f"Date comparison failed"
    
    # === TEST 9: Calculate end date from start + duration ===
    print_test("TEST 9: Calculate End Date (start + duration)")
    
    test_cases = [
        ("2025-03-17", 5, "2025-03-24", "5 working days from Monday"),
        ("2025-03-17", 10, "2025-03-31", "10 working days from Monday"),
        ("2025-03-20", 3, "2025-03-25", "3 working days from Thursday"),
    ]
    
    for start, duration, expected, description in test_cases:
        result = test_with_calendar(menai, f'''
            (let ((calc-end (dict-get calendar-mod "calculate-end-date")))
              (calc-end "{start}" {duration} calendar))
        ''', test_calendar)
        status = "✓" if result == expected else "✗"
        print(f"  {status} {description}: {result} (expected {expected})")
        assert result == expected, f"End date calculation failed: {description}"
    
    # === TEST 10: Calculate start date from end - duration ===
    print_test("TEST 10: Calculate Start Date (end - duration)")
    
    test_cases = [
        ("2025-03-24", 5, "2025-03-17", "5 working days before Monday"),
        ("2025-03-31", 10, "2025-03-17", "10 working days before Monday"),
    ]
    
    for end, duration, expected, description in test_cases:
        result = test_with_calendar(menai, f'''
            (let ((calc-start (dict-get calendar-mod "calculate-start-date")))
              (calc-start "{end}" {duration} calendar))
        ''', test_calendar)
        status = "✓" if result == expected else "✗"
        print(f"  {status} {description}: {result} (expected {expected})")
        assert result == expected, f"Start date calculation failed: {description}"
    
    # === TEST 11: Calculate duration from start to end ===
    print_test("TEST 11: Calculate Duration (start to end)")
    
    test_cases = [
        ("2025-03-17", "2025-03-24", 5, "Monday to Monday"),
        ("2025-03-17", "2025-03-31", 10, "Monday to Monday (2 weeks)"),
        ("2025-03-21", "2025-03-24", 1, "Friday to Monday"),
    ]
    
    for start, end, expected, description in test_cases:
        result = test_with_calendar(menai, f'''
            (let ((calc-duration (dict-get calendar-mod "calculate-duration")))
              (calc-duration "{start}" "{end}" calendar))
        ''', test_calendar)
        status = "✓" if result == expected else "✗"
        print(f"  {status} {description}: {result} (expected {expected})")
        assert result == expected, f"Duration calculation failed: {description}"
    
    # === SUMMARY ===
    print_test("SUMMARY")
    print("\n  ✓ All calendar arithmetic tests passed!")
    print("  ✓ Date parsing and formatting work correctly")
    print("  ✓ Day of week calculation is accurate")
    print("  ✓ Working day detection handles weekends and holidays")
    print("  ✓ Calendar day arithmetic works")
    print("  ✓ Working day arithmetic works for 5-day and 7-day calendars")
    print("  ✓ Duration calculations are accurate")
    print("  ✓ Date comparisons work correctly")
    print("  ✓ High-level scheduling helpers work")
    
    print("\n" + "="*80)
    print(" Calendar functions are ready for use!")
    print("="*80 + "\n")


if __name__ == "__main__":
    main()
