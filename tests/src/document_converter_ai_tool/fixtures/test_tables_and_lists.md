# Table and Nested List Test Document

This document is designed to test DOCX generation of tables and nested bullet lists.

---

## 1. Simple Table

A basic table with a header row and several data rows.

| Name       | Role         | Status   |
|------------|--------------|----------|
| Alice      | Developer    | Active   |
| Bob        | Designer     | Active   |
| Carol      | Manager      | On Leave |
| Dave       | QA Engineer  | Active   |

---

## 2. Table with Alignment

A table using left, centre, and right column alignment.

| Item          | Quantity | Unit Price |
|:--------------|:--------:|-----------:|
| Widget A      |    10    |      £1.99 |
| Widget B      |     3    |     £15.50 |
| Gadget Pro    |     1    |    £249.00 |
| Spare Part    |    50    |      £0.75 |

---

## 3. Table with Inline Formatting

A table containing bold, italic, and inline code in cells.

| Feature          | Supported | Notes                          |
|------------------|-----------|--------------------------------|
| **Bold text**    | Yes       | Uses `**bold**` syntax         |
| *Italic text*    | Yes       | Uses `*italic*` syntax         |
| `Inline code`    | Yes       | Uses backtick syntax           |
| ~~Strikethrough~~| Partial   | Renderer-dependent             |

---

## 4. Simple Nested Bullet List

A flat list followed by a two-level nested list.

- Fruit
- Vegetables
- Dairy

Nested:

- Fruit
    - Apple
    - Banana
    - Cherry
- Vegetables
    - Carrot
    - Broccoli
- Dairy
    - Milk
    - Cheese

---

## 5. Deeply Nested Bullet List

A three-level nested bullet list.

- Level 1 Item A
    - Level 2 Item A1
        - Level 3 Item A1a
        - Level 3 Item A1b
    - Level 2 Item A2
        - Level 3 Item A2a
- Level 1 Item B
    - Level 2 Item B1
        - Level 3 Item B1a
    - Level 2 Item B2
- Level 1 Item C

---

## 6. Nested List with Inline Formatting

- **Bold top-level item**
    - *Italic sub-item*
    - Sub-item with `inline code`
        - Deep item with **bold** and *italic* combined
- Regular top-level item
    - Sub-item referencing a term: `config.yaml`
    - Another sub-item

---

## 7. Mixed Content: Table then List

A table followed immediately by a nested list to test transitions between block types.

| Step | Action          | Result        |
|------|-----------------|---------------|
| 1    | Parse input     | AST produced  |
| 2    | Transform AST   | IR produced   |
| 3    | Serialise IR    | DOCX produced |

Steps broken down:

- Parse input
    - Read the file from disk
    - Run the markdown lexer
    - Build the AST
- Transform AST
    - Walk each node
    - Emit doc IR elements
- Serialise IR
    - Convert to DOCX AST
    - Write binary output

---

## 8. Numbered List with Nested Bullets

1. First ordered item
    - Bullet under first
    - Another bullet under first
2. Second ordered item
    - Bullet under second
        - Deep bullet under second
3. Third ordered item

---

## 9. Wide Table

A table with many columns to test width handling.

| ID  | First Name | Last Name | Department   | Location  | Start Date | Salary   | Active |
|-----|------------|-----------|--------------|-----------|------------|----------|--------|
| 001 | Alice      | Smith     | Engineering  | London    | 2019-03-01 | £65,000  | Yes    |
| 002 | Bob        | Jones     | Design       | Manchester| 2020-07-15 | £52,000  | Yes    |
| 003 | Carol      | Williams  | Management   | London    | 2018-01-10 | £80,000  | Yes    |
| 004 | Dave       | Brown     | QA           | Bristol   | 2021-11-22 | £48,000  | No     |

---

## 10. Summary

This document covered:

- **Tables**
    - Simple tables
    - Tables with column alignment
    - Tables with inline formatting
    - Wide tables with many columns
- **Nested lists**
    - Two-level nesting
    - Three-level nesting
    - Lists with inline formatting
    - Mixed ordered and unordered lists
- **Transitions**
    - Table followed by a list
    - List followed by a table
