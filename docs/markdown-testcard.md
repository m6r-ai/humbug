# Markdown Renderer Testcard

This file exercises every construct the markdown renderer supports. Open it in a
preview tab to verify correct rendering after any changes to the renderer.

---

## 1. Headings

# Heading Level 1
## Heading Level 2
### Heading Level 3
#### Heading Level 4
##### Heading Level 5
###### Heading Level 6

---

## 2. Paragraphs and inline formatting

This is a normal paragraph. It should have a bottom margin separating it from the
next paragraph.

This second paragraph follows the first. The spacing between them should be consistent
with the spacing between all other block-level elements.

This paragraph contains **bold text**, *italic text*, ~~strikethrough text~~, and
`inline code`. It also contains **bold with *nested italic* inside it**.

This paragraph contains a [link to example.com](https://example.com) and a
[link with a title](https://example.com "Example Domain").

This paragraph ends with a hard line break.  
This line follows the hard line break and should appear on the next line within the
same paragraph block.

---

## 3. Code blocks

A Python code block with syntax highlighting:

```python
def fibonacci(n: int) -> int:
    """Return the nth Fibonacci number."""
    if n <= 1:
        return n

    a, b = 0, 1
    for _ in range(2, n + 1):
        a, b = b, a + b

    return b


print([fibonacci(i) for i in range(10)])
```

A Rust code block:

```rust
fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    let doubled: Vec<i32> = numbers.iter().map(|x| x * 2).collect();
    println!("{:?}", doubled);
}
```

A plain text code block (no language):

```text
This is plain preformatted text.
No syntax highlighting is applied.
    Indentation is preserved.
```

---

## 4. Unordered lists

A tight unordered list:

- First item
- Second item
- Third item

A loose unordered list:

- First item with some longer text to make it wrap if the window is narrow enough
  to demonstrate that continuation lines are handled correctly.

- Second item with **bold**, *italic*, and `inline code` inline formatting.

- Third item with a [link](https://example.com) inside it.

A nested unordered list (tight inside tight):

- Top level item one
  - Nested item one
  - Nested item two
    - Doubly nested item
- Top level item two
- Top level item three

A nested unordered list (loose inside tight):

- Top level item one
  - Nested item one

  - Nested item two (loose — has blank line above)
- Top level item two

A nested unordered list (tight inside loose):

- Top level item one

  - Nested item one
  - Nested item two (tight inside loose outer)

- Top level item two

---

## 5. Ordered lists

A tight ordered list:

1. First item
2. Second item
3. Third item

A loose ordered list:

1. First item with some longer text to make it wrap if the window is narrow.

2. Second item with **bold** and `inline code`.

3. Third item.

A nested ordered list (tight inside tight):

1. Top level item one
   1. Nested item one
   2. Nested item two
2. Top level item two
3. Top level item three

A nested ordered list (loose inside tight):

1. Top level item one
   1. Nested item one

   2. Nested item two (loose — has blank line above)
2. Top level item two

An ordered list starting from a number other than 1:

42. Forty-two
43. Forty-three
44. Forty-four

---

## 6. Mixed nesting: ordered inside unordered

Tight ordered list nested inside a tight unordered list:

- Unordered item one
  1. Ordered nested one
  2. Ordered nested two
- Unordered item two
  1. Ordered nested one
  2. Ordered nested two
- Unordered item three

Loose ordered list nested inside a loose unordered list:

- Unordered item one

  1. Ordered nested one

  2. Ordered nested two

- Unordered item two

---

## 7. Mixed nesting: unordered inside ordered

Tight unordered list nested inside a tight ordered list:

1. Ordered item one
   - Unordered nested one
   - Unordered nested two
2. Ordered item two
   - Unordered nested one
   - Unordered nested two
3. Ordered item three

Loose unordered list nested inside a loose ordered list:

1. Ordered item one

   - Unordered nested one

   - Unordered nested two

2. Ordered item two

---

## 8. Consecutive lists

An unordered list immediately followed by an ordered list — there should be a gap
between them:

- Unordered item one
- Unordered item two
- Unordered item three

1. Ordered item one
2. Ordered item two
3. Ordered item three

An ordered list immediately followed by an unordered list:

1. Ordered item one
2. Ordered item two

- Unordered item one
- Unordered item two

---

## 9. Code blocks inside lists

A tight list with a code block in one item (makes the list loose):

- Item one

- Item two with a code block:

  ```python
  print("Hello from inside a list item")
  ```

- Item three

A tight list where every item has a code block:

- Item one:

  ```python
  x = 1
  ```

- Item two:

  ```python
  x = 2
  ```

An ordered list with a code block:

1. Step one: write the function

   ```python
   def greet(name: str) -> str:
       return f"Hello, {name}!"
   ```

2. Step two: call it

   ```python
   print(greet("World"))
   ```

---

## 10. Blockquotes

A simple blockquote:

> This is a blockquote. It should have a coloured vertical bar on the left margin.

A blockquote containing multiple paragraphs:

> First paragraph inside the blockquote.
>
> Second paragraph inside the blockquote. The bar should run continuously down
> the left side of both paragraphs.

A nested blockquote:

> Outer blockquote text.
>
> > Inner nested blockquote. The inner bar should be inset from the outer one.
>
> Back to the outer blockquote.

A blockquote containing an unordered list:

> - Item one inside a blockquote
> - Item two inside a blockquote
> - Item three inside a blockquote

A blockquote containing an ordered list:

> 1. First ordered item inside a blockquote
> 2. Second ordered item inside a blockquote
> 3. Third ordered item inside a blockquote

A blockquote containing a loose unordered list:

> - Item one inside a blockquote
>
> - Item two inside a blockquote
>
> - Item three inside a blockquote

A blockquote containing a code block:

> ```python
> print("Hello from inside a blockquote")
> ```

A blockquote containing a heading, a paragraph, a list, and a code block:

> ### Heading inside a blockquote
>
> Paragraph inside the blockquote.
>
> - List item one
> - List item two
>
> ```python
> x = 42
> ```

---

## 11. Blockquotes inside lists

A list where one item contains a blockquote (tight list):

- Item one
- Item two with a blockquote:
  > This blockquote is nested inside a list item. It should have its bar
  > indented to match the list content.
- Item three

An ordered list (tight) where every item contains a blockquote:

1. First item
   > Blockquote inside first ordered item.
2. Second item
   > Blockquote inside second ordered item.

A list where one item contains a blockquote (loose list):

- Item one

- Item two with a blockquote:

  > This blockquote is nested inside a list item. It should have its bar
  > indented to match the list content.

- Item three

An ordered list (loose) where every item contains a blockquote:

1. First item

   > Blockquote inside first ordered item.

2. Second item

   > Blockquote inside second ordered item.

A list with a blockquote containing a list:

- Item one

- Item two with a blockquote:

  > This blockquote is nested inside a list item. It should have its bar
  > indented to match the list content.
  >
  > - Then we have a list
  > - inside
  > - and a third item
  > - > the fourth is a blockquote

- Item three

A nested list with a blockquote inside

- Item one

- Item two with nested list

  1. Item #1

  2. Item #2

     > blockquote
     >
     > with some more test

- item three

---

## 12. Tables

A basic table:

| Name | Type | Description |
|------|------|-------------|
| `char_start` | `int` | Start position in the document |
| `char_end` | `int` | End position in the document |
| `fingerprint` | `int` | Hash of the node's content |

A table with column alignment:

| Left aligned | Centre aligned | Right aligned |
|:-------------|:--------------:|--------------:|
| Apple | Banana | Cherry |
| One | Two | Three |
| 100 | 200 | 300 |

---

## 13. List followed by table

There should be a gap between the list and the table below it:

- Item one
- Item two
- Item three

| Column A | Column B |
|----------|----------|
| Value 1  | Value 2  |
| Value 3  | Value 4  |

---

## 14. Horizontal rules

A paragraph before a horizontal rule.

---

A paragraph after a horizontal rule. The rule above and the heading below should
both have correct spacing.

---

## 15. Images

A local image that does not exist (should show a placeholder):

![Alt text for a missing image](missing-image.png "Optional title")

---

## 16. Mixed content

A paragraph, then a list, then a code block, then a table, then a blockquote — all
in sequence to verify spacing between different block types:

This is the opening paragraph.

- List item one
- List item two

```python
x = 42
```

| Col 1 | Col 2 |
|-------|-------|
| A | B |

> Closing blockquote.

Final paragraph after all the mixed content.
