# Code Block Alignment Test

This document tests code blocks mixed with paragraphs at various nesting levels.

## Top Level

A paragraph before a code block.

```python
def greet(name: str) -> str:
    return f"Hello, {name}!"
```

A paragraph between two code blocks.

```javascript
function greet(name) {
    return `Hello, ${name}!`;
}
```

A paragraph after a code block.

## Inside a Blockquote

> A paragraph inside a blockquote before a code block.
>
> ```python
> def blockquote_example():
>     return 42
> ```
>
> A paragraph between two code blocks inside a blockquote.
>
> ```javascript
> function blockquoteExample() {
>     return 42;
> }
> ```
>
> A paragraph after a code block inside a blockquote.

## Inside a Single-Level Unordered List

- A list item before any code.

- A list item with a code block:

  ```python
  def list_item_example():
      return "unordered"
  ```

- A list item between two code block items.

- Another list item with a code block:

  ```javascript
  function listItemExample() {
      return "unordered";
  }
  ```

- A list item after the code block items.

## Inside a Single-Level Ordered List

1. A list item before any code.

2. A list item with a code block:

   ```python
   def ordered_example():
       return "ordered"
   ```

3. A list item between two code block items.

4. Another list item with a code block:

   ```javascript
   function orderedExample() {
       return "ordered";
   }
   ```

5. A list item after the code block items.

## Inside a Two-Level Unordered List

- Outer item one.

  - Inner item before any code.

  - Inner item with a code block:

    ```python
    def nested_unordered():
        return "level 2"
    ```

  - Inner item between two code block items.

  - Another inner item with a code block:

    ```javascript
    function nestedUnordered() {
        return "level 2";
    }
    ```

  - Inner item after the code block items.

- Outer item two (no code).

## Inside a Two-Level Ordered List

1. Outer item one.

   1. Inner item before any code.

   2. Inner item with a code block:

      ```python
      def nested_ordered():
          return "level 2"
      ```

   3. Inner item between two code block items.

   4. Another inner item with a code block:

      ```javascript
      function nestedOrdered() {
          return "level 2";
      }
      ```

   5. Inner item after the code block items.

2. Outer item two (no code).
