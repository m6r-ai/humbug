"""Example Python file for testing the patcher."""


def hello_world():
    """Print hello world."""
    print("Hello, World!")


def add_numbers(a, b):
    """Add two numbers."""
    return a + b


def multiply_numbers(a, b):
    """Multiply two numbers."""
    return a * b


def main():
    """Main function."""
    hello_world()
    result = add_numbers(5, 3)
    print(f"5 + 3 = {result}")
    
    result = multiply_numbers(4, 7)
    print(f"4 * 7 = {result}")


if __name__ == "__main__":
    main()
