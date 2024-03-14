Functions in Fig are blocks of code that perform a specific task. They are declared using the `fn` keyword followed by the function name, parameters (if any), and return type (if any). Here's the general syntax for defining a function:

```rust
fn function_name(parameter1: type1, parameter2: type2, ...): return_type {
    // Function body
    // Code to perform the task
    return result; // (if return_type is specified)
}
```

### Function Parameters

Functions can have zero or more parameters, which are variables used to pass data into the function. Parameters are specified within the parentheses after the function name.

```rust
fn greet(name: char[]) {
    print_str("Hello, ");
    print_str(name);
}
```

### Return Type

Functions can specify a return type to indicate the type of value they return. If a function doesn't return any value, it doesn't specify a return type.

```rust
fn add(x: i32, y: i32): i32 {
    return x + y;
}
```

### Calling Functions

To call a function, you simply write the function name followed by parentheses containing the arguments (if any) passed to the function.

```rust
let result = add(3, 5);
```

### Examples

Here are some more examples of functions in Fig:
Calculate Factorial

```rust
fn factorial(n: i32): i32 {
    if (n <= 1) {
        return 1;
    } else {
        return n * factorial(n - 1);
    }
}
```

