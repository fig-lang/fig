---
sidebar_position: 1
---

Variables in Fig are used to store and manipulate data. There are two types of variables:

### Mutable Variables

Mutable variables allow their values to be changed after they have been declared using the `let` keyword.

```rust
let x = 10;
x = 20; // Valid - x can be changed
```

### Immutable Variables (const)

Immutable variables have constant values that cannot be changed once assigned. They are declared using the const keyword and only support static-sized values and types.

```rust
const PI: f64 = 3.14159;
```

You can also use const with array and string types:

```rust
const ARRAY: i32[] = [1, 2, 3, 4, 5];
const STRING: char[] = "Hello, World!";
```

