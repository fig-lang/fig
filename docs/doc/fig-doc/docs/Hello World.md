---
sidebar_position: 3
---

To create a simple "Hello World" program in Fig, follow these steps:

* Create a new Fig file with the following content:

```rust
import std;

export fn main() {
    print_str("Hello World");
}
```

* Save the file with a .fig extension (e.g., hello.fig).

* To run the Fig program, execute the following command in your terminal:

```bash
fig run ./hello.fig
```
You should see the output:

```
Hello World
```
