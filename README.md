# Fig
Fig-lang source code, containing the compiler, CLI, web, and examples

# Stage
Fig-lang is currently in the early stages of development. You can test the Early development version on [Playground](https://fig-lang.github.io/fig/).

# Try Fig-lang local
First compile the FigCli with `cargo build --release`

then create a file `example.fig`:

```
external fig {
    fn print_str(str: char[]);
}

export fn main() {
    print_str("Hello World");
}
```

finally run `./target/release/fig run ./example.fig`

*The output should be `Hello World`.*

Also, you can test out fig files in `examples` folder.
