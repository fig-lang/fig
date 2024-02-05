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

# Server-side
You can write a server side applications with FigLang, FigCli has an command called `server` That will listen on address and port you specified,
each time an request arives the cli will run the script and pass the HTTP request content to the main function.

### Server command
Here is an example usage of the `server` command:

`./target/release/fig server ./test.fig --addr localhost:8080`

### Example server side code
Here is an example source code of using server feature in FigCli, It's just like an ordinary Fig program but the main function will get one parameter, Which will contain the content of HTTP request as `char[]` or a string.

`test.fig`:

```
export fn main (request: char[]): char[] {
    // Your code
}
```

The `main` function must return the HTTP response as an `char[]` or string. The return will be your app's response to the request.
