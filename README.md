
<p align="center" width="100%">
  <img width="25%" src="https://github.com/fig-lang/fig/blob/main/logo/transparent-fig-white.png?raw=true"/>
</p>


# The Fig Programming Language
Fig-lang source code, containing the compiler, CLI, web, and examples

## Stage
Fig-lang is currently in the early stages of development. You can test the Early development version on [Playground](https://fig-lang.github.io/fig/).

## Installation
Here's a bash command to download the latest FigLang release and install it on your Linux or MacOS machine.

```bash
curl https://raw.githubusercontent.com/fig-lang/fig/main/install.sh | bash
```
**This installation script currently doesn't support `zsh`**

## Hello World in Fig
```
import std;

export fn main() {
    print_str("Hello World");
}
```

To run this simple fig program run `fig run ./{file name}`

*The output should be `Hello World`.*

Also, you can test out fig files in `examples` folder.

## Server-side
You can write server-side applications with FigLang, FigCli has a command called `server` That will listen to the address and port you specified,
each time a request arrives the CLI will run the script and pass the HTTP request content to the main function.

### Server command
Here is an example usage of the `server` command:

`fig server ./test.fig --addr localhost:8080`

### Example server-side code
Here is an example source code of using the server feature in FigCli, It's just like an ordinary Fig program but the main function will get one parameter, Which will contain the content of the HTTP request as `char[]` or a string.

`test.fig`:

```
import server;

export fn main (req: char[]): char[] {
    let headers = "Content-Type: text/html
Connection: Closed";

    let res = new_response("200", "OK", headers, "<h1>Hello World</h1>");

    return res;
}
```

The `main` function must return the HTTP response as a `char[]` or string. The return will be your app's response to the request.

Also checkout the [server.fig](https://github.com/fig-lang/fig/blob/main/examples/server.fig) example file there is a lot of helper functions that will make the generation of response, Or reading the request easier.
