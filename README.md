
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
```fig
import std;

export fn main() {
    print_str("Hello World");
}
```

To run this simple fig program run `fig run ./{file name}`

*The output should be `Hello World`.*

Also, you can test out fig files in `examples` folder.

## Learn Fig
Fig has a [book](https://fig-lang.github.io/book/docs/Fig%20introduction) for learning the syntax and basic principles of fig.
