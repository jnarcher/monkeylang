# Monkeylang

An interpreter written in Zig for a made up C-like language following the book "Writing an Interpreter In Go" by Thorsten Ball.

## As of now...

# NOTE

This project only works with version <0.12.1 of the Zig compiler. As this is a constantly changing language still under development, some things may be broken on newer versions.

Running the command

```bash
zig build run
```

will start an interactive REPL (minus the evaluate part) where for every input, a lexer will tokenize the
input, then be passed into a parser which reads the tokens and creates an
Abstract Syntax Tree representing the entered "program". The REPL will then print
out the AST converted back into string form. You will be able to see it grouping
expressions based on precedence as well as format your statements. If a syntax error
is found it will tell you the expected token and the one it found. A possible REPL
session looks like so

```bash
$ zig build run
Monkeylang REPL ('CTRL-C' to quit)
>> let x = 10;
let x = 10;
>> x > 10 == 39 - a
((x > 10) == (39 - a))
>> let x 20 *;
parser error:   expected Token(=), got Token(INT): '20'
```
