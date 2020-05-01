# MiniRuby

[![build](https://github.com/zyla/miniruby/workflows/build/badge.svg)](https://github.com/zyla/miniruby/actions?query=workflow:build)

A minimal Ruby-like programming language.

It has less syntax, but still tries to retain the core Ruby features (namely: classes and methods). Most notably, it has no special syntax for defining classes and methods - everything is done via method calls.

For example, the following Ruby code:

```ruby
class Num
  def plus(n)
    # ...
  end
end
```

would be written in MiniRuby as:

```ruby
class :Num do
  def :plus do |n|
    # ...
  end
end
```

Statement and expression syntax is similar to Ruby, but there are no infix operators except `=` (assignment).

## What is implemented

- Lexer
- A tiny bit of parser

## Syntax

```ebnf
expr = "nil"
     | "self"
     | integer_literal
     | string_literal
     | identifier                        (* variable reference *)
     | expr, ".", method_call            (* method call on a given receiver *)
     | method_call                       (* method call on self *)
     | ":" identifier                    (* symbol *)
     | "@" identifier                    (* instance variable *)
     | "if", expr, "then", stmt,
          [ "else", stmt ], "end"        (* conditional *)
     | block
     | "(", expr, ")"
     ;

method_call = identifier, expr_list, [ block ];

expr_list = [ expr, { "," expr } ];

identifier_list = [ identifier, { "," identifier } ];

block = "do", [ "|", identifier_list, "|" ], stmt, "end"

stmt = expr
     | expr, "=", expr                   (* assignment *)
     | "while", expr, "do", stmt, "end"  (* while loop *)
     | stmt, stmt                        (* sequence *)
```
