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

Expression syntax is similar to Ruby, but there are no infix operators except `=` (assignment). Statements and expressions are the same syntactic category.

## What is implemented

- Lexer
- Most of the parser

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
     | "if", expr, "then", expr,
          [ "else", expr ], "end"        (* conditional *)
     | "(", expr, ")"
     | expr, { newline, expr }           (* sequence *)
     | "while", expr, "do", expr, "end"  (* while loop *)
     | expr, "=", expr                   (* assignment *)
     ;

method_call = identifier, expr_list, [ block ];

expr_list = [ expr, { "," expr } ];

identifier_list = [ identifier, { "," identifier } ];

block = "do", [ "|", identifier_list, "|" ], expr, "end"
```
