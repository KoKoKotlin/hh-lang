# hh-lang

Programming language that I make for solving AOC 2023.
It uses duck typing like python and has support only for the bare minimum that is needed for solving AOC.
For usage see the example folder.

## Grammar:
```
Program     => [ Block ]
Block       => [ "let" IDENT "=" Primary {, IDENT "=" Primary } ";" ] |
            [ "var" IDENT { "=" Primary } {, IDENT {"=" Primary}} ";" ] |
            [ Statement ]
Statement   => IDENT { "." IDENT } "=" Expr ";" |
            IDENT "[" Expr "]" "=" Expr ";" |
            "if" Expr "then" Block {"else" Block} "end" |
            "while" Expr "do" Block "end" |
            "func" IDENT {IDENT} "start" Block "end" |
            "record" "start" IDENT "end" |
            "return" Expr ";" |
            Expr ";"
Expr        => LogicExpr |
            FuncCall |
            BuiltInCall |
            "[" Expr "]" {"(" {Expr, } ")"} |
            "new" IDENT "(" { Expr, } ")"
ExprList    => { Expr, }
FuncCall    => "call" IDENT {IDENT}
BuiltInCall => BuiltIn { Expr {, Expr} }
LogicExpr   => CompExpr { LOGIC_OP CompExpr }
CompExpr    => AddExpr { COMP_OP AddExpr }
AddExpr     => MultExpr { ADD_OP MultExpr }
MultExpr    => Factor { MULT_OP Factor }
Factor      => [ UNARY_OP ] IDENT { "." IDENT } | IDENT "[" Expr "]" | Primary | "(" Expr ")"
Primary     => INT | FLOAT | STRING | BOOL | CHAR
BuiltIn     => "print" | "println" | "dbg" | "readfile" | "writefile" | "appendfile" | "len"
```

## Macros:
There is a simple macro system for defining C-like text replacement macros.
Macros are replaced and completely removed from the code in a preprocessing step before tokenization.
This means, that there syntax is not part of the general grammar.

#### Macro Syntax:
`$ MACRO_NAME $$ arg0 $ argo1 $ ... $$`

#### Special Hardcoded Macros:
There are 2 special macros, that are directly hardcoded into the preprocessor.
  - `$import $$ FILE_PATH $$`: This macro loads the file at `FILE_PATH` and replaces itself with the text contents from the file.
  - `$define $$ MACRO_NAME $ ARG0 $ ARG1 $ ... $ REPLACE_STR $$`: This macro can be used to define new macros. The first argument is the name of the new macro, followed by a list of argument names that the new macro should have. The last argument is the replace string. The newly generated macro will replace the argument names inside of the replace string with the given argument values at the macro invocation. Then it will replace itself with the generated string. Macro redefinition is not allowed!

#### Macro Examples:
  - Loading the standard library into your script:
  ```hll
  $import $$ stdlib.h $$
  ```
  - Defining a macro with `$define` and invoking it inside of the main function:
  ```
  $define $$ GREETING $ NAME $ "Hello, NAME" $$  
  
  func main start
    println $GREETING $$ Adam $$;
  end
  ```

## Current Builtin Functions:
  - `print` and `println`: Take an arbitrary amount of arguments and print them to stdout. Return: `()`
  - `dbg`: Take an arbitrary amount of arguments and print their internal debug representation to stdout. Return: `()` 
  - `readfile`: Takes a file path as argument and returns the file content as string. Return: `String`
  - `writefile`: first argument: file path, second argument: object. Writes the objects string repr into the given file. When the file does not exists, it creates the file. Return: `()`
  - `appendfile`: first argument: file path, second argument: object. Writes the objects string repr into the given file appending to existing text. When the file does not exists, it creates the file. Return: `()`
  - `len`: Takes one argument and returns its length if its an container type. Return: `Int`

### TODO:
  - lambdas, functions as values
  - for loop
  - remove *.borrow().clone() by using get_type with another enum variant Type
  - replace import macro with import statement, that loads the file and executes its content with the interpreter

### BUGS:
  - when passing list literals behind idents in func argument list, then the parser thinks the len expr of the list is an indexing of the ident
  - grammar issues when using calls and builtins inside of argument lists or conditions => root problem with expr_list
