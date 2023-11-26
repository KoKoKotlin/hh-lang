# hh-lang

Programming language that I make for solving AOC 2023.
It uses duck typing like python and has support only for the bare minimum that is needed for solving AOC.
For usage see the example folder.

## Grammar:
```
Program     => [ Block ]
Block       => [ "let" IDENT "=" Literal {, IDENT "=" Literal } ";" ] |
            [ "var" IDENT { "=" Literal } {, IDENT {"=" Literal}} ";" ] |
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
CompExpr    => AddExpr { LOGIC_OP AddExpr }
AddExpr     => MultExpr { ADD_OP MultExpr }
MultExpr    => Factor { MULT_OP Factor }
Factor      => [ UNARY_OP ] IDENT { "." IDENT } | IDENT "[" Expr "]" | Literal | "(" Expr ")"
Literal     => INT | FLOAT | STRING | BOOL | CHAR | RecordInstance | List
BuiltIn     => "print" | "println" | "dbg" | "readfile" | "writefile" | "appendfile"
```

### Current Builtin Functions:
  - `print` and `println`: Take an arbitrary amount of arguments and print them to stdout. Return: `()`
  - `dbg`: Take an arbitrary amount of arguments and print their internal debug representation to stdout. Return: `()` 
  - `readfile`: Takes a file path as argument and returns the file content as string. Return: `String`
  - `writefile`: first argument: file path, second argument: object. Writes the objects string repr into the given file. When the file does not exists, it creates the file. Return: `()`
  - `appendfile`: first argument: file path, second argument: object. Writes the objects string repr into the given file appending to existing text. When the file does not exists, it creates the file. Return: `()`

### TODO:
  - lambdas, functions as values
  - string indexing, char type
  - for loop

### BUGS:
  - when passing list literals behind idents in func argument list, then the parser thinks the len expr of the list is an indexing of the ident

