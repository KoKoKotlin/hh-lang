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
Expr        => {UN_OP} Term { ADD_OP term } |
            FuncCall |
            BuiltInCall |
            "[" Expr "]" {"(" {Expr, } ")"} |
            "new" IDENT "(" { Expr, } ")"
ExprList    => { Expr, }
FuncCall    => "call" IDENT {IDENT}
BuiltInCall => BuiltIn { Expr {, Expr} }
Term        => Factor { MULT_OP Factor }
Factor      => IDENT { "." IDENT } | IDENT "[" Expr "]" | NUMBER | STRING | "(" Expr ")"
Literal     => NUMBER | STRING | BOOL | RecordInstance | ListLiteral
BuiltIn     => "print" | "println" | "dbg"
```

### Current Builtin Functions:
  - `print` and `println`: Take an arbitrary amount of arguments and print them to stdout. Return: `()`
  - `dbg`: Take an arbitrary amount of arguments and print their internal debug representation to stdout. Return: `()`

### TODO:
  - floats
  - logic, unary, comparision operations
  - function, builtin returns

### BUGS:
  - when passing list literals behind idents in func argument list, then the parser thinks the len expr of the list is an indexing of the ident 

