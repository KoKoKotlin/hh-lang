
# there are several builtin functions which extend the functionality of the language
# they are directly implemented in the interpreter and can return values
# there arguments are seperated with commas unlike in function calls
# for a full list, see the README.md

println "Hello, World";

record Point start
    x
    y
end

dbg new Point(1, 2);