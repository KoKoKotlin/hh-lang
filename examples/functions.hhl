
# this is the syntax of a function decleration whith empty function body
func foo start
end

# function can take an arbitrary amount of arguments which become variables
# inside of the function scope
# variables in the global scope are also available inside of functions
# but they are shadowed by local variables or arguments to the function
func foo2 arg1 arg2 start
end

# it is possible to return values from the function
func add a b start
    return a + b
end

let a = 1, b = 2;
var c;
# when calling a function the "call" keyword has to be used
c = call add a b;
println c;
