
# it is possible to define records which are simply a collection of names
# behind one common name
record Point start
    x
    y
end

# instatiating a record:
var p;
p = new Point(1, 2);
# you have to provide as many arguments as there are fields in the record

# accessing the records field:
println p.x, p.y;

# reassigning record fields:
p.x = 4;