let add = (x: int) -> (y: int) -> x + y;
let add_five = add 5;

// right now alpha renaming in closures is a bit broken,
// so calling this variable "x" would break the program
let eq_six = (z: int) -> add_five 1 == z;

print (eq_six 6);