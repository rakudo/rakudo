unit module TypedExports;

# An exported variable answers the type of the value it holds, which says
# nothing about what its container accepts.
our $count is export = 0;
our @names is export;

# A constant is the value it stands for, wherever it is read from.
constant Answer is export = 42;
