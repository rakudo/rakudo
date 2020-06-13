my class X::Cannot::Capture { ... }

my role Callable[::T = Mu] {
    method of() { T }
    method returns() { T }
    method Capture() { X::Cannot::Capture.new( :what(self) ).throw }
}

# vim: expandtab shiftwidth=4
