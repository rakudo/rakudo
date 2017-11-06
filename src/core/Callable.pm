my class X::Cannot::Capture { ... }

my role Callable[::T = Mu] {
    method of() { T }
    method returns() { T }
    method Capture() { die X::Cannot::Capture.new: :what(self) }
}

# vim: ft=perl6 expandtab sw=4
