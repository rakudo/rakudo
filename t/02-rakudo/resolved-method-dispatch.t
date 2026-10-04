use Test;

plan 6;

is (try EVAL(q[my class P { method m { "P" } }; my class C is P { method m { "C" ~ callsame() } }; my $m = C.^find_method("m"); (^4).map({ $m.wrap(sub (|c) { "W[" ~ callsame() ~ "]" }) if $_ == 2; $m(C.new) }).join(" ")])), 'CP CP W[CP] W[CP]',
    'a method called as code takes the wrapper put on it after earlier calls';
# These hold with the dispatch of a resolved method keyed on the method itself
# too, and guard what sharing it keeps.
is (try EVAL(q[my class P { method x { "Px" }; method y { "Py" } }; my class C is P { }; my $t = method (C:) { "C" ~ nextsame }; my $mx = $t.clone; $mx.set_name("x"); my $my = $t.clone; $my.set_name("y"); C.^add_method("x", $mx); C.^add_method("y", $my); C.^compose; ($mx, $my, $mx, $my).map({ $_(C.new) }).join(" ")])), 'Px Py Px Py',
    'clones of a method named apart defer by their own names when called as code';
is (try EVAL(q[my class P { method m { "P" } }; my class C is P { method m { "C" ~ callsame } }; my $m = C.^find_method("m"); (^3).map({ C.new.$m() }).join(" ")])), 'CP CP CP',
    'a method called as code defers to the parent on each call';
is (try EVAL(q[my class P { method m($x) { "P$x" } }; my class C is P { method m($x) { "C" ~ callwith($x + 1) } }; my $m = C.^find_method("m"); (^3).map({ C.new.$m($_) }).join(" ")])), 'CP1 CP2 CP3',
    'a method called as code defers with new arguments on each call';
is (try EVAL(q[sub g($i) { my method m { $i * 10 }; 5.&m }; (^4).map({ g($_) }).join(" ")])), '0 10 20 30',
    'a lexical method called as code sees each call of its routine';
is (try EVAL(q[my $c = 0; sub f($i) { my token t { \d+ }; $c++ if "a$i" ~~ /<&t>/ }; f($_) for ^5; $c])), 5,
    'a lexical token called as code matches on each call of its routine';

# vim: expandtab shiftwidth=4
