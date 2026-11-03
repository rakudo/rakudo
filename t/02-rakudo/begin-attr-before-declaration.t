use Test;

plan 7;

# A method literal built inside a BEGIN block generates its code before the
# class finishes parsing, so an attribute declared further down the body is
# not on the class yet at that point. The access used to compile to a null,
# so touching the attribute at runtime failed on a VMNull invocant.

{
    my class C {
        my $m = BEGIN method () { @!later.push('pushed'); @!later };
        has @.later;
        method run() { $m(self) }
    }
    is-deeply C.new.run, ['pushed'],
        'a BEGIN-built method can read an attribute declared after the BEGIN';
}

{
    my class C {
        my $m = BEGIN method () { $!scalar };
        has $.scalar = 'default';
        method run() { $m(self) }
    }
    is C.new.run, 'default',
        'a BEGIN-built method reads a later scalar attribute default';
}

{
    my class C {
        my $m = BEGIN method ($value) { $!scalar = $value; $!scalar };
        has $.scalar is rw;
        method run($value) { $m(self, $value) }
    }
    is C.new.run(42), 42,
        'a BEGIN-built method can assign a later scalar attribute';
}

{
    my class C {
        my $m = BEGIN method () { $!scalar = 5; $!scalar };
        has $.scalar is rw;
        method run() { $m(self) }
    }
    is C.new.run, 5,
        'a BEGIN-built method assigns a later untyped scalar attribute';
}

{
    my class C {
        my $m = BEGIN method () { $!count++; $!count };
        has Int $.count = 1;
        method run() { $m(self) }
    }
    is C.new.run, 2,
        'a BEGIN-built method steps a later Int attribute';
}

{
    my class C {
        my $m = BEGIN method ($v) { $!scalar := $v; $!scalar };
        has $.scalar;
        method run($v) { $m(self, $v) }
    }
    is C.new.run(7), 7,
        'a BEGIN-built method binds a later untyped scalar attribute';
}

# The undeclared-attribute error still fires for an attribute the class
# never declares.
throws-like 'class C { my $m = BEGIN method () { $!nope }; }',
    X::Attribute::Undeclared,
    'an attribute never declared in the class is still rejected';

# vim: expandtab shiftwidth=4
