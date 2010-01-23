augment class List {
    method elems() { +self!generate; }

    method list() { self; }

    method Num() { self.elems; }

    method Int() { self.elems.Int; }

    method Str() {
        pir::join(' ', self!generate);
    }

    method perl() {
        if self.elems() == 0 { return Nil.WHAT };
        # XXX: $_.perl and .perl don't work, but this does...
        '(' ~ self.map({ $^a.perl }).join(', ') ~ ')';
    }
}

# Throw together an iterator interface, just so we have something to talk about

role Iterator {
    multi method get() {
        Nil; # probably should use a special terminator instead
    }

    multi method list() {
        self;
    }
}

class ScalarIterator does Iterator {
    has $.value;
    has $.done;

    multi method new($value) {
        self.bless(*, :value($value),
                      :done(Bool::False));
    }

    multi method get() {
        if $.done {
            Nil;
        }
        else {
            $.done = Bool::True;
            $.value;
        }
    }
}


# vim: ft=perl6
