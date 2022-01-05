# This is what Mu::WALK method returns.
my class WalkList is List {
    has Mu $.invocant;
    has $.is-quiet is built(False) = False;

    proto method invoke(|) {*}
    multi method invoke(::?CLASS:D: Capture:D $args --> Seq:D) {
        Seq.new(class :: does Iterator {
            has $!is-quiet;
            has $!invocant;
            has $!wl-iterator;
            has $.is-lazy = True;
            method !SET-SELF(\wlist) {
                $!is-quiet = wlist.is-quiet;
                $!wl-iterator = wlist.iterator;
                $!invocant = wlist.invocant;
                self
            }
            method new(\wlist) { nqp::create(self)!SET-SELF(wlist) }
            method pull-one() {
                CATCH {
                    .rethrow unless $!is-quiet;
                    return Failure.new($_)
                }
                nqp::eqaddr(
                  (my $method := nqp::decont($!wl-iterator.pull-one)),
                  IterationEnd
                ) ?? IterationEnd
                  !! $!invocant.$method(|$args)
            }
        }.new(self))
    }
    multi method invoke(::?CLASS:D: |c --> Seq:D) {
        self.invoke(c)
    }

    method quiet(::?CLASS:D: Bool() $quiet = True --> ::?CLASS:D) {
        $!is-quiet = $quiet;
        self
    }

    method reverse(::?CLASS:D: --> ::?CLASS:D) {
        self.WHAT.new(|self.List::reverse)
            .set_invocant($!invocant)
            .quiet($!is-quiet)
    }

    method set_invocant(::?CLASS:D: Mu \inv) {
        $!invocant = inv;
        self
    }

    method CALL-ME(::?CLASS:D: |c) {
        self.invoke(c)
    }
}

# vim: expandtab shiftwidth=4
