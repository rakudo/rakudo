# This is what Mu::WALK method returns.
my class WalkList is List {
    has Mu $.invocant;
    has $.quiet = False;

    method new(|) {
        callsame.quiet(False)
    }

    proto method invoke(|) {*}
    multi method invoke(::?CLASS:D: Capture:D $args) {
        my @rv;
        for |self -> &method {
            my $val = $!invocant.&method(|$args);
            @rv.push: $val ~~ Slip ?? $val.List !! $val;
            CATCH {
                default {
                    $_.throw unless $!quiet;
                    @rv.push: Failure.new($_)
                }
            }
        }
        @rv
    }
    multi method invoke(::?CLASS:D: |c) {
        note "invoke(", c.raku, ")" if %*ENV<RAKUDO_DEBUG>;
        samewith(c)
    }

    method quiet(::?CLASS:D: Bool() $quiet = True --> ::?CLASS:D) {
        $!quiet = $quiet;
        self
    }

    method reverse(::?CLASS:D: --> ::?CLASS:D) {
        self.WHAT.new(|self.List::reverse)
            .set_invocant($!invocant)
            .quiet($!quiet)
    }

    method set_invocant(::?CLASS:D: Mu \inv) {
        $!invocant = inv;
        self
    }

    method CALL-ME(::?CLASS:D: |c) {
        self.invoke(c)
    }
}
