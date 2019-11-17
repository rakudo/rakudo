# This is what Mu::WALK method returns.
my class WalkList is List {
    has Mu $.invocant;
    has $.quiet = False;
    has $.reverse = False;

    method new(|) {
        callsame.quiet(False).reversed(False)
    }

    proto method invoke(|) {*}
    multi method invoke(::?CLASS:D: Capture:D $args) {
        my @rv;
        for |($!reverse ?? self.List::reverse !! self) -> &method {
            @rv.push: $($!invocant.&method(|$args));
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
        note "invoke(", c.perl, ")" if %*ENV<RAKUDO_DEBUG>;
        samewith(c)
    }

    method quiet(::?CLASS:D: Bool() $quiet = True --> ::?CLASS:D) {
        $!quiet = $quiet;
        self
    }

    method reversed(::?CLASS:D: Bool() $reverse = True --> ::?CLASS:D) {
        $!reverse = $reverse;
        self
    }

    method set_invocant(::?CLASS:D: Mu \inv) {
        $!invocant = inv;
        self
    }

    method CALL-ME(::?CLASS:D: |c) {
        self.invoke(c)
    }
}
