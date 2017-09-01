my class X::Routine::Unwrap { ... }

my role HardRoutine {
    method soft(--> False) { }
}
my role SoftRoutine {
    method soft(--> True) { }
}

my class Routine { # declared in BOOTSTRAP
    # class Routine is Block
    #     has @!dispatchees;
    #     has Mu $!dispatcher_cache;
    #     has Mu $!dispatcher;
    #     has int $!rw;
    #     has Mu $!inline_info;
    #     has int $!yada;
    #     has Mu $!package;
    #     has int $!onlystar;
    #     has @!dispatch_order;
    #     has Mu $!dispatch_cache;

    method onlystar() { nqp::p6bool($!onlystar) }

    method candidates() {
        self.is_dispatcher ??
            nqp::hllize(@!dispatchees) !!
            (self,)
    }

    method cando(Capture $c) {
        my $disp;
        if self.is_dispatcher {
            $disp := self;
        }
        else {
            $disp := nqp::create(self);
            nqp::bindattr($disp, Routine, '@!dispatchees', nqp::list(self));
        }
        # Call this lexical sub to get rid of 'self' in the signature.
        sub checker(|) {
            nqp::hllize($disp.find_best_dispatchee(nqp::usecapture(), 1))
        }
        checker(|$c);
    }

    method multi() {
        self.dispatcher.defined
    }

    multi method perl(Routine:D:) {
        my $perl = ( self.^name ~~ m/^\w+/ ).lc;
        if self.name() -> $n {
            $perl ~= " $n";
        }
        $perl ~= ' ' ~ substr(self.signature().perl,1); # lose colon prefix
        $perl ~= ' { #`(' ~ self.WHICH ~ ') ... }';
        $perl
    }

    method soft( --> Nil ) { }

    method wrap(&wrapper) {
        my class WrapHandle {
            has $!dispatcher;
            has $!wrapper;
            method restore() {
                nqp::p6bool($!dispatcher.remove($!wrapper));
            }
        }
        my role Wrapped {
            has $!dispatcher;
            method UNSHIFT_WRAPPER(&wrapper) {
                # Add candidate.
                $!dispatcher := WrapDispatcher.new()
                    unless nqp::isconcrete($!dispatcher);
                $!dispatcher.add(&wrapper);

                # Return a handle.
                my $handle := nqp::create(WrapHandle);
                nqp::bindattr($handle, WrapHandle, '$!dispatcher', $!dispatcher);
                nqp::bindattr($handle, WrapHandle, '$!wrapper', &wrapper);
                $handle
            }
            method CALL-ME(|c) is raw {
                $!dispatcher.enter(|c);
            }
            method soft(--> True) { }
        }

        # We can't wrap a hardened routine (that is, one that's been
        # marked inlinable).
        if nqp::istype(self, HardRoutine) {
            die "Cannot wrap a HardRoutine, since it may have been inlined; " ~
                "use the 'soft' pragma to avoid marking routines as hard.";
        }

        # If we're not wrapped already, do the initial dispatcher
        # creation.
        unless nqp::istype(self, Wrapped) {
            my $orig = self.clone();
            self does Wrapped;
            self.UNSHIFT_WRAPPER($orig);
        }

        # Add this wrapper.
        self.UNSHIFT_WRAPPER(&wrapper);
    }

    method unwrap($handle) {
        $handle.can('restore') && $handle.restore() ||
            X::Routine::Unwrap.new.throw
    }

    method yada() {
        nqp::p6bool(nqp::getattr_i(self, Routine, '$!yada'))
    }

    method package() { $!package }

    method leave(*@) {
        X::NYI.new(:feature("{self.^name}.leave()")).throw;
    }
}

# vim: ft=perl6 expandtab sw=4
