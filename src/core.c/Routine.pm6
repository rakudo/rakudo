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
    #     has int $!flags;
    #     has Mu $!inline_info;
    #     has Mu $!package;
    #     has int $!onlystar;
    #     has @!dispatch_order;
    #     has Mu $!dispatch_cache;

    method onlystar() { nqp::hllbool($!onlystar) }

    method candidates() {
        self.is_dispatcher ??
            nqp::hllize(@!dispatchees) !!
            (self,)
    }

    proto method cando(|) {*}
    multi method cando(Capture:D $c) {
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
    multi method cando(|c) { self.cando(c) }

    method multi() {
        self.dispatcher.defined
    }

    multi method gist(Routine:D:) {
        if self.name -> $name {
            "&$name"
        }
        else {
            ( self.^name ~~ m/^\w+/ ).lc ~ ' { }'
        }
    }

    multi method raku(Routine:D:) {
        my $raku = ( self.^name ~~ m/^\w+/ ).lc;
        if self.is_dispatcher {
            $raku = "proto $raku";
        }
        elsif self.multi {
            $raku = "multi $raku";
        }
        if self.name() -> $n {
            $raku ~= " $n";
        }
        my $sig := self.signature.raku;
        $raku ~= " $sig.substr(1)" unless $sig eq ':()';
        $raku ~= self.onlystar
          ?? ' {*}'
          !! self.yada
            ?? ' { ... }'
            !! ' { #`(' ~ self.WHICH ~ ') ... }';
        $raku
    }

    method soft( --> Nil ) { }

    method wrap(&wrapper) {
        my class WrapHandle {
            has $!dispatcher;
            has $!wrapper;
            method restore() {
                nqp::hllbool($!dispatcher.remove($!wrapper));
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
            $!onlystar = 0; # disable optimization if no body there
            self.UNSHIFT_WRAPPER($orig);
        }

        # Add this wrapper.
        self.UNSHIFT_WRAPPER(&wrapper);
    }

    my role Wrapped {
        has Mu $!wrappers;
        has Routine $!wrapper-type;
        method WRAPPERS() { $!wrappers }
        method WRAPPER-TYPE() { $!wrapper-type }
        method ADD-WRAPPER(&wrapper --> Nil) {
            my $new-wrappers := nqp::isconcrete($!wrappers)
                ?? nqp::clone($!wrappers)
                !! IterationBuffer.new;
            nqp::unshift($new-wrappers, &wrapper);
            $!wrappers := $new-wrappers;
        }
        method REMOVE-WRAPPER(&wrapper --> Bool) {
            my $new-wrappers := IterationBuffer.new;
            my int $i = 0;
            my Bool $found := False;
            while $i < nqp::elems($!wrappers) {
                my $wrapper := nqp::atpos($!wrappers, $i);
                if nqp::eqaddr(&wrapper, $wrapper) {
                    $found := True;
                }
                else {
                    nqp::push($new-wrappers, $wrapper);
                }
                $i++;
            }
            $!wrappers := $new-wrappers if $found;
            $found
        }
    }
    my class WrapHandle {
        has &!routine;
        has $!wrapper;
        method restore(--> Bool) {
            nqp::can(&!routine, 'REMOVE-WRAPPER')
                ?? &!routine.REMOVE-WRAPPER($!wrapper)
                !! False
        }
    }
    method new-disp-wrap(&wrapper) {
        # We can't wrap a hardened routine (that is, one that's been
        # marked inlinable).
        if nqp::istype(self, HardRoutine) {
            die "Cannot wrap a HardRoutine, since it may have been inlined; " ~
                "use the 'soft' pragma to avoid marking routines as hard.";
        }

        # Mix in the Wrapped role if needed and add the wrapper.
        unless nqp::istype(self, Wrapped) {
            self does Wrapped;
            nqp::bindattr(self, self.WHAT, '$!wrapper-type', self.WHAT);
        }
        self.ADD-WRAPPER(&wrapper);

        # Create and return a wrap handle
        my $handle := nqp::create(WrapHandle);
        nqp::bindattr($handle, WrapHandle, '&!routine', self);
        nqp::bindattr($handle, WrapHandle, '$!wrapper', &wrapper);
        $handle
    }

    method unwrap($handle) {
        $handle.can('restore') && $handle.restore() ||
            X::Routine::Unwrap.new.throw
    }

    method new-disp-unwrap($handle) {
        $handle.can('restore') && $handle.restore() ||
            X::Routine::Unwrap.new.throw
    }

    method package() { $!package }

    method leave(*@) {
        X::NYI.new(:feature("{self.^name}.leave()")).throw;
    }
}

# vim: expandtab shiftwidth=4
