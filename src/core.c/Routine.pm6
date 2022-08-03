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
    #     has Mu $!dispatcher;
    #     has int $!flags;
    #     has Mu $!inline_info;
    #     has Mu $!package;
    #     has @!dispatch_order;
    #     has Mu $!dispatch_cache;

    method candidates(Bool :$local = True, Bool() :$with-proto) {
        $local
            ?? (self.is_dispatcher ?? nqp::hllize(@!dispatchees) !! (self,))
            !! Seq.new(self.iterator(:candidates, :!local, :$with-proto))
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

    method soft(--> Nil) { }

    method is-wrapped(--> False) { }

#?if !moar
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
            method WRAPPERS() { IterationBuffer.new($!dispatcher.candidates) }
            method soft(--> True) { }
            method is-wrapped(--> Bool) { $!dispatcher.candidates > 1 }
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
#?endif

#?if moar
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
        method is-wrapped(--> Bool) { nqp::elems($!wrappers) > 1 }
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
    method wrap(&wrapper) {
        # We can't wrap a hardened routine (that is, one that's been
        # marked inlinable).
        if nqp::istype(self, HardRoutine) {
            die "Cannot wrap a HardRoutine, since it may have been inlined; " ~
                "use the 'soft' pragma to avoid marking routines as hard.";
        }

        # Mix in the Wrapped role if needed and add the wrapper.
        unless nqp::istype(self, Wrapped) {
            my $orig := self.clone;
            self does Wrapped;
            nqp::bindattr(self, self.WHAT, '$!wrapper-type', self.WHAT);
            self.ADD-WRAPPER($orig);
        }
        self.ADD-WRAPPER(&wrapper);

        # Create and return a wrap handle
        my $handle := nqp::create(WrapHandle);
        nqp::bindattr($handle, WrapHandle, '&!routine', self);
        nqp::bindattr($handle, WrapHandle, '$!wrapper', &wrapper);
        $handle
    }
#?endif

    method unwrap($handle) {
        $handle.can('restore') && $handle.restore() ||
            X::Routine::Unwrap.new.throw
    }

    method package() { $!package }

    method leave(*@) { NYI("{self.^name}.leave()").throw; }

    my class CandidateIterator does Iterator {
        has $!routine;
        has Mu $!candidates;
        has int $!pos;
        has Mu $!backlog;
        has $!local;
        has $!with-proto;

        method !SET-SELF($routine, $!local, $!with-proto) {
            self!SET-FROM-CANDIDATE($routine);
            $!backlog := nqp::list();
            self
        }

        method new(Routine:D $routine, $local, $with-proto) {
            nqp::create(self)!SET-SELF($routine, $local, $with-proto)
        }

        method !SET-FROM-CANDIDATE($routine) {
            $!routine := nqp::decont($routine);
            $!pos = 0;
            my $candidates;
            if nqp::istype($!routine, Routine) {
                if $!routine.?is-wrapped {
                    $candidates := $!routine.WRAPPERS;
                }
                elsif $!routine.?is_dispatcher {
                    $candidates := nqp::getattr($!routine, Routine, '@!dispatchees');
                    $!pos = -1 if $!with-proto;
                }
            }
            if nqp::defined($candidates) {
                $!candidates := $candidates;
            }
            else {
                $!candidates := nqp::list($!routine);
            }
        }

        method pull-one() {
            my $cand := Nil;
            while nqp::eqaddr($cand, Nil) {
                while $!pos >= nqp::elems($!candidates) {
                    return IterationEnd unless nqp::elems($!backlog);
                    my $state := nqp::pop($!backlog);
                    $!candidates := nqp::atpos($state, 0);
                    $!pos = nqp::atpos($state, 1);
                    $!routine := nqp::atpos($state, 2);
                }

                my $pos = $!pos;
                ++$!pos;
                if $pos == -1 {
                    $cand := $!routine;
                }
                else {
                    $cand := nqp::atpos($!candidates, $pos);
                }

                if !$!local
                    && ( $cand.?is-wrapped
                        || ($pos > -1 && $cand.?is_dispatcher) )
                {
                    nqp::push($!backlog, nqp::list($!candidates, nqp::unbox_i($!pos), $!routine));
                    self!SET-FROM-CANDIDATE($cand);
                    $cand := Nil;
                }
            }
            $cand
        }

        method is-lazy(--> True) {}
    }

    method iterator(Bool :$candidates, Bool() :$local, Bool() :$with-proto) {
        return self.Mu::iterator unless $candidates && (self.is_dispatcher || self.is-wrapped);
        CandidateIterator.new(self, $local, $with-proto)
    }

    method IS-SETTING-ONLY(:$U, :$D, :$with-proto --> Bool:D) is implementation-detail {
        for self.candidates(:!local, :$with-proto) -> &cand {
            if $U || $D {
                next unless nqp::istype(&cand, Method) || nqp::istype(&cand, Submethod);
                my $invocant-type := &cand.signature.params[0].type;
                my $is-definite := $invocant-type.^archetypes.definite && $invocant-type.^definite;
                next unless ($U && !$is-definite) || ($D && $is-definite);
            }
            return False unless &cand.file.starts-with: 'SETTING::';
        }
        True
    }
}

# vim: expandtab shiftwidth=4
