my class X::Routine::Unwrap { ... }

my role HardRoutine {
    method soft(--> False) { }
}
my role SoftRoutine {
    method soft(--> True) { }
}

my class Routine { # declared in BOOTSTRAP
    # class Routine is Block
    #     has Mu $!dispatcher;
    #     has int $!flags;
    #     has Mu $!inline_info;
    #     has Mu $!package;
    #     has Mu $!op_props;  # to be DEPRECATED
    #--- proto specific ---
    #     has @!dispatchees;
    #     has Mu $!dispatch_info;
    #     has @!dispatch_order;
    #     has Mu $!dispatch_cache;  # NOT on MoarVM

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
        (my $name := self.name)
          ?? "&$name"
          !! (self.^name ~~ m/^\w+/).lc ~ ' { }'
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
        has Mu      $!wrappers;
        has Routine $!wrapper-type;
        method WRAPPERS()     { $!wrappers     }
        method WRAPPER-TYPE() { $!wrapper-type }

        method ADD-WRAPPER(&wrapper --> Nil) {
            my $new-wrappers := nqp::isconcrete($!wrappers)
              ?? nqp::clone($!wrappers)
              !! IterationBuffer.new;
            nqp::unshift($new-wrappers, &wrapper);
            $!wrappers := $new-wrappers;
        }

        method REMOVE-WRAPPER(&wrapper --> Bool:D) {
            my $wrappers := nqp::clone($!wrappers);

            my int $m = nqp::elems($wrappers);
            my int $i;
            while $i < $m {
                my $wrapper := nqp::atpos($wrappers, $i);
                if nqp::eqaddr(&wrapper, nqp::atpos($wrappers, $i)) {
                    nqp::splice($wrappers, nqp::list, $i, 1);
                    $!wrappers := $wrappers;
                    return True;
                }
                ++$i;
            }
            False
        }
        method is-wrapped(--> Bool) { nqp::elems($!wrappers) > 1 }
    }
    my class WrapHandle {
        has &!routine;
        has $!wrapper;
        method restore(--> Bool:D) {
            nqp::can(&!routine, 'REMOVE-WRAPPER')
              ?? &!routine.REMOVE-WRAPPER($!wrapper)
              !! False
        }
    }

    method wrap(&wrapper) {
        # We can't wrap a hardened routine (that is, one that's been
        # marked inlinable).
        die "Cannot wrap a HardRoutine, since it may have been inlined; "
          ~ "use the 'soft' pragma to avoid marking routines as hard."
          if nqp::istype(self, HardRoutine);

        # Mix in the Wrapped role if needed and add the wrapper
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
        X::Routine::Unwrap.new.throw
          unless $handle.can('restore') && $handle.restore;
    }

    method package() { $!package }

    method leave(*@) { NYI("{self.^name}.leave()").throw; }

    my class CandidateIterator does Iterator {
        has     $!routine;
        has Mu  $!candidates;
        has int $!pos;
        has Mu  $!backlog;
        has     $!local;
        has     $!with-proto;

        method !SET-SELF($routine, $!local, $!with-proto) {
            self!SET-FROM-CANDIDATE($routine);
            $!backlog := nqp::list;
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
            $!candidates := nqp::defined($candidates)
              ?? $candidates
              !! nqp::list($!routine);
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
        $candidates && (self.is_dispatcher || self.is-wrapped)
          ?? CandidateIterator.new(self, $local, $with-proto)
          !! self.Mu::iterator
    }

    # Return 1 if all candidates of the given proto have a :U invocant
    # constraint, else 0.
    method IS-SETTING-ONLY-U() is implementation-detail {
        for self.candidates(:!local, :with-proto) -> &cand {
            next unless nqp::istype(&cand, Method)
                     || nqp::istype(&cand, Submethod);

            my $invocant-type := &cand.signature.params[0].type;
            next unless $invocant-type.^archetypes.definite
                     && $invocant-type.^definite;

            return 0 unless &cand.file.starts-with: 'SETTING::';
        }
        1
    }

    # Return 1 if all candidates of the given proto have a :D invocant
    # constraint, else 0.
    method IS-SETTING-ONLY-D() is implementation-detail {
        for self.candidates(:!local, :with-proto) -> &cand {
            next unless nqp::istype(&cand, Method)
                     || nqp::istype(&cand, Submethod);

            my $invocant-type := &cand.signature.params[0].type;
            next if $invocant-type.^archetypes.definite
                 && $invocant-type.^definite;

            return 0 unless &cand.file.starts-with: 'SETTING::';
        }
        1
    }

#-------------------------------------------------------------------------------
# The REST of this file can be REMOVED **AFTER** the Raku grammar has
# become the grammar to build the setting with.  XXX

    method prec(|c --> Hash:D) {
        ($!op_props // OperatorProperties).prec(|c)
    }

    method !proto() { $!dispatcher // self }

    # Return the OperatorProperties of the proto of the invocant
    method op_props(Routine:D:
      --> OperatorProperties) is implementation-detail {
        nqp::getattr(self!proto,Routine,'$!op_props')
          // OperatorProperties
    }

    method precedence(Routine:D:   --> Str:D) { self.op_props.precedence  }
    method associative(Routine:D:  --> Str:D) { self.op_props.associative }
    method thunky(Routine:D:       --> Str:D) { self.op_props.thunky      }
    method iffy(Routine:D:        --> Bool:D) { self.op_props.iffy.Bool   }
    method reducer(Routine:D: --> Callable:D) { self.op_props.reducer     }

    # Set operator properties, usually called through trait_mods
    method equiv(Routine:D: &op --> Nil) {
        nqp::bindattr(self!proto,Routine,'$!op_props',
          &op.op_props.equiv(self.associative)
        )
    }
    method tighter(Routine:D: &op --> Nil) {
        nqp::bindattr(self!proto,Routine,'$!op_props',
          &op.op_props.tighter(self.associative)
        )
    }
    method looser(Routine:D: &op --> Nil) {
        nqp::bindattr(self!proto,Routine,'$!op_props',
          &op.op_props.looser(self.associative)
        )
    }
    method assoc(Routine:D: Str:D $associative --> Nil) {
        nqp::bindattr(self!proto,Routine,'$!op_props',
          self.op_props.new(:$associative))
    }

    # Internal helper method to set operator properties
    method set_op_props(Routine:D:) is implementation-detail {
        (my str $type, my str $name) = self.name.split(":",2);
        $name = nqp::eqat($name,'<<',0)
          ?? nqp::substr($name,2,nqp::chars($name) - 4)
          !! nqp::substr($name,1,nqp::chars($name) - 2);
        nqp::bindattr(self,Routine,'$!op_props',
          OperatorProperties."$type"($name))
    }

    # Helper method to apply a trait by name and given operator target string
    # using information of target operator of the same category
    method apply-operator-trait(Routine:D:
      Str:D $trait, Str:D $target --> Nil
    ) is implementation-detail {
        my str $name  = self.name;
        my int $index = nqp::index($name,':');
        die "Operator given to 'is $trait' does not appear to be an operator"
          if $index < 0;

        my $fqn := '&'
          ~ nqp::substr($name,0,$index)
          ~ ($target.contains('<') || $target.contains('>')
              ?? ":«$target»"
              !! ":<$target>"
            );
        nqp::istype((my $op := ::($fqn)),Failure)
          ?? $op.throw
          !! self."$trait"($op)
    }
}

multi sub trait_mod:<is>(Routine:D $r, :&equiv! --> Nil) {
    $r.equiv(&equiv)
}
multi sub trait_mod:<is>(Routine:D $r, Str:D :$equiv! --> Nil) {
    $r.apply-operator-trait('equiv', $equiv)
}

multi sub trait_mod:<is>(Routine:D $r, :&tighter! --> Nil) {
    $r.tighter(&tighter)
}
multi sub trait_mod:<is>(Routine:D $r, Str:D :$tighter!) {
    $r.apply-operator-trait('tighter', $tighter)
}

multi sub trait_mod:<is>(Routine:D $r, :&looser! --> Nil) {
    $r.looser(&looser)
}
multi sub trait_mod:<is>(Routine:D $r, Str:D :$looser!) {
    $r.apply-operator-trait('looser', $looser)
}

multi sub trait_mod:<is>(Routine:D $r, :$assoc! --> Nil) {    # --> Nil
    $r.assoc($assoc)
}

# old interface, should probably be marked DEPRECATED
multi sub trait_mod:<is>(Routine:D $r, :%prec! --> Nil) {
    nqp::bindattr($r,Routine,'$!op_props',
      OperatorProperties.new-compat(|%prec)
    )
}

# vim: expandtab shiftwidth=4
