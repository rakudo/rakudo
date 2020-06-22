class Rakudo::Supply {
    my constant ADD_WHENEVER_PROMPT = Mu.new;

    class CachedAwaitHandle does Awaitable {
        has $.get-await-handle;
    }

    class BlockAddWheneverAwaiter does Awaiter {
        has $!continuations;

        method await(Awaitable:D $a) {
            my $handle := $a.get-await-handle;
            if $handle.already {
                $handle.success
                    ?? $handle.result
                    !! $handle.cause.rethrow
            }
            else {
                my $reawaitable := CachedAwaitHandle.new(get-await-handle => $handle);
                $!continuations := nqp::list() unless nqp::isconcrete($!continuations);
                nqp::continuationcontrol(0, ADD_WHENEVER_PROMPT, -> Mu \c {
                    nqp::push($!continuations, -> $delegate-awaiter {
                        nqp::continuationinvoke(c, {
                            $delegate-awaiter.await($reawaitable);
                        });
                    });
                });
            }
        }

        method await-all(Iterable:D \i) {
            $!continuations := nqp::list() unless nqp::isconcrete($!continuations);
            nqp::continuationcontrol(0, ADD_WHENEVER_PROMPT, -> Mu \c {
                nqp::push($!continuations, -> $delegate-awaiter {
                    nqp::continuationinvoke(c, {
                        $delegate-awaiter.await-all(i);
                    });
                });
            });
        }

        method take-all() {
            if nqp::isconcrete($!continuations) {
                my \result = $!continuations;
                $!continuations := Mu;
                result
            }
            else {
                Empty
            }
        }
    }

    class BlockState {
        has &.emit;
        has &.done;
        has &.quit;
        has @.close-phasers;
        has $.active;
        has $!lock;
        has %!active-taps;
        has $.run-async-lock;
        has $.awaiter;

        method new(:&emit!, :&done!, :&quit!) {
            self.CREATE!SET-SELF(&emit, &done, &quit)
        }

        method !SET-SELF(&emit, &done, &quit) {
            &!emit := &emit;
            &!done := &done;
            &!quit := &quit;
            $!active = 1;
            $!lock := Lock.new;
            $!run-async-lock := Lock::Async.new;
            $!awaiter := BlockAddWheneverAwaiter.CREATE;
            self
        }

        method decrement-active() {
            $!lock.protect: { --$!active }
        }

        method get-and-zero-active() {
            $!lock.protect: {
                my $result = $!active;
                $!active = 0;
                $result
            }
        }

        method add-active-tap($tap --> Nil) {
            $!lock.protect: {
                ++$!active;
                %!active-taps{nqp::objectid($tap)} = $tap;
            }
        }

        method delete-active-tap($tap --> Nil) {
            $!lock.protect: {
                %!active-taps{nqp::objectid($tap)}:delete;
            }
        }

        method teardown(--> Nil) {
            my $to-close := nqp::create(IterationBuffer);
            $!lock.protect: {
                %!active-taps.values.iterator.push-all($to-close);
                %!active-taps = ();
                $!active = 0;
            }
            my int $n = nqp::elems($to-close);
            loop (my int $i = 0; $i < $n; $i++) {
                nqp::atpos($to-close, $i).close();
            }
            my @close-phasers := @!close-phasers;
            while @close-phasers {
                @close-phasers.pop()();
            }
        }

        method run-emit(--> Nil) {
            if $!active {
                my \ex := nqp::exception();
                my $emit-handler := &!emit;
                $emit-handler(nqp::getpayload(ex)) if $emit-handler.DEFINITE;
                nqp::resume(ex)
            }
        }

        method run-done(--> Nil) {
            self.get-and-zero-active();
            self.teardown();
            my $done-handler := &!done;
            $done-handler() if $done-handler.DEFINITE;
        }

        method run-last(Tap $tap, &code --> Nil) {
            self.delete-active-tap($tap);
            self.decrement-active();
            $tap.close();
            &code.fire_if_phasers("LAST");
            $!lock.protect: {
                if $!active == 0 {
                    self.teardown();
                    my $done-handler := &!done;
                    $done-handler() if $done-handler.DEFINITE;
                }
            }
        }


        method run-catch(--> Nil) {
            my \ex = EXCEPTION(nqp::exception());
            self.get-and-zero-active();
            self.teardown();
            my $quit-handler = &!quit;
            $quit-handler(ex) if $quit-handler;
        }
    }

    class BlockTappable does Tappable {
        has &!block;

        submethod BUILD(:&!block --> Nil) { }

        method tap(&emit, &done, &quit, &tap) {
            # Create state for this tapping.
            my $state := Rakudo::Supply::BlockState.new(:&emit, :&done, :&quit);

            # Placed here so it can close over $state, but we only need to
            # closure-clone it once per Supply block, not once per whenever.
            sub add-whenever($supply, &whenever-block) {
                my $tap;
                $state.run-async-lock.with-lock-hidden-from-recursion-check: {
                    my $*AWAITER := $state.awaiter;
                    nqp::continuationreset(ADD_WHENEVER_PROMPT, {
                        $supply.tap(
                            tap => {
                                $tap := $_;
                                $state.add-active-tap($tap);
                            },
                            -> \value {
                                self!run-supply-code(&whenever-block, value, $state,
                                    &add-whenever, $tap)
                            },
                            done => {
                                $state.delete-active-tap($tap);
                                my @phasers := &whenever-block.phasers('LAST');
                                if @phasers {
                                    self!run-supply-code({ .() for @phasers }, Nil, $state,
                                        &add-whenever, $tap)
                                }
                                $tap.close;
                                self!deactivate-one($state);
                            },
                            quit => -> \ex {
                                $state.delete-active-tap($tap);
                                my $handled := False;
                                self!run-supply-code({
                                    my $phaser := &whenever-block.phasers('QUIT')[0];
                                    if $phaser.DEFINITE {
                                        $handled := $phaser(ex) === Nil;
                                    }
                                    if !$handled && $state.get-and-zero-active() {
                                        $state.quit().(ex) if $state.quit;
                                        $state.teardown();
                                    }
                                }, Nil, $state, &add-whenever, $tap);
                                if $handled {
                                    $tap.close;
                                    self!deactivate-one($state);
                                }
                            });
                    });
                }
                $tap
            }

            # Stash away any CLOSE phasers.
            if nqp::istype(&!block, Block) {
                $state.close-phasers.append(&!block.phasers('CLOSE'));
            }

            # Create and pass on tap; when closed, tear down the state and all
            # of our subscriptions.
            my $t := Tap.new(-> { $state.teardown() });
            tap($t);

            # Run the Supply block, then decrease active count afterwards (it
            # counts as an active runner).
            self!run-supply-code:
                { &!block(); self!deactivate-one-internal($state) },
                Nil, $state, &add-whenever, $t;

            # Evaluate to the Tap.
            $t
        }

        method !run-supply-code(&code, \value, BlockState $state, &add-whenever, $tap) {
            my @run-after;
            my $queued := $state.run-async-lock.protect-or-queue-on-recursion: {
                my &*ADD-WHENEVER := &add-whenever;
                $state.active > 0 and nqp::handle(code(value),
                    'EMIT', $state.run-emit(),
                    'DONE', $state.run-done(),
                    'CATCH', $state.run-catch(),
                    'LAST', $state.run-last($tap, &code),
                    'NEXT', 0);
                @run-after = $state.awaiter.take-all;
            }
            if $queued.defined {
                $queued.then({ self!run-add-whenever-awaits(@run-after) });
            }
            else {
                self!run-add-whenever-awaits(@run-after);
            }
        }

        method !run-add-whenever-awaits(@run-after --> Nil) {
            if @run-after {
                my $nested-awaiter := BlockAddWheneverAwaiter.CREATE;
                my $delegate-awaiter := $*AWAITER;
                while @run-after.elems {
                    my $*AWAITER := $nested-awaiter;
                    nqp::continuationreset(ADD_WHENEVER_PROMPT, {
                        @run-after.shift()($delegate-awaiter);
                    });
                    @run-after.append($nested-awaiter.take-all);
                }
            }
        }

        method !deactivate-one(BlockState $state) {
            $state.run-async-lock.protect-or-queue-on-recursion:
                { self!deactivate-one-internal($state) };
        }

        method !deactivate-one-internal(BlockState $state) {
            if $state.decrement-active() == 0 {
                my $done-handler := $state.done;
                $done-handler() if $done-handler;
                $state.teardown();
            }
        }

        method live(--> False) { }
        method sane(--> True) { }
        method serial(--> True) { }
    }

    class OneWheneverState {
        has &.emit;
        has &.done;
        has &.quit;
        has @.close-phasers;
        has $.tap is rw;
        has $.active;

        method new(:&emit!, :&done!, :&quit!) {
            self.CREATE!SET-SELF(&emit, &done, &quit)
        }

        method !SET-SELF(&emit, &done, &quit) {
            &!emit := &emit;
            &!done := &done;
            &!quit := &quit;
            $!active = 1;
            self
        }

        method teardown(--> Nil) {
            $!active = 0;
            $!tap.close if $!tap;
            my @close-phasers := @!close-phasers;
            while @close-phasers {
                @close-phasers.pop()();
            }
        }

        method run-emit(--> Nil) {
            if $!active {
                my \ex := nqp::exception();
                my $emit-handler := &!emit;
                $emit-handler(nqp::getpayload(ex)) if $emit-handler.DEFINITE;
                nqp::resume(ex)
            }
        }

        method run-done(--> Nil) {
            if $!active {
                self.teardown();
                my $done-handler := &!done;
                $done-handler() if $done-handler.DEFINITE;
            }
        }

        method run-last(&code, --> Nil) {
            &code.fire_if_phasers("LAST");
            self.run-done;
        }

        method run-catch(--> Nil) {
            if $!active {
                my \ex = EXCEPTION(nqp::exception());
                self.teardown();
                my $quit-handler = &!quit;
                $quit-handler(ex) if $quit-handler;
            }
        }
    }

    class OneWheneverTappable does Tappable {
        has &!block;

        submethod BUILD(:&!block --> Nil) { }

        method tap(&emit, &done, &quit, &tap) {
            # Create state for this tapping.
            my $state := Rakudo::Supply::OneWheneverState.new(:&emit, :&done, :&quit);

            # We only expcet one whenever; detect getting a second and complain.
            my $*WHENEVER-SUPPLY-TO-ADD := Nil;
            my &*WHENEVER-BLOCK-TO-ADD := Nil;
            sub add-whenever(\the-supply, \the-whenever-block) {
                if $*WHENEVER-SUPPLY-TO-ADD =:= Nil {
                    $*WHENEVER-SUPPLY-TO-ADD := the-supply;
                    &*WHENEVER-BLOCK-TO-ADD := the-whenever-block;
                }
                else {
                    die "Single whenever block special case tried to add second whenever";
                }
            }

            # Stash away any CLOSE phasers.
            if nqp::istype(&!block, Block) {
                $state.close-phasers.append(&!block.phasers('CLOSE'));
            }

            # Create and pass on tap; when closed, tear down the state and all
            # of our subscriptions.
            my $t := Tap.new(-> { $state.teardown() });
            tap($t);

            # Run the Supply block. Only proceed if it didn't send done/quit.
            self!run-supply-code: { &!block() }, Nil, $state, &add-whenever;
            if $state.active {
                # If we didn't get a whenever, something is badly wrong.
                if $*WHENEVER-SUPPLY-TO-ADD =:= Nil {
                    die "Single whenever block special case did not get a whenever block";
                }

                # Otherwise, we can now tap that whenever block. Since it is the
                # only one, and we know from compile-time analysis it is the last
                # thing in the block, then it's safe to do it now the block is
                # completed and without any concurrency control. However, we do
                # call .sanitize just in case, to ensure that we have a serial and
                # protocol-following Supply. That is enough.
                my $supply := $*WHENEVER-SUPPLY-TO-ADD.sanitize;
                my &whenever-block := &*WHENEVER-BLOCK-TO-ADD;
                my $tap;
                $supply.tap(
                    tap => {
                        $tap := $_;
                        $state.tap = $tap;
                    },
                    -> \value {
                        self!run-supply-code(&whenever-block, value, $state,
                            &add-whenever)
                    },
                    done => {
                        my @phasers := &whenever-block.phasers('LAST');
                        if @phasers {
                            self!run-supply-code({ .() for @phasers }, Nil, $state,
                                &add-whenever)
                        }
                        $tap.close;
                        $state.run-done();
                    },
                    quit => -> \ex {
                        my $handled := False;
                        self!run-supply-code({
                            my $phaser := &whenever-block.phasers('QUIT')[0];
                            if $phaser.DEFINITE {
                                $handled := $phaser(ex) === Nil;
                            }
                            if !$handled {
                                $state.quit().(ex) if $state.quit;
                                $state.teardown();
                            }
                        }, Nil, $state, &add-whenever);
                        if $handled {
                            $tap.close;
                            $state.run-done();
                        }
                    });
            }

            # Evaluate to the Tap.
            $t
        }

        method !run-supply-code(&code, \value, OneWheneverState $state, &add-whenever) {
            my &*ADD-WHENEVER := &add-whenever;
            {
                $state.active > 0 and nqp::handle(code(value),
                    'EMIT', $state.run-emit(),
                    'DONE', $state.run-done(),
                    'CATCH', $state.run-catch(),
                    'LAST', $state.run-last(&code),
                    'NEXT', 0);
            }(); # XXX Workaround for optimizer bug
        }

        method live(--> False) { }
        method sane(--> True) { }
        method serial(--> True) { }
    }

    class OneEmitTappable does Tappable {
        has &!block;

        submethod BUILD(:&!block! --> Nil) {}

        method tap(&emit, &done, &quit, &tap) {
            my $t := Tap.new;
            tap($t);
            try {
                emit(&!block());
                done();
                CATCH {
                    default {
                        quit($_);
                    }
                }
            }
            $t
        }

        method live(--> False) { }
        method sane(--> True) { }
        method serial(--> True) { }
    }
}

sub SUPPLY(&block) is implementation-detail {
    Supply.new(Rakudo::Supply::BlockTappable.new(:&block))
}

sub WHENEVER(Supply() $supply, &block) is implementation-detail {
    my \adder := nqp::getlexdyn('&*ADD-WHENEVER');
    nqp::isnull(adder)
        ?? X::WheneverOutOfScope.new.throw
        !! adder.($supply, &block)
}

sub REACT(&block) is implementation-detail {
    my $s := SUPPLY(&block);
    my $p := Promise.new;
    $s.tap(
        { warn "Useless use of emit in react" },
        done => { $p.keep(Nil) },
        quit => { $p.break($_) });
    await $p;
}

sub SUPPLY-ONE-EMIT(&block) is implementation-detail {
    Supply.new(Rakudo::Supply::OneEmitTappable.new(:&block))
}

sub SUPPLY-ONE-WHENEVER(&block) is implementation-detail {
    Supply.new(Rakudo::Supply::OneWheneverTappable.new(:&block))
}

sub REACT-ONE-WHENEVER(&block) is implementation-detail {
    my $s := SUPPLY-ONE-WHENEVER(&block);
    my $p := Promise.new;
    $s.tap(
        { warn "Useless use of emit in react" },
        done => { $p.keep(Nil) },
        quit => { $p.break($_) });
    await $p;
}

# vim: expandtab shiftwidth=4
