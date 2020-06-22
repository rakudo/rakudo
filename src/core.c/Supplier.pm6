# A Supplier is a convenient way to create a live Supply. The publisher can
# be used to emit/done/quit. The Supply objects obtained from it will tap into
# the same live Supply.
my class Supplier {
    my class TapList does Tappable {
        my class TapListEntry {
            has &.emit;
            has &.done;
            has &.quit;
        }

        # Lock serializes updates to tappers.
        has Lock $!lock = Lock.new;

        # An immutable list of tappers. Always replaced on change, never
        # mutated in-place ==> thread safe together with lock (and only
        # need lock on modification).
        has Mu $!tappers;

        method tap(&emit, &done, &quit, &tap) {
            my $tle := TapListEntry.new(:&emit, :&done, :&quit);
            # Since we run `tap` before adding, there's a small chance of
            # a tap removal attempt happening for the add attempt. We use
            # these two flags to handle that case. This is safe since we
            # only ever access them under lock.
            my $added := False;
            my $removed := False;
            my $t = Tap.new({
                $!lock.protect({
                    if $added {
                        my Mu $update := nqp::list();
                        for nqp::hllize($!tappers) -> \entry {
                            nqp::push($update, entry) unless entry =:= $tle;
                        }
                        $!tappers := $update;
                    }
                    $removed := True;
                });
            });
            tap($t);
            $!lock.protect({
                unless $removed {
                    my Mu $update := nqp::isconcrete($!tappers)
                        ?? nqp::clone($!tappers)
                        !! nqp::list();
                    nqp::push($update, $tle);
                    $!tappers := $update;
                }
                $added := True;
            });
            $t
        }

        method emit(Mu \value --> Nil) {
            nqp::if(
              nqp::isconcrete(my $snapshot := $!tappers)
                && (my int $n = nqp::elems($snapshot)),
              nqp::if(                                 # at least one tap
                nqp::isgt_i($n,1),
                nqp::stmts(                            # multiple taps
                  (my int $i = -1),
                  nqp::while(
                    nqp::islt_i(($i = nqp::add_i($i,1)),$n),
                    nqp::atpos($snapshot,$i).emit()(value)
                  )
                ),
                nqp::atpos($snapshot,0).emit()(value)  # only one tap
              )
            )
        }

        method done(--> Nil) {
            my $snapshot := $!tappers;
            if nqp::isconcrete($snapshot) {
                my int $n = nqp::elems($snapshot);
                loop (my int $i = 0; $i < $n; $i = $i + 1) {
                    nqp::atpos($snapshot, $i).done()();
                }
            }
        }

        method quit($ex --> Nil) {
            my $snapshot := $!tappers;
            if nqp::isconcrete($snapshot) {
                my int $n = nqp::elems($snapshot);
                loop (my int $i = 0; $i < $n; $i = $i + 1) {
                    nqp::atpos($snapshot, $i).quit()($ex);
                }
            }
        }

        method live(--> True) { }
        method serial(--> False) { }
        method sane(--> False)  { }
    }

    has $!taplist;

    method new() {
        self.bless(taplist => TapList.new)
    }
    submethod BUILD(:$!taplist! --> Nil) { }

    method emit(Supplier:D: Mu \value --> Nil) {
        $!taplist.emit(value);
    }

    method done(Supplier:D: --> Nil) {
        $!taplist.done();
    }

    proto method quit($) {*}
    multi method quit(Supplier:D: Exception $ex) {
        $!taplist.quit($ex);
    }
    multi method quit(Supplier:D: Str() $message) {
        $!taplist.quit(X::AdHoc.new(payload => $message));
    }

    method Supply(Supplier:D:) {
        Supply.new($!taplist).sanitize
    }

    method unsanitized-supply(Supplier:D:) {
        Supply.new($!taplist)
    }
}

# A preserving supplier holds on to emitted values and state when nobody is
# tapping. As soon as there a tap is made, any preserved events will be
# immediately sent to that tapper.
my class Supplier::Preserving is Supplier {
    my class PreservingTapList does Tappable {
        my class TapListEntry {
            has &.emit;
            has &.done;
            has &.quit;
        }

        # Lock serializes updates to tappers.
        has Lock $!lock = Lock.new;

        # An immutable list of tappers. Always replaced on change, never
        # mutated in-place ==> thread safe together with lock (and only
        # need lock on modification).
        has Mu $!tappers;

        # Events to reply, whether the replay was done, and a lock to protect
        # updates to these.
        has @!replay;
        has int $!replay-done;
        has $!replay-lock = Lock.new;

        method tap(&emit, &done, &quit, &tap) {
            my $tle := TapListEntry.new(:&emit, :&done, :&quit);
            my int $replay = 0;
            # Since we run `tap` before adding, there's a small chance of
            # a tap removal attempt happening for the add attempt. We use
            # these two flags to handle that case. This is safe since we
            # only ever access them under lock.
            my $added := False;
            my $removed := False;
            my $t = Tap.new({
                $!lock.protect({
                    if $added {
                        my Mu $update := nqp::list();
                        for nqp::hllize($!tappers) -> \entry {
                            nqp::push($update, entry) unless entry =:= $tle;
                        }
                        $!replay-done = 0 if nqp::elems($update) == 0;
                        $!tappers := $update;
                    }
                    $removed := True;
                });
            });
            tap($t);
            $!lock.protect({
                unless $removed {
                    my Mu $update := nqp::isconcrete($!tappers)
                        ?? nqp::clone($!tappers)
                        !! nqp::list();
                    nqp::push($update, $tle);
                    $replay = 1 if nqp::elems($update) == 1;
                    self!replay($tle) if $replay;
                    $!tappers := $update;
                }
                $added := True;
            });
            $t
        }

        method emit(\value --> Nil) {
            loop {
                my int $sent = 0;
                my $snapshot := $!tappers;
                if nqp::isconcrete($snapshot) {
                    $sent = nqp::elems($snapshot);
                    loop (my int $i = 0; $i < $sent; $i = $i + 1) {
                        nqp::atpos($snapshot, $i).emit()(value);
                    }
                }
                return if $sent;
                return if self!add-replay({ $_.emit()(value) });
            }
        }

        method done(--> Nil) {
            loop {
                my int $sent = 0;
                my $snapshot := $!tappers;
                if nqp::isconcrete($snapshot) {
                    $sent = nqp::elems($snapshot);
                    loop (my int $i = 0; $i < $sent; $i = $i + 1) {
                        nqp::atpos($snapshot, $i).done()();
                    }
                }
                return if $sent;
                return if self!add-replay({ $_.done()() });
            }
        }

        method quit($ex --> Nil) {
            loop {
                my int $sent = 0;
                my $snapshot := $!tappers;
                if nqp::isconcrete($snapshot) {
                    $sent = nqp::elems($snapshot);
                    loop (my int $i = 0; $i < $sent; $i = $i + 1) {
                        nqp::atpos($snapshot, $i).quit()($ex);
                    }
                }
                return if $sent;
                return if self!add-replay({ $_.quit()($ex) });
            }
        }

        method !add-replay(&replay --> Bool) {
            $!replay-lock.protect: {
                if $!replay-done {
                    False
                }
                else {
                    @!replay.push(&replay);
                    True
                }
            }
        }

        method !replay($tle) {
            $!replay-lock.protect: {
                while @!replay.shift -> $rep {
                    $rep($tle);
                }
                $!replay-done = 1;
            }
        }

        method live(--> True) { }
        method serial(--> False) { }
        method sane(--> False) { }
    }

    method new() {
        self.bless(taplist => PreservingTapList.new)
    }
}

# vim: expandtab shiftwidth=4
