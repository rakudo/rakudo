# Waits for a promise to be kept or a channel to be able to receive a value
# and, once it can, unwraps or returns the result. This should be made more
# efficient by using continuations to suspend any task running in the thread
# pool that blocks; for now, this cheat gets the basic idea in place.

proto sub await(|) { * }
multi sub await() {
    die "Must specify a Promise or Channel to await on (got an empty list)";
}
multi sub await(Any:U $x) {
    die "Must specify a defined Promise, Channel, or Supply to await on (got an undefined $x.^name())";
}
multi sub await(Any:D $x) {
    die "Must specify a Promise, Channel, or Supply to await on (got a $x.^name())";
}
multi sub await(Iterable:D $i) { $i.eager.map({ await $_ }) }
multi sub await(Promise:D $p)  { $p.result }
multi sub await(Channel:D $c)  { $c.receive }
multi sub await(Supply:D $s)   { $s.wait }
multi sub await(*@awaitables)  { @awaitables.eager.map({await $_}) }

sub awaiterator(@promises) {
    Seq.new(class :: does Iterator {
        has @!todo;
        has @!done;
        method !SET-SELF(\todo) { @!todo = todo; self }
        method new(\todo) { nqp::create(self)!SET-SELF(todo) }
        method pull-one() is raw {
            if @!done {
                @!done.shift
            }
            elsif @!todo {
                Promise.anyof(@!todo).result;
                my @next;
                .status == Planned
                  ?? @next.push($_)
                  !! @!done.push($_.result)
                    for @!todo;
                @!todo := @next;
                @!done.shift
            }
            else {
                IterationEnd
            }
        }
        method sink-all(--> IterationEnd) { Promise.allof(@promises).result }
    }.new(@promises))
}

# vim: ft=perl6 expandtab sw=4
