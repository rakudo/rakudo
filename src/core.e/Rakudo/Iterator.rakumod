augment class Rakudo::Iterator {
    # Return an iterator that generates Lists of a given iterator
    # and one or Callables with conditions.  This is basically the
    # Haskell "span" functionality.
    my class Snip does Iterator {
        has $!tests;   # Iterator producing smartmatch targets
        has $!values;  # Iterator producing values
        has $!next;    # next value to produce between pull-one calls

        method !SET-SELF($!tests, $!values) {
            $!next := $!values.pull-one;
            self
        }

        method new($tests, $values) {
            nqp::create(self)!SET-SELF($tests, $values)
        }

        method pull-one() {
            if nqp::eqaddr($!next,IterationEnd) {
                IterationEnd
            }
            else {
                my $buffer := nqp::create(IterationBuffer);
                nqp::push($buffer,$!next);

                my $values := $!values;
                my $test;
                my $pulled;

                # no more tests, produce the rest
                if nqp::eqaddr(($test := $!tests.pull-one),IterationEnd) {
                    $!next := IterationEnd;
                    nqp::bindattr((my \result := nqp::create(List)),
                      List,'$!reified',$buffer);
                    nqp::bindattr((my \todo := nqp::create(List::Reifier)),
                      List::Reifier,'$!reified',$buffer);
                    nqp::bindattr(todo,
                      List::Reifier,'$!current-iter',$values);
                    nqp::bindattr(todo,
                      List::Reifier,'$!reification-target',$buffer);
                    nqp::p6bindattrinvres(result,List,'$!todo',todo)
                }

                # produce matching this test
                else {
                    nqp::until(
                      nqp::eqaddr(($pulled := $values.pull-one),IterationEnd)
                        || nqp::isfalse($test.ACCEPTS($pulled)),
                      nqp::push($buffer,$pulled)
                    );
                    $!next := $pulled;
                    $buffer.List
                }
            }
        }
    }

    proto method Snip(|) {*}
    multi method Snip(Iterator:D $tests, Iterator:D $values) {
        Snip.new: $tests, $values
    }

    # Return an iterator that will skip / produce values as specified by
    # another iterator.
    my class Skipper does Iterator {
        has $!values;
        has $!skips;
        has uint $!produce;

        method !SET-SELF($values, $skips) {
            if nqp::eqaddr((my $produce := $skips.pull-one),IterationEnd) {
                $values  # nothing to skip
            }
            else {
                $!values := $values;
                $!skips  := $skips;
                $!produce = $produce;
                self
            }
        }

        method new($values, $skips) {
            nqp::create(self)!SET-SELF($values, $skips)
        }

        method pull-one() {

            # Still something to produce, so produce
            if $!produce {
                --$!produce;
                $!values.pull-one
            }

            # Skip rest if no skipper or *
            elsif nqp::eqaddr((my $skipper := $!skips.pull-one),IterationEnd)
              || nqp::istype($skipper,Whatever) {
                IterationEnd
            }

            # Skip a number
            else {
                my uint $to-skip = $skipper;
                nqp::until(
                  nqp::isle_i($to-skip,0)
                    || nqp::eqaddr($!values.pull-one,IterationEnd),
                  --$to-skip
                );

                # not enough values to skip, so we're done
                if $to-skip {
                    IterationEnd
                }

                # find out how many values to produce next
                else {
                    $!produce = nqp::eqaddr(
                      (my $produce := $!skips.pull-one),
                      IterationEnd
                    ) || nqp::istype($produce,Whatever)
                      ?? -1  # produce the rest
                      !! $produce - 1;
                    $!values.pull-one
                }
            }
        }
    }

    proto method Skipper(|) {*}
    multi method Skipper(\values,  \skips) { Skipper.new: values, skips }
}

# vim: expandtab shiftwidth=4
