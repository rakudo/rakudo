augment class Rakudo::Iterator {
    # Return an iterator that generates Lists of a given iterator
    # and one or Callables with conditions.  This is basically the
    # Haskell "span" functionality.
    my class Snip does Iterator {
        has $!tests;
        has $!iterator;
        has $!next;
        
        method !SET-SELF(@tests, $iterator) {
            $!tests    := nqp::getattr(@tests,List,'$!reified');
            $!iterator := $iterator;
            $!next     := $iterator.pull-one;
            self
        }
        
        method new(@tests, $iterator) {
            nqp::create(self)!SET-SELF(@tests, $iterator)
        }
        
        method pull-one() {
            if nqp::eqaddr($!next,IterationEnd) {
                IterationEnd
            }
            else { 
                my $buffer := nqp::create(IterationBuffer);
                nqp::push($buffer,$!next);
                
                my $iterator := $!iterator;
                my $pulled;
                
                if nqp::elems($!tests) {
                    my $test := nqp::shift($!tests);
                    nqp::until(
                      nqp::eqaddr(($pulled := $iterator.pull-one),IterationEnd)
                        || nqp::isfalse($test.ACCEPTS($pulled)),
                      nqp::push($buffer,$pulled)
                    );
                
                }
                else {
                    nqp::until(
                      nqp::eqaddr(($pulled := $iterator.pull-one),IterationEnd),
                      nqp::push($buffer,$pulled)
                    );
                }
                
                $!next := $pulled;
                $buffer.List
            }
        }
    }

    proto method Snip(|) {*}
    multi method Snip(\test,  $iterator) { Snip.new: (test,), $iterator }
    multi method Snip(@tests, $iterator) { Snip.new: @tests,  $iterator }

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
