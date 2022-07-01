augment class Rakudo::Iterator {
    # Return an iterator that generates Lists of a given iterator
    # and one or Callables with conditions.  This is basically the
    # Haskell "span" functionality.
    my class Span does Iterator {
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
                    my &test := nqp::shift($!tests);
                    nqp::until(
                      nqp::eqaddr(($pulled := $iterator.pull-one),IterationEnd)
                        || nqp::isfalse(test($pulled)),
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

    proto method Span(|) {*}
    multi method Span(&test,  $iterator) { Span.new: (&test,), $iterator }
    multi method Span(@tests, $iterator) { Span.new: @tests,   $iterator }
}

# vim: expandtab shiftwidth=4
