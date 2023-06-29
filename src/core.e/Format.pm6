#-------------------------------------------------------------------------------
# The class underlying the format string quote q:o/:format type.  An instance
# contains a Callable that will be called with the CALL-ME method.
# Otherwise acts as a normal string.

my class Format is Str {
    has str @.directives;
    has     &.code;

    method new(Str:D $format, :$class = Formatter) {
        my @*DIRECTIVES := my str @;
        my &code := $class.new($format);

        my $obj := nqp::create(self);
        nqp::bindattr_s($obj,Str,'$!value',$format);
        nqp::bindattr($obj,Format,'@!directives',@*DIRECTIVES);
        nqp::bindattr($obj,Format,'&!code',&code);
        $obj
    }

    method signature() { &!code.signature       }
    method arity()     { &!code.signature.arity }
    method count()     { &!code.signature.count }

    method CALL-ME(|c) {
        CATCH {
            die ("Error occurred during processing format '"
              ~ self
              ~ "' with value"
              ~ (c == 1 ?? " " !! "s ")
              ~ c.raku.substr(1)
              ~ ":"
            ).naive-word-wrapper
              ~ "\n\n"
              ~ .message.indent(4)
        }

        &!code(|c)
    }

    # Helper method for .fmt support
    method handle-iterator(Format:D:
      Iterator:D $iterator, $separator = "\n"
    --> Str:D) is implementation-detail {
        my &handler  := &!code;

        # at least one arg required for format
        if self.count -> int $count {
            my str @parts;
            if $count == 1 {
                nqp::until(
                  nqp::eqaddr((my $pulled := $iterator.pull-one),IterationEnd),
                  nqp::push_s(@parts,handler($pulled))
                );
            }

            # 2 or more args required
            else {

                # collect values for args, throw if insufficient
                my sub next-batch() {
                    my $buffer  := nqp::create(IterationBuffer);
                    my int $todo = $count + 1;
                    nqp::while(
                      --$todo,
                      nqp::if(
                        nqp::eqaddr(
                          (my $pulled := $iterator.pull-one),
                          IterationEnd
                        ),
                        ($todo = 1),
                        nqp::push($buffer,$pulled)
                      )
                    );
                    
                    my int $found = nqp::elems($buffer);
                    $found
                      ?? $found == $count
                        ?? $buffer.List
                        !! self!throw-count(nqp::elems($buffer), $count)
                      !! Nil
                }

                nqp::while(
                  (my $params := next-batch),
                  nqp::push_s(@parts,handler(|$params))
                );
            }

            @parts.join($separator);
        }

        # no args, throw if any values
        else {
            nqp::eqaddr($iterator.pull-one,IterationEnd)
              ?? ''
              !! self!throw-count(0, 1)
        }
    }

    # helper for throwing
    method !throw-count($args-have, $args-used) is hidden-from-backtrace {
        X::Str::Sprintf::Directives::Count.new(
          :$args-have, :$args-used, :format(self)
        ).throw
    }

    multi method raku(Format:D:) { 'Format.new(' ~ self.Str::raku ~ ')' }

    # mostly for debugging, but also for consistency
    method AST(Format:D:) { Formatter.AST: self }
}

#-------------------------------------------------------------------------------
# Subroutine access to Formatter logic

# the procedural frontend of sprintf functionality
multi sub sprintf(Format:D $format, *@args) {  # until zprintf gone
    $format(@args)
}

proto sub zprintf($, |) {*}
multi sub zprintf(Str(Cool) $format, \value) {
    Formatter.new($format)(nqp::list(value))
}
multi sub zprintf(Str(Cool) $format, @args) {
    Formatter.new($format)(@args)
}
multi sub zprintf(Str(Cool) $format, *@args) {
    Formatter.new($format)(@args)
}
multi sub zprintf(Format:D $format, *@args) {
    $format(@args)
}

augment class Cool {
    method zprintf(*@args) { zprintf(self, @args) }
}

# vim: expandtab shiftwidth=4
