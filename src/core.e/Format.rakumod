#-------------------------------------------------------------------------------
# The class underlying the format string quote q:o/:format type.  An instance
# contains a Callable that will be called with the CALL-ME method.
# Otherwise acts as a normal string.

my class Format is Str is Callable {
    has str @!directives;
    has     &!code;

    method new(Str:D $format, :$class = Formatter) {
        my @*DIRECTIVES := my str @;
        my &code := $class.new($format);

        my $obj := nqp::create(self);
        nqp::bindattr_s($obj,Str,'$!value',$format);
        nqp::bindattr($obj,Format,'@!directives',@*DIRECTIVES);
        nqp::bindattr($obj,Format,'&!code',&code);
        $obj
    }

    method directives(Format:D:) { @!directives.List      }
    method Callable(Format:D:)   { &!code                 }
    method arity(Format:D:)      { &!code.signature.arity }
    method count(Format:D:)      { &!code.signature.count }

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
        if self.arity -> int $arity {
            my str @parts;
            if $arity == 1 {
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
                    my int $todo = $arity + 1;
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
                      ?? $found == $arity
                        ?? $buffer.List
                        !! self!throw-arity(nqp::elems($buffer), $arity)
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
              !! self!throw-arity(0, 1)
        }
    }

    # helper for throwing
    method !throw-arity($args-have, $args-used) is hidden-from-backtrace {
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
    $format(|@args)
}

proto sub zprintf($, |) {*}
multi sub zprintf(Str(Cool) $format, \value) {
    Formatter.new($format)(value)
}
multi sub zprintf(Str(Cool) $format, |c) {
    Formatter.new($format)(|c)
}

augment class Cool {
    method zprintf(|c) { zprintf(self,|c) }
}

# vim: expandtab shiftwidth=4
