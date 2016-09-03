# A HyperWorkBuffer represents a chunk of work to be processed as part of a
# parallelized operation (either thanks to hyper or race). It carries a
# sequence number, and input buffer (items to process), and an output buffer
# (results of processing them).
my class HyperWorkBuffer {
    has int $.sequence-number is rw;
    has $.input;
    has $.output;

    method new() {
        my \wb = nqp::create(self);
        nqp::bindattr(wb, HyperWorkBuffer, '$!input', nqp::create(IterationBuffer));
        nqp::bindattr(wb, HyperWorkBuffer, '$!output', nqp::create(IterationBuffer));
        wb
    }

    # Clears both buffers.
    method clear() {
        nqp::setelems($!input, 0);
        nqp::setelems($!output, 0);
        Nil
    }

    # Swaps around the input/output buffers, and clears the output buffer.
    # (This is used between pipelined stages, where the next stage will
    # use the items in the first.)
    method swap() {
        my $new-input := $!output;
        $!output := $!input;
        $!input := $new-input;
        nqp::setelems($!output, 0);
        Nil
    }

    # Gets an iterator of the input.
    method input-iterator() {
        class :: does Iterator {
            has $!buffer;
            has int $!i;

            method new(\buffer) {
                nqp::p6bindattrinvres(
                  nqp::create(self),self,'$!buffer',buffer
                )
            }

            method pull-one() {
                my int $i = $!i;
                if $i < nqp::elems($!buffer) {
                    $!i = $i + 1;
                    nqp::atpos($!buffer, $i)
                }
                else {
                    IterationEnd
                }
            }
        }.new($!input)
    }
}

# vim: ft=perl6 expandtab sw=4
