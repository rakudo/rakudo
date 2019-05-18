# Rakudo::Internals.EVALUATE-FEED takes a source and a list of stages, which
# are callbacks that make routine calls, and runs them in parallel. In the
# following code:
#
# my @squares <== map { $_ ** 2 } <== 1...*;
#
# 1...* is the source and stages includes just one callback, which calls
# `map { $_ ** 2 }, $source`. The result is then stored in @squares from
# Perl6::Actions.

augment class Rakudo::Internals {
    my class Pipeline is repr('ConcBlockingQueue') {}

    method EVALUATE-FEED(Mu $source is raw, *@stages) {
        my Pipeline $pipeline := nqp::create(Pipeline);
        nqp::push($pipeline, $source);
        await @stages.map(-> &stage {
            start {
                my Mu $input  := nqp::shift($pipeline);
                my Mu $output := &stage($input);
                nqp::push($pipeline, $output);
            }
        });
        nqp::shift($pipeline)
    }
}
