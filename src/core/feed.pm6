# Rakudo::Internals.EVALUATE-FEED takes a source and a list of stages, which
# are callbacks that make routine calls, and runs them in parallel. In the
# following code:
#
# my @squares <== map { $_ ** 2 } <== 1...*;
#
# 1...* is the source and stages includes just one callback, which calls
# `map { $_ ** 2 }, $source`. The result is then stored in @squares from
# Perl6::Actions (by &make_feed and &make_feed_result specifically).

augment class Rakudo::Internals {
    my class Pipeline is repr('ConcBlockingQueue') {}

    method EVALUATE-FEED(Mu $source is raw, *@stages) {
        my Pipeline $pipeline := nqp::create(Pipeline);
        nqp::push($pipeline, $source);

        await @stages.map(-> &stage {
            start {
                my Mu $input  := nqp::shift($pipeline);
                my Mu $output := &stage($input);

                # If $input does Sequence and the current stage returns the
                # same Sequence instance, its iterator will be consumed and an
                # exception will be thrown on the call to the next stage. The
                # solution to this is to push $output's cache to the pipeline
                # rather than $output itself when this is the case.
                nqp::if(
                  nqp::istype($output, Sequence),
                  ($output := $output.cache)
                );

                nqp::push($pipeline, $output);
            }
        });

        nqp::shift($pipeline)
    }
}
