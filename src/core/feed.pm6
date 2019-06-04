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
    method EVALUATE-FEED(Mu $source is raw, *@stages) {
        my Channel $pipeline .= new;
        $pipeline.send: $source;

        await @stages.map(-> &stage {
            start {
                my Mu $input  := $pipeline.receive;
                my Mu $output := &stage($input);
                $pipeline.send: $output;
            }
        });

        my Mu $result := $pipeline.receive;
        $pipeline.close;
        $result
    }
}
