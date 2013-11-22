# Various generators and combinators are provided by Publish.

my class Publish {
    method for(*@values, :$scheduler = $*SCHEDULER) {
        my class ForSupply does Supply {
            has @!values;
            has $!scheduler;

            submethod BUILD(:@!values, :$!scheduler) {}

            method tap(|c) {
                my $sub = self.Supply::tap(|c);
                $!scheduler.cue(
                    {
                        for @!values -> \val {
                            $sub.more().(val);
                        }
                        if $sub.done -> $l { $l() }
                    },
                    :catch(-> $ex { if $sub.quit -> $t { $t($ex) } })
                );
                $sub
            }
        }
        ForSupply.new(:@values, :$scheduler)
    }

    method interval($interval, $delay = 0, :$scheduler = $*SCHEDULER) {
        my class IntervalSupply does Supply {
            has $!scheduler;
            has $!interval;
            has $!delay;

            submethod BUILD(:$!scheduler, :$!interval, :$!delay) {}

            method tap(|c) {
                my $sub = self.Supply::tap(|c);
                $!scheduler.cue(
                    {
                        state $i = 0;
                        $sub.more().($i++);
                    },
                    :every($!interval), :in($!delay)
                );
                $sub
            }
        }
        IntervalSupply.new(:$interval, :$delay, :$scheduler)
    }
}
