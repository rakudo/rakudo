# Various generators and combinators are provided by Publish.
my class Publish {
    method for(*@values, :$scheduler = $*SCHEDULER) {
        my class ForSubscribable does Subscribable {
            has @!values;
            has $!scheduler;

            submethod BUILD(:@!values, :$!scheduler) {}

            method subscribe(|c) {
                my $sub = self.Subscribable::subscribe(|c);
                $!scheduler.schedule_with_catch(
                    {
                        for @!values -> \val {
                            $sub.next().(val);
                        }
                        if $sub.last -> $l { $l() }
                    },
                    -> $ex { if $sub.fail -> $t { $t($ex) } }
                );
                $sub
            }
        }
        ForSubscribable.new(:@values, :$scheduler)
    }

    method interval($interval, $delay = 0, :$scheduler = $*SCHEDULER) {
        my class IntervalSubscribable does Subscribable {
            has $!scheduler;
            has $!interval;
            has $!delay;

            submethod BUILD(:$!scheduler, :$!interval, :$!delay) {}

            method subscribe(|c) {
                my $sub = self.Subscribable::subscribe(|c);
                $!scheduler.schedule_every(
                    {
                        state $i = 0;
                        $sub.next().($i++);
                    },
                    $!interval, $!delay
                );
                $sub
            }
        }
        IntervalSubscribable.new(:$interval, :$delay, :$scheduler)
    }
}
