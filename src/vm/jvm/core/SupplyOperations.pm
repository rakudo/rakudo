# Operations we can do on Subscribables. Note, many of them need to compose
# the Subscribable role into classes they create along the way, so they must
# be declared outside of Subscribable.
my class SubscribableOperations is repr('Uninstantiable') {
    # Private versions of the methods to relay events to subscribers, used in
    # implementing various operations.
    my role PrivatePublishing {
        method !next(\msg) {
            for self.subscriptions {
                .next().(msg)
            }
            Nil;
        }

        method !last() {
            for self.subscriptions {
                if .last { .last().() }
            }
            Nil;
        }

        method !fail($ex) {
            for self.subscriptions {
                if .fail { .fail().($ex) }
            }
            Nil;
        }
    }
    
    method do($a, &side_effect) {
        on -> $res {
            $a => sub (\val) { side_effect(val); $res.next(val) }
        }
    }
    
    method grep(Subscribable $a, &filter) {
        my class GrepSubscribable does Subscribable does PrivatePublishing {
            has $!source;
            has &!filter;
            
            submethod BUILD(:$!source, :&!filter) { }
            
            method subscribe(|c) {
                my $sub = self.Subscribable::subscribe(|c);
                my $ssn = $!source.subscribe(
                    -> \val {
                        if (&!filter(val)) { self!next(val) }
                    },
                    { self!last(); },
                    -> $ex { self!fail($ex) }
                );
                $sub
            }
        }
        GrepSubscribable.new(:source($a), :&filter)
    }
    
    method map(Subscribable $a, &mapper) {
        my class MapSubscribable does Subscribable does PrivatePublishing {
            has $!source;
            has &!mapper;
            
            submethod BUILD(:$!source, :&!mapper) { }
            
            method subscribe(|c) {
                my $sub = self.Subscribable::subscribe(|c);
                my $ssn = $!source.subscribe(
                    -> \val {
                        self!next(&!mapper(val))
                    },
                    { self!last(); },
                    -> $ex { self!fail($ex) }
                );
                $sub
            }
        }
        MapSubscribable.new(:source($a), :&mapper)
    }
    
    method merge(Subscribable $a, Subscribable $b) {
        my $lasts = 0;
        on -> $res {
            $a => {
                next => sub ($val) { $res.next($val) },
                last => {
                    $res.last() if ++$lasts == 2;
                }
            },
            $b => {
                next => sub ($val) { $res.next($val) },
                last => {
                    $res.last() if ++$lasts == 2;
                }
            }
        }
    }
    
    method zip(Subscribable $a, Subscribable $b, &with = &infix:<,>) {
        my @as;
        my @bs;
        on -> $res {
            $a => sub ($val) {
                @as.push($val);
                if @as && @bs {
                    $res.next(with(@as.shift, @bs.shift));
                }
            },
            $b => sub ($val) {
                @bs.push($val);
                if @as && @bs {
                    $res.next(with(@as.shift, @bs.shift));
                }
            }
        }
    }
}
