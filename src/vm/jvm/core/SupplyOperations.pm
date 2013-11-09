# Operations we can do on Supplies. Note, many of them need to compose
# the Supply role into classes they create along the way, so they must
# be declared outside of Supply.

my class SupplyOperations is repr('Uninstantiable') {
    # Private versions of the methods to relay events to subscribers, used in
    # implementing various operations.
    my role PrivatePublishing {
        method !next(\msg) {
            for self.tappers {
                .next().(msg)
            }
            Nil;
        }

        method !last() {
            for self.tappers {
                if .last { .last().() }
            }
            Nil;
        }

        method !fail($ex) {
            for self.tappers {
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
    
    method grep(Supply $a, &filter) {
        my class GrepSupply does Supply does PrivatePublishing {
            has $!source;
            has &!filter;
            
            submethod BUILD(:$!source, :&!filter) { }
            
            method tap(|c) {
                my $sub = self.Supply::tap(|c);
                my $tap = $!source.tap(
                    -> \val {
                        if (&!filter(val)) { self!next(val) }
                    },
                    { self!last(); },
                    -> $ex { self!fail($ex) }
                );
                $sub
            }
        }
        GrepSupply.new(:source($a), :&filter)
    }
    
    method map(Supply $a, &mapper) {
        my class MapSupply does Supply does PrivatePublishing {
            has $!source;
            has &!mapper;
            
            submethod BUILD(:$!source, :&!mapper) { }
            
            method tap(|c) {
                my $sub = self.Supply::tap(|c);
                my $tap = $!source.tap(
                    -> \val {
                        self!next(&!mapper(val))
                    },
                    { self!last(); },
                    -> $ex { self!fail($ex) }
                );
                $sub
            }
        }
        MapSupply.new(:source($a), :&mapper)
    }
    
    method merge(Supply $a, Supply $b) {
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
    
    method zip(Supply $a, Supply $b, &with = &infix:<,>) {
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
