augment class Supply {
    proto method snip($, |) {*}
    multi method snip(Supply:D: $test) {
        self.snip( ($test,) )
    }

    multi method snip(Supply:D: @tests) {
        my @left    = @tests;
        my $test   := @left ?? @left.shift !! Nil;
        my $buffer := nqp::create(IterationBuffer);
        supply {
            whenever self -> \val {
                if nqp::eqaddr($test,Nil) {
                    nqp::push($buffer,val);
                }
                elsif $test.ACCEPTS(val) {
                    emit $buffer.List;
                    nqp::push(($buffer := nqp::create(IterationBuffer)),val);
                    $test := @left ?? @left.shift !! Nil;
                }
                else {
                    nqp::push($buffer,val);
                }
                LAST {
                    emit $buffer.List;
                }
            }
        }
    }

    multi method snip(Supply:D: *@tests) {
        self.snip(@tests)
    }
}

# vim: expandtab shiftwidth=4
