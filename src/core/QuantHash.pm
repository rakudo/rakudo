my role QuantHash does Associative {
    method Int     ( --> Int)     { self.total.Int }
    method Num     ( --> Num)     { self.total.Num }
    method Numeric ( --> Numeric) { self.total.Numeric }
    method Real    ( --> Real)    { self.total.Real }

    method list() { self.pairs.cache }

    method minpairs {
        my @found;
        my $min = Inf;
        my $value;
        for self.pairs {
            if ($value := .value) < $min {
                @found = $_;
                $min   = $value;
            }
            elsif $value == $min {
                @found.push: $_;
            }
        }
        @found
    }

    method maxpairs {
        my @found;
        my $max = -Inf;
        my $value;
        for self.pairs {
            if ($value := .value) > $max {
                @found = $_;
                $max   = $value;
            }
            elsif $value == $max {
                @found.push: $_;
            }
        }
        @found
    }

    method fmt(QuantHash: Cool $format = "%s\t\%s", $sep = "\n") {
        nqp::iseq_i(nqp::sprintfdirectives( nqp::unbox_s($format.Stringy)),1)
          ?? self.keys.fmt($format, $sep)
          !! self.pairs.fmt($format, $sep)
    }
}

# vim: ft=perl6 expandtab sw=4
