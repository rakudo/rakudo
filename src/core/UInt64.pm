my $UINT64_UPPER = nqp::pow_I(2, 64, Num);
subset UInt64 of Int where { 0 <= $_ < $UINT64_UPPER } 
