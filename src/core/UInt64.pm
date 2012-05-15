my Int $UINT64_UPPER = nqp::pow_I(2, 64, Num, Int); 
subset UInt64 of Int where { 0 <= $_ < $UINT64_UPPER } 
