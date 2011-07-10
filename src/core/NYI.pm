# things that are known to be NYI

sub NYI(*@msg) { die @msg };

sub callsame(|$) { NYI "callsame not yet implemented" };
sub nextsame(|$) { NYI "nextsame not yet implemented" };
sub nextwith(|$) { NYI "nextwith not yet implemented" };


