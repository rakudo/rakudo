constant Least = Any.new;
constant Most  = Any.new;

$*LEAST = Least;
$*MOST  = Most;

multi sub infix:<cmp>(Least, Least --> Same) { }
multi sub infix:<cmp>(Least, Mu --> Less) { }
multi sub infix:<cmp>(Mu, Least --> More) { }

multi sub infix:<cmp>(Most, Most --> Same) { }
multi sub infix:<cmp>(Most, Mu --> More) { }
multi sub infix:<cmp>(Mu, Most --> Less) { }

multi sub infix:<leg>(Least, Least --> Same) { }
multi sub infix:<leg>(Least, Mu --> Less) { }
multi sub infix:<leg>(Mu, Least --> More) { }

multi sub infix:<leg>(Most, Most --> Same) { }
multi sub infix:<leg>(Most, Mu --> More) { }
multi sub infix:<leg>(Mu, Most --> Less) { }

multi sub infix:<eq>(Least, Least --> True) { }
multi sub infix:<eq>(Least, Mu --> False) { }
multi sub infix:<eq>(Mu, Least --> False) { }

multi sub infix:<eq>(Most, Most --> True) { }
multi sub infix:<eq>(Most, Mu --> False) { }
multi sub infix:<eq>(Mu, Most --> False) { }

multi sub infix:<lt>(Least, Least --> False) { }
multi sub infix:<lt>(Least, Mu --> True) { }
multi sub infix:<lt>(Mu, Least --> False) { }

multi sub infix:<lt>(Most, Most --> False) { }
multi sub infix:<lt>(Most, Mu --> False) { }
multi sub infix:<lt>(Mu, Most --> True) { }

multi sub infix:<le>(Least, Least --> True) { }
multi sub infix:<le>(Least, Mu --> True) { }
multi sub infix:<le>(Mu, Least --> False) { }

multi sub infix:<le>(Most, Most --> True) { }
multi sub infix:<le>(Most, Mu --> False) { }
multi sub infix:<le>(Mu, Most --> True) { }

multi sub infix:<==>(Least, Least --> True) { }
multi sub infix:<==>(Least, Mu --> False) { }
multi sub infix:<==>(Mu, Least --> False) { }

multi sub infix:<==>(Most, Most --> True) { }
multi sub infix:<==>(Most, Mu --> False) { }
multi sub infix:<==>(Mu, Most --> False) { }

multi sub infix:«<»(Least, Least --> False) { }
multi sub infix:«<»(Least, Mu --> True) { }
multi sub infix:«<»(Mu, Least --> False) { }

multi sub infix:«<»(Most, Most --> False) { }
multi sub infix:«<»(Most, Mu --> False) { }
multi sub infix:«<»(Mu, Most --> True) { }

multi sub infix:«<=»(Least, Least --> True) { }
multi sub infix:«<=»(Least, Mu --> True) { }
multi sub infix:«<=»(Mu, Least --> False) { }

multi sub infix:«<=»(Most, Most --> True) { }
multi sub infix:«<=»(Most, Mu --> False) { }
multi sub infix:«<=»(Mu, Most --> True) { }

multi sub infix:<ne>(Least, Least --> False) { }
multi sub infix:<ne>(Least, Mu --> True) { }
multi sub infix:<ne>(Mu, Least --> True) { }

multi sub infix:<ne>(Most, Most --> False) { }
multi sub infix:<ne>(Most, Mu --> True) { }
multi sub infix:<ne>(Mu, Most --> True) { }

multi sub infix:<gt>(Least, Least --> False) { }
multi sub infix:<gt>(Least, Mu --> False) { }
multi sub infix:<gt>(Mu, Least --> True) { }

multi sub infix:<gt>(Most, Most --> False) { }
multi sub infix:<gt>(Most, Mu --> True) { }
multi sub infix:<gt>(Mu, Most --> False) { }

multi sub infix:<ge>(Least, Least --> True) { }
multi sub infix:<ge>(Least, Mu --> False) { }
multi sub infix:<ge>(Mu, Least --> True) { }

multi sub infix:<ge>(Most, Most --> True) { }
multi sub infix:<ge>(Most, Mu --> True) { }
multi sub infix:<ge>(Mu, Most --> False) { }

multi sub infix:«>»(Least, Least --> False) { }
multi sub infix:«>»(Least, Mu --> False) { }
multi sub infix:«>»(Mu, Least --> True) { }

multi sub infix:«>»(Most, Most --> False) { }
multi sub infix:«>»(Most, Mu --> True) { }
multi sub infix:«>»(Mu, Most --> False) { }

multi sub infix:«>=»(Least, Least --> True) { }
multi sub infix:«>=»(Least, Mu --> False) { }
multi sub infix:«>=»(Mu, Least --> True) { }

multi sub infix:«>=»(Most, Most --> True) { }
multi sub infix:«>=»(Most, Mu --> True) { }
multi sub infix:«>=»(Mu, Most --> False) { }

# vim: expandtab shiftwidth=4
