## This is used by integer computations, to upgrade the answer and return a
## Num if we overflow. We may want to return something like a BigInt in the
## future, but we don't have that yet and this gives something closer to the
## correct semantics than not upgrading an Int at all.

.namespace []
.sub '!upgrade_to_num_if_needed'
    .param num test
    if test > 2147483647.0 goto upgrade
    if test < -2147483648.0 goto upgrade
    $I0 = test
    .return ($I0)
  upgrade:
    .return (test)
.end


