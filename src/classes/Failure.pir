.namespace []

.namespace [ 'Failure' ]

.sub 'onload' :anon :init :load
    .local pmc p6meta, failureproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    failureproto = p6meta.'new_class'('Failure', 'parent'=>'Undef Any')
    p6meta.'register'('Undef', 'parent'=>failureproto, 'protoobject'=>failureproto)
.end

.sub 'perl' :method
    .return ('undef')
.end

.namespace []
.sub 'undef'
    .param pmc x               :slurpy
    ## 0-argument test, RT#56366
    ## but see also C<< term:sym<undef> >> in STD.pm
    unless x goto no_args
    die "Obsolete use of undef; in Perl 6 please use undefine instead"
  no_args:
    $P0 = new 'Failure'
    .return ($P0)
.end


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
