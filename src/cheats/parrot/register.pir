=head1 TITLE

src/cheats/parrot/register.pir - register Parrot classes with Perl 6 identity

=head1 DESCRIPTION

This file registers Parrot classes such as C<Integer>, C<Float>, etc.
under their Rakudo equivalents (C<Int>, C<Num>, etc.).  This allows Parrot
objects (when we get them) to somewhat invisibly act like their Perl 6
counterparts, although sometimes the leaks show through.

This needs to appear last in the compilation sequence, to allow any
other :load/:init subs to have a chance to add methods to the base
Rakudo classes before we compose the Parrot equivalents.

=cut

.HLL .RAKUDO_HLL

.sub '' :anon :load :init
    .local pmc p6meta, rakudons, proto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'

    proto = get_hll_global 'Bool'
    p6meta.'register'('Boolean', 'parent'=>proto, 'protoobject'=>proto)

    proto = get_hll_global 'Code'
    p6meta.'register'('Sub', 'parent'=>proto, 'protoobject'=>proto)

    proto = get_hll_global 'Int'
    p6meta.'register'('Integer', 'parent'=>proto, 'protoobject'=>proto)
    p6meta.'register'('BigInt', 'parent'=>proto, 'protoobject'=>proto)

    proto = get_hll_global 'Num'
    p6meta.'register'('Float', 'parent'=>proto, 'protoobject'=>proto)

    proto = get_hll_global 'Str'
    p6meta.'register'('Perl6Str', 'parent'=>proto, 'protoobject'=>proto)
    p6meta.'register'('String', 'parent'=>proto, 'protoobject'=>proto)

.end
