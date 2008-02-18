## $Id$

=head1 NAME

src/classes/Hash.pir - Perl 6 Hash class, and related functions

=head1 Methods

=over 4

=cut

.namespace ['Hash']

.sub 'onload' :anon :load :init
    $P1 = get_hll_global ['Perl6Object'], 'make_proto'
    $P1('Hash', 'Hash')
.end


.sub 'get_string' :vtable :method
    $S0 = ''
    .local pmc iter
    iter = new 'Iterator', self
  loop:
    unless iter goto end
    $S1 = shift iter
    $S2 = iter[$S1]
    $S0 = concat $S0, $S1
    concat $S0, "\t"
    concat $S0, $S2
    concat $S0, "\n"
    goto loop
  end:
    .return ($S0)
.end


## FIXME:  Parrot currently requires us to write our own "clone" method.
.sub 'clone' :vtable :method
    $P0 = new 'Perl6Hash'
    .local pmc iter
    iter = new 'Iterator', self
  loop:
    unless iter goto end
    $P1 = shift iter
    $P2 = iter[$P1]
    $P0[$P1] = $P2
    goto loop
  end:
    .return ($P0)
.end


=back

=head1 Functions

=over 4

=back

=head1 TODO: Functions

=over 4

=cut

.namespace

=item delete

 our List  multi method Hash::delete ( *@keys )
 our Scalar multi method Hash::delete ( $key ) is default

Deletes the elements specified by C<$key> or C<$keys> from the invocant.
returns the value(s) that were associated to those keys.

=item exists

 our Bool multi method Hash::exists ( $key )

True if invocant has an element whose key matches C<$key>, false
otherwise.

=item keys

=item kv

=item pairs

=item values

 multi Int|List Hash::keys ( %hash : MatchTest *@keytests )
 multi Int|List Hash::kv ( %hash : MatchTest *@keytests )
 multi Int|(List of Pair) Hash::pairs  (%hash : MatchTest *@keytests )
 multi Int|List Hash::values ( %hash : MatchTest *@keytests )

Iterates the elements of C<%hash> in no apparent order, but the order
will be the same between successive calls to these functions, as long as
C<%hash> doesn't change.

If C<@keytests> are provided, only elements whose keys evaluate
C<$key ~~ any(@keytests)> as true are iterated.

What is returned at each element of the iteration varies with function.
C<keys> only returns the key; C<values> the value; C<kv> returns both as
a 2 element list in (key, value) order, C<pairs> a C<Pair(key, value)>.

Note that C<kv %hash> returns the same as C<zip(keys %hash; values %hash)>

In Scalar context, they all return the count of elements that would have
been iterated.

The lvalue form of C<keys> is not longer supported. Use the C<.buckets>
property instead.

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
