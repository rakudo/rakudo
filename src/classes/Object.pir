=head1 TITLE

Object - Perl 6 Object class

=cut

.namespace ['Perl6Object']

.sub 'onload' :anon :init :load
    $P0 = newclass 'Perl6Protoobject'
    addattribute $P0, 'shortname'

    $P0 = newclass 'Perl6Object'
    $P1 = new $P0
    set_hll_global 'Object', $P1

    .local pmc protohash
    protohash = new 'Hash'
    set_global '%!proto', protohash
.end


.sub 'new' :method
    $P0 = typeof self
    $P1 = new $P0
    .return ($P1)
.end


.sub 'WHAT' :method
    $S0 = typeof self
    $P0 = get_global '%!proto'
    $P0 = $P0[$S0]
    .return ($P0)
.end


.sub 'make_class'
    .param pmc subc
    .param pmc superc          :optional :named('super')
    .param int has_super       :opt_flag
    .param pmc parrotc         :optional :named('parrot')
    .param int has_parrot      :opt_flag

    ##  if new class is given as a string, split it on '::'
    $I0 = isa subc, 'String'
    unless $I0 goto have_subc
    $S0 = subc
    subc = split '::', $S0
  have_subc:

    ##  if no superclass provided, use Perl6Object as the superclass
    if has_super goto have_superc
    superc = get_class 'Perl6Object'
  have_superc:

    ##  if no parrot classname provided, use the perl6 name for parrot
    if has_parrot goto have_parrotc
    parrotc = subc
  have_parrotc:

    ##  create the new class
    .local pmc class
    class = subclass superc, parrotc
    $I0 = isa class, 'Perl6Object'
    if $I0 goto object_done
    $P0 = get_class 'Perl6Object'
    class.'add_parent'($P0)
  object_done:

    ##  create the protoobject's class
    .local pmc protoclass
    protoclass = new 'Class'
    $P0 = get_class 'Perl6Protoobject'
    protoclass.'add_parent'($P0)
    protoclass.'add_parent'(class)

    ##  create the protoobject, set up its attributes
    .local pmc protoobject
    protoobject = new protoclass
    $P0 = subc[-1]
    setattribute protoobject, 'shortname', $P0
    ##  store the protoobject
    $S0 = pop subc
    set_hll_global subc, $S0, protoobject
    ##  store in the protohash
    .local pmc protohash
    protohash = get_global '%!proto'
    $S0 = class.'name'()
    protohash[$S0] = protoobject
    .return (protoobject)
.end

.namespace ['Perl6Protoobject']

.sub 'get_string' :vtable :method
    $P0 = getattribute self, 'shortname'
    $S0 = $P0
    .return ($S0)
.end


.sub 'WHAT' :method
    .return (self)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:

