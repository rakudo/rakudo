## $Id$

=head1 NAME

src/builtins/list.pir - Perl 6 List class

=head1 Functions

=over 4

=cut

.namespace

.sub '__onload' :load :init
    $P0 = subclass 'ResizablePMCArray', 'Perl6List'
.end

# FIXME:  The 'clone' opcode doesn't seem to naturally work
# for subclasses of ResizablePMCArray, so we write our own here.
# FIXME:  #2 - iterators don't properly work for subclasses
# of ResizablePMCArray (RT #40958), so we have to enumerate the
# elements by index.

.namespace [ 'Perl6List' ]

.sub '__clone' :method
    $P0 = new 'Perl6List'
    $I1 = elements self
    $I0 = 0
  loop:
    if $I0 >= $I1 goto end
    $P1 = self[$I0]
    $P0[$I0] = $P1
    inc $I0
    goto loop
  end:
    .return ($P0)
.end

.namespace [ 'Perl6List' ]

.sub '__get_string' :method
    $S0 = ''
    $I1 = elements self
    if $I1 < 1 goto end
    $I0 = 1
    $S0 = self[0]
  loop:
    if $I0 >= $I1 goto end
    $S1 = self[$I0]
    $S0 = concat $S0, ' '
    $S0 .= $S1
    inc $I0
    goto loop
  end:
    .return ($S0)
.end


.sub 'elems' :method
    $I0 = elements self
    .return ($I0)
.end


=item C<list(...)>

Build a Perl6List from its arguments.

=cut

.namespace

.sub 'list'
    .param pmc args            :slurpy
    .local pmc list
    list = new 'Perl6List'
  args_loop:
    unless args goto end
    $P0 = shift args
    push list, $P0
    goto args_loop
  end:
    .return (list)
.end


=item C<infix:,(...)>

Operator form for building a list from its arguments.

=cut

.sub 'infix:,'
    .param pmc args            :slurpy
    .return 'list'(args :flat)
.end


## TODO: grep join map reduce reverse sort zip


=back

=cut

## vim: expandtab sw=4
