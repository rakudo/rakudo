## $Id$

=head1 NAME

src/classes/List.pir - Perl 6 List class

=head1 Methods

=over 4

=cut

.namespace ['List']

.sub 'onload' :anon :load :init
    $P0 = subclass 'ResizablePMCArray', 'List'
    $P1 = get_hll_global ['Perl6Object'], 'make_proto'
    $P1($P0, 'List')
.end


=item get_string()    (vtable method)

Return the elements of the list joined by spaces.

=cut

.sub 'get_string' :vtable :method
    $S0 = join ' ', self
    .return ($S0)
.end


=item elems()

Return the number of elements in the list.

=cut

.sub 'elems' :method
    $I0 = elements self
    .return ($I0)
.end

=item unshift(ELEMENTS)

Prepends ELEMENTS to the front of the list.

=cut

.sub 'unshift' :method
    .param pmc args :slurpy
    .local int narg
    .local int i

    narg = args
    i = 0

    .local pmc tmp
  loop:
    if i == narg goto done
    pop tmp, args
    unshift self, tmp
    inc i
    goto loop
  done:	
.end

=item shift()

Shifts the first item off the list and returns it.

=cut

.sub 'shift' :method
    .local pmc x
    x = shift self
    .return (x)
.end

=item pop()

Treats the list as a stack, popping the last item off the list and returning it.

=cut

.sub 'pop' :method
    .local pmc x
    .local int len

    len = elements self

    if len == 0 goto empty
    pop x, self
    goto done
    
  empty:
    x = undef()
    goto done

  done:	
    .return (x)
.end

=item push(ELEMENTS)

Treats the list as a stack, pushing ELEMENTS onto the end of the list.  Returns the new length of the list.

=cut

.sub 'push' :method
    .param pmc args :slurpy
    .local int len
    .local pmc tmp
    .local int i

    len = args
    i = 0

  loop:
    if i == len goto done
    shift tmp, args
    push self, tmp
    inc i
    goto loop
  done:
    len = elements self
    .return (len)
.end

=item join(SEPARATOR)

Returns a string comprised of all of the list, separated by the string SEPARATOR.  Given an empty list, join returns the empty string.

=cut

.sub 'join' :method
    .param string sep
    .local string res
    .local string tmp
    .local int len
    .local int i
	
    res = ""

    len = elements self
    if len == 0 goto done

    len = len - 1
    i = 0

  loop:
    if i == len goto last

    tmp = self[i]
    concat res, tmp
    concat res, sep
    
    inc i
    goto loop

  last:
    tmp = self[i]
    concat res, tmp
  
  done:
    .return(res)	
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
