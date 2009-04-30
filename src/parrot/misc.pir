=item ResizablePMCArray.list

This version of list morphs a ResizablePMCArray into a List.

=cut

.namespace ['ResizablePMCArray']
.sub 'list' :method
    ##  this code morphs a ResizablePMCArray into a List
    ##  without causing a clone of any of the elements
    $P0 = new 'ResizablePMCArray'
    splice $P0, self, 0, 0
    $P1 = new 'List'
    copy self, $P1
    splice self, $P0, 0, 0
    .return (self)
.end


## special method to cast Parrot String into Rakudo Str.
.namespace ['String']
.sub 'Scalar' :method
    $P0 = new 'Str'
    assign $P0, self
    copy self, $P0
    .return (self)
.end


=item count()

Return the number of required and optional parameters for a Block.
Note that we currently do this by adding the method to Parrot's
"Sub" PMC, so that it works for non-Rakudo subs.

=cut

.namespace ['Sub']
.sub 'count' :method
    $P0 = inspect self, "pos_required"
    $P1 = inspect self, "pos_optional"
    add $P0, $P1
    .return ($P0)
.end



