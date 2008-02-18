## $Id$

=head1 NAME

src/classes/Junction.pir - Perl 6 Junction and related functions

=head1 Methods

=over 4

=cut

.namespace ['Junction']

# Constants for types of junctions.
.const int JUNCTION_TYPE_ALL  = 1
.const int JUNCTION_TYPE_ANY  = 2
.const int JUNCTION_TYPE_ONE  = 3
.const int JUNCTION_TYPE_NONE = 4

.sub 'onload' :anon :load :init
    $P0 = subclass 'Perl6Object', 'Junction'
    addattribute $P0, "@values"
    addattribute $P0, "$type"
    $P1 = get_hll_global ['Perl6Object'], 'make_proto'
    $P1($P0, 'Junction')
.end


=item values()

Get the values in the junction.

=cut

.sub 'values' :method
    $P0 = getattribute self, "@values"
    $P0 = clone $P0
    .return($P0)
.end


=item !values(...)

Private method to sets the values in the junction.

=cut

.sub '!values' :method
    .param pmc l
    l = 'list'(l :flat)
    setattribute self, "@values", l
.end


=item !type(...)

Private method to set the type of the junction.

=cut

.sub '!type' :method
    .param pmc type     :optional
    .param int got_type :opt_flag
    unless got_type goto ret_type
    setattribute self, "$type", type
    .return()
ret_type:
    type = getattribute self, "$type"
    .return(type)
.end


=item clone

Clone v-table method.

=cut

.sub 'clone' :method :vtable
    .local pmc junc
    junc = new 'Junction'

    # Copy values and set type.
    $P0 = self.'values'()
    $P0 = clone $P0
    junc.'!values'($P0)
    $P0 = self.'!type'()
    junc.'!type'($P0)

    .return(junc)
.end


=back

=head1 Functions

=over 4

=cut

.namespace

=item C<all(...)>

Builds an 'all' junction from its arguments.

=cut

.sub 'all'
    .param pmc args            :slurpy
    .local pmc junc
    junc = new 'Junction'

    junc."!values"(args)
    junc."!type"(JUNCTION_TYPE_ALL)
    
    .return (junc)
.end


=item C<infix:&(...)>

Operator form for building an 'all' junction.

=cut

.sub 'infix:&'
    .param pmc args            :slurpy
    .return 'all'(args :flat)
.end


=item C<any(...)>

Builds an 'any' junction from its arguments.

=cut

.sub 'any'
    .param pmc args            :slurpy
    .local pmc junc
    junc = new 'Junction'

    junc."!values"(args)
    junc."!type"(JUNCTION_TYPE_ANY)

    .return (junc)
.end


=item C<infix:|(...)>

Operator form for building an 'any' junction.

=cut

.sub 'infix:|'
    .param pmc args            :slurpy
    .return 'any'(args :flat)
.end


=item C<one(...)>

Builds a 'one' junction from its arguments.

=cut

.sub 'one'
    .param pmc args            :slurpy
    .local pmc junc
    junc = new 'Junction'

    junc."!values"(args)
    junc."!type"(JUNCTION_TYPE_ONE)
    
    .return (junc)
.end


=item C<infix:^(...)>

Operator form for building a 'one' junction.

=cut

.sub 'infix:^'
    .param pmc args            :slurpy
    .return 'one'(args :flat)
.end


=item C<none(...)>

Builds a 'none' junction from its arguments.

=cut

.sub 'none'
    .param pmc args            :slurpy
    .local pmc junc
    junc = new 'Junction'

    junc."!values"(args)
    junc."!type"(JUNCTION_TYPE_NONE)
    .return (junc)
.end


=item C<!junction_dispatcher(...)>

Takes a name or Sub PMC along with a set of arguments, and auto-threads the
call.

TODO: Collect return values.

TODO: Handle the case where junctions contain other junctions.

TODO: When we get the type system in place, check for Junction in signature of
callee; in these cases, we needn't auto-thread, but instead should pass the
junction.

=cut

.sub '!junction_dispatcher'
    .param pmc the_sub
    .param pmc args :slurpy

    # Build list of lists of possible arguments.
    .local int num_args
    .local int i
    .local pmc possibles
    possibles = new 'List'
    num_args = elements args
    i = 0
get_possibles_loop:
    if i >= num_args goto get_possibles_loop_end
    $P0 = args[i]
    $I0 = isa $P0, 'Junction'
    if $I0 goto is_junction
    $P1 = new 'List'
    push $P1, $P0
    goto done_possible
is_junction:
    $P1 = $P0.values()
done_possible:
    possibles[i] = $P1
    inc i
    goto get_possibles_loop
get_possibles_loop_end:

    # Get all permutations.
    .local pmc perms
    perms = 'infix:X'(possibles :flat)
    $I0 = elements perms

    # If we have a sub name, we need to look it up.
    $I0 = isa the_sub, 'Code'
    if $I0 goto have_code
    $S0 = the_sub
    the_sub = find_global $S0
have_code:

    # Now call it for each permutation.
    num_args = elements perms
    i = 0
call_loop:
    if i >= num_args goto call_loop_end
    $P0 = perms[i]
    the_sub($P0 :flat)
    inc i
    goto call_loop
call_loop_end:
.end


=item C<postfix:++(...)>

Override postfix increment for junctions.

=cut

.sub 'postfix:++' :multi('Junction')
    .param pmc j
    $P0 = find_global 'postfix:++'
    .return unary_junction_helper($P0, j)
.end


=item C<postfix:--(...)>

Override postfix decrement for junctions.

=cut

.sub 'postfix:--' :multi('Junction')
    .param pmc j
    $P0 = find_global 'postfix:--'
    .return unary_junction_helper($P0, j)
.end


=item C<prefix:++(...)>

Override prefix increment for junctions.

=cut

.sub 'prefix:++' :multi('Junction')
    .param pmc j
    $P0 = find_global 'prefix:++'
    .return unary_junction_helper($P0, j)
.end


=item C<prefix:--(...)>

Override prefix decrement for junctions.

=cut

.sub 'prefix:--' :multi('Junction')
    .param pmc j
    $P0 = find_global 'prefix:--'
    .return unary_junction_helper($P0, j)
.end


=item C<prefix:!(...)>

Override not for junctions.

=cut

.sub 'prefix:!' :multi('Junction')
    .param pmc j
    $P0 = find_global 'prefix:!'
    .return unary_junction_helper($P0, j)
.end


=item C<prefix:+(...)>

Override numification for junctions.

=cut

.sub 'prefix:+' :multi('Junction')
    .param pmc j
    $P0 = find_global 'prefix:+'
    .return unary_junction_helper($P0, j)
.end


=item C<prefix:-(...)>

Override negation for junctions.

=cut

.sub 'prefix:-' :multi('Junction')
    .param pmc j
    $P0 = find_global 'prefix:-'
    .return unary_junction_helper($P0, j)
.end


=item C<prefix:~(...)>

Override stringification for junctions.

=cut

.sub 'prefix:~' :multi('Junction')
    .param pmc j
    $P0 = find_global 'prefix:~'
    .return unary_junction_helper($P0, j)
.end


=item C<prefix:?(...)>

Override boolification for junctions.

=cut

.sub 'prefix:?' :multi('Junction')
    .param pmc j
    $P0 = find_global 'prefix:?'
    .return unary_junction_helper($P0, j)
.end


=item C<prefix:=(...)>

Override iteration for junctions.

=cut

.sub 'prefix:=' :multi('Junction')
    .param pmc j
    $P0 = find_global 'prefix:='
    .return unary_junction_helper($P0, j)
.end


=item C<prefix:~^(...)>

Override string bitwise negation for junctions.

=cut

.sub 'prefix:~^' :multi('Junction')
    .param pmc j
    $P0 = find_global 'prefix:~^'
    .return unary_junction_helper($P0, j)
.end


=item C<prefix:+^(...)>

Override numeric bitwise negation for junctions.

=cut

.sub 'prefix:+^' :multi('Junction')
    .param pmc j
    $P0 = find_global 'prefix:+^'
    .return unary_junction_helper($P0, j)
.end


=item C<prefix:?^(...)>

Override boolean bitwise negation for junctions.

=cut

.sub 'prefix:?^' :multi('Junction')
    .param pmc j
    $P0 = find_global 'prefix:?^'
    .return unary_junction_helper($P0, j)
.end


=item C<infix:**(...)>

Override exponentiation for junctions.

=cut

.sub 'infix:**' :multi('Junction','Junction')
    .param pmc j1
    .param pmc j2
    $P0 = find_global 'infix:**'
    .return infix_junction_helper($P0, j1, j2)
.end

.sub 'infix:**' :multi('Junction',_)
    .param pmc j
    .param pmc x
    $P0 = find_global 'infix:**'
    .return infix_junction_helper($P0, j, x)
.end

.sub 'infix:**' :multi(_,'Junction')
    .param pmc x
    .param pmc j
    $P0 = find_global 'infix:**'
    .return infix_junction_helper($P0, j, x, 1)
.end


=item C<infix:%(...)>

Override modulo for junctions.

=cut

.sub 'infix:%' :multi('Junction','Junction')
    .param pmc j1
    .param pmc j2
    $P0 = find_global 'infix:%'
    .return infix_junction_helper($P0, j1, j2)
.end

.sub 'infix:%' :multi('Junction',_)
    .param pmc j
    .param pmc x
    $P0 = find_global 'infix:%'
    .return infix_junction_helper($P0, j, x)
.end

.sub 'infix:%' :multi(_,'Junction')
    .param pmc x
    .param pmc j
    $P0 = find_global 'infix:%'
    .return infix_junction_helper($P0, j, x, 1)
.end


=item C<infix:/(...)>

Override division for junctions.

=cut

.sub 'infix:/' :multi('Junction','Junction')
    .param pmc j1
    .param pmc j2
    $P0 = find_global 'infix:/'
    .return infix_junction_helper($P0, j1, j2)
.end

.sub 'infix:/' :multi('Junction',_)
    .param pmc j
    .param pmc x
    $P0 = find_global 'infix:/'
    .return infix_junction_helper($P0, j, x)
.end

.sub 'infix:/' :multi(_,'Junction')
    .param pmc x
    .param pmc j
    $P0 = find_global 'infix:/'
    .return infix_junction_helper($P0, j, x, 1)
.end


=item C<infix:*(...)>

Override multiply for junctions.

=cut

.sub 'infix:*' :multi('Junction','Junction')
    .param pmc j1
    .param pmc j2
    $P0 = find_global 'infix:*'
    .return infix_junction_helper($P0, j1, j2)
.end

.sub 'infix:*' :multi('Junction',_)
    .param pmc j
    .param pmc x
    $P0 = find_global 'infix:*'
    .return infix_junction_helper($P0, j, x)
.end

.sub 'infix:*' :multi(_,'Junction')
    .param pmc x
    .param pmc j
    $P0 = find_global 'infix:*'
    .return infix_junction_helper($P0, j, x, 1)
.end


=item C<infix:+&(...)>

Override numeric bitwise and for junctions.

=cut

.sub 'infix:+&' :multi('Junction','Junction')
    .param pmc j1
    .param pmc j2
    $P0 = find_global 'infix:+&'
    .return infix_junction_helper($P0, j1, j2)
.end

.sub 'infix:+&' :multi('Junction',_)
    .param pmc j
    .param pmc x
    $P0 = find_global 'infix:+&'
    .return infix_junction_helper($P0, j, x)
.end

.sub 'infix:+&' :multi(_,'Junction')
    .param pmc x
    .param pmc j
    $P0 = find_global 'infix:+&'
    .return infix_junction_helper($P0, j, x, 1)
.end


=item C<infix:+<(...)>

Override numeric left shift for junctions.

=cut

.sub 'infix:+<' :multi('Junction','Junction')
    .param pmc j1
    .param pmc j2
    $P0 = find_global 'infix:+<'
    .return infix_junction_helper($P0, j1, j2)
.end

.sub 'infix:+<' :multi('Junction',_)
    .param pmc j
    .param pmc x
    $P0 = find_global 'infix:+<'
    .return infix_junction_helper($P0, j, x)
.end

.sub 'infix:+<' :multi(_,'Junction')
    .param pmc x
    .param pmc j
    $P0 = find_global 'infix:+<'
    .return infix_junction_helper($P0, j, x, 1)
.end


=item C<infix:+>(...)>

Override numeric right shift for junctions.

=cut

.sub 'infix:+>' :multi('Junction','Junction')
    .param pmc j1
    .param pmc j2
    $P0 = find_global 'infix:+>'
    .return infix_junction_helper($P0, j1, j2)
.end

.sub 'infix:+>' :multi('Junction',_)
    .param pmc j
    .param pmc x
    $P0 = find_global 'infix:+>'
    .return infix_junction_helper($P0, j, x)
.end

.sub 'infix:+>' :multi(_,'Junction')
    .param pmc x
    .param pmc j
    $P0 = find_global 'infix:+>'
    .return infix_junction_helper($P0, j, x, 1)
.end


=item C<infix:div(...)>

Override div for junctions.

=cut

.sub 'infix:div' :multi('Junction','Junction')
    .param pmc j1
    .param pmc j2
    $P0 = find_global 'infix:div'
    .return infix_junction_helper($P0, j1, j2)
.end

.sub 'infix:div' :multi('Junction',_)
    .param pmc j
    .param pmc x
    $P0 = find_global 'infix:div'
    .return infix_junction_helper($P0, j, x)
.end

.sub 'infix:div' :multi(_,'Junction')
    .param pmc x
    .param pmc j
    $P0 = find_global 'infix:div'
    .return infix_junction_helper($P0, j, x, 1)
.end


=item C<infix:mod(...)>

Override mod for junctions.

=cut

.sub 'infix:mod' :multi('Junction','Junction')
    .param pmc j1
    .param pmc j2
    $P0 = find_global 'infix:mod'
    .return infix_junction_helper($P0, j1, j2)
.end

.sub 'infix:mod' :multi('Junction',_)
    .param pmc j
    .param pmc x
    $P0 = find_global 'infix:mod'
    .return infix_junction_helper($P0, j, x)
.end

.sub 'infix:mod' :multi(_,'Junction')
    .param pmc x
    .param pmc j
    $P0 = find_global 'infix:mod'
    .return infix_junction_helper($P0, j, x, 1)
.end


=item C<infix:~&(...)>

Override buffer bitwise and for junctions.

=cut

.sub 'infix:~&' :multi('Junction','Junction')
    .param pmc j1
    .param pmc j2
    $P0 = find_global 'infix:~&'
    .return infix_junction_helper($P0, j1, j2)
.end

.sub 'infix:~&' :multi('Junction',_)
    .param pmc j
    .param pmc x
    $P0 = find_global 'infix:~&'
    .return infix_junction_helper($P0, j, x)
.end

.sub 'infix:~&' :multi(_,'Junction')
    .param pmc x
    .param pmc j
    $P0 = find_global 'infix:~&'
    .return infix_junction_helper($P0, j, x, 1)
.end


=item C<infix:~<(...)>

Override buffer bitwise left shift for junctions.

=cut

.sub 'infix:~<' :multi('Junction','Junction')
    .param pmc j1
    .param pmc j2
    $P0 = find_global 'infix:~<'
    .return infix_junction_helper($P0, j1, j2)
.end

.sub 'infix:~<' :multi('Junction',_)
    .param pmc j
    .param pmc x
    $P0 = find_global 'infix:~<'
    .return infix_junction_helper($P0, j, x)
.end

.sub 'infix:~<' :multi(_,'Junction')
    .param pmc x
    .param pmc j
    $P0 = find_global 'infix:~<'
    .return infix_junction_helper($P0, j, x, 1)
.end


=item C<infix:~>(...)>

Override buffer bitwise right shift for junctions.

=cut

.sub 'infix:~>' :multi('Junction','Junction')
    .param pmc j1
    .param pmc j2
    $P0 = find_global 'infix:~>'
    .return infix_junction_helper($P0, j1, j2)
.end

.sub 'infix:~>' :multi('Junction',_)
    .param pmc j
    .param pmc x
    $P0 = find_global 'infix:~>'
    .return infix_junction_helper($P0, j, x)
.end

.sub 'infix:~>' :multi(_,'Junction')
    .param pmc x
    .param pmc j
    $P0 = find_global 'infix:~>'
    .return infix_junction_helper($P0, j, x, 1)
.end


=item C<infix:?&(...)>

Override boolean bitwise and for junctions.

=cut

.sub 'infix:?&' :multi('Junction','Junction')
    .param pmc j1
    .param pmc j2
    $P0 = find_global 'infix:?&'
    .return infix_junction_helper($P0, j1, j2)
.end

.sub 'infix:?&' :multi('Junction',_)
    .param pmc j
    .param pmc x
    $P0 = find_global 'infix:?&'
    .return infix_junction_helper($P0, j, x)
.end

.sub 'infix:?&' :multi(_,'Junction')
    .param pmc x
    .param pmc j
    $P0 = find_global 'infix:?&'
    .return infix_junction_helper($P0, j, x, 1)
.end


=item C<infix:+(...)>

Override addition for junctions.

=cut

.sub 'infix:+' :multi('Junction','Junction')
    .param pmc j1
    .param pmc j2
    $P0 = find_global 'infix:+'
    .return infix_junction_helper($P0, j1, j2)
.end

.sub 'infix:+' :multi('Junction',_)
    .param pmc j
    .param pmc x
    $P0 = find_global 'infix:+'
    .return infix_junction_helper($P0, j, x)
.end

.sub 'infix:+' :multi(_,'Junction')
    .param pmc x
    .param pmc j
    $P0 = find_global 'infix:+'
    .return infix_junction_helper($P0, j, x, 1)
.end


=item C<infix:-(...)>

Override subtraction for junctions.

=cut

.sub 'infix:-' :multi('Junction','Junction')
    .param pmc j1
    .param pmc j2
    $P0 = find_global 'infix:-'
    .return infix_junction_helper($P0, j1, j2)
.end

.sub 'infix:-' :multi('Junction',_)
    .param pmc j
    .param pmc x
    $P0 = find_global 'infix:-'
    .return infix_junction_helper($P0, j, x)
.end

.sub 'infix:-' :multi(_,'Junction')
    .param pmc x
    .param pmc j
    $P0 = find_global 'infix:-'
    .return infix_junction_helper($P0, j, x, 1)
.end


=item C<infix:x(...)>

Override repeat for junctions.

=cut

.sub 'infix:x' :multi('Junction','Junction')
    .param pmc j1
    .param pmc j2
    $P0 = find_global 'infix:x'
    .return infix_junction_helper($P0, j1, j2)
.end

.sub 'infix:x' :multi('Junction',_)
    .param pmc j
    .param pmc x
    $P0 = find_global 'infix:x'
    .return infix_junction_helper($P0, j, x)
.end

.sub 'infix:x' :multi(_,'Junction')
    .param pmc x
    .param pmc j
    $P0 = find_global 'infix:x'
    .return infix_junction_helper($P0, j, x, 1)
.end


=item C<infix:xx(...)>

Override array repeat for junctions.

=cut

.sub 'infix:xx' :multi('Junction','Junction')
    .param pmc j1
    .param pmc j2
    $P0 = find_global 'infix:xx'
    .return infix_junction_helper($P0, j1, j2)
.end

.sub 'infix:xx' :multi('Junction',_)
    .param pmc j
    .param pmc x
    $P0 = find_global 'infix:xx'
    .return infix_junction_helper($P0, j, x)
.end

.sub 'infix:xx' :multi(_,'Junction')
    .param pmc x
    .param pmc j
    $P0 = find_global 'infix:xx'
    .return infix_junction_helper($P0, j, x, 1)
.end


=item C<infix:~(...)>

Override concatenation for junctions.

=cut

.sub 'infix:~' :multi('Junction','Junction')
    .param pmc j1
    .param pmc j2
    $P0 = find_global 'infix:~'
    .return infix_junction_helper($P0, j1, j2)
.end

.sub 'infix:~' :multi('Junction',_)
    .param pmc j
    .param pmc x
    $P0 = find_global 'infix:~'
    .return infix_junction_helper($P0, j, x)
.end

.sub 'infix:~' :multi(_,'Junction')
    .param pmc x
    .param pmc j
    $P0 = find_global 'infix:~'
    .return infix_junction_helper($P0, j, x, 1)
.end


=item C<infix:==(...)>

Override numerical equality for junctions.

=cut

.sub 'infix:==' :multi('Junction','Junction')
    .param pmc j1
    .param pmc j2
    $P0 = find_global "infix:=="
    .return junction_comparrison_helper($P0, j1, j2, 0)
.end

.sub 'infix:==' :multi('Junction',_)
    .param pmc j
    .param pmc x
    $P0 = find_global "infix:=="
    .return junction_comparrison_helper($P0, j, x, 0)
.end

.sub 'infix:==' :multi(_,'Junction')
    .param pmc x
    .param pmc j
    $P0 = find_global "infix:=="
    .return junction_comparrison_helper($P0, j, x, 1)
.end


=item C<infix:!=(...)>

Override numerical inequality for junctions.

=cut

.sub 'infix:!=' :multi('Junction','Junction')
    .param pmc j1
    .param pmc j2
    $P0 = find_global "infix:!="
    .return junction_comparrison_helper($P0, j1, j2, 0)
.end

.sub 'infix:!=' :multi('Junction',_)
    .param pmc j
    .param pmc x
    $P0 = find_global "infix:!="
    .return junction_comparrison_helper($P0, j, x, 0)
.end

.sub 'infix:!=' :multi(_,'Junction')
    .param pmc x
    .param pmc j
    $P0 = find_global "infix:!="
    .return junction_comparrison_helper($P0, j, x, 1)
.end


=item C<infix:>(...)>

Override numerical greater than for junctions.

=cut

.sub 'infix:>' :multi('Junction','Junction')
    .param pmc j1
    .param pmc j2
    $P0 = find_global "infix:>"
    .return junction_comparrison_helper($P0, j1, j2, 0)
.end

.sub 'infix:>' :multi('Junction',_)
    .param pmc j
    .param pmc x
    $P0 = find_global "infix:>"
    .return junction_comparrison_helper($P0, j, x, 0)
.end

.sub 'infix:>' :multi(_,'Junction')
    .param pmc x
    .param pmc j
    $P0 = find_global "infix:>"
    .return junction_comparrison_helper($P0, j, x, 1)
.end


=item C<infix:<(...)>

Override numerical less than for junctions.

=cut

.sub 'infix:<' :multi('Junction','Junction')
    .param pmc j1
    .param pmc j2
    $P0 = find_global "infix:<"
    .return junction_comparrison_helper($P0, j1, j2, 0)
.end

.sub 'infix:<' :multi('Junction',_)
    .param pmc j
    .param pmc x
    $P0 = find_global "infix:<"
    .return junction_comparrison_helper($P0, j, x, 0)
.end

.sub 'infix:<' :multi(_,'Junction')
    .param pmc x
    .param pmc j
    $P0 = find_global "infix:<"
    .return junction_comparrison_helper($P0, j, x, 1)
.end


=item C<infix:>=(...)>

Override numerical greater than or equal to for junctions.

=cut

.sub 'infix:>=' :multi('Junction','Junction')
    .param pmc j1
    .param pmc j2
    $P0 = find_global "infix:>="
    .return junction_comparrison_helper($P0, j1, j2, 0)
.end

.sub 'infix:>=' :multi('Junction',_)
    .param pmc j
    .param pmc x
    $P0 = find_global "infix:>="
    .return junction_comparrison_helper($P0, j, x, 0)
.end

.sub 'infix:>=' :multi(_,'Junction')
    .param pmc x
    .param pmc j
    $P0 = find_global "infix:>="
    .return junction_comparrison_helper($P0, j, x, 1)
.end


=item C<infix:<=(...)>

Override numerical less than or equal to for junctions.

=cut

.sub 'infix:<=' :multi('Junction','Junction')
    .param pmc j1
    .param pmc j2
    $P0 = find_global "infix:<="
    .return junction_comparrison_helper($P0, j1, j2, 0)
.end

.sub 'infix:<=' :multi('Junction',_)
    .param pmc j
    .param pmc x
    $P0 = find_global "infix:<="
    .return junction_comparrison_helper($P0, j, x, 0)
.end

.sub 'infix:<=' :multi(_,'Junction')
    .param pmc x
    .param pmc j
    $P0 = find_global "infix:<="
    .return junction_comparrison_helper($P0, j, x, 1)
.end


=item C<infix:eq(...)>

Override string equality for junctions.

=cut

.sub 'infix:eq' :multi('Junction','Junction')
    .param pmc j1
    .param pmc j2
    $P0 = find_global "infix:eq"
    .return junction_comparrison_helper($P0, j1, j2, 0)
.end

.sub 'infix:eq' :multi('Junction',_)
    .param pmc j
    .param pmc x
    $P0 = find_global "infix:eq"
    .return junction_comparrison_helper($P0, j, x, 0)
.end

.sub 'infix:eq' :multi(_,'Junction')
    .param pmc x
    .param pmc j
    $P0 = find_global "infix:eq"
    .return junction_comparrison_helper($P0, j, x, 1)
.end


=item C<infix:ne(...)>

Override string inequality for junctions.

=cut

.sub 'infix:ne' :multi('Junction','Junction')
    .param pmc j1
    .param pmc j2
    $P0 = find_global "infix:ne"
    .return junction_comparrison_helper($P0, j1, j2, 0)
.end

.sub 'infix:ne' :multi('Junction',_)
    .param pmc j
    .param pmc x
    $P0 = find_global "infix:ne"
    .return junction_comparrison_helper($P0, j, x, 0)
.end

.sub 'infix:ne' :multi(_,'Junction')
    .param pmc x
    .param pmc j
    $P0 = find_global "infix:ne"
    .return junction_comparrison_helper($P0, j, x, 1)
.end


=item C<infix:lt(...)>

Override string less than for junctions.

=cut

.sub 'infix:lt' :multi('Junction','Junction')
    .param pmc j1
    .param pmc j2
    $P0 = find_global "infix:lt"
    .return junction_comparrison_helper($P0, j1, j2, 0)
.end

.sub 'infix:lt' :multi('Junction',_)
    .param pmc j
    .param pmc x
    $P0 = find_global "infix:lt"
    .return junction_comparrison_helper($P0, j, x, 0)
.end

.sub 'infix:lt' :multi(_,'Junction')
    .param pmc x
    .param pmc j
    $P0 = find_global "infix:lt"
    .return junction_comparrison_helper($P0, j, x, 1)
.end


=item C<infix:gt(...)>

Override string greater than for junctions.

=cut

.sub 'infix:gt' :multi('Junction','Junction')
    .param pmc j1
    .param pmc j2
    $P0 = find_global "infix:gt"
    .return junction_comparrison_helper($P0, j1, j2, 0)
.end

.sub 'infix:gt' :multi('Junction',_)
    .param pmc j
    .param pmc x
    $P0 = find_global "infix:gt"
    .return junction_comparrison_helper($P0, j, x, 0)
.end

.sub 'infix:gt' :multi(_,'Junction')
    .param pmc x
    .param pmc j
    $P0 = find_global "infix:gt"
    .return junction_comparrison_helper($P0, j, x, 1)
.end


=item C<infix:le(...)>

Override string less than or equal for junctions.

=cut

.sub 'infix:le' :multi('Junction','Junction')
    .param pmc j1
    .param pmc j2
    $P0 = find_global "infix:le"
    .return junction_comparrison_helper($P0, j1, j2, 0)
.end

.sub 'infix:le' :multi('Junction',_)
    .param pmc j
    .param pmc x
    $P0 = find_global "infix:le"
    .return junction_comparrison_helper($P0, j, x, 0)
.end

.sub 'infix:le' :multi(_,'Junction')
    .param pmc x
    .param pmc j
    $P0 = find_global "infix:le"
    .return junction_comparrison_helper($P0, j, x, 1)
.end


=item C<infix:ge(...)>

Override string greater than or equal for junctions.

=cut

.sub 'infix:ge' :multi('Junction','Junction')
    .param pmc j1
    .param pmc j2
    $P0 = find_global "infix:ge"
    .return junction_comparrison_helper($P0, j1, j2, 0)
.end

.sub 'infix:ge' :multi('Junction',_)
    .param pmc j
    .param pmc x
    $P0 = find_global "infix:ge"
    .return junction_comparrison_helper($P0, j, x, 0)
.end

.sub 'infix:ge' :multi(_,'Junction')
    .param pmc x
    .param pmc j
    $P0 = find_global "infix:ge"
    .return junction_comparrison_helper($P0, j, x, 1)
.end


# Helper sub for applying non-comparative infixes to junctions.
.sub infix_junction_helper :anon
    .param pmc op_sub
    .param pmc j
    .param pmc x
    .param int second_arg :optional

    # Build hash of results, to ensure we are unique.
    .local pmc ResultHash
    ResultHash = new 'Hash'

    # Get values array.
    .local pmc values
    values = j.'values'()

    # Loop over it and call inc on each element.
    .local int count
    .local int i
    .local pmc cur_elem
    count = elements values
    i = 0
loop:
    if i >= count goto loop_end
    cur_elem = values[i]
    if second_arg goto sa
    $P0 = op_sub(cur_elem, x)
    goto nsa
sa:
    $P0 = op_sub(x, cur_elem)
nsa:
    ResultHash[$P0] = 1
    inc i
    goto loop
loop_end:
    
    # Build junction of results.
    .local pmc new_junc
    .local pmc new_values
    .local pmc iterator
    .local pmc type
    
    new_junc = new 'Junction'
    type = j.'!type'()
    new_junc.'!type'(type)
    
    new_values = new 'List'
    iterator = iter ResultHash
nv_loop:
    unless iterator goto nv_loop_end
    $P0 = shift iterator
    push new_values, $P0
    goto nv_loop
nv_loop_end:
    new_junc.'!values'(new_values)
    
    .return(new_junc)
.end

# Helper sub for junction comparrisons.
.sub junction_comparrison_helper :anon
    .param pmc op_func
    .param pmc j
    .param pmc x
    .param int second_arg
    
    # We need to find how many values are equal.
    .local pmc values
    .local int num_equal
    .local int count
    .local int i
    values = j.'values'()
    count = elements values
    i = 0
    num_equal = 0
loop:
    if i >= count goto end_loop
    $P0 = values[i]
    if second_arg goto sa
    $I0 = op_func($P0, x)
    goto not_sa
sa:
    $I0 = op_func(x, $P0)
not_sa:
    num_equal += $I0
    inc i
    goto loop
end_loop:
    
    # Now go by juction type.
    .local int type
    type = j.'!type'()
    if type == JUNCTION_TYPE_ALL goto all
    if type == JUNCTION_TYPE_ANY goto any
    if type == JUNCTION_TYPE_ONE goto one
    if type == JUNCTION_TYPE_NONE goto none

all:
    if num_equal == count goto ret_true
    goto ret_false
any:
    if num_equal > 0 goto ret_true
    goto ret_false
one:
    if num_equal == 1 goto ret_true
    goto ret_false
none:
    if num_equal == 0 goto ret_true
    goto ret_false

ret_true:
    $P0 = get_hll_global ['Bool'], 'True'
    .return($P0)
ret_false:
    $P0 = get_hll_global ['Bool'], 'False'
    .return($P0)
.end


# Helper sub for implementing unary operators.
.sub unary_junction_helper :anon
    .param pmc op_sub
    .param pmc j

    # Build hash of results, to ensure we are unique.
    .local pmc ResultHash
    ResultHash = new 'Hash'

    # Loop over and call multiply on each value.
    # Get values array.
    .local pmc values
    values = j.'values'()

    # Loop over it and call inc on each element.
    .local int count
    .local int i
    .local pmc cur_elem
    count = elements values
    i = 0
loop:
    if i >= count goto loop_end
    cur_elem = values[i]
    $P0 = op_sub(cur_elem)
    ResultHash[$P0] = 1
    inc i
    goto loop
loop_end:
    
    # Build junction of results.
    .local pmc new_junc
    .local pmc new_values
    .local pmc iterator
    .local pmc type
    
    new_junc = new 'Junction'
    type = j.'!type'()
    new_junc.'!type'(type)
    
    new_values = new 'List'
    iterator = iter ResultHash
nv_loop:
    unless iterator goto nv_loop_end
    $P0 = shift iterator
    push new_values, $P0
    goto nv_loop
nv_loop_end:
    new_junc.'!values'(new_values)
    
    .return(new_junc)
.end


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
