=head1 TITLE

parse.pir - Parsing support subroutines

=head2 DESCRIPTION

This file contains numerous support subroutines for parsing
Perl 6 programs.  The most important is actually the "__onload"
sub, which creates an operator precedence parser for Perl 6
expressions.  Other specialized parsing subroutines will
appear here as the parser grows.

=over 4

=cut

.namespace [ "Perl6::Grammar" ]

.include "cclass.pasm"

=item C<__onload_parse()>

This subroutine creates an operator precedence parser
and initializes it with the operators defined by Perl 6.

=cut

.sub "__onload_parse" :load
    .local pmc optable, op
    $I0 = find_type "PGE::OPTable"
    optable = new $I0
    store_global "Perl6::Grammar", "$optable", optable

    $P0 = find_global "Perl6::Grammar", "ws"
    setattribute optable, "PGE::OPTable\x0&!ws", $P0

    # terms
    $P0 = find_global "Perl6::Grammar", "term"
    op = optable.addtok("term:", "22=", "left", $P0)
    op['pastrule'] = 'past_term'
    op = optable.addtok("circumfix:( )", "term:")
    op['pircode'] = '    # circumfix:( )'

    # method postfix
    optable.addtok("postfix:.", "21=")
    optable.addtok("postcircumfix:.( )", "postfix:.")
    optable.addtok("postcircumfix:.[ ]", "postfix:.")
    optable.addtok("postcircumfix:.{ }", "postfix:.")
    optable.addtok("postcircumfix:( )", "postfix:.", "nows")
    optable.addtok("postcircumfix:[ ]", "postfix:.", "nows")
    optable.addtok("postcircumfix:{ }", "postfix:.", "nows")

    # autoincrement
    optable.addtok("postfix:++", "20=")
    optable.addtok("postfix:--", "postfix:++")

    # exponentiation
    optable.addtok("infix:**", "19=")

    # symbolic unary
    optable.addtok("prefix:!", "18=")
    optable.addtok("prefix:+", "prefix:!")
    optable.addtok("prefix:-", "prefix:!")
    optable.addtok("prefix:~", "prefix:!")
    optable.addtok("prefix:?", "prefix:!")
    optable.addtok("prefix:*", "prefix:!")
    optable.addtok("prefix:**", "prefix:!")

    # multiplicative
    op = optable.addtok("infix:*", "17=")
    op['pircode'] = '    %r = %0 * %1'
    op = optable.addtok("infix:/", "infix:*")
    op['pircode'] = '    %r = %0 / %1'
    optable.addtok("infix:%", "infix:*")
    optable.addtok("infix:x", "infix:*")
    optable.addtok("infix:xx", "infix:*")
    optable.addtok("infix:+&", "infix:*")
    optable.addtok("infix:+<", "infix:*")

    # additive
    op = optable.addtok("infix:+", "16=")
    op['pircode'] = '    %r = %0 + %1'
    optable.addtok("infix:-", "infix:+")

    # named unary
    optable.addtok("prefix:rand", "13=", "nullterm")
    optable.addtok("prefix:sleep", "prefix:rand", "nullterm")

    # nonchaining binary
    optable.addtok("infix:<=>", "12=")
    optable.addtok("infix:cmp", "infix:<=>")
    optable.addtok("infix:..", "infix:<=>")
    optable.addtok("infix:^..", "infix:<=>")
    optable.addtok("infix:..^", "infix:<=>")
    optable.addtok("infix:^..^", "infix:<=>")

    # chaining binary
    optable.addtok("infix:==", "11=")
    optable.addtok("infix:!=", "infix:==")
    optable.addtok("infix:<=", "infix:==")
    optable.addtok("infix:>=", "infix:==")
    optable.addtok("infix:<", "infix:==")
    optable.addtok("infix:>", "infix:==")

    # tight and
    op = optable.addtok("infix:&&", "10=")

    # tight or
    optable.addtok("infix:||", "09=")
    optable.addtok("infix:^^", "infix:||")
    optable.addtok("infix://", "infix:||")

    # ternary
    optable.addtok("ternary:?? !!", "08=", "right")

    # assignment
    optable.addtok("infix:=", "07=", "right")
    optable.addtok("infix::=", "infix:=", "right")
    optable.addtok("infix:::=", "infix:=", "right")
   
    # list item separator
    op = optable.addtok("infix:,", "06=", "list,nullterm")
    op['subname'] = 'list'

    # list op
    optable.addtok("infix:<==", "05=", "right")
    $P0 = find_global "Perl6::Grammar", "listop"
    op = optable.addtok("prelist:", "infix:<==", "right,nullterm", $P0)

    # pipe forward
    optable.addtok("infix:==>", "04=")

    # loose and
    op = optable.addtok("infix:and", "03=")

    # loose or
    optable.addtok("infix:or", "02=")
    optable.addtok("infix:xor", "infix:or")
    optable.addtok("infix:err", "infix:or")

    # expr terminator
    op = optable.addtok("infix:;", "01=", "nullterm")
    op['pircode'] = "    #"

.end

=item C<opparse(PMC mob)>

The C<opparse> subroutine implements the Perl6::Grammar
<opparse> subrule.  It accepts a match object representing
the current state of the parse, passes the match object
to the operator precedence parser to obtain an expression,
and returns the result to the caller.

=cut

.sub "opparse" 
    .param pmc mob
    .local pmc optable
    optable = find_global "Perl6::Grammar", "$optable"
    $P0 = optable."parse"(mob)
    .return ($P0)
.end

=item C<die(PMC mob, string message)>

The C<die> subroutine implements a special-purpose
<die> subrule.  The <die> subrule outputs a C<message>
indicating why the parse failed, displays the line
number and context of the failure, and returns a
PGE_CUT_MATCH result to abandon the parse altogether.

=cut

.sub "die"
    .param pmc mob
    .param string message      # XXX: TODO: :optional
    .local pmc newfrom
    .local string target
    .local pmc mfrom, mpos
    .local int pos, len, newline, lines
    newfrom = find_global "PGE::Match", "newfrom"
    (mob, target, mfrom, mpos) = newfrom(mob, 0)

    pos = mfrom
    newline = 0
    lines = 1
  newline_loop:
    $I0 = find_cclass .CCLASS_NEWLINE, target, newline, pos
    if $I0 >= pos goto print_message
    newline = $I0 + 1
    inc lines
    goto newline_loop

  print_message:
    print message
    print " at line "
    print lines
    print ", near \""
    $I0 = length target
    $I0 -= pos
    if $I0 < 10 goto print_message_1
    $I0 = 10
  print_message_1:
    $S0 = substr target, pos, $I0
    $S0 = escape $S0
    print $S0
    print "\"\n"
    mpos = -3
    .return (mob)
.end
   
=pod

=back

=cut
