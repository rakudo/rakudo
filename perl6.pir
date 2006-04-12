=head1 TITLE

perl6.pir - A Perl 6 parser (someday a Perl 6 compiler?)

=head2 Description

This is the base file for the Perl 6 parser; eventually
it will likely become the base file for the Perl 6 compiler.

This file simply includes the parsing and grammar rules
from the Perl6/ directory, provides a ":load" routine
to make sure the relevant PGE libraries are loaded,
and then registers a compiler under the name "Perl6".

Just to make sure we aren't fooling anyone -- at the
moment the "Perl6" compiler here only parses Perl 6 code.
But that should change soon.

=cut

.include "lib/parse.pir"

.namespace [ "Perl6::Grammar" ]
.include "lib/grammar.pir"

.namespace [ "Perl6" ]

=over 4

=head2 Functions

=item C<__onload()>

Loads the PGE libraries needed for running the parser,
and registers the "parse" subroutine as the "Perl6"
compiler.

=cut

.sub "__onload" :load
    load_bytecode "PGE.pbc"
    load_bytecode "PGE/Text.pir"

    $I0 = find_type "Perl6::Grammar"
    if $I0 != 0 goto onload_1
    load_bytecode "PGE.pbc"
    $P0 = getclass "PGE::Rule"
    $P0 = subclass $P0, "Perl6::Grammar"
  onload_1:

    $P0 = compreg "PGE::P6Rule"
    $P1 = $P0("^<Perl6::Grammar::program>")
    store_global "Perl6", "&parse", $P1
   
    $P0 = find_global "Perl6", "compile" 
    compreg "Perl6", $P0
.end


=item C<compile(STR code [, 'target' => target])>

Compile the Perl6 C<code>.  The C<target> named parameter 
allows the caller to specify the degree of compilation to 
be performed; a value of C<parse> returns the parse tree, 
C<PAST> returns the abstract syntax tree, C<PIR> returns
the generated PIR code, and other values return the
compiled code as a PMC.

=cut

.sub 'compile'
    .param pmc code
    .param string target       :named('target') :optional
    .param int has_target      :opt_flag

    if has_target goto parse
    target = 'pbc'

  parse:
    .local pmc parse
    .local pmc match
    parse = find_global 'Perl6', '&parse'
    match = parse(code)

    unless match goto return_match
    if target == 'parse' goto return_match
    goto return_match

  build_ast:
    match = match['Perl::Grammar::statement_list']
    .local pmc astgrammar, astbuilder, ast
    astgrammar = new 'ASTGrammar'
    astbuilder = astgrammar.apply(match)
    ast = astbuilder.get('result')
    if target == 'PAST' goto return_ast

  build_pir:
    .local pmc pirgrammar, pirbuilder
    .local string pir
    pirgrammar = new 'PIRGrammar'
    pirbuilder = pirgrammar.'apply'(ast)
    pir = pirbuilder.get('pir')
    if target == 'PIR' goto return_pir

 compile_pir:
    $P0 = compreg "PIR"
    $P1 = $P0(pir)
    .return ($P1)

  return_match:
    .return (match)
  return_ast:
    .return (ast)
  return_pir:
    .return (pir)
.end

.include "lib/main.pir"

=pod

=back

=cut
