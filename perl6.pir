=head1 TITLE

perl6.pir - A Perl 6 compiler.

=head2 Description

This is the base file for the Perl 6 compiler.

This file includes the parsing and grammar rules from
the src/ directory, loads the relevant PGE libraries,
and registers the compiler under the name 'Perl6'.

=head2 Functions

=over 4

=item C<__onload()>

Loads the PGE libraries needed for running the parser,
and registers the "compile" subroutine as the "Perl6"
compiler.

=cut

.namespace [ 'Perl6' ]

.loadlib 'perl6_group'

.sub '__onload' :load :init
    load_bytecode 'PGE.pbc'
    load_bytecode 'PGE/Text.pbc'
    load_bytecode 'PGE/Util.pbc'
    load_bytecode 'TGE.pbc'
    load_bytecode 'Parrot/HLLCompiler.pbc'
    load_bytecode 'PAST-pm.pbc'

    $P0 = getclass 'TGE::Grammar'
    $P1 = subclass $P0, 'Perl6::PAST::Grammar'

    $P0 = subclass 'PGE::Match', 'Match'
    $P0 = subclass 'Match', 'Grammar'
    $P0 = subclass 'Grammar', 'Perl6::Grammar'

    $P0 = find_global 'Perl6', 'compile'
    $P1 = new [ 'HLLCompiler' ]
    $P1.'register'('Perl6', $P0)

    ##   XXX: this is a scaffold to map Perl 6 types into
    ##   the appropriate Parrot class.  We'll likely
    ##   eliminate this when we can map classnames more easily.
    $P0 = new .Hash
    $P0['Str'] = '.Perl6Str'
    $P0['Any'] = '_'
    store_global 'Perl6', '%!parrotclass', $P0
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
    .param pmc adverbs         :slurpy :named

    .local string target
    target = adverbs['target']
    target = downcase target

  parse:
    .local pmc parse
    .local pmc match
    parse = find_global 'Perl6::Grammar', 'program'
    match = parse(code, 'grammar'=>'Perl6::Grammar', 'pos'=>0)

    unless match goto return_match
    if target == 'parse' goto return_match

  build_ast:
    .local pmc astgrammar, astbuilder, ast
    astgrammar = new 'Perl6::PAST::Grammar'
    astbuilder = astgrammar.apply(match)
    ast = astbuilder.get('past')
    .return ast.'compile'(adverbs :flat :named)

  return_match:
    .return (match)
.end


.sub 'main' :main
    .param pmc args

    load_bytecode 'PGE/Dumper.pbc'

    $P0 = compreg 'Perl6'
    .return $P0.'command_line'(args)
.end


.include 'src/parse.pir'

.include 'src/builtins_gen.pir'

.namespace [ 'Perl6::Grammar' ]
.include 'src/grammar_gen.pir'

.namespace [ 'Perl6::PAST::Grammar' ]
.include 'src/parse2past_gen.pir'

=back

=cut
