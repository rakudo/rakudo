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

    $P0 = getclass 'TGE::Grammar'
    $P1 = subclass $P0, 'Perl6::PAST::Grammar'
    $P1 = subclass $P0, 'Perl6::POST::Grammar'

    $P0 = subclass 'PGE::Match', 'Match'
    $P0 = subclass 'Match', 'Grammar'
    $P0 = subclass 'Grammar', 'Perl6::Grammar'
    $P0 = subclass 'ResizablePMCArray', 'Perl6List'

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
    .param string target       :named('target') :optional
    .param int has_target      :opt_flag
    .param int dump            :named('dump') :optional
    .param int has_dump        :opt_flag

    if has_target goto set_dump
    target = 'pbc'
  set_dump:
    if has_dump goto parse
    dump = 0

  parse:
    .local pmc parse
    .local pmc match
    parse = find_global 'Perl6::Grammar', 'program'
    match = parse(code, 'grammar'=>'Perl6::Grammar', 'pos'=>0)

    unless match goto return_match
    unless dump goto parse_1
    '_dumper'(match, 'parse')
  parse_1:
    if target == 'parse' goto return_match

  build_ast:
    .local pmc astgrammar, astbuilder, ast
    astgrammar = new 'Perl6::PAST::Grammar'
    astbuilder = astgrammar.apply(match)
    ast = astbuilder.get('past')
    unless dump goto build_ast_1
    '_dumper'(match, 'PAST')
  build_ast_1:
    if target == 'PAST' goto return_ast

  build_post:
    .local pmc postgrammar, postbuilder, post
    postgrammar = new 'Perl6::POST::Grammar'
    postbuilder = postgrammar.'apply'(ast)
    post = postbuilder.get('root')
    unless dump goto build_post_1
    '_dumper'(match, 'POST')
  build_post_1:
    if target == 'POST' goto return_post

  build_pir:
    .local string pir
    pir = post.'root_pir'()
    unless dump goto build_pir_1
    print pir
  build_pir_1:
    if target == 'PIR' goto return_pir

 compile_pir:
    $P0 = compreg 'PIR'
    $P1 = $P0(pir)
    .return ($P1)

  return_match:
    .return (match)
  return_ast:
    .return (ast)
  return_post:
    .return (post)
  return_pir:
    .return (pir)
.end


.sub 'main' :main
    .param pmc args

    $P0 = compreg 'Perl6'
    .return $P0.'command_line'(args)
.end


.include 'src/parse.pir'

.include 'src/PAST.pir'

.include 'src/POST.pir'

.include 'src/builtins_gen.pir'

.namespace [ 'Perl6::Grammar' ]
.include 'src/grammar_gen.pir'

.namespace [ 'Perl6::PAST::Grammar' ]
.include 'src/pge2past_gen.pir'

.namespace [ 'Perl6::POST::Grammar' ]
.include 'src/past2post_gen.pir'


=back

=cut
