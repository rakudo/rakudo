=head1 TITLE

perl6doc.pir - The Rakudo Perl 6 document reader.

=head2 Description

This is the base file for the Rakudo Perl 6 document reader, C<perl6doc>.

This file includes the parsing and grammar rules from
the perl6doc/ directory, loads the relevant PGE libraries,
and registers the compiler under the name 'Perl6doc'.

=head2 Functions

=over 4

=item onload()

Creates the Perl 6 document reader by subclassing a C<PCT::HLLCompiler> object.

=cut


.namespace [ 'Perl6doc' ]

.sub 'onload' :load :init :anon
    load_bytecode 'PCT.pbc'
    load_bytecode 'Protoobject.pbc'

    $P0 = get_hll_global 'Protomaker'
    $P1 = get_class ['PCT::HLLCompiler']
    $P0.'new_subclass'($P1, 'Perl6doc')
.end


.sub 'init' :vtable :method
    load_bytecode 'config.pbc'

    self.'language'('Perl6doc')
    self.'parsegrammar'('Perl6doc::Grammar')
    self.'parseactions'('Perl6doc::Grammar::Actions')

    ##  set the compilation stages in the @stages attribute
    $P0 = split ' ', 'parse past post pir evalpmc'
    setattribute self, '@stages', $P0

    ##  set the command line options
    $P0 = split ' ', 'help|h target=s trace|t=s version|V'
    setattribute self, '@cmdoptions', $P0

    ##  set the $usage attribute
    $P0 = new 'String'
    $P0 = <<'USAGE'
Usage: perl6 [switches] [--] [programfile] [arguments]
  -h, --help           display this help text
  --target=[stage]     specify compilation stage to emit
  -t, --trace=[flags]  enable trace flags
  -V, --version        display version information
USAGE
    setattribute self, '$usage', $P0

    ##  set the $version attribute
    .local pmc cfg
    $P0  = new 'String'
    $P0  = 'This is Rakudo perl6doc'
    push_eh _handler
    cfg  = _config()    # currently works in the build tree, but not in the install tree
    $P0 .= ', revision '
    $S0  = cfg['revision']
    $P0 .= $S0
    $P0 .= ' built on parrot '
    $S0  = cfg['VERSION']
    $P0 .= $S0
    $S0  = cfg['DEVEL']
    $P0 .= $S0
    $P0 .= "\n"
    $P0 .= 'for '
    $S0  = cfg['archname']
    $P0 .= $S0
  _handler:
    $P0 .= ".\n\nCopyright 2006-2008, The Perl Foundation.\n"
    setattribute self, '$version', $P0
.end


.namespace ['Perl6doc::Compiler']

=item main(args :slurpy)  :main

Start compilation by passing any command line C<args>
to the Perl 6 compiler.

=cut

.sub 'main' :main
    .param pmc args_str

    ##  create @ARGS global.  We could possibly use the args pmc
    ##  coming directly from Parrot, but currently Parrot provides
    ##  it as a ResizableStringArray and we need Undefs for
    ##  non-existent elements (RSA gives empty strings).
    .local pmc args, iter
    args = new 'ResizableStringArray'
    iter = new 'Iterator', args_str
  args_loop:
    unless iter goto args_end
    $P0 = shift iter
    push args, $P0
    goto args_loop
  args_end:
    set_hll_global '@ARGS', args

    $P0 = compreg 'Perl6doc'
    $P1 = $P0.'command_line'(args)
.end


.include 'perl6doc/gen_grammar.pir'
.include 'perl6doc/gen_actions.pir'


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
