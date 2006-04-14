=head1 NAME

main.pir -- code for running Perl 6 from command line

=head1 DESCRIPTION

See Perl6.pir for a synopsis.

=head1 Functions

=over 4

=item C<main(PMC args)>

Handles program control when the Perl6.pbc file is executed
directly from the Parrot command line.  It calls all of
the submodule :load routines, then processes commands from
the file specified on the command line or standard input.

The --target= command line option allows the compilation
to display a parse tree or abstract syntax tree in lieu of
executing program statements.

=cut

.namespace [ 'Perl6' ]

.sub 'main' :main
    .param pmc args
    $P0 = find_global 'Perl6', '__onload'
    $P0()
    $P0 = find_global 'Perl6::Grammar', '__onload_parse'
    $P0()
    $P0 = find_global 'Perl6::PAST', '__onload'
    $P0()

    load_bytecode 'dumper.pbc'
    load_bytecode 'PGE/Dumper.pbc'
    load_bytecode 'Getopt/Obj.pbc'

    .local pmc getopts, opts
    .local string arg0
    arg0 = shift args
    getopts = new 'Getopt::Obj'
    getopts.'notOptStop'(1)
    push getopts, 'target=s'
    push getopts, 'dump-optable'
    push getopts, 'help|h'
    opts = getopts.'get_options'(args)

    $S0 = opts['dump-optable']
    if $S0 goto dump_optable
    $S0 = opts['help']
    if $S0 goto usage

    .local string target
    target = opts['target']

    .local pmc perl6
    perl6 = compreg 'Perl6'

    $I0 = elements args
    if $I0 > 0 goto file_arg

    .local pmc stdin
    stdin = getstdin
    push stdin, 'utf8'

  stmt_loop:
    .local string stmt
    stmt = readline stdin
    unless stmt goto end
    bsr perl6_eval
    goto stmt_loop

  file_arg:
    .local string filename
    filename = args[1]
    $P0 = open filename, '<'
    unless $P0 goto err_no_file
    push $P0, 'utf8'
    stmt = read $P0, 65535
    close $P0
    bsr perl6_eval
    goto end

  perl6_eval:
    $I0 = find_charset 'iso-8859-1'                # XXX: Note 2006-04-14
    trans_charset stmt, $I0                        
    $P0 = perl6(stmt, 'target' => target)
    if target == 'PIR' goto dump_pir
    if target goto dump_object
    $P0()
    ret
  dump_pir:
    print $P0
    ret
  dump_object:
    '_dumper'($P0, target)
    ret

  err_no_file:
    print 'Cannot open file '
    print filename
    print "\n"
    end

  dump_optable:
    $P0 = find_global "Perl6::Grammar", "$optable"
    "_dumper"($P0, "$Perl6::Grammar::optable")
    goto end

  usage:
    print "usage: perl6.pbc [--dump-optable] [--target=OUT] [file]\n"

  end:
.end

=back

=head2 Notes

2006-04-14:  This implementation currently converts program text
to iso-8859-1 instead of leaving things as unicode.  Ideally we'd
like to do everything in unicode, since that's what Perl 6 says
to do, but unfortunately Parrot doesn't know how to perform some
ops (notably C<downcase>) on unicode strings unless ICU is present,
and not everyone has ICU yet.  So, until we get that worked out
in Parrot, we'll compile the program as ISO-8859-1.  

=head1 LICENSE

Copyright (c) 2006 The Perl Foundation

This is free software; you may redistribute it and/or modify
it under the same terms as Parrot.

=cut
