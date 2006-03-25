=head1 TITLE

p6shell.pir - Program to exercise the Perl 6 parser

=head2 SYNOPSIS

    parrot p6shell.pir [file]

=head2 Description

This is a simple program that exercises the Perl 6 parser
in perl6.pbc.  If given a file argument on the command line,
it will run the Perl 6 parser on the file and dump the
resulting parse tree.

If no file is provided, then p6shell enters an interactive
mode, reading Perl 6 statements from the command line and
dumping the resulting parse trees to the standard output.

=cut

.sub main :main
    .param pmc args
    .local pmc p6compile
    .local string code

    load_bytecode "PGE.pbc"
    load_bytecode "perl6.pbc"
    load_bytecode "dumper.pir"
    load_bytecode "PGE/Dumper.pir"
    load_bytecode "Getopt/Obj.pir"

    p6compile = compreg "Perl6"

    .local pmc getopts, opts
    .local string arg0
    arg0 = shift args
    getopts = new "Getopt::Obj"
    getopts."notOptStop"(1)
    push getopts, "dump-optable"
    push getopts, "help|h"
    opts = getopts."get_options"(args)

    $S0 = opts["dump-optable"]
    if $S0 goto dump_optable
    $S0 = opts["help"]
    if $S0 goto usage

    $I0 = elements args
    if $I0 > 1 goto usage
    if $I0 < 1 goto read_stdin

    .local string filename
    .local pmc filehandle
    filename = args[1]
    filehandle = open filename, "<"
    code = read filehandle, 65535
    close filehandle

    $P0 = p6compile(code)
    unless $P0 goto end
    "_dumper"($P0)
    goto end

  read_stdin:
    .local pmc stdin
    stdin = getstdin

  stdin_loop:
    code = readline stdin
    if code == "" goto end
    $P0 = p6compile(code)
    unless $P0 goto end
    "_dumper"($P0)
    goto stdin_loop

  dump_optable:
    print "$Perl6::Grammar::optable :\n"
    $P0 = find_global "Perl6::Grammar", "$optable"
    "_dumper"($P0)
    goto end

  usage:
    print "usage: p6shell.pir [--dump-optable] [file]\n"

  end:
.end
