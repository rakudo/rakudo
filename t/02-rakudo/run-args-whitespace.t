use Test;

plan 1;

# On Windows, arguments are joined into one command line and must be
# quoted so that the child splits them back into the same arguments.
# Tabs, newlines, vertical tabs and empty arguments need quoting too.

my @args = "a\tb", "c\nd", "e\x0Bf", "", "g";
my $proc = run $*EXECUTABLE, '-e', 'print @*ARGS.raku', |@args, :out;
is $proc.out.slurp(:close), @args.raku,
  'arguments with tabs, newlines and empty arguments arrive unchanged';

# vim: expandtab shiftwidth=4
