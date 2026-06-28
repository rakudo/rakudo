use Test;

plan 4;

# A label records the source location and surrounding text of its declaration.

FOO: my $foo-line = $?LINE;
is FOO.line, $foo-line, 'a label reports the source line of its declaration';
is FOO.name, 'FOO', 'a label reports its name';

# The 20-character window after the name shows up in the gist's postmatch.
ok FOO.gist.contains(': my $foo-line'),
    'a label gist includes the text following the label name';

# A second label on a later line reports a different line.
BAR: my $bar-line = $?LINE;
isnt BAR.line, FOO.line, 'distinct labels report distinct source lines';

# vim: expandtab shiftwidth=4
