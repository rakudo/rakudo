use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 4;

# Regression test for a precomp crash originally observed under the
# RakuAST frontend:
#   Missing serialize REPR function for REPR MVMContext (BOOTContext)
# A `*` or `**` in an attribute default produced an AST node that
# held a reference to the enclosing CompUnit, which transitively
# reaches MVMContext frames with no serialize REPR. The plain `class`
# form below is what crashed in the wild (`zef install
# IO::Socket::Async::SSL`); a leading `unit module` shifted the AST
# layout enough to keep the node out of the SC.

sub precomp-and-load($name, $source, $desc) {
    my $tmp       = make-temp-dir;
    my $mod-store = $tmp.add('module-store');
    $mod-store.mkdir;
    $mod-store.add("$name.rakumod").spurt: $source;

    my $proc = run :out, :err,
        $*EXECUTABLE.absolute,
        '-I', $mod-store.absolute,
        '-e', "use $name; say Foo.new";

    my $err = $proc.err.slurp(:close);
    $proc.out.close;

    is  $proc.exitcode, 0, "$desc: exits cleanly";
    nok $err.contains('MVMContext'),
        "$desc: no MVMContext serialize REPR error";
}

precomp-and-load 'WhateverDefault', q:to/EOF/, 'Whatever attribute default';
#| docs for class Foo
class Foo is export {
    has $.x = *;
}
EOF

precomp-and-load 'HyperWhateverDefault', q:to/EOF/, 'HyperWhatever attribute default';
#| docs for class Foo
class Foo is export {
    has $.x = **;
}
EOF

# vim: expandtab shiftwidth=4
