use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 1;

# A `{ :pair, :pair }` block-or-hash inside a routine that has a `#|`
# declarator-doc comment used to crash precomp with
# "Serialization Error: missing static code ref for closure". The
# discarded Block AST had run IMPL-STUB-CODE, leaving an orphan
# freshcoderef on its Code object; the declarator-doc compose at
# check time pulled that Code object into the SC, breaking
# serialization.

my $tmp = make-temp-dir;
$tmp.add('OrphanStubRepro.rakumod').spurt: q:to/EOF/;
sub helper { 1 }
#| docs
sub trigger { my %h := { a => helper() } }
EOF

is-run 'use OrphanStubRepro; print "ok"',
    'precomp of `{ :pair }` block-or-hash inside a #|-documented sub does not orphan a stub',
    :compiler-args['-I', $tmp.absolute], :out<ok>;

# vim: expandtab shiftwidth=4
