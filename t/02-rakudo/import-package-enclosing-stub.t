use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 7;

# A unit whose package block imports the class its package name is rooted
# in, as `class Pkg::Lite { use Pkg }` does, keeps its own stub for that name
# in its GLOBAL. A unit importing it then declares its own packages in that
# stub rather than in the class of another compilation unit. Otherwise each
# precompiled unit in such a chain records the class's stash as it saw it,
# and loading the chain restores the stash to what the last unit saw.

my $mod-store = make-temp-dir;
$mod-store.add('StubRoot.rakumod').spurt: "unit class StubRoot;\nmethod hi \{ 'hi' }";
$mod-store.add('StubRoot').mkdir;
$mod-store.add('StubRoot/Base.rakumod').spurt: "use StubRoot;\nunit class StubRoot::Base;";
$mod-store.add('StubRoot/Other').mkdir;
$mod-store.add('StubRoot/Other/Leaf.rakumod').spurt: 'unit class StubRoot::Other::Leaf;';
$mod-store.add('StubRoot/Lite.rakumod').spurt: 'class StubRoot::Lite { use StubRoot; method hi { StubRoot.hi } }';
$mod-store.add('StubRoot/Unit.rakumod').spurt: "unit class StubRoot::Unit;\nuse StubRoot;";
$mod-store.add('StubRoot/UnitTop.rakumod').spurt: "unit module StubRoot::UnitTop;\nuse StubRoot::Unit;";
$mod-store.add('StubRoot/Middle.rakumod').spurt: "unit module StubRoot::Middle;\nuse StubRoot::Lite;";
$mod-store.add('StubRoot/Top.rakumod').spurt: "unit module StubRoot::Top;\nuse StubRoot::Middle;";
my @compiler-args = '-I', $mod-store.absolute;

my $code = q:to/CODE/;
    use StubRoot::Base;
    use StubRoot::Other::Leaf;
    require ::('StubRoot::Top');
    print StubRoot.WHO.keys.sort.join(' ')
    CODE

is-run $code, :@compiler-args, :out('Base Lite Other'),
    'loading the chain keeps the symbols the class already had';
is-run $code, :@compiler-args, :out('Base Lite Other'),
    'loading the precompiled chain keeps the symbols the class already had';

is-run 'use StubRoot::Middle; print StubRoot::Middle.^name, " ", StubRoot::Lite.^name',
    :@compiler-args, :out('StubRoot::Middle StubRoot::Lite'),
    'the packages of the chain are reachable by qualified name';

is-run 'use StubRoot; use StubRoot::Top; print StubRoot::Top.^name, " ", StubRoot.^name',
    :@compiler-args, :out('StubRoot::Top StubRoot'),
    'a unit importing both the class and the chain sees both';

is-run 'use StubRoot::Lite; print StubRoot::Lite.hi',
    :@compiler-args, :out<hi>,
    'the imported class is the lexical inside the package that imports it';

is-run 'use StubRoot::Lite; print StubRoot.^name, " ", StubRoot.WHO.keys.sort.join(" ")',
    :@compiler-args, :out('StubRoot Lite'),
    'a program importing only the package sees the stub it declared';

is-run q:to/CODE/, :@compiler-args, :out('Base Other Unit'),
    use StubRoot::Base;
    use StubRoot::Other::Leaf;
    require ::('StubRoot::UnitTop');
    print StubRoot.WHO.keys.sort.join(' ')
    CODE
    'loading a chain through a unit class keeps the symbols the class already had';

# vim: expandtab shiftwidth=4
