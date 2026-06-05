use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 5;

my $tmp = make-temp-dir;
$tmp.add('ClassHOWCustom.rakumod').spurt: q:to/EOF/;
my package EXPORTHOW {
    package DECLARE {
        constant mything = Metamodel::ClassHOW;
    }
}
EOF

is-run 'use ClassHOWCustom;
        mything Counter { has $!n = 0; method inc { $!n++ }; method n { $!n } };
        my $c = Counter.new; $c.inc; $c.inc; print $c.n',
    'a custom ClassHOW declarator supports attributes',
    :compiler-args['-I', $tmp.absolute], :out<2>;

$tmp.add('SubclassedClassHOW.rakumod').spurt: q:to/EOF/;
class MetamodelX::DerivedClassHOW is Metamodel::ClassHOW { }
my package EXPORTHOW {
    package DECLARE {
        constant derived = MetamodelX::DerivedClassHOW;
    }
}
EOF

is-run 'use SubclassedClassHOW;
        derived Counter { has $!n = 0; method bump { $!n++ }; method n { $!n } };
        my $c = Counter.new; $c.bump; print $c.n',
    'a HOW that subclasses ClassHOW (the OO::Monitors shape) supports attributes',
    :compiler-args['-I', $tmp.absolute], :out<1>;

$tmp.add('ModuleHOWCustom.rakumod').spurt: q:to/EOF/;
my package EXPORTHOW {
    package DECLARE {
        constant mymod = Metamodel::ModuleHOW;
    }
}
EOF

is-run 'use ModuleHOWCustom; mymod Foo { has $!x }',
    'a non-Attachable HOW emits the typed error for a `has` declaration',
    :compiler-args['-I', $tmp.absolute],
    :err(rx/'===SORRY!===' .* "A mymod cannot have attributes, but you tried to declare '\$!x'"/),
    :exitcode(1);

is-run 'use ModuleHOWCustom; mymod Foo { method m { $!x } }',
    'a non-Attachable HOW emits a compile-time error for a stray attribute usage',
    :compiler-args['-I', $tmp.absolute],
    :err(rx/'===SORRY!===' .* '$!x'/),
    :exitcode(1);

$tmp.add('RoleHOWCustom.rakumod').spurt: q:to/EOF/;
my package EXPORTHOW {
    package DECLARE {
        constant myrole = Metamodel::ParametricRoleHOW;
    }
}
EOF

is-run 'use RoleHOWCustom; myrole Foo { has $!x }; print "ok"',
    'a ParametricRoleHOW-backed custom declarator does not crash on attribute usage',
    :compiler-args['-I', $tmp.absolute], :out<ok>;

# vim: expandtab shiftwidth=4
