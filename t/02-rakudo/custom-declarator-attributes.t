use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 7;

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

# A custom declarator can register a companion attribute meta-object under
# `<declarator>-attr`. Attributes in its body are built from that class, so its
# container_initializer and compose take effect. ValueClass relies on this to
# default `@`/`%` attributes and to reject `is rw`.
$tmp.add('CompanionAttrHOW.rakumod').spurt: q:to/EOF/;
class DefaultingAttribute is Attribute {
    method container_initializer(|) { -> { 42 } }
}
class NoRwAttribute is Attribute {
    method compose(|) {
        die "companion attribute rejected rw for { self.name }" if self.rw;
        nextsame;
    }
}
my package EXPORTHOW {
    package DECLARE {
        constant defattr       = Metamodel::ClassHOW;
        constant defattr-attr  = DefaultingAttribute;
        constant norw          = Metamodel::ClassHOW;
        constant norw-attr     = NoRwAttribute;
    }
}
EOF

is-run 'use CompanionAttrHOW; defattr Foo { has $.a }; print Foo.new.a',
    'a companion `-attr` meta-object supplies an attribute default',
    :compiler-args['-I', $tmp.absolute], :out<42>;

is-run 'use CompanionAttrHOW; norw Foo { has $.a is rw }',
    'a companion `-attr` meta-object composes attributes, so it can reject `is rw`',
    :compiler-args['-I', $tmp.absolute],
    :err(rx/'===SORRY!===' .* 'companion attribute rejected rw'/),
    :exitcode(1);

# vim: expandtab shiftwidth=4
