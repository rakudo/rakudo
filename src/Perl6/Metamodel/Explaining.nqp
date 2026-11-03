#- Metamodel::Explaining  ------------------------------------------------------
# Helper class for "will complain"
role Perl6::Metamodel::Explaining {
    has $!complainee;

    method complainee() { $!complainee }
    method SET-COMPLAINEE($complainee) { $!complainee := $complainee }
}

# vim: expandtab sw=4
