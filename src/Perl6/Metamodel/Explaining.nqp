role Perl6::Metamodel::Explaining {
    has $!complainee;

    method complainee() { $!complainee }
    method SET-COMPLAINEE($complainee) { $!complainee := $complainee }
}
