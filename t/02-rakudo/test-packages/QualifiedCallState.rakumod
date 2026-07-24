unit module QualifiedCallState;

my $state = 0;
our sub set-state($v) { $state = $v }
our sub get-state()   { $state      }
our sub whoami()      { &?ROUTINE.WHERE }
