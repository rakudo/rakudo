use v6;
my $parrot = %*ENV{'PARROT'};
run("$parrot t/02-embed/01-load.pir");
