use v6;
my $bindir = $*VM<config><bindir> or die 'No bindir found';
my $exe    = $*VM<config><exe>    // '';
my $parrot = "$bindir/parrot$exe";
shell("$parrot t/02-embed/01-load.pir");
