use lib '.';
use Ser;

my $file = 'TEST_FILE'.IO;

my $a = $file.e ?? deserialize($file.slurp) !! [];

say '';
say $a.perl;

#~ $a.unshift: 42; # works
$a.push: 42; # explodes
#~ $a[0]++; # works
#~ $a<foo>++; # works

$file.spurt: serialize($a);
