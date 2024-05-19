use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 1;

# mkdir soft-fails when directory name is too long (over 255 bytes).
my $path = make-temp-dir.add("foobar").add("universe" x 42);
fails-like { $path.mkdir }, X::IO::Mkdir;
