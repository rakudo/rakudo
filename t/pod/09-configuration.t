use Test;
plan 9; # ha ha
my $r;

=begin pod
    =begin code :allow<B>
    =end code
=end pod

$r = $=POD[0].content[0];
isa_ok $r, Pod::Block::Code;
is $r.config<allow>, 'B';

=begin pod
    =config head2  :like<head1> :formatted<I>
=end pod

$r = $=POD[1].content[0];
isa_ok $r, Pod::Config;
is $r.type, 'head2';
is $r.config<like>, 'head1';
is $r.config<formatted>, 'I';

=begin pod
    =pod :number(42) :zebras :!sheep
=end pod

$r = $=POD[2].content[0];
is $r.config<number>, 42;
is $r.config<zebras>.Bool, True;
is $r.config<sheep>.Bool, False;
