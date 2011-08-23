use Test;
plan 12;
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

=begin pod
=for DESCRIPTION :title<presentation template>
=                :author<John Brown> :pubdate(2011)
=end pod

$r = $=POD[3].content[0];
is $r.config<title>, 'presentation template';
is $r.config<author>, 'John Brown';
is $r.config<pubdate>, 2011;
