use Test;
plan 9;
my $r;

=begin pod
=for comment
foo foo
bla bla    bla

This isn't a comment
=end pod

$r = $=POD[0];
isa_ok $r.content[0], Pod::Block::Comment;
is $r.content[0].content.elems, 1;
is $r.content[0].content, "foo foo\nbla bla    bla\n";

# from S26
=comment
This file is deliberately specified in Perl 6 Pod format

$r = $=POD[1];
isa_ok $r, Pod::Block::Comment;
is $r.content.elems, 1;
is $r.content[0],
   "This file is deliberately specified in Perl 6 Pod format\n";

# this happens to break hilighting in some editors,
# so I put it at the end
=begin comment
foo foo
=begin invalid pod
=as many invalid pod as we want
===yay!
=end comment

$r = $=POD[2];
isa_ok $r, Pod::Block;
is $r.content.elems, 1;
is $r.content[0], "foo foo\n=begin invalid pod\n"
                ~ "=as many invalid pod as we want\n===yay!\n";
