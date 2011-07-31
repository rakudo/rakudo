use Test;
plan 30;
my $r;

=foo

$r = $=POD[0];
isa_ok $r, Pod::Block, 'returns a Pod6 Block';
isa_ok $r, Pod::Block::Named, 'returns a named Block';
is $r.content, [], 'no content, all right';

=foo some text

$r = $=POD[1];
is $r.content[0].content, "some text", 'the content is all right';

=foo some text
and some more

$r = $=POD[2];
is $r.content[0].content, "some text and some more", 'the content is all right';

=begin pod

=got Inside
got

=bidden Inside
bidden

Outside blocks
=end pod

$r = $=POD[3];
isa_ok $r.content[0], Pod::Block;
is $r.content[0].content[0].content, "Inside got",
   'paragraph block content ok, 1/2';
isa_ok $r.content[1], Pod::Block;
is $r.content[1].content[0].content, "Inside bidden",
   'paragraph block content ok, 1/2';
isa_ok $r.content[2], Pod::Block::Para;
is $r.content[2].content, "Outside blocks",
   'content outside blocks is all right';

# mixed blocks
=begin pod
    =begin one
    one, delimited block
    =end one
    =two two,
    paragraph block
    =for three
    three, still a parablock

    =begin four
    four, another delimited one
    =end four
    =head1 And just for the sake of having a working =head1 :)
=end pod

$r = $=POD[4];
is $r.content[0].content[0].content,
   "one, delimited block", "mixed blocks, 1";
is $r.content[1].content[0].content,
   "two, paragraph block", "mixed blocks, 2";
is $r.content[2].content[0].content,
   "three, still a parablock", "mixed blocks, 3";
is $r.content[3].content[0].content,
   "four, another delimited one", "mixed blocks, 4";
is $r.content[4].content[0].content,
   "And just for the sake of having a working =head1 :)", 'mixed blocks, 5';

=begin foo
and so,  all  of  the  villages chased
Albi,   The   Racist  Dragon, into the
very   cold   and  very  scary    cave

and it was so cold and so scary in
there,  that  Albi  began  to  cry

    =bold Dragon Tears!

Which, as we all know...

    =bold Turn
          into
          Jelly
          Beans!
=end foo

$r = $=POD[5];
isa_ok $r, Pod::Block;
is $r.content.elems, 5, '5 sub-nodes in foo';
is $r.content[0].content,
   'and so, all of the villages chased Albi, The Racist Dragon, ' ~
   'into the very cold and very scary cave',
   '...in the marmelade forest';
is $r.content[1].content,
   'and it was so cold and so scary in there, that Albi began to cry',
   '...between the make-believe trees';
is $r.content[2].content[0].content, "Dragon Tears!",
   '...in a cottage cheese cottage';
is $r.content[3].content, "Which, as we all know...",
   '...lives Albi! Albi!';
is $r.content[4].content[0].content, "Turn into Jelly Beans!",
   '...Albi, the Racist Dragon';

# from S26
=table_not
    Constants 1
    Variables 10
    Subroutines 33
    Everything else 57

$r = $=POD[6];
isa_ok $r, Pod::Block;
is $r.content.elems, 1;
is $r.content[0].content,
   'Constants 1 Variables 10 Subroutines 33 Everything else 57';

=head3
Heading level 3

$r = $=POD[7];
isa_ok $r, Pod::Block;
isa_ok $r, Pod::Heading;
is $r.level, '3';
is $r.content[0].content, 'Heading level 3';
