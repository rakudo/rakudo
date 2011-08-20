use Test;
plan 39;
my $r;

=begin foo
=end foo

$r = $=POD[0];
isa_ok $r, Pod::Block, 'returns a Pod Block';
isa_ok $r, Pod::Block::Named, 'returns a named Block';
is $r.name, 'foo', 'name is ok';
is $r.content, [], 'no content, all right';

=begin foo
some text
=end foo

$r = $=POD[1];
isa_ok $r.content[0], Pod::Block::Para;
is $r.content[0].content, "some text", 'the content is all right';
is $r.name, 'foo', 'name is ok';

=begin foo
some
spaced   text
=end foo

$r = $=POD[2];
is $r.name, 'foo', 'name is ok';
is $r.content[0].content,
   "some spaced text", 'additional whitespace removed from the content';

=begin foo
paragraph one

paragraph
two
=end foo
$r = $=POD[3];
is $r.name, 'foo', 'name is ok';
isa_ok $r.content[0], Pod::Block::Para;
isa_ok $r.content[1], Pod::Block::Para;
is $r.content[0].content, "paragraph one", 'paragraphs ok, 1/2';
is $r.content[1].content, "paragraph two", 'paragraphs ok, 2/2';

=begin something
    =begin somethingelse
    toot tooot!
    =end somethingelse
=end something

$r = $=POD[4];
is $r.name, 'something', 'parent name ok';
isa_ok $r.content[0], Pod::Block, "nested blocks work";
isa_ok $r.content[0].content[0], Pod::Block::Para, "nested blocks work";
is $r.content[0].content[0].content, "toot tooot!", "and their content";
is $r.content[0].name, 'somethingelse', 'child name ok';

# Albi
=begin foo
and so,  all  of  the  villages chased
Albi,   The   Racist  Dragon, into the
very   cold   and  very  scary    cave

and it was so cold and so scary in
there,  that  Albi  began  to  cry

    =begin bar
    Dragon Tears!
    =end bar

Which, as we all know...

    =begin bar
    Turn into Jelly Beans!
    =end bar
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

=begin pod

someone accidentally left a space
 
between these two paragraphs

=end pod

$r = $=POD[6];
isa_ok $r, Pod::Block;
is $r.content[0].content, 'someone accidentally left a space',
   'accidental space, 1/2';
is $r.content[1].content, 'between these two paragraphs',
   'accidental space, 2/2';

# various things which caused the spectest to fail at some point
=begin kwid

= DESCRIPTION
bla bla

foo
=end kwid

$r = $=POD[7];
is $r.content[0].content, '= DESCRIPTION bla bla';
isa_ok $r.content[1], Pod::Block::Para;
is $r.content[1].content, 'foo';

=begin more-discussion-needed

XXX: chop(@array) should return an array of chopped strings?
XXX: chop(%has)   should return a  hash  of chopped strings?

=end more-discussion-needed

$r = $=POD[8];
isa_ok $r, Pod::Block;

=begin pod
    =head1 This is a heading block

    This is an ordinary paragraph.
    Its text  will   be     squeezed     and
    short lines filled. It is terminated by
    the first blank line.

    This is another ordinary paragraph.
    Its     text    will  also be squeezed and
    short lines filled. It is terminated by
    the trailing directive on the next line.
        =head2 This is another heading block

        This is yet another ordinary paragraph,
        at the first virtual column set by the
        previous directive
=end pod

$r = $=POD[9];
isa_ok $r.content[0], Pod::Heading;
isa_ok $r.content[1], Pod::Block::Para;
isa_ok $r.content[2], Pod::Block::Para;
isa_ok $r.content[3], Pod::Heading;
isa_ok $r.content[4], Pod::Block::Para;
is $r.content.elems, 5;
