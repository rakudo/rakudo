use Test;
plan 28;
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
is $r.content[0], "some text", 'the content is all right';
is $r.name, 'foo', 'name is ok';

=begin foo
some
spaced   text
=end foo

$r = $=POD[2];
is $r.name, 'foo', 'name is ok';
is $r.content[0], "some spaced text", 'additional whitespace removed ' ~
                                      'from the content';

=begin foo
paragraph one

paragraph
two
=end foo
$r = $=POD[3];
is $r.name, 'foo', 'name is ok';
is $r.content[0], "paragraph one", 'paragraphs ok, 1/2';
is $r.content[1], "paragraph two", 'paragraphs ok, 2/2';

=begin something
    =begin somethingelse
    toot tooot!
    =end somethingelse
=end something

$r = $=POD[4];
is $r.name, 'something', 'parent name ok';
isa_ok $r.content[0], Pod::Block, "nested blocks work";
is $r.content[0].content[0], "toot tooot!", "and their content";
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
is $r.content[0],
   'and so, all of the villages chased Albi, The Racist Dragon, ' ~
   'into the very cold and very scary cave',
   '...in the marmelade forest';
is $r.content[1],
   'and it was so cold and so scary in there, that Albi began to cry',
   '...between the make-believe trees';
is $r.content[2].content[0], "Dragon Tears!",
   '...in a cottage cheese cottage';
is $r.content[3], "Which, as we all know...",
   '...lives Albi! Albi!';
is $r.content[4].content[0], "Turn into Jelly Beans!",
   '...Albi, the Racist Dragon';

=begin pod

someone accidentally left a space
 
between these two paragraphs

=end pod

$r = $=POD[6];
isa_ok $r, Pod::Block;
is $r.content[0], 'someone accidentally left a space',
   'accidental space, 1/2';
is $r.content[1], 'between these two paragraphs',
   'accidental space, 2/2';

# various things which caused the spectest to fail at some point
=begin kwid

= DESCRIPTION
bla bla

foo
=end kwid

$r = $=POD[7];
is $r.content[0], '= DESCRIPTION bla bla';
is $r.content[1], 'foo';

=begin more-discussion-needed

XXX: chop(@array) should return an array of chopped strings?
XXX: chop(%has)   should return a  hash  of chopped strings?

=end more-discussion-needed

$r = $=POD[8];
isa_ok $r, Pod::Block;
