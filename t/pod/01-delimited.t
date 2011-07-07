use Test;
my $r;

=begin foo
=end foo

$r = $POD[0];
isa_ok $r, Pod__Block, 'returns a Pod Block';
isa_ok $r, Pod__Block__Named, 'returns a named Block';
is $r.name, 'foo', 'name is ok';
is $r.content, [], 'no content, all right';

=begin foo
some text
=end foo

$r = $POD[1];
is $r.content[0], "some text", 'the content is all right';
is $r.name, 'foo', 'name is ok';

=begin foo
some
spaced   text
=end foo

$r = $POD[2];
is $r.name, 'foo', 'name is ok';
is $r.content[0], "some spaced text", 'additional whitespace removed ' ~
                                      'from the content';

=begin foo
paragraph one

paragraph
two
=end foo
$r = $POD[3];
is $r.name, 'foo', 'name is ok';
is $r.content[0], "paragraph one", 'paragraphs ok, 1/2';
is $r.content[1], "paragraph two", 'paragraphs ok, 2/2';

done;
