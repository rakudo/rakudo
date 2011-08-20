use Test;
plan 50;
my $r;

=begin pod
This ordinary paragraph introduces a code block:

    $this = 1 * code('block');
    $which.is_specified(:by<indenting>);
=end pod

$r = $=POD[0];
is $r.content[0].content, 'This ordinary paragraph introduces a code block:';
isa_ok $r.content[1], Pod::Block::Code;
is $r.content[1].content.Str, q[$this = 1 * code('block');
$which.is_specified(:by<indenting>);];

# more fancy code blocks
=begin pod
This is an ordinary paragraph

    While this is not
    This is a code block

    =head1 Mumble mumble

    Suprisingly, this is not a code block
        (with fancy indentation too)

But this is just a text. Again

=end pod

$r = $=POD[1];
is $r.content.elems, 5;
is $r.content[0].content, 'This is an ordinary paragraph';
isa_ok $r.content[1], Pod::Block::Code;
is $r.content[1].content, "While this is not\nThis is a code block";
isa_ok $r.content[2], Pod::Block;
is $r.content[2].content[0].content, 'Mumble mumble';
isa_ok $r.content[3], Pod::Block::Para;
is $r.content[3].content, "Suprisingly, this is not a code block"
                        ~ " (with fancy indentation too)";
is $r.content[4].content, "But this is just a text. Again";

=begin pod

Tests for the feed operators

    ==> and <==
    
=end pod

$r = $=POD[2];
is $r.content[0].content, 'Tests for the feed operators';
isa_ok $r.content[1], Pod::Block::Code;
is $r.content[1].content, "==> and <==";

=begin pod
Fun comes

    This is code
  Ha, what now?

 one more block of code
 just to make sure it works
  or better: maybe it'll break!
=end pod

$r = $=POD[3];
is $r.content.elems, 4;
is $r.content[0].content, 'Fun comes';
isa_ok $r.content[1], Pod::Block::Code;
is $r.content[1].content, 'This is code';
isa_ok $r.content[2], Pod::Block::Code;
is $r.content[2].content, 'Ha, what now?';
isa_ok $r.content[3], Pod::Block::Code;
is $r.content[3].content, "one more block of code\n"
                        ~ "just to make sure it works\n"
                        ~ " or better: maybe it'll break!";

=begin pod

=head1 A heading

This is Pod too. Specifically, this is a simple C<para> block

    $this = pod('also');  # Specifically, a code block

=end pod

$r = $=POD[4];
is $r.content.elems, 3;
isa_ok $r.content[0], Pod::Block;
is $r.content[0].content[0].content, 'A heading';
is $r.content[1].content[0],
   'This is Pod too. Specifically, this is a simple ';
isa_ok $r.content[1].content[1], Pod::FormattingCode;
is $r.content[1].content[1].type, 'C';
is $r.content[1].content[1].content, 'para';
is $r.content[1].content[2], ' block';
isa_ok $r.content[2], Pod::Block::Code;
is $r.content[2].content,
   q[$this = pod('also');  # Specifically, a code block];

=begin pod
    this is code

    =for podcast
        this is not

    this is not code either

    =begin itemization
        this is not
    =end itemization

    =begin quitem
        and this is not
    =end quitem

    =begin item
        and this is!
    =end item
=end pod

$r = $=POD[5];
is $r.content.elems, 6;
isa_ok $r.content[0], Pod::Block::Code;
is $r.content[0].content, 'this is code';

isa_ok $r.content[1], Pod::Block::Named;
is $r.content[1].name, 'podcast';
is $r.content[1].content[0].content, 'this is not';

isa_ok $r.content[2], Pod::Block::Para;
is $r.content[2].content, 'this is not code either';

isa_ok $r.content[3], Pod::Block::Named;
is $r.content[3].name, 'itemization';
is $r.content[3].content[0].content, 'this is not';

isa_ok $r.content[4], Pod::Block::Named;
is $r.content[4].name, 'quitem';
is $r.content[4].content[0].content, 'and this is not';

isa_ok $r.content[5].content[0], Pod::Block::Code;
is $r.content[5].content[0].content, 'and this is!';

=begin code
    foo foo
    =begin code
    =end code
=end code

$r = $=POD[6];
isa_ok $r, Pod::Block::Code;
