use Test;
my $r;

=pod
B<I am a formatting code>

$r = $=POD[0].content[0].content[1];
isa_ok $r, Pod::FormattingCode;
is $r.type, 'B';
is $r.content[0], 'I am a formatting code';

=pod
The basic C<ln> command is: C<ln> B<R<source_file> R<target_file>>

$r = $=POD[1].content[0].content;
is $r[0], 'The basic ';
isa_ok $r[1], Pod::FormattingCode;
is $r[1].type, 'C';
is $r[1].content, 'ln';
is $r[2], ' command is: ';
isa_ok $r[3], Pod::FormattingCode;
is $r[3].type, 'C';
is $r[3].content, 'ln';
isa_ok $r[5], Pod::FormattingCode;
is $r[4], " ";
is $r[5].type, 'B';
$r = $r[5].content;
is $r[0], "";
isa_ok $r[1], Pod::FormattingCode;
is $r[1].type, 'R';
is $r[1].content, 'source_file';
is $r[2], ' ';
isa_ok $r[3], Pod::FormattingCode;
is $r[3].type, 'R';
is $r[3].content, 'target_file';

=pod
L<C<b>|a>
L<C<b>|a>

$r = $=POD[2].content[0].content;
for $r[1], $r[3] -> $link {
    is $link.type, 'L';
    is $link.content[0], '';
    isa_ok $link.content[1], Pod::FormattingCode;
    is $link.content[1].content, 'b';
    is $link.content[2], '|a';
}

=begin pod

=head1 A heading

This is Pod too. Specifically, this is a simple C<para> block

    $this = pod('also');  # Specifically, a code block

=end pod

$r = $=POD[3];
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

=pod V<C<boo> B<bar> asd>

$r = $=POD[4];
is $r.content[0].content, 'C<boo> B<bar> asd';

done;
