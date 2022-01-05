use v6.c;
use Test;

use Pod::To::Text;

plan 18;

my $r;
my $rp;
my $p = -1;

# explicit code blocks
{
=begin code
say 1;
say 2;
=end code
$r = $=pod[++$p];
isa-ok $r, Pod::Block::Code;
# code blocks should get indented 4 spaces by Pod::To::Text
$rp = Pod::To::Text.render($r),
is $rp,
q:to/END/;
    say 1;
    say 2;
END

=code
say 1;
say 2;

$r = $=pod[++$p];
isa-ok $r, Pod::Block::Code;
# code blocks should get indented 4 spaces by Pod::To::Text
$rp = Pod::To::Text.render($r),
is $rp,
q:to/END/;
    say 1;
    say 2;
END

=for code
say 1;
say 2;

$r = $=pod[++$p];
isa-ok $r, Pod::Block::Code;
# code blocks should get indented 4 spaces by Pod::To::Text
$rp = Pod::To::Text.render($r),
is $rp,
q:to/END/;
    say 1;
    say 2;
END
}

# implicit code blocks: input
{
=begin input
say 1;
say 2;
=end input
$r = $=pod[++$p];
isa-ok $r, Pod::Block::Code;
# code blocks should get indented 4 spaces by Pod::To::Text
$rp = Pod::To::Text.render($r),
is $rp,
q:to/END/;
    say 1;
    say 2;
END

=input
say 1;
say 2;

$r = $=pod[++$p];
isa-ok $r, Pod::Block::Code;
# code blocks should get indented 4 spaces by Pod::To::Text
$rp = Pod::To::Text.render($r),
is $rp,
q:to/END/;
    say 1;
    say 2;
END

=for input
say 1;
say 2;

$r = $=pod[++$p];
isa-ok $r, Pod::Block::Code;
# code blocks should get indented 4 spaces by Pod::To::Text
$rp = Pod::To::Text.render($r),
is $rp,
q:to/END/;
    say 1;
    say 2;
END
}

# implicit code blocks: output
{
=begin output
say 1;
say 2;
=end output
$r = $=pod[++$p];
isa-ok $r, Pod::Block::Code;
# code blocks should get indented 4 spaces by Pod::To::Text
$rp = Pod::To::Text.render($r),
is $rp,
q:to/END/;
    say 1;
    say 2;
END

=output
say 1;
say 2;

$r = $=pod[++$p];
isa-ok $r, Pod::Block::Code;
# code blocks should get indented 4 spaces by Pod::To::Text
$rp = Pod::To::Text.render($r),
is $rp,
q:to/END/;
    say 1;
    say 2;
END

=for output
say 1;
say 2;

$r = $=pod[++$p];
isa-ok $r, Pod::Block::Code;
# code blocks should get indented 4 spaces by Pod::To::Text
$rp = Pod::To::Text.render($r),
is $rp,
q:to/END/;
    say 1;
    say 2;
END
}

# vim: expandtab shiftwidth=4
