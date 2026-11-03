use Test;

plan 8;

# Prefix ~ defers to a Blob type's own Stringy, so the encoding-carrying
# types decode like they do for .Str, .Stringy, interpolation, and infix ~,
# while an encoding-less Blob still refuses.

{
    my $b = 'abc'.encode;
    is ~$b, 'abc', 'prefix ~ on a utf8 decodes';
    isa-ok ~$b, Str, 'prefix ~ on a utf8 gives a Str';
}

is ~'abc'.encode('utf16'), 'abc', 'prefix ~ on a utf16 decodes';

throws-like { ~Buf.new(65) }, X::Buf::AsStr,
    message => rx/"'~'"/,
    'prefix ~ on a Buf still throws, naming the ~ operator';

throws-like { '' ~ Buf.new(65) }, X::Buf::AsStr,
    message => rx/"'~'"/,
    'concatenating a Buf throws, naming the ~ operator';

throws-like { ~Buf.new(65) }, X::Buf::AsStr,
    message => rx/decode/,
    'the error still suggests decode';

throws-like { ~Blob.new(65) }, X::Buf::AsStr,
    'prefix ~ on a Blob still throws';

is 'abc'.encode ~ 'bar', 'abcbar', 'infix ~ with a utf8 still concatenates';

# vim: expandtab shiftwidth=4
