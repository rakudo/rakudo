use Test;

plan 2;

# A stub's yada bit is set before its traits run, so a trait constrained with
# `where { .yada }` matches the stub and applies. LibXML's `is dom-boxed` is
# such a trait.

multi trait_mod:<is>(Method:D $m where { .yada }, :$replace-method!) {
    $m.wrap: method { 'method-replaced' };
}
multi trait_mod:<is>(Sub:D $s where { .yada }, :$replace-sub!) {
    $s.wrap: sub { 'sub-replaced' };
}

class Widget {
    method make(--> Str) is replace-method {...}
}
sub build(--> Str) is replace-sub {...}

is Widget.new.make, 'method-replaced', 'a where {.yada} trait applies to a stub method';
is build(),         'sub-replaced',    'a where {.yada} trait applies to a stub sub';
