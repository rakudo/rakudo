use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 6;

# `my %h{K}` without an explicit value type is revision-gated:
#   6.c/6.d default the value type to Any
#   6.e defaults it to Mu, so Junctions and other Mu-typed values fit
# An explicit value type combines with the shape as `Hash[Of, K]` on
# either revision. Without a shape the variable stays the generic Hash.

is-run 'my %h{Mu}; print %h.WHAT.^name',
    '6.d default: my %h{Mu} defaults value type to Any',
    :out<Hash[Any,Mu]>;

is-run 'use v6.e.PREVIEW; my %h{Mu}; print %h.WHAT.^name',
    '6.e: my %h{Mu} defaults value type to Mu',
    :out<Hash[Mu,Mu]>;

is-run 'my %h{Str}; print %h.WHAT.^name',
    '6.d default: my %h{Str} defaults value type to Any',
    :out<Hash[Any,Str]>;

is-run 'use v6.e.PREVIEW; my %h{Str}; print %h.WHAT.^name',
    '6.e: my %h{Str} defaults value type to Mu',
    :out<Hash[Mu,Str]>;

is-run 'my Int %h{Str}; print %h.WHAT.^name',
    'my Int %h{Str} keeps the explicit value type on either revision',
    :out<Hash[Int,Str]>;

is-run 'my %h; print %h.WHAT.^name',
    'my %h without shape stays the generic Hash on either revision',
    :out<Hash>;

# vim: expandtab shiftwidth=4
