use v6.e.PREVIEW;
use Test;

plan 2;

my %h{Str;Int};
%h<a>{1} = 42;
ok %h<a>.of === Mu,
    'the innermost values of an untyped hash of two dimensions are Mu';
%h<a>{2} = Mu;
ok %h<a>{2} === Mu,
    'the innermost values of an untyped hash of two dimensions take Mu';

# vim: expandtab shiftwidth=4
