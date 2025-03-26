use Test;
use experimental :macros;

=begin pod
=head1 DESCRIPTION

Tests for macros which return quasi but do not do splicing

See L<S06/"Macros">.

=end pod

plan 7;

# L<S06/Macros>
macro four () { quasi { 2+2 } }

is(four, 4, "macro returning quasi");

#?rakudo skip ':COMPILING flag'
{
    macro hi () { quasi :COMPILING { "hello $s" } }

    macro hey () { ({ "hello $^s" }.body) }

    my $s="world";
    is(hi(),"hello world","macros can bind in caller's lexical env");

    $s="paradise";
    is(hi(),"hello paradise","macros but it's a binding only");
    is(hey(),"hello paradise","macros but it's a binding only");
}

{
    my $x;
    macro noop ()  { $x = "Nothing happened"; quasi { } }
    noop();

    is($x,"Nothing happened", "Macros can return noops");
}


{
    macro hygienic ($ast) {
        my $x = 3;
        quasi { $x + {{{ $ast }}} }
    }
    my $x = 4;
    is hygienic($x), 7, 'lexical vars in macros are not visible to the AST vars';
}

#?rakudo skip 'return from macro'
{
    macro outside-declaration ( $ast ) {
        my $COMPILING::x = 4;
        return quasi { {{{ $ast }}} }
    }
    is outside-declaration( { $x * 2 } ), 8,
       'A macro can declare lexicals that are visible where called';
}

# vim: expandtab shiftwidth=4
