use v6.c;
use Test;

use Pod::To::Text;

plan 1;

my $ix = -1;

subtest 'Input blocks' => {
    plan 3;

    =begin input
    say 1;

    say 2;
    =end input

    is Pod::To::Text.render($=pod[++$ix]),
        q:to/END/, "Empty lines don't get added spaces";
            say 1;

            say 2;
        END

    =begin input
    my $a = -5;
    say ++$a.=abs;
    # OUTPUT: «6␤»
    =end input

    is Pod::To::Text.render($=pod[++$ix]),
        q:to/END/, "Plain continuation lines are aligned";
            my $a = -5;
            say ++$a.=abs;
            # OUTPUT: «6␤»
        END

    =begin input :allow<B L>
    sub exclaim B<($phrase)> {
        say $phrase L<~> "!!!!"
    }
    exclaim "Howdy, World";
    =end input
    is Pod::To::Text.render($=pod[++$ix]),
        q:to/END/, "Formatting Codes in code block";
            sub exclaim ($phrase) {
                say $phrase ~ "!!!!"
            }
            exclaim "Howdy, World";
        END

=begin comment
    =input
    line 1
    line 2

    is Pod::To::Text.render($=pod[++$ix]),
        q:to/END/, "Simple block with no blank lines";
            line 1
            line 2
        END

    =for input
    line 1
    line 2

    is Pod::To::Text.render($=pod[++$ix]),
        q:to/END/, "Simple block with no blank lines";
            line 1
            line 2
        END
=end comment
}

=begin comment
subtest 'Output blocks' => {
    plan 3;

    =begin output
    say 1;

    say 2;
    =end output

    is Pod::To::Text.render($=pod[++$ix]),
        q:to/END/, "Empty lines don't get added spaces";
            say 1;

            say 2;
        END

    =begin output
    my $a = -5;
    say ++$a.=abs;
    # OUTPUT: «6␤»
    =end output

    is Pod::To::Text.render($=pod[++$ix]),
        q:to/END/, "Plain continuation lines are aligned";
            my $a = -5;
            say ++$a.=abs;
            # OUTPUT: «6␤»
        END

    =begin output :allow<B L>
    sub exclaim B<($phrase)> {
        say $phrase L<~> "!!!!"
    }
    exclaim "Howdy, World";
    =end output
    is Pod::To::Text.render($=pod[++$ix]),
        q:to/END/, "Formatting Codes in code block";
            sub exclaim ($phrase) {
                say $phrase ~ "!!!!"
            }
            exclaim "Howdy, World";
        END

    =output
    line 1
    line 2

    is Pod::To::Text.render($=pod[++$ix]),
        q:to/END/, "Simple block with no blank lines";
            line 1
            line 2
        END
}
=end comment

# vim: expandtab shiftwidth=4 ft=perl6
