use v6.c;
use lib <t/spec/packages>;
use Test;

use Pod::To::Text;

plan 2;

my $ix = -1;

subtest 'Code blocks' => {
    plan 3;

    =begin code
    say 1;

    say 2;
    =end code

    is Pod::To::Text.render($=pod[++$ix]),
        q:to/END/, "Empty lines don't get added spaces";
            say 1;

            say 2;
        END

    =begin code
    my $a = -5;
    say ++$a.=abs;
    # OUTPUT: «6␤»
    =end code

    is Pod::To::Text.render($=pod[++$ix]),
        q:to/END/, "Plain continuation lines are aligned";
            my $a = -5;
            say ++$a.=abs;
            # OUTPUT: «6␤»
        END

    =begin code :allow<B L>
    sub exclaim B<($phrase)> {
        say $phrase L<~> "!!!!"
    }
    exclaim "Howdy, World";
    =end code
    is Pod::To::Text.render($=pod[++$ix]),
        q:to/END/, "Formatting Codes in code block";
            sub exclaim ($phrase) {
                say $phrase ~ "!!!!"
            }
            exclaim "Howdy, World";
        END
}

subtest 'Tables' => {
    plan 1;

=begin table
    \+term          | prefix
    term1 \+ term2  | infix
    term\+\+        | postfix
    (term)          | circumfix
    term1[term2]    | postcircumfix
=end table
    is Pod::To::Text.render($=pod[++$ix]),
        q:to/END/, "Final table row is not space-padded";
        +term          prefix
        term1 + term2  infix
        term++         postfix
        (term)         circumfix
        term1[term2]   postcircumfix
        END
}

# vim: expandtab shiftwidth=4 ft=perl6
