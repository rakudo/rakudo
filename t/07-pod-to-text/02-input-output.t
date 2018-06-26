use v6.c;
use Test;

use Pod::To::Text;

plan 1;

my $r:
my $p = -1;

subtest 'Input blocks' => {
    plan 3;

    =begin input
    say 1;

    say 2;
    =end input

    $r = $=pod[++$p];
    is Pod::To::Text.render($r),
        q:to/END/, "Empty lines don't get added spaces";
            say 1;

            say 2;
        END

    =begin input
    my $a = -5;
    say ++$a.=abs;
    # OUTPUT: «6␤»
    =end input

    is Pod::To::Text.render($=pod[++$p]),
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
    is Pod::To::Text.render($=pod[++$p]),
        q:to/END/, "Formatting Codes in code block";
            sub exclaim ($phrase) {
                say $phrase ~ "!!!!"
            }
            exclaim "Howdy, World";
        END

    =input
    line 1
    line 2

    my $r = $=pod[++$p];
    my $fp = open 't.test', :w;
    $fp.say: '# pod type:';
    my $t = $r.WHAT;
    $fp.say: $t;
    $fp.say: '# .perl:';
    $fp.say: $r.perl;
    $fp.say: '# render:';
    $fp.print(Pod::To::Text.render($r));

    =begin input
    line 1
    line 2
    =end input
    $r = $=pod[++$p];
    $fp.say: '# .perl:';
    $fp.say: $r.perl;
    $fp.say: '# render:';
    $fp.print(Pod::To::Text.render($r));

    $fp.close;
=begin comment
    is Pod::To::Text.render($=pod[++$p]),
        q:to/END/, "Simple block with no blank lines";
            line 1
            line 2
        END

    =for input
    line 1
    line 2

    is Pod::To::Text.render($=pod[++$p]),
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

    is Pod::To::Text.render($=pod[++$p]),
        q:to/END/, "Empty lines don't get added spaces";
            say 1;

            say 2;
        END

    =begin output
    my $a = -5;
    say ++$a.=abs;
    # OUTPUT: «6␤»
    =end output

    is Pod::To::Text.render($=pod[++$p]),
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
    is Pod::To::Text.render($=pod[++$p]),
        q:to/END/, "Formatting Codes in code block";
            sub exclaim ($phrase) {
                say $phrase ~ "!!!!"
            }
            exclaim "Howdy, World";
        END

    =output
    line 1
    line 2

    is Pod::To::Text.render($=pod[++$p]),
        q:to/END/, "Simple block with no blank lines";
            line 1
            line 2
        END
}
=end comment

# return type of pod
    given $pod {
        when Pod::Heading      { heading2text($pod)             }
        when Pod::Block::Code  { code2text($pod)                }
        when Pod::Block::Named { named2text($pod)               }
        when Pod::Block::Para  { twrap( $pod.contents.map({pod2text($_)}).join("") ) }
        when Pod::Block::Table { table2text($pod)               }
        when Pod::Block::Declarator { declarator2text($pod)     }
        when Pod::Item         { item2text($pod).indent(2)      }
        when Pod::FormattingCode { formatting2text($pod)        }
        when Positional        { .flat».&pod2text.grep(?*).join: "\n\n" }
        when Pod::Block::Comment { '' }
        when Pod::Config       { '' }
        default                { $pod.Str                       }
    }

sub pod-type($pod) {
    my $t = 'unknown';
    given $pod {
        when Pod::Block::Code { return 'Pod::Block::Code' }
        default { 
    }
}
# vim: expandtab shiftwidth=4 ft=perl6
