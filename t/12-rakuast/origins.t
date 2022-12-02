use nqp;
use NQPP6QRegex:from<NQP>;
use NQPHLL:from<NQP>;
use Perl6::Compiler:from<NQP>;
use Raku::Grammar:from<NQP>;
use Raku::Actions:from<NQP>;
use Test;

subtest "Bisection" => {
    my $origins;
    my $target = q:to/ORIG/;
my $line = 1;
for ^10 {
#line 10 test.raku
    say "line ", ($line = 2),
        "+++",
        "---",
        "###",
        "^^^";
}
exit(0);
ORIG

    my $comp := Perl6::Compiler.new();
    $comp.language('Raku');
    $comp.parsegrammar(Raku::Grammar);
    $comp.parseactions(Raku::Actions);
    $comp.addstage('syntaxcheck', :before<ast>);
    $comp.addstage('qast', :after<ast>);
    my $root := $comp.compile($target, :target<ast>, :source-name<foo.raku>, :compunit_ok);

    # diag $root.dump();

    multi sub tp(Str:D $needle) {
        $target.index($needle)
    }
    multi sub tp(Regex:D $needle) {
        ($target ~~ $needle).from
    }

    # diag "Target pos Str: " ~ tp('for ^10');
    # diag "Target pos Rx : " ~ tp(/'+",' \n \s+ '"-'/);

    my @tests =
        (tp('my $line'),
            %( :line(1), :orig-line(1), :file("foo.raku"), from => tp('my $line'), # Non-key node
                :WHAT(RakuAST::VarDeclaration::Simple) ),
            %( :line(1), :orig-line(1), :file("foo.raku"), from => tp('my $line'), # Key node
                :WHAT(RakuAST::Statement::Expression) )),
        (tp('for ^10'),
            %( :line(2), :orig-line(2), :file("foo.raku"), from => tp('for ^10'), :WHAT(RakuAST::Statement::For) ),
            %( :line(2), :orig-line(2), :file("foo.raku"), from => tp('for ^10'), :WHAT(RakuAST::Statement::For) )),
        (tp('^10 {'),
            %( :line(2), :orig-line(2), :file("foo.raku"), from => tp('^10 {'), :WHAT(RakuAST::Prefix) ),
            %( :line(2), :orig-line(2), :file("foo.raku"), from => tp('for ^10 {'), :WHAT(RakuAST::Statement::For) )),
        (tp('ine ",'),
            %( :line(10), :orig-line(4), :file("test.raku"), from => tp('"line ",'), :WHAT(RakuAST::QuotedString) ),
            %( :line(10), :orig-line(4), :file("test.raku"), from => tp('say "line'), :WHAT(RakuAST::Statement::Expression) )),
        (tp(', ($line'),
            # The comma belongs to arglist which starts with the first space after `say`
            %( :line(10), :orig-line(4), :file("test.raku"), from => tp(' "line ",'), :WHAT(RakuAST::ArgList) ),
            %( :line(10), :orig-line(4), :file("test.raku"), from => tp('say "line'), :WHAT(RakuAST::Statement::Expression) )),
        (tp('$line = 2'),
            %( :line(10), :orig-line(4), :file("test.raku"), from => tp('$line = 2'), :WHAT(RakuAST::Statement::Expression) ),
            %( :line(10), :orig-line(4), :file("test.raku"), from => tp('say "line'), :WHAT(RakuAST::Statement::Expression) )),
        (tp('++",'),
            %( :line(11), :orig-line(5), :file("test.raku"), from => tp('"+++",'), :WHAT(RakuAST::QuotedString) ),
            %( :line(10), :orig-line(4), :file("test.raku"), from => tp('say "line'), :WHAT(RakuAST::Statement::Expression) )),
        (tp(/',' \n \s+ '"---'/),
            %( :line(10), :orig-line(4), :file("test.raku"), from => tp(' "line ",'), :WHAT(RakuAST::ArgList) ),
            %( :line(10), :orig-line(4), :file("test.raku"), from => tp('say "line'), :WHAT(RakuAST::Statement::Expression) )),
        (tp('--",'),
            %( :line(12), :orig-line(6), :file("test.raku"), from => tp('"---",'), :WHAT(RakuAST::QuotedString) ),
            %( :line(10), :orig-line(4), :file("test.raku"), from => tp('say "line'), :WHAT(RakuAST::Statement::Expression) )),
        (tp('##",'),
            %( :line(13), :orig-line(7), :file("test.raku"), from => tp('"###",'), :WHAT(RakuAST::QuotedString) ),
            %( :line(10), :orig-line(4), :file("test.raku"), from => tp('say "line'), :WHAT(RakuAST::Statement::Expression) )),
        (tp(";\n}"),
            %( :line(2), :orig-line(2), :file("foo.raku"), from => tp(/\n '#line 10'/), :WHAT(RakuAST::StatementList) ),
            %( :line(2), :orig-line(2), :file("foo.raku"), from => tp('for ^10 {'), :WHAT(RakuAST::Statement::For) )),
        (tp('exit(0)'),
            %( :line(16), :orig-line(10), :file("test.raku"), from => tp('exit(0)'), :WHAT(RakuAST::Call::Name) ),
            %( :line(16), :orig-line(10), :file("test.raku"), from => tp('exit(0)'), :WHAT(RakuAST::Statement::Expression) )),
        ;

    plan +@tests;

    for ^@tests -> $test-idx {
        my Int:D $pos = @tests[$test-idx][0];
        # diag "POS: $pos: «" ~ $target.substr($pos, 10) ~ "»";
        subtest "Position " ~ $pos => {
            plan 2;
            for 0,1 -> $key {
                next unless @tests[$test-idx][$key + 1];
                subtest ($key ?? "Key" !! "Non-key") ~ " node" => {
                    my %attrs = @tests[$test-idx][$key + 1];
                    plan %attrs.elems;
                    my $node := $root.locate-node($pos, :$key);
                    my $node-match := $node.origin.as-match;
                    # diag "NODE TYPE: " ~ $node.^name;
                    # diag "LOCATED: " ~ $node-match.line ~ "/" ~ $node-match.orig-line ~ "\n" ~ $node-match;
                    for %attrs.pairs.sort -> (:key($attr), Mu :$value) {
                        given $attr {
                            when 'WHAT' {
                                cmp-ok $node.WHAT, &[~~], $value, "node type";
                            }
                            default {
                                is-deeply $node-match."$attr"(), $value, "attribute $attr";
                            }
                        }
                    }
                }
            }
        }
    }
}