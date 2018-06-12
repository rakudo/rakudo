# This file is not stand-alone as it is part of another file: it will
# be concatenated with src/Perl6/Actions.nqp during the build process.
#
# Files should be concatenated in this order (see
# tools/build/Makefile*[JVM|Moarvm]*in):
#
#   Actions.nqp
#   QActions,nqp
#   RegexActions.nqp
#   P5RegexActions.nqp
#   PodActions.nqp

class Perl6::QActions is HLL::Actions does STDActions {
    # This overrides NQP during the deprecation period for Unicode 1 names not covered by Alias Names
    method charname-panic($/) { $/.panic("Unrecognized character name [$/]") }
    method charname($/) {
        my $codepoint := $<integer>
                         ?? nqp::chr($<integer>.made)
                         !! nqp::getstrfromname(~$/);
        $codepoint := self.charname-notfound($/) if $codepoint eq '';
        make $codepoint;
    }
    method charname-notfound($/) {
        my @worry-text := ( "LINE FEED, NEW LINE, END OF LINE, LF, NL or EOL",
                            "FORM FEED or FF",
                            "CARRIAGE RETURN or CR",
                            "NEXT LINE or NEL" );
        my $text := "Deprecated character name [%s] in lookup of Unicode character by name.\n" ~
                    "Unicode 1 names are deprecated.\nPlease use %s";
        if ~$/ eq "LINE FEED (LF)" {
            $/.worry(nqp::sprintf($text, (~$/, @worry-text[0]) ) );
            return nqp::getstrfromname("LINE FEED");
        }
        if ~$/ eq "FORM FEED (FF)" {
            $/.worry(nqp::sprintf($text, (~$/, @worry-text[1]) ) );
            return nqp::getstrfromname("FORM FEED");
        }
        if ~$/ eq "CARRIAGE RETURN (CR)" {
            $/.worry(nqp::sprintf($text, (~$/, @worry-text[2]) ) );
            return nqp::getstrfromname("CARRIAGE RETURN");
        }
        if ~$/ eq "NEXT LINE (NEL)" {
            $/.worry(nqp::sprintf($text, (~$/, @worry-text[3]) ) );
            return nqp::getstrfromname("NEXT LINE");
        }

        self.charname-panic($/);
    }
    method nibbler($/) {
        my @asts;
        my $lastlit := '';

        for @*nibbles {
            if nqp::istype($_, NQPMatch) {
                if nqp::istype($_.ast, QAST::Node) {
                    if $lastlit ne '' {
                        @asts.push($*W.add_string_constant($lastlit));
                        $lastlit := '';
                    }
                    @asts.push($_.ast.ann('ww_atom')
                        ?? WANTED($_.ast, 'nibbler1')
                        !! QAST::Op.new( :op('callmethod'), :name('Stringy'),  WANTED($_.ast, 'nibbler2') ));
                }
                else {
                    $lastlit := $lastlit ~ $_.ast;
                }
            }
            else {
                $lastlit := $lastlit ~ $_;
            }
        }
        if $lastlit ne '' || !@asts {
            @asts.push($*W.add_string_constant($lastlit));
        }

        my $past := @asts.shift();
        for @asts {
            $past := QAST::Op.new( :op('call'), :name('&infix:<~>'), $past, $_ );
        }

        if nqp::can($/, 'postprocessors') {
            for $/.postprocessors -> $pp {
                $past := self."postprocess_$pp"($/, $past);
            }
        }

        $past.node($/);
        make $past;
    }

    method postprocess_run($/, $past) {
        QAST::Op.new( :name('&QX'), :op('call'), :node($/), $past )
    }

    method postprocess_val($/, $qast) {
        if nqp::istype($qast, QAST::Stmts) && nqp::istype($qast[0], QAST::Op) && $qast[0].name eq '&infix:<,>' { # qw/qqww list
            my @results := [];

            for $qast[0].list -> $thisq {
                if $thisq.has_compile_time_value {
                    try {
                        my $result := $*W.find_symbol(['&val'])($thisq.compile_time_value);
                        $*W.add_object($result);
                        nqp::push(@results, QAST::WVal.new(:value($result), :node($/)));

                        CATCH { nqp::push(@results, $thisq) }
                    }
                } else {
                   nqp::push(@results, QAST::Op.new(:name('&val'), :op('call'), :node($/), $thisq));
                }
            }

            # replace the existing children with what we processed
            $qast[0].set_children(@results);
            $qast[0].annotate("qw",1);
        } elsif $qast.has_compile_time_value { # a single string that we can handle
            try {
                my $result := $*W.find_symbol(['&val'])($qast.compile_time_value);
                $*W.add_object($result);
                $qast := QAST::WVal.new(:value($result));

                CATCH { }
            }
        } else { # no compile time value, resort to the run-time call
            $qast := QAST::Op.new(:name('&val'), :op('call'), :node($/), $qast);
        }

        $qast
    }

    method postprocess_words($/, $past) {
        if $past.has_compile_time_value {
            my @words := HLL::Grammar::split_words($/,
                nqp::unbox_s($past.compile_time_value));
            if +@words != 1 {
                $past := QAST::Op.new( :op('call'), :name('&infix:<,>'), :node($/) );
                for @words { $past.push($*W.add_string_constant(~$_)); }
                $past := QAST::Stmts.new($past);
            }
            else {
                $past := $*W.add_string_constant(~@words[0]);
            }
        }
        else {
            $past := QAST::Op.new( :op('callmethod'), :name('words'), :node($/), $past, QAST::IVal.new( :value(1), :named('autoderef') ) );
        }
        $past
    }

    method postprocess_quotewords($/, $past) {
        my $result := QAST::Op.new( :op('call'), :name('&infix:<,>'), :node($/) );
        sub walk($node) {
            if $node.ann('ww_atom') {
                $result.push($node);
            }
            elsif nqp::istype($node, QAST::Op) && $node.name eq '&infix:<~>' {
                walk($node[0]);
                walk($node[1]);
            }
            # (can't just use postprocess_words here because it introduces spurious comma operations)
            elsif $node.has_compile_time_value {
                my @words := HLL::Grammar::split_words($/,
                    nqp::unbox_s($node.compile_time_value));
                for @words { $result.push($*W.add_string_constant(~$_)); }
            }
            else {
                $result.push(
                    QAST::Op.new(
                        :op('callmethod'),
                        :name('Slip'),
                        QAST::Op.new(
                            :op('callmethod'),
                            :name('words'),
                            :node($/),
                            $node,
                            QAST::IVal.new( :value(1), :named('autoderef') )
                        )
                    )
                );
            }
        }
        walk($past);

        # Strip out list op and possible Slip if only one resulting word
        +@($result) == 1
            ?? nqp::istype($result[0], QAST::Op) && $result[0].name eq 'Slip'
                ?? $result[0][0]
                !! $result[0]
            !! QAST::Stmts.new( $result );
    }

    method postprocess_heredoc($/, $past) {
        QAST::Stmts.new(
            QAST::Op.new( :op<die_s>, QAST::SVal.new( :value("Premature heredoc consumption") ) ),
            $past)
    }

    method escape:sym<\\>($/) { make $<item>.ast; }
    method backslash:sym<qq>($/) { make $<quote>.ast; }
    method backslash:sym<\\>($/) { make $<text>.Str; }
    method backslash:delim ($/) { make $<text>.Str; }
    method backslash:sym<miscq>($/) { make '\\' ~ ~$/; }
    method backslash:sym<misc>($/) { make ~$/; }

    method backslash:sym<a>($/) { make nqp::chr(7) }
    method backslash:sym<b>($/) { make "\b" }
    method backslash:sym<c>($/) { make $<charspec>.ast }
    method backslash:sym<e>($/) { make "\c[27]" }
    method backslash:sym<f>($/) { make "\c[12]" }
    method backslash:sym<n>($/) {
        my str $nl := nqp::unbox_s($*W.find_symbol(['$?NL']));
        if nqp::can($/, 'parsing_heredoc') {
            # In heredocs, we spit out a QAST::SVal here to prevent newlines
            # being taken literally and affecting the dedent.
            make QAST::SVal.new( :value($nl) );
        }
        else {
            make $nl;
        }
    }
    method backslash:sym<o>($/) { make self.ints_to_string( $<octint> ?? $<octint> !! $<octints><octint> ) }
    method backslash:sym<r>($/) {
        make nqp::can($/, 'parsing_heredoc')
            ?? QAST::SVal.new( :value("\r") )
            !! "\r";
    }
    method backslash:sym<rn>($/) {
        make nqp::can($/, 'parsing_heredoc')
            ?? QAST::SVal.new( :value("\r\n") )
            !! "\r\n";
    }
    method backslash:sym<t>($/) {
        make nqp::can($/, 'parsing_heredoc')
            ?? QAST::SVal.new( :value("\t") )
            !! "\t";
    }
    method backslash:sym<x>($/) { make self.ints_to_string( $<hexint> ?? $<hexint> !! $<hexints><hexint> ) }
    method backslash:sym<0>($/) { make "\c[0]" }

    method escape:sym<{ }>($/) {
        my $blast := $<block>.ast;
        $blast.annotate('past_block', WANTALL($blast.ann('past_block'),'escape{}'));
        make QAST::Op.new(
            :op('callmethod'), :name('Stringy'),
            WANTED(
                QAST::Op.new( :op('call'), block_closure($blast), :node($/) ),
                'escape{}'));
    }

    # The next three are currently only used for tr///.
    method escape:ch ($/)     { make ~$/; }
    method escape:sym<..>($/) { make ~$/; }
    method escape:ws ($/)     { make ~$/; }

    method escape:sym<$>($/) { make $<EXPR>.ast; }
    method escape:sym<@>($/) { make $<EXPR>.ast; }
    method escape:sym<%>($/) { make $<EXPR>.ast; }
    method escape:sym<&>($/) { make $<EXPR>.ast; }

    method escape:sym<'>($/) { make mark_ww_atom($<quote>.ast); }
    method escape:sym<colonpair>($/) { make mark_ww_atom($<colonpair>.ast); }
    sub mark_ww_atom($ast) {
        $ast.annotate('ww_atom', 1);
        $ast;
    }
}


# vim: ft=perl6 et sw=4
