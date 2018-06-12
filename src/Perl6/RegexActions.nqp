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

class Perl6::RegexActions is QRegex::P6Regex::Actions does STDActions {

    method metachar:sym<:my>($/) {
        my $past := $<statement>.ast;
        make QAST::Regex.new( $past, :rxtype('qastnode'), :subtype('declarative') );
    }

    method metachar:sym<{ }>($/) {
        make QAST::Regex.new( $<codeblock>.ast,
                              :rxtype<qastnode>, :node($/) );
    }

    method metachar:sym<qw>($/) {
        my $qast := QAST::Regex.new( :rxtype<alt>, :node($/) );
        my $nib  := $<nibble>.ast[0];
        my @nibs := +@($nib) ?? @($nib) !! [$nib];
        for @nibs {
            unless $_.has_compile_time_value {
                $/.panic("Quote words construct too complex to use in a regex");
            }
            $qast.push(%*RX<i>
                ?? QAST::Regex.new( $_.compile_time_value, :rxtype<literal>, :subtype<ignorecase> )
                !! QAST::Regex.new( $_.compile_time_value, :rxtype<literal> ));
        }
        make $qast;
    }

    method metachar:sym<'>($/) { self.rxquote($/) }

    method rxquote($/) {
        my $quote := $<quote>.ast;
        if $quote.has_compile_time_value {
            my $qast := QAST::Regex.new( :rxtype<literal>, nqp::unbox_s($quote.compile_time_value) );
            if %*RX<i> && %*RX<m> { # >
                $qast.subtype('ignorecase+ignoremark')
            }
            elsif %*RX<i> {
                $qast.subtype('ignorecase')
            }
            elsif %*RX<m> { # >
                $qast.subtype('ignoremark')
            }
            make $qast;
        }
        else {
            make QAST::Regex.new( QAST::NodeList.new(
                                        QAST::SVal.new( :value('!LITERAL') ),
                                        $quote,
                                        QAST::IVal.new( :value(%*RX<i> ?? 1 !! 0) ) ),
                                :rxtype<subrule>, :subtype<method>, :node($/));
        }
    }

    method quantifier:sym<**>($/) {
        my $qast;
        if $<codeblock> {
            $qast := QAST::Regex.new( :rxtype<dynquant>, :node($/),
                QAST::Op.new( :op('callmethod'), :name('DYNQUANT_LIMITS'),
                    QAST::Var.new( :name('$¢'), :scope('lexical') ),
                    $<codeblock>.ast
                ),
            );
        }
        else {
            my $min := 0;
            if $<min> { $min := $<min>.ast; }

            my $max := -1;
            my $upto := $<upto>;

            if $<from> eq '^' { $min++ }

            if ! $<max> {
                $max := $min
            }
            elsif $<max> ne '*' {
                $max := $<max>.ast;
                if $<upto> eq '^' {
                    $max--;
                }

                $/.typed_panic(
                  'X::Syntax::Regex::QuantifierValue', :empty-range
                ) if $min > $max;
            }
            $qast := QAST::Regex.new( :rxtype<quant>, :min($min), :max($max), :node($/) );
        }
        make backmod($qast, $<backmod>);
    }

    sub backmod($ast, $backmod) {
        if $backmod eq ':' { $ast.backtrack('r') }
        elsif $backmod eq ':?' || $backmod eq '?' { $ast.backtrack('f') }
        elsif $backmod eq ':!' || $backmod eq '!' { $ast.backtrack('g') }
        $ast;
    }

    method metachar:sym<rakvar>($/) {
        my $varast := $<var>.ast;
        if nqp::istype($varast, QAST::Var) {
            # See if it's a constant Scalar, in which case we can turn it to
            # a Str and use the value as a literal, so we get LTM.
            if nqp::eqat($varast.name, '$', 0) {
                my $constant;
                try {
                    my $found := $*W.find_symbol([$varast.name]);
                    $constant := $found.Str if nqp::isconcrete($found);
                }
                if nqp::isconcrete($constant) {
                    make self.apply_literal_modifiers(QAST::Regex.new(
                        nqp::unbox_s($constant), :rxtype<literal>, :node($/)
                    ));
                    return;
                }
            }

            # If it's a variable, but statically typed as a string, we know
            # it's a simple interpolation; use LITERAL.
            if nqp::istype($varast.returns, $*W.find_symbol(['Str'])) {
                make QAST::Regex.new(QAST::NodeList.new(
                        QAST::SVal.new( :value('!LITERAL') ),
                        $varast,
                        QAST::IVal.new( :value(%*RX<i> ?? 1 !! 0) )
                    ),
                    :rxtype<subrule>, :subtype<method>, :node($/));
                return;
            }
        }

        if $<var><sigil> eq '%' {
            $<var>.typed_panic('X::Syntax::Reserved', :reserved('use of hash variables in regexes'))
        }

        # Otherwise, slow path that checks what we have.
        make QAST::Regex.new(QAST::NodeList.new(
                QAST::SVal.new( :value('INTERPOLATE') ),
                $varast,
                QAST::IVal.new( :value(%*RX<i> && %*RX<m> ?? 3 !! %*RX<m> ?? 2 !! %*RX<i> ?? 1 !! 0) ),
                QAST::IVal.new( :value(monkey_see_no_eval($/)) ),
                QAST::IVal.new( :value($*SEQ ?? 1 !! 0) ),
                QAST::IVal.new( :value(0) ),
                QAST::Op.new( :op<callmethod>, :name<new>,
                    QAST::WVal.new( :value($*W.find_symbol(['PseudoStash']))),
                )
            ),
            :rxtype<subrule>, :subtype<method>, :node($/));
    }

    method assertion:sym<{ }>($/) {
        make QAST::Regex.new(
                 QAST::NodeList.new(
                    QAST::SVal.new( :value('INTERPOLATE_ASSERTION') ),
                    $<codeblock>.ast,
                    QAST::IVal.new( :value(%*RX<i> && %*RX<m> ?? 3 !! %*RX<m> ?? 2 !! %*RX<i> ?? 1 !! 0) ),
                    QAST::IVal.new( :value(monkey_see_no_eval($/)) ),
                    QAST::IVal.new( :value($*SEQ ?? 1 !! 0) ),
                    QAST::IVal.new( :value(1) ),
                    QAST::Op.new( :op<callmethod>, :name<new>,
                        QAST::WVal.new( :value($*W.find_symbol(['PseudoStash']))),
                    ),
                ),
                 :rxtype<subrule>, :subtype<method>, :node($/));
    }

    method assertion:sym<?{ }>($/) {
        make QAST::Regex.new( $<codeblock>.ast,
                              :subtype<zerowidth>, :negate( 0 ),
                              :rxtype<qastnode>, :node($/) );
    }

    method assertion:sym<!{ }>($/) {
        make QAST::Regex.new( $<codeblock>.ast,
                              :subtype<zerowidth>, :negate( 1 ),
                              :rxtype<qastnode>, :node($/) );
    }

    method assertion:sym<var>($/) {
        if $<arglist> {
            my $ast := make QAST::Regex.new(
                QAST::NodeList.new(
                    QAST::SVal.new( :value('CALL_SUBRULE') ),
                    $<var>.ast ),
                :rxtype<subrule>, :subtype<method>, :node($/));
            for @($<arglist>.ast) {
                $ast[0].push(wanted($_, 'assertvar1'));
            }
        } else {
            make QAST::Regex.new(
                QAST::NodeList.new(
                    QAST::SVal.new( :value('INTERPOLATE_ASSERTION') ),
                    wanted($<var>.ast, 'assertvar2'),
                    QAST::IVal.new( :value(%*RX<i> && %*RX<m> ?? 3 !! %*RX<m> ?? 2 !! %*RX<i> ?? 1 !! 0) ),
                    QAST::IVal.new( :value(monkey_see_no_eval($/)) ),
                    QAST::IVal.new( :value($*SEQ ?? 1 !! 0) ),
                    QAST::IVal.new( :value(1) ),
                    QAST::Op.new( :op<callmethod>, :name<new>,
                        QAST::WVal.new( :value($*W.find_symbol(['PseudoStash']))),
                    ),
                ),
                :rxtype<subrule>, :subtype<method>, :node($/));
        }
    }

    method assertion:sym<name>($/) {
        my $lng := $*W.dissect_longname($<longname>);
        my $qast;
        # We got something like <::($foo)>
        if $lng.contains_indirect_lookup() {
            if $<assertion> {
                if +$lng.components() > 1 {
                    $/.typed_panic('X::Syntax::Regex::Alias::LongName');
                }
                else {
                    # If ever implemented, take care with RESTRICTED
                    $/.typed_panic('X::Syntax::Reserved', :reserved('dynamic alias name in regex'));
                }
            }
            if +$lng.components() > 1 {
                # If ever implemented, take care with RESTRICTED
                $/.typed_panic('X::NYI', :feature('long dynamic name in regex assertion'));
            }
            if $*RESTRICTED {
                $/.typed_panic('X::SecurityPolicy::Eval', :payload($*RESTRICTED));
            }
            $qast := QAST::Regex.new( :rxtype<subrule>, :subtype<method>, :node($/),
                QAST::NodeList.new(QAST::SVal.new( :value('INDMETHOD') ), $lng.name_past()) );
        }
        else {
            my @parts := $lng.components();
            my $name  := @parts.pop();
            my $c     := $/;
            if $<assertion> {
                if +@parts {
                    $c.typed_panic('X::Syntax::Regex::Alias::LongName');
                }
                $qast := $<assertion>.ast;
                if $qast.rxtype eq 'subrule' {
                    self.subrule_alias($qast, $name);
                }
                else {
                    $qast := QAST::Regex.new( $qast, :name($name),
                                              :rxtype<subcapture>, :node($/) );
                }
            }
            elsif !@parts && $name eq 'sym' {
                my str $fullrxname := %*RX<name>;
                my int $loc := nqp::index($fullrxname, ':sym<');
                if $loc < 0 {
                    $c.panic('Can only use <sym> token in a proto regex')
                        if ($loc := nqp::index($fullrxname, ':sym«')) < 0;
                }
                my str $rxname := nqp::substr($fullrxname, $loc + 5, nqp::chars($fullrxname) - $loc - 6);
                $qast := QAST::Regex.new(:name('sym'), :rxtype<subcapture>, :node($/),
                    QAST::Regex.new(:rxtype<literal>, $rxname, :node($/)));
            }
            else {
                if +@parts {
                    my $gref := QAST::WVal.new( :value($*W.find_symbol(@parts)) );
                    $qast := QAST::Regex.new(:rxtype<subrule>, :subtype<capture>,
                                             :node($/), QAST::NodeList.new(
                                                QAST::SVal.new( :value('OTHERGRAMMAR') ),
                                                $gref, QAST::SVal.new( :value($name) )),
                                             :name(~$<longname>) );
                } elsif $*W.regex_in_scope('&' ~ $name) && nqp::substr($c.orig, $/.from - 1, 1) ne '.' {
                    # The lookbehind for . is because we do not yet call $~MAIN's methodop, and our recognizer for
                    # . <assertion>, which is a somewhat bogus recursion, comes from QRegex, not our own grammar.
                    my $coderef := $*W.find_symbol(['&' ~ $name]);
                    my $var := QAST::Var.new( :name('&' ~ $name), :scope<lexical> );
                    $var.annotate('coderef',$coderef);
                    my $c := $var.ann('coderef');
                    $qast := QAST::Regex.new(:rxtype<subrule>, :subtype<capture>,
                                             :node($/), QAST::NodeList.new($var),
                                             :name($name) );
                }
                else {
                    $qast := QAST::Regex.new(:rxtype<subrule>, :subtype<capture>,
                                             :node($/), QAST::NodeList.new(QAST::SVal.new( :value($name) )),
                                             :name($name) );
                }
                if $<arglist> {
                    for $<arglist>.ast.list { $qast[0].push(wanted($_, 'assertname')) }
                }
                elsif $<nibbler> {
                    my $nibbled := $name eq 'after'
                        ?? self.flip_ast($<nibbler>.ast)
                        !! $<nibbler>.ast;
                    my $sub := $/.slang_actions('Regex').qbuildsub($nibbled, :anon(1), :addself(1));
                    $qast[0].push($sub);
                }
            }
        }
        make $qast;
    }

    method assertion:sym<~~>($/) {
        if $<num> {
            $/.panic('Sorry, ~~ regex assertion with a capture is not yet implemented');
        }
        elsif $<desigilname> {
            $/.panic('Sorry, ~~ regex assertion with a capture is not yet implemented');
        }
        else {
            make QAST::Regex.new( :rxtype<subrule>, :subtype<method>,
                QAST::NodeList.new(QAST::SVal.new( :value('RECURSE') )), :node($/) );
        }
    }

    method codeblock($/) {
        make QAST::Stmts.new(
            QAST::Op.new(
                :op('p6store'),
                QAST::Var.new( :name('$/'), :scope<lexical> ),
                QAST::Op.new(
                    QAST::Var.new( :name('$¢'), :scope<lexical> ),
                    :name('MATCH'),
                    :op('callmethod')
                )
            ),
            QAST::Op.new( :op<call>, block_closure($<block>.ast) )
        );
    }

    method arglist($/) {
        my $arglist := $<arglist>.ast;
        make $arglist;
    }

    method create_regex_code_object($block) {
        $*W.create_code_object($block, 'Regex',
            $*W.create_signature(nqp::hash('parameter_objects', [])))
    }

    method store_regex_nfa($code_obj, $block, $nfa) {
        $code_obj.SET_NFA($nfa.save);
    }
}


# vim: ft=perl6 et sw=4
