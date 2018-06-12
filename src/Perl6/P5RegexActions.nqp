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


class Perl6::P5RegexActions is QRegex::P5Regex::Actions does STDActions {
    method create_regex_code_object($block) {
        $*W.create_code_object($block, 'Regex',
            $*W.create_signature(nqp::hash('parameter_objects', [])))
    }

    method p5metachar:sym<(?{ })>($/) {
        make QAST::Regex.new( $<codeblock>.ast,
                              :rxtype<qastnode>, :node($/) );
    }

    method p5metachar:sym<(??{ })>($/) {
        make QAST::Regex.new(
                 QAST::NodeList.new(
                    QAST::SVal.new( :value($*INTERPOLATION ?? 'INTERPOLATE_ASSERTION' !! 'INTERPOLATE') ),
                    $<codeblock>.ast,
                    QAST::IVal.new( :value(%*RX<i> ?? 1 !! 0) ),
                    QAST::IVal.new( :value(monkey_see_no_eval($/)) ),
                    QAST::IVal.new( :value($*SEQ ?? 1 !! 0) ),
                    QAST::IVal.new( :value($*INTERPOLATION) ),
                    QAST::Op.new( :op<callmethod>, :name<new>,
                        QAST::WVal.new( :value($*W.find_symbol(['PseudoStash']))),
                    ),
                 ),
                 :rxtype<subrule>, :subtype<method>, :node($/));
    }

    method p5metachar:sym<var>($/) {
        make QAST::Regex.new(
                 QAST::NodeList.new(
                    QAST::SVal.new( :value($*INTERPOLATION ?? 'INTERPOLATE_ASSERTION' !! 'INTERPOLATE') ),
                    wanted($<var>.ast, 'p5var'),
                    QAST::IVal.new( :value(%*RX<i> ?? 1 !! 0) ),
                    QAST::IVal.new( :value(monkey_see_no_eval($/)) ),
                    QAST::IVal.new( :value($*SEQ ?? 1 !! 0) ),
                    QAST::IVal.new( :value($*INTERPOLATION) ),
                    QAST::Op.new( :op<callmethod>, :name<new>,
                        QAST::WVal.new( :value($*W.find_symbol(['PseudoStash']))),
                    ),
                 ),
                 :rxtype<subrule>, :subtype<method>, :node($/));
    }

    method codeblock($/) {
        my $blockref := $<block>.ast;
        my $past :=
            QAST::Stmts.new(
                QAST::Op.new(
                    :op('p6store'),
                    QAST::Var.new( :name('$/'), :scope<lexical> ),
                    QAST::Op.new(
                        QAST::Var.new( :name('$¢'), :scope<lexical> ),
                        :name('MATCH'),
                        :op('callmethod')
                    )
                ),
                QAST::Op.new(:op<call>, $blockref)
            );
        make $past;
    }

    method store_regex_nfa($code_obj, $block, $nfa) {
        $code_obj.SET_NFA($nfa.save);
    }
}

# vim: ft=perl6 et sw=4
