# This file is not stand-alone as it is part of another file: it will
# be concatenated with src/Perl6/Grammar.nqp during the build process.
#
# Files should be concatenated in this order (see
# tools/build/Makefile*[JVM|Moarvm]*in):
#
#   Grammar.nqp
#   QGrammar,nqp
#   RegexGrammar.nqp
#   P5RegexGrammar.nqp
#   PodGrammar.nqp

grammar Perl6::P5RegexGrammar is QRegex::P5Regex::Grammar does STD does MatchPackageNibbler {
    method nibbler() {
        self.nibble-in-cursor(QRegex::P5Regex::Grammar)
    }

    token rxstopper { <stopper> }

    token p5metachar:sym<(?{ })> {
        '(?' <?[{]> <codeblock> ')'
    }

    token p5metachar:sym<(??{ })> {
        '(??' <?[{]> <codeblock> ')'
    }

    token p5metachar:sym<var> {
        <?[$]> <var=.LANG('MAIN', 'variable')>
    }

    token codeblock {
        :my $*ESCAPEBLOCK := 1;
        <block=.LANG('MAIN','block')>
    }
}


# vim: ft=perl6 et sw=4
