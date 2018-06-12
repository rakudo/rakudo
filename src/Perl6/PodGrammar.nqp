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

# place holder for a new slang for Pod
grammar Perl6::PodGrammar is Perl6::Grammar does STD does MatchPackageNibbler {
}

# vim: ft=perl6 et sw=4
