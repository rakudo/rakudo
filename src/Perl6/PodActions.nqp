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

# place holder for a new slang for Pod
class Perl6::PodActions is Perl6::Actions does STDActions {
}

# vim: ft=perl6 et sw=4
