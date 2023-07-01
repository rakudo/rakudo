# This file contains that HTML highlighting logic Raku Programming Language
# syntax features.
#
# Please check the lib/RakuAST/Deparse/Highlight/CORE.rakumod module
# for an overview of the types that can be expected.

unit role RakuAST::Deparse::Highlight::HTML;

# Implement basic HTML highlighting by embedding the given type and
# content varbatim.  If this proves to provide too much information,
# any logic inside "hsyn" method in another role can be used to
# provide a simpler subset of classes inside the <span> container.
method hsyn(str $type, str $content) {
    qq|<span class="raku-$type">$content\</span>|
}

# vim: expandtab shiftwidth=4
