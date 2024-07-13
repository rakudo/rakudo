# This file contains that HTML highlighting logic Raku Programming Language
# syntax features.

use RakuAST::Deparse::Highlight;

role RakuAST::Deparse::Highlight::HTML {
    method hsyn(str $key, str $content) {
        if hsyn-key2color($key) -> $color {
            qq|<span style="color:$color;">$content\</span>|
        }
        else {
            $content
        }
    }
}

# vim: expandtab shiftwidth=4
