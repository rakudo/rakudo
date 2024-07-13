# This file contains that text highlighting logic Raku Programming Language
# syntax features, using ANSI escape code sequences.

use RakuAST::Deparse::Highlight;

my constant %color =
  black   => "\e[30m",
  blue    => "\e[34m",
  cyan    => "\e[36m",
  green   => "\e[32m",
  magenta => "\e[35m",
  red     => "\e[31m",
  white   => "\e[37m",
  yellow  => "\e[33m",
;
my constant RESET = "\e[0m";

role RakuAST::Deparse::Highlight::Text {
    method hsyn(str $key, str $content) {
        if hsyn-key2color($key) -> $color {
            if %color{$color} -> $ansi-color {
                $ansi-color ~ $content.subst(RESET, $ansi-color, :g) ~ RESET
            }
            else {
                $content
            }
        }
        else {
            $content
        }
    }
}

# vim: expandtab shiftwidth=4
