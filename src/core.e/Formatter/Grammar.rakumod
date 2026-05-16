#-------------------------------------------------------------------------------
# The grammar for parsing format strings.  Intentionally globally visible
# to allow the ecosystem to subclass it if necessary

grammar Formatter::Syntax {
    token TOP { ^ <statement>* $ }

    token statement {
        [
        | <?[%]> [ <directive> || <panic> ]
        | <![%]> <literal>
        ]
    }

    proto token directive { <...> }
    token directive:sym<b> {
        '%' <idx>? <flags>* <size>? <precision>? $<sym>=<[bB]>
    }
    token directive:sym<c> {
        '%' <idx>? <flags>* <size>? <sym>
    }
    token directive:sym<d> {
        '%' <idx>? <flags>* <size>? <precision>? $<sym>=<[dDi]>
    }
    token directive:sym<e> {
        '%' <idx>? <flags>* <size>? <precision>? $<sym>=<[eE]>
    }
    token directive:sym<f> {
        '%' <idx>? <flags>* <size>? <precision>? $<sym>=<[fF]>
    }
    token directive:sym<g> {
        '%' <idx>? <flags>* <size>? <precision>? $<sym>=<[gG]>
    }
    token directive:sym<o> {
        '%' <idx>? <flags>* <size>? <precision>? <sym>
    }
    token directive:sym<O> {
        '%' <idx>? <flags>* <size>? <precision>? <sym>
    }
    token directive:sym<s> {
        '%' <idx>? <flags>* <size>? <precision>? <sym>
    }
    token directive:sym<u> {
        '%' <idx>? <flags>* <size>? <sym>
    }
    token directive:sym<x> {
        '%' <idx>? <flags>* <size>? <precision>? $<sym>=<[xX]>
    }
    token directive:sym<%> { '%%' }

    token panic {
        '%' <idx>? <flags>* <size>? <precision>? $<sym>=.
    }

    token literal { <-[%]>+ }

    token idx { [\d+] '$' }

    token flags { <[\ +0#-]> }

    token size { \d* | $<star>='*' <idx>? }

    token precision { '.' <size>? }
}

# vim: expandtab shiftwidth=4
