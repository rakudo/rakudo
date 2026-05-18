use Test;

plan 7;

# RakuAST used to refuse `sub f { 7 } #= trailing` at parse time with
# "Strange text after block (missing semicolon or comma?)".  Two
# entangled issues:
#
#   1. `comment:sym<#=>` (and `#|`) captured the trailing `\n`, so
#      `end-statement`'s `<.horizontal-whitespace>? $$ <.ws>` leg
#      couldn't anchor `$$` before the newline once the comment had
#      consumed it.  Legacy's `comment:sym<#=>` deliberately stops
#      before the newline.
#
#   2. When the .ws-consumed comment did finally run its action, the
#      most-recently-set `$*DECLARAND` was the last signature
#      parameter, not the enclosing routine.  `Parameter`'s
#      set-declarand happens after `enter-block-scope`'s set-declarand
#      on the routine stub, so the trailing #= attached to the wrong
#      AST node.

is EVAL(q:to/CODE/), 'trailing',
sub TDD-A { 7 } #= trailing
&TDD-A.WHY.contents.head
CODE
    'trailing #= attaches to a parameterless sub';

is EVAL(q:to/CODE/), 'bdoc',
sub TDD-B($x) { $x } #= bdoc
&TDD-B.WHY.contents.head
CODE
    'trailing #= attaches to a sub past its signature';

is EVAL(q:to/CODE/), 'one two',
sub TDD-C { 7 } #= one
sub TDD-D($x) { $x } #= two
&TDD-C.WHY.contents.head ~ ' ' ~ &TDD-D.WHY.contents.head
CODE
    'trailing #= attaches correctly to a chain of subs';

is EVAL(q:to/CODE/), 'sigdoc',
sub TDD-E($x) #= sigdoc
  { $x }
&TDD-E.WHY.contents.head
CODE
    'trailing #= between signature and body still works';

ok EVAL(q:to/CODE/),
sub TDD-F { 7 } # not a declarator doc
!(try &TDD-F.WHY.contents.defined)
CODE
    'plain trailing `#` comment does not produce a declarator doc';

is EVAL(q:to/CODE/), 'leading',
#| leading
sub TDD-G { 7 }
&TDD-G.WHY.contents.head
CODE
    'leading #| declarator doc still works';

# Guard against the easy over-promotion: a *mid-signature* trailing
# `#=` should stay attached to the Parameter, not get promoted up to
# the enclosing Routine.
is EVAL(q:to/CODE/), 'xdoc:nosubdoc',
sub TDD-H($x #= xdoc
         ) { $x }
(&TDD-H.signature.params[0].WHY ?? &TDD-H.signature.params[0].WHY.contents.head !! 'NIL')
  ~ ':' ~ (&TDD-H.WHY.contents.defined ?? 'subdoc' !! 'nosubdoc')
CODE
    'mid-signature trailing #= stays on the parameter, not on the routine';

# vim: ft=raku
