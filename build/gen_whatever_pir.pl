#!/usr/bin/perl
# Copyright (C) 2008-2009, The Perl Foundation.
# $Id$

use strict;
use warnings;

my @ops = qw(
  infix:** infix:* infix:/ infix:% infix:div infix:mod infix:-
  infix:== infix:!= infix:<  infix:>  infix:<= infix:>= infix:<=>
  infix:.. infix:^.. infix:..^ infix:^..^
  prefix:+ prefix:- prefix:~ prefix:? prefix:! prefix:^
);

for (@ops) {
    if (/^infix:/) {
        print qq{
            .namespace []
            .sub '$_' :multi('Whatever', _)
                .param pmc x
                .param pmc y
                .tailcall '!whatever_helper'('$_', x, y)
            .end
            .sub '$_' :multi(_, 'Whatever')
                .param pmc x
                .param pmc y
                .tailcall '!whatever_helper'('$_', x, y)
            .end
            .sub '$_' :multi('WhateverCode', _)
                .param pmc x
                .param pmc y
                .tailcall '!whatever_helper'('$_', x, y)
            .end
            .sub '$_' :multi(_, 'WhateverCode')
                .param pmc x
                .param pmc y
                .tailcall '!whatever_helper'('$_', x, y)
            .end
        };
    }
    else {
        print qq{
            .namespace []
            .sub '$_' :multi('Whatever')
                .param pmc x
                .tailcall '!whatever_helper'('$_', x)
            .end
            .sub '$_' :multi('WhateverCode', _)
                .param pmc x
                .param pmc y
                .tailcall '!whatever_helper'('$_', x, y)
            .end
        };
    }
}

print q{
    .namespace []
    .sub '!whatever_helper' :anon
        .param string opname
        .param pmc left
        .param pmc right       :optional
        .local pmc opfunc
        opfunc = find_name opname
        .lex '$opfunc', opfunc
        .lex '$left', left
        .lex '$right', right
        .const 'Sub' $P0 = '!whatever_closure'
        $P1 = newclosure $P0
        '!fixup_routine_type'($P1, 'WhateverCode')
        .return ($P1)
    .end
    .sub '!whatever_closure' :anon :outer('!whatever_helper')
        .param pmc arg
        .local pmc opfunc, left, right
        opfunc = find_lex '$opfunc'
        left   = find_lex '$left'
        right  = find_lex '$right'
        left   = '!whatever_eval'(left, arg)
        if null right goto unary
        right  = '!whatever_eval'(right, arg)
        .tailcall opfunc(left, right)
      unary:
        .tailcall opfunc(left)
    .end
    .sub '!whatever_eval' :multi(_)
        .param pmc whatever
        .param pmc arg
        .return (whatever)
    .end
    .sub '!whatever_eval' :multi('Whatever')
        .param pmc whatever
        .param pmc arg
        .return (arg)
    .end
    .sub '!whatever_eval' :multi('WhateverCode')
        .param pmc whatever
        .param pmc arg
        .tailcall whatever(arg)
    .end
};
