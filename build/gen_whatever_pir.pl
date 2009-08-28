#!/usr/bin/perl
# Copyright (C) 2008-2009, The Perl Foundation.
# $Id$

use strict;
use warnings;

my @ops = qw(
  infix:% infix:div infix:mod
  infix:== infix:!= infix:<  infix:>  infix:<= infix:>= infix:<=>
  infix:.. infix:^.. infix:..^ infix:^..^
  prefix:+ prefix:? prefix:! prefix:^
);

for (@ops) {
    if (/^infix:/) {
        print qq{
            .namespace []
            .sub '$_' :multi('Whatever', _)
                .param pmc x
                .param pmc y
                .tailcall 'WhateverCodeX'('$_', x, y)
            .end
            .sub '$_' :multi(_, 'Whatever')
                .param pmc x
                .param pmc y
                .tailcall 'WhateverCodeX'('$_', x, y)
            .end
            .sub '$_' :multi('WhateverCode', _)
                .param pmc x
                .param pmc y
                .tailcall 'WhateverCodeX'('$_', x, y)
            .end
            .sub '$_' :multi(_, 'WhateverCode')
                .param pmc x
                .param pmc y
                .tailcall 'WhateverCodeX'('$_', x, y)
            .end
        };
    }
    else {
        print qq{
            .namespace []
            .sub '$_' :multi('Whatever')
                .param pmc x
                .tailcall 'WhateverCodeX'('$_', x)
            .end
            .sub '$_' :multi('WhateverCode', _)
                .param pmc x
                .param pmc y
                .tailcall 'WhateverCodeX'('$_', x, y)
            .end
        };
    }
}

