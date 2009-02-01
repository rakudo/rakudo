#! perl
# Copyright (C) 2008 The Perl Foundation

use strict;
use warnings;

chdir '../..';
`$^X -Ilib tools/dev/reconfigure.pl --step=gen::languages --languages=rakudo`;
