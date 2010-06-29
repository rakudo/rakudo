#!/usr/bin/env perl

exec "ulimit -t 45; ulimit -v 1048576; ./perl6 @ARGV";
