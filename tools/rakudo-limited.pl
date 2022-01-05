#!/usr/bin/env perl

exec "ulimit -t 45; ulimit -v 2048576; @ARGV";
