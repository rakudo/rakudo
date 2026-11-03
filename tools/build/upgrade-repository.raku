#!/usr/bin/env raku
use nqp;
my $rakudo-bin-dir = shift @*ARGS;
my $reloc = nqp::gethllsym('default', 'SysConfig').rakudo-build-config()<static-rakudo-home> eq '';
my $wrapper-mode = $reloc ?? 'relative' !! 'absolute';

for @*ARGS -> $prefix {
    my $wrapper-rakudo-dir = $wrapper-mode eq 'absolute'
        ?? $rakudo-bin-dir
        !! $rakudo-bin-dir.IO.relative($prefix.IO.add('bin')).Str;
    CompUnit::Repository::Installation.new(:$prefix, :$wrapper-mode, :$wrapper-rakudo-dir).upgrade-repository;
}

# vim: expandtab sw=4
