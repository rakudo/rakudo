#!/usr/bin/env raku

for @*ARGS -> $prefix {
    CompUnit::Repository::Installation.new(:$prefix).upgrade-repository;
}

# vim: expandtab sw=4
