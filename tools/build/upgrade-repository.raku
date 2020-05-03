#!/usr/bin/env raku

CompUnit::Repository::Installation.new(:prefix(@*ARGS[0])).upgrade-repository;

# vim: ft=perl6
