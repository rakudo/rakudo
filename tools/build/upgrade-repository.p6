#!/usr/bin/env perl6

CompUnit::Repository::Installation.new(:prefix(@*ARGS[0])).upgrade-repository;

# vim: ft=perl6
