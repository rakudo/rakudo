use lib <t/02-rakudo/test-packages>;
use Test;

plan 2;

# ExportHowBare overrides the `class` declarator through a bare `package
# EXPORTHOW`. Loading it must not die "Merging GLOBAL symbols failed: duplicate
# definition of symbol class", which happened when the bare EXPORTHOW was
# `our`-scoped, leaked into GLOBAL, and clashed with the setting's own EXPORTHOW.
# The load happens via EVAL so a regression fails this test rather than aborting
# the whole file.
lives-ok { EVAL 'use ExportHowBare; 1' },
    'a module with a bare `package EXPORTHOW` loads without a GLOBAL clash';

# The override reaches the consumer: a class declared after the use is built by
# the exported HOW, which adds a `composed-by-bare-exporthow` method at compose.
is EVAL('use ExportHowBare; class Marker { }; Marker.composed-by-bare-exporthow'),
    True, 'the bare EXPORTHOW override composes classes with its own HOW';
