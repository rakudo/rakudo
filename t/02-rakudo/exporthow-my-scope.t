use lib <t/02-rakudo/test-packages>;
use Test;
use nqp;

plan 4;

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

# EXPORTHOW is per compilation unit: a module that merely uses an
# EXPORTHOW-carrying module must not hand an EXPORTHOW of its own to
# consumers. When resolving the name hoisted the setting's EXPORTHOW into
# the intermediate module's GLOBALish, a consumer re-registered every
# setting entry as a package declarator, and its next `subset` parsed as a
# package whose SubsetHOW.new_type died asking for the refinee. This broke
# any module using Terminal::Print, whose Grid uses OO::Monitors.
lives-ok { EVAL 'use UsesExportHowBare; subset TransitiveProbe of Int where 1; 1' },
    'a subset still parses after using a module that used an EXPORTHOW module';

todo 'the legacy frontend hands a used module\'s declarator overrides on to consumers'
    unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
is EVAL('use UsesExportHowBare; class TransitiveMarker { }; TransitiveMarker.^find_method("composed-by-bare-exporthow") ?? "leaked" !! "clean"'),
    'clean', 'the class declarator override does not travel transitively';
