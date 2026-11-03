# Consumes a module that carries an EXPORTHOW without declaring one of its
# own, so a consumer of this module can check that no EXPORTHOW travels along.
use ExportHowBare;
unit module UsesExportHowBare;
