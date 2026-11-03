# The indirect lookups here run at BEGIN time, so they read this unit's
# lexicals through a pseudo-stash while the unit is still compiling. Their
# results are held in constants, so they are serialized when this module is
# precompiled: a lookup that came back empty would put a Failure carrying
# compiler frames into the constant and break serialization.
use BeginIndirectExporter;

unit module BeginIndirectLexical;

my sub local-hex($x) { "local-" ~ $x }
my class LocalClass { method which() { "local-class" } }

our constant $local-sub = ::('&local-hex');
our constant $imported-sub = ::('&exported-hex');
our constant $setting-sub = ::('&sprintf');
our constant $local-class = ::('LocalClass');
our constant $computed-sub = ::("&" ~ "exported-hex");
# A name nothing declares, so the resolver is consulted and comes up
# empty. The boolified Failure must not poison this unit's serialization.
our constant $missing-lookup = ?::('&nowhere-declared');
