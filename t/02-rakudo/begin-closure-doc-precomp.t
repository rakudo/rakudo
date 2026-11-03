use lib <t/02-rakudo/test-packages>;
use Test;
use BeginClosureDoc;

plan 3;

# Loading the module precompiles it, so the BEGIN-built closures come back
# through serialization. A declarator doc in the same compunit used to put
# the declarand node into $=rakudoc, whose serialization snapshotted live
# compile-time frames; the closures then deserialized with an outer chain
# that dead-ended before the setting, and calling any setting routine from
# their bodies died with a VMNull invocation.

is BeginClosureDoc.call-sub('q'), 'Q',
    'a precompiled BEGIN-built closure can call a setting sub';
is-deeply BeginClosureDoc.call-table('q'), ['q'],
    'a precompiled BEGIN-built table closure can use the array composer';
is BeginClosureDoc.^find_method('documented').WHY.leading,
    'documented after the BEGIN closures',
    'the declarator doc is still reachable through .WHY after precompilation';

# vim: expandtab shiftwidth=4
