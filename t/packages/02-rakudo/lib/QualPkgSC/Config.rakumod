unit class QualPkgSC::Config;
use QualPkgSC::Raw;

# A BEGIN-time `constant` whose initializer names a qualified symbol led by
# this unit's own parent package (`QualPkgSC`). That parent is vivified for our
# name and must reach the serialization context, or precompiling this unit dies
# with "Object of type QualPkgSC in QAST::WVal, but not in SC".
my constant @Names = @QualPkgSC::Raw::ClassMap.map(*.^name);

method names { @Names }
