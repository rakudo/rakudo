unit class QualifiedOurAnchor;

# Qualified our-scoped declarations anchor at their leading package,
# not at this class, so a user of this module reaches them through
# the merged GLOBAL.

our $QOA::Target::flag = False;
our &QOA::Target::set-flag = sub () {
    $QOA::Target::flag = True;
}
