use nqp;

# This constant must specify current CORE revision
# Must preceede class declarations to allow correct recording of their respective language version.
my constant CORE-SETTING-REV = nqp::clone(2) but Metamodel::Configuration.language_revision_role;

# vim: expandtab shiftwidth=4
