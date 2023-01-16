use nqp;

# This constant must specify current CORE revision
# Must preceede class declarations to allow correct recording of their respective language version.
my constant CORE-SETTING-REV = nqp::box_i(2, Metamodel::Configuration.language_revision_type());

# vim: expandtab shiftwidth=4
