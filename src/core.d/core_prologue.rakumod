use nqp;

# This constant specifies the current CORE revision.  It must precede class
# declarations to allow correct recording of their respective language version.
my constant CORE-SETTING-REV = nqp::box_i(2, Metamodel::Configuration.language_revision_type);

# vim: expandtab shiftwidth=4
