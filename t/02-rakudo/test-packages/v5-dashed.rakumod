# Named so that a leading `use v5-dashed` puts a version-like part where the
# compilation unit looks for a `use v6` language declaration.
unit module v5-dashed;

our sub dashed-module-loaded() is export { True }

# vim: expandtab shiftwidth=4
