use nqp;

# This constant must specify current CORE revision
# Must preceede class declarations to allow correct recording of their respective language version.
my constant CORE-SETTING-REV = 'e';

# Re-register with a deprecation warning
Rakudo::Internals.REGISTER-DYNAMIC: '$*PERL', {
    DEPRECATED('$*RAKU', :what<$*PERL>, :2up );
    PROCESS::<$PERL> := Raku.new;
}, :override;

# vim: expandtab shiftwidth=4
