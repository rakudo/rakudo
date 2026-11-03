use Test;

plan 4;

# A multi-part indirect module name starts with an empty part for its
# leading `::`, which is not part of the name being loaded.

throws-like 'require ::("RequireIndirectNoSuch")::("Module")',
    X::CompUnit::UnsatisfiedDependency,
    message => /'RequireIndirectNoSuch::Module'/,
    'a multi-part indirect name compiles and looks up the joined name';

is EVAL('(require ::("NativeCall")::("Types")).^name'), 'NativeCall::Types',
    'a multi-part indirect name loads the module it names';

my ($top, $rest) = 'NativeCall', 'Types';
is EVAL('(require ::($top)::($rest)).^name'), 'NativeCall::Types',
    'a multi-part indirect name built from variables loads the module it names';

is EVAL('(require ::("NativeCall")::Types).^name'), 'NativeCall::Types',
    'a multi-part indirect name with a literal part loads the module it names';

# vim: expandtab shiftwidth=4
