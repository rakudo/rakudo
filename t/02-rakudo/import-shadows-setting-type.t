use lib <t/02-rakudo/test-packages>;
use Test;

plan 6;

# A module may export its own type under a name the setting already
# uses, such as a refined DateTime subclass exported as `DateTime`.
# The import must install a fresh lexical that shadows the setting
# type, as the traditional frontend does. The module's compilation
# must not publish the setting type into its GLOBALish along the way.
# Consumers merge that stash in, and a setting type there collides
# with the module's own export. DateTime::strftime failed this way
# with "Redeclaration of symbol 'DateTime'". Modules declaring
# packages nested under a setting name leaked the setting type to
# every consumer, CompUnit::Repository::Staging among them.

{
    use ShadowsSettingType :refine;
    is Set.shadow-tag, 'shadowed',
        'the imported same-named subclass shadows the setting type';
    nok Set === CORE::Set,
        'the shadowing import is the module class, not the setting type';
}

{
    use ShadowsSettingType;
    ok Set === CORE::Set,
        'without the export tag the setting type is untouched';
    is shadow-helper(), 'helper',
        'the default exports of the module still import';
}

{
    use NestedUnderSettingName;
    is CompUnit::Repository::TestShadow.tag, 'nested',
        'a package nested under a setting name is reachable in the consumer';
    my class CompUnit { method tag() { 'local' } }
    is CompUnit.tag, 'local',
        'the consumer can declare its own class under the setting name the module nested under';
}
