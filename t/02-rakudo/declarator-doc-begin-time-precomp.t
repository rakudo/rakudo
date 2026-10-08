use lib <t/02-rakudo/test-packages>;
use Test;
use DeclaratorDocBeginTime;

plan 5;

# Loading the module precompiles it, so the docs set at BEGIN time come back
# through serialization.

is &DeclaratorDocBeginTime::documented.WHY.Str, "lead\ntrail",
    'a precompiled sub has its leading and trailing doc';
ok DeclaratorDocBeginTime::held()<> =:= &DeclaratorDocBeginTime::documented.WHY<>,
    'the doc a trait of a precompiled sub held is the doc of the sub';
is DeclaratorDocBeginTime::C.^attributes.head.WHY.Str, 'role attribute',
    'a precompiled class has the doc of an attribute of a role it does';
is DeclaratorDocBeginTime::Positive.WHY.Str, 'subset doc',
    'a precompiled subset with a parenthesized where block has its leading doc';
is DeclaratorDocBeginTime::Letters.WHY.Str, 'enum doc',
    'a precompiled enum whose term declares a sub has its leading doc';

# vim: expandtab shiftwidth=4
