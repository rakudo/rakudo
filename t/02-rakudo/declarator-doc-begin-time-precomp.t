use lib <t/02-rakudo/test-packages>;
use Test;
use DeclaratorDocBeginTime;

plan 3;

# Loading the module precompiles it, so the docs set at BEGIN time come back
# through serialization.

is &DeclaratorDocBeginTime::documented.WHY.Str, "lead\ntrail",
    'a precompiled sub has its leading and trailing doc';
ok DeclaratorDocBeginTime::held()<> =:= &DeclaratorDocBeginTime::documented.WHY<>,
    'the doc a trait of a precompiled sub held is the doc of the sub';
is DeclaratorDocBeginTime::C.^attributes.head.WHY.Str, 'role attribute',
    'a precompiled class has the doc of an attribute of a role it does';

# vim: expandtab shiftwidth=4
