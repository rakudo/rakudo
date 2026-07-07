use lib <t/packages/02-rakudo/lib>;
use Test;

# A constant initialized from a native-typed expression must hold the boxed
# Raku value, not a bare VM-level box. A bare box has no Raku method table
# and a null WHO, so it breaks package-qualified access and, when exported
# through two dependency paths (as HarfBuzz's HB_SET_VALUE_INVALID is), it
# crashed the compunit GLOBAL merge with "Cannot find method
# 'FLATTENABLE_HASH' on object of type VMNull".

# Both use lines matter: NativeConstDiamond also uses NativeConstDefs, so
# the constant reaches this compunit's GLOBAL by two paths and the loader
# has to merge them.
use NativeConstDiamond;
use NativeConstDefs;

plan 4;

is-deeply NATIVE-UINT, 4294967295,
  'an imported native-valued constant is the boxed value';
is-deeply NativeConstDefs::NATIVE-UINT, 4294967295,
  'package-qualified access to a native-valued constant gives the boxed value';
ok NativeConstDefs::NATIVE-UINT.defined,
  'method calls work on a package-qualified native-valued constant';
is-deeply native-uint-via-diamond(), 4294967295,
  'the constant has the same value when reached through the diamond';

# vim: expandtab shiftwidth=4
