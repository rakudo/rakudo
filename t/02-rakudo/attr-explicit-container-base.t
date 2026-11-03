use Test;
use NativeCall;

plan 8;

# An explicit container base type on an attribute (e.g. `has Int @.a is
# Array`) must become the attribute's type, as it does for HAS scoped
# attributes. An attribute typed Mu instead makes introspection lie and
# makes REPRs that check attribute types reject valid code. A CStruct with
# `has CArray[uint8] @.zero[4] is CArray` fails to compose that way.

my class ImplicitBase { has Int @.a; }
is ImplicitBase.^attributes[0].type.^name, 'Positional[Int]',
    'a typed array attribute without an explicit base is typed Positional';

my class ArrayBase { has Int @.a is Array; }
is ArrayBase.^attributes[0].type.^name, 'Array[Int]',
    'is Array on a typed array attribute parameterizes the base type';

my class ListBase { has @.a is List; }
is ListBase.^attributes[0].type.^name, 'List',
    'is List on an untyped array attribute becomes the attribute type';

my class HashBase { has Int %.h is Hash; }
is HashBase.^attributes[0].type.^name, 'Hash[Int]',
    'is Hash on a typed hash attribute parameterizes the base type';

my class ParameterizedBase { has %.h is Hash[Int]; }
is ParameterizedBase.^attributes[0].type.^name, 'Hash[Int]',
    'an already parameterized base type is used as-is';

my class Struct is repr<CStruct> {
    has CArray[uint8] @.zero[4] is CArray;
}
is Struct.^attributes[0].type.^name,
    'NativeCall::Types::CArray[NativeCall::Types::CArray[uint8]]',
    'is CArray on a shaped CStruct attribute types it as a CArray';
ok Struct.^attributes[0].type.REPR eq 'CArray',
    'the CStruct composes because the attribute type has the CArray REPR';

my class Inlined is repr<CStruct> {
    HAS int32 @.b[3] is CArray;
}
is Inlined.^attributes[0].type.REPR, 'CArray',
    'an inlined HAS attribute with is CArray keeps its CArray type';

# vim: expandtab shiftwidth=4
