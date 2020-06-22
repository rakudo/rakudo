use v6;

use lib <lib>;
use NativeCall :ALL;
use Test;


class A is repr('CStruct') {
    has int32 $a;
}

class B {};

sub goodPointer(Pointer $a)  { * };

sub goodPointer2(Pointer[int32]) { * };

sub badPointer(Pointer[int] $a) { * };

sub goodCStruct(A $a) { * };

sub goodBuf(Buf $a) { * };

sub badclass(B $a) { * };

sub goodCArray(CArray $a)  { * };

sub badCArray(CArray[int] $a) { * };

sub goodint(int32 $a)  { * };

sub badint(Int $a)  { * };

sub badint2(int $a)  { * };

sub goodnum(num32 $a)  { * };

sub badnum(Num $a)  { * };

sub badnum2(num $a)  { * };

sub goodstr(Str $a)  { * };

sub badstr(str $a)  { * };


sub goodretint() returns int32  { * };

sub badretint() returns int  { * };

sub badretint2() returns Int  { * };

sub goodretnum() returns num64  { * };

sub badretnum() returns num  { * };

sub badretnum2() returns Num  { * };

sub goodretbool() returns bool  { * };

sub badretbool() returns Bool  { * };

sub goodretPointer() returns Pointer  { * };

sub goodretCStruct() returns A  { * };

sub goodretCArray() returns CArray  { * };


sub goodstrencoded(Str is encoded('utf8'))  { * };
sub goodretstrencoded() returns Str is encoded('utf8')  { * };

class X::FailOnWarn is Exception {
        method message() {
        }
};

sub testr($r) {
    # upgrade NativeCall warnings to exceptions for easier testing
    CONTROL { when CX::Warn { die X::FailOnWarn.new() } }
    check_routine_sanity($r);
}

lives-ok {testr(&goodPointer)}, "Taking a pointer is fine";
lives-ok {testr(&goodPointer2)}, "Taking a Pointer[int32] is fine";
throws-like {testr(&badPointer)}, X::FailOnWarn, "Taking a Pointer[Int] is NOT fine";
lives-ok {testr(&goodCStruct)}, "Taking a CStruct is fine";
lives-ok {testr(&goodCArray)}, "Taking a CArray is fine";
lives-ok {testr(&goodBuf)}, "Taking a Buf is fine";
throws-like {testr(&badCArray)}, X::FailOnWarn, "Taking a CArray[int] is not fine";
throws-like {testr(&badclass)}, X::FailOnWarn, "Taking a Perl6 class is NOT fine";
lives-ok {testr(&goodint)}, "Taking a int32 is fine";
throws-like {testr(&badint)}, X::FailOnWarn, "Taking a Int is NOT fine";
throws-like {testr(&badint2)}, X::FailOnWarn, "Taking a int is NOT fine";
lives-ok {testr(&goodnum)}, "Taking a num32 is fine";
throws-like {testr(&badnum)}, X::FailOnWarn, "Taking a Num is NOT fine";
throws-like {testr(&badnum2)}, X::FailOnWarn, "Taking a num is NOT fine";
lives-ok {testr(&goodstr)}, "Taking a Str is fine";
lives-ok {testr(&badstr)}, "FIXME: Taking a str is buggy but should be fine?";


lives-ok {testr(&goodretPointer)}, "Returning a pointer is fine";
lives-ok {testr(&goodretCStruct)}, "Returning a CStruct is fine";
lives-ok {testr(&goodretCArray)}, "Returning a CArray is fine";
lives-ok {testr(&goodretint)}, "Returning a int32 is fine";
throws-like {testr(&badretint)}, X::FailOnWarn, "Returning a Int is NOT fine";
throws-like {testr(&badretint2)}, X::FailOnWarn, "Returning a int is NOT fine";
lives-ok {testr(&goodretnum)}, "Returning a num32 is fine";
throws-like {testr(&badretnum)}, X::FailOnWarn, "Returning a Num is NOT fine";
throws-like {testr(&badretnum2)}, X::FailOnWarn, "Returning a num is NOT fine";
lives-ok {testr(&goodretbool)}, "Returning a bool is fine";
lives-ok {testr(&badretbool)}, "FIXME: Returning a Bool maybe be bugged";

lives-ok {testr(&goodstrencoded)}, "Taking an encoded Str is fine";
lives-ok {testr(&goodretstrencoded)}, "Returning an encoded Str is fine";

# TODO rewrite this test to test for a warning instead of a fatal error:
#eval-dies-ok 'use NativeCall; sub test(Int $a) is native("fake") {*};', "Bad trait declaration";

eval-lives-ok 'use NativeCall; sub test2(int32 $a) is native("fake") {*};', "Good trait declaration";
eval-lives-ok 'use NativeCall; class A is repr("CPointer") { sub foo(A $a) is native("fake") {*} }', "Embeded type";
eval-lives-ok 'use NativeCall; sub foo() is native("fake") {*}', "Void function";
eval-lives-ok 'use NativeCall; class Piko { method All_The_Things(int8, int16, int32, long, num32, num64) returns long is native("./11-cpp") is nativeconv("thisgnu") { * }}', "Method are silly";
eval-lives-ok 'use NativeCall; sub p5_str_to_sv(int32, long, Blob) is native("foo") { * };', "Blob should work";
eval-lives-ok 'use NativeCall; class Foo is repr("CPointer") {sub foo() returns Foo is native("foo") { * } }', "Return a type in its definition";

done-testing


# vim: expandtab shiftwidth=4
