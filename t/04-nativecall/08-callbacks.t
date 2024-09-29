use lib <lib t/04-nativecall>;
use CompileTestLib;
use NativeCall;
use Test;

plan(13);

compile_test_lib('08-callbacks');

class Struct is repr('CStruct') {
    has Str  $.str;
    has long $.ival;

    submethod TWEAK {
        $!str := 'Tweedledum, tweedledee';
        $!ival = 314;
    }
}

sub TakeACallback(&cb ()) is native('./08-callbacks') { * }
sub OptionallyTakeACallback(&cb ()) is native('./08-callbacks') { * }
sub TakeIntCallback(&cb (int32)) is native('./08-callbacks') { * }
sub TakeStringCallback(&cb (Str)) is native('./08-callbacks') { * }
sub TakeStructCallback(&cb (Struct)) is native('./08-callbacks') { * }

sub CheckReturnsFloat(&cb (--> num64))   returns int32 is native('./08-callbacks') { * }
sub CheckReturnsStr(&cb (--> Str))       returns int32 is native('./08-callbacks') { * }
sub CheckReturnsStruct(&cb (--> Struct)) returns int32 is native('./08-callbacks') { * }
sub CheckChangingCallback(&cb (--> int)) returns int32 is native('./08-callbacks') { * }

sub simple_callback() {
    pass 'simple callback';
}

sub int_callback(int32 $x) {
    is $x, 17, 'int callback argument'
}

sub str_callback(Str $x) {
    is $x, 'lorem ipsum', 'string callback argument'
}

sub struct_callback(Struct $struct) {
    is $struct.str, 'foobar', 'struct callback string argument';
    is $struct.ival, -42, 'struct callback int argument';
}

sub return_float() returns num64 {
    return 1.23e0;
}

sub return_str() returns Str {
    return 'Herps and derps';
}

sub return_struct() returns Struct {
    my Struct $struct .= new;

    return $struct;
}

TakeACallback(&simple_callback);
OptionallyTakeACallback(&simple_callback);
todo 'does not work yet on JVM', 1 if $*VM.name eq 'jvm';
lives-ok { OptionallyTakeACallback(Code) }, "optional callback with Code type object";
lives-ok { OptionallyTakeACallback(Pointer) }, "optional callback with Pointer type object";
TakeIntCallback(&int_callback);
TakeStringCallback(&str_callback);
TakeStructCallback(&struct_callback);

is CheckReturnsFloat(&return_float),   6, 'callback returned a float to C';
is CheckReturnsStr(&return_str),       7, 'callback returned a string to C';
is CheckReturnsStruct(&return_struct), 8, 'callback returned a struct to C';

for -> { 0 }, -> { 1 } -> \callback {
    is CheckChangingCallback(callback), 0 ;
}

# vim: expandtab shiftwidth=4
