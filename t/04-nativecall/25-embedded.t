use lib <lib t/04-nativecall>;
use CompileTestLib;
use NativeCall;
use Test;

plan 1;

BEGIN if $*VM.name eq 'jvm' {
    plan :skip-all<NullPointerException in sub ReturnAStruct>;
};

compile_test_lib('25-embedded');

class Nested is repr("CUnion") {
	has int64 $!integer;

	method integer is rw {$!integer};
}
class Outer is repr("CStruct") {
	HAS Nested $!nested; #Note: HAS, not has
	submethod TWEAK {
		$!nested := Nested.new;
	}

	method nested is rw {$!nested};
}

my $out = Outer.new;
$out.nested.integer = 17;

sub prinx(Outer $v) is native("./25-embedded") { * }
lives-ok { prinx $out }

# vim: expandtab shiftwidth=4
