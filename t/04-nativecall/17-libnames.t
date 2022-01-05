use v6;

use lib <lib>;
use NativeCall :TEST;
use Test;

plan 7;

if $*KERNEL ~~ 'linux' {
	is guess_library_name("foo"), "libfoo.so", "foo is libfoo.so and should warn";
	is guess_library_name(("foo", Version.new(1))), "libfoo.so.1", "foo , 1  is libfoo.so.1";
	is guess_library_name(("foo", v1.2.3)), "libfoo.so.1.2.3", "foo , v1.2.3  is libfoo.so.1.2.3";
	is guess_library_name("libfoo.so"), "libfoo.so", "libfoo.so is libfoo.so";
	is guess_library_name("./foo"), "$*CWD/libfoo.so", "./foo is ./libfoo.so";
	is guess_library_name("./libfoo.so"), "./libfoo.so", "./libfoo.so is ./libfoo.so";
	is guess_library_name("/libfoo.so"), "/libfoo.so", "/libfoo.so is /libfoo.so";
} else {
	skip-rest;
}

# vim: expandtab shiftwidth=4
