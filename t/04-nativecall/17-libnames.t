use Test;

use NativeCall :TEST;

plan 7;

if $*KERNEL ~~ 'linux' {
	ok guess_library_name("libfoo") eq "libfoo.so", "libfoo is libfoo.so and should warn";
	ok guess_library_name(("libfoo", 1)) eq "libfoo.so.1", "libfoo , 1  is libfoo.so.1";
	ok guess_library_name(("libfoo", v1.2.3)) eq "libfoo.so.1.2.3", "libfoo , v1.2.3  is libfoo.so.1.2.3";
	ok guess_library_name("libfoo.so") eq "libfoo.so", "libfoo.so is libfoo.so";
	ok guess_library_name("./libfoo") eq "./libfoo.so", "./libfoo is ./libfoo.so";
	ok guess_library_name("./libfoo.so") eq "./libfoo.so", "./libfoo.so is ./libfoo.so";
	ok guess_library_name("/libfoo.so") eq "/libfoo.so", "/libfoo.so is /libfoo.so";
} else {
	skip 7;
}