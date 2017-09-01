use v6;

use lib <lib>;
use NativeCall :TEST;
use Test;

plan 16;

if $*KERNEL ~~ 'linux' {
	is guess_library_name("foo"), "libfoo.so", "foo is libfoo.so and should warn";
	is guess_library_name(("foo", Version.new(1))), "libfoo.so.1", "foo , 1  is libfoo.so.1";
	is guess_library_name(("foo", v1.2.3)), "libfoo.so.1.2.3", "foo , v1.2.3  is libfoo.so.1.2.3";
	is guess_library_name("libfoo.so"), "libfoo.so", "libfoo.so is libfoo.so";
	is guess_library_name("./foo"), "$*CWD/libfoo.so", "./foo is ./libfoo.so";
	is guess_library_name("./libfoo.so"), "./libfoo.so", "./libfoo.so is ./libfoo.so";
	is guess_library_name("/libfoo.so"), "/libfoo.so", "/libfoo.so is /libfoo.so";
} else {
	skip "Not on a Linux like system", 7;
}

#Check for warning
{
  dies-ok {CONTROL { when CX::Warn { .die } }; guess_library_name('foo')}, "No version provided to guess_library_name, so die";
  lives-ok {CONTROL { when CX::Warn { .die } }; guess_library_name('foo', True)}, "Testing the second argument of g-l-n that avoid the warning";
}

use NativeCall;

INIT {
  %*ENV<BAZ> = "/lib/baz";
}


lives-ok {register-native-library('Foo', 'foo', :version(v1))}, "Simple register work";
dies-ok {register-native-library('Foo', 'foo', :version(v1))}, "Can't register the same lib twice.";
dies-ok {register-native-library('Foo2', 'foo')}, "Without version should die";
lives-ok {register-native-library('Bar', 'bar', :no-version)}, "Explicitly no-version";
lives-ok {register-native-library('Foo2', $*EXECUTABLE.Str)}, "Full path with no version";

my $LIB_BAZ = register-native-library('Baz', 'baz', :no-version, :ENV<BAZ>);
is $LIB_BAZ.library-file, "/lib/baz", "Test ENV";
eval-lives-ok 'use NativeCall; my NC-Library-Handle $LIB = register-native-library("Piko", "piko", :version(v1)); sub foo is native($LIB) { * }', "Testing a full usage";

