use lib <lib t/packages/Test-Helpers t/04-nativecall>;
use NativeCall;
use Test;
use Test::Helpers;
use CompileTestLib;
compile_test_lib '00-misc';


# All possible identifiers and their namespaces
my constant AllExports = Q:to/EXPORTS/.chomp;
ALL
  &cglobal
  &check_routine_sanity
  &explicitly-manage
  &guess_library_name
  &nativecast
  &nativesizeof
  &postcircumfix:<[ ]>
  &refresh
  &trait_mod:<is>
  CArray
  OpaquePointer
  Pointer
  bool
  long
  longlong
  size_t
  ssize_t
  ulong
  ulonglong
  void
DEFAULT
  &cglobal
  &explicitly-manage
  &nativecast
  &nativesizeof
  &postcircumfix:<[ ]>
  &refresh
  &trait_mod:<is>
  CArray
  OpaquePointer
  Pointer
  bool
  long
  longlong
  size_t
  ssize_t
  ulong
  ulonglong
  void
TEST
  &check_routine_sanity
  &guess_library_name
traits
  &trait_mod:<is>
types
  &postcircumfix:<[ ]>
  CArray
  OpaquePointer
  Pointer
  bool
  long
  longlong
  size_t
  ssize_t
  ulong
  ulonglong
  void
utils
  &explicitly-manage
  &refresh
EXPORTS

my str @parts;
for NativeCall::EXPORT::.keys.sort {
    @parts.push($_);
    @parts.push("  $_") for NativeCall::EXPORT::{$_}.WHO.keys.sort
}
is @parts.join("\n"), AllExports, "are all identifiers reachable?";

if $*VM.name eq 'jvm' {
    plan :skip-all<NullPointerException in sub NCstrlen>;
}

{ # https://github.com/rakudo/rakudo/issues/3235
    role Foo {
        sub NCstrlen(Str --> int32) is native('./00-misc') { !!! };
        method test() {
            NCstrlen '123'
        }
    };

    is Foo.test, 3, "body of a native sub declared in a role body replaced";

    my &NCstrlen := BEGIN {
        sub NCstrlen(Str --> int32) is native('./00-misc') { !!! };
    };

    is NCstrlen('123'), 3, "body of a native sub declared in a BEGIN block replaced";
}

unless $*DISTRO.is-win { # https://github.com/rakudo/rakudo/issues/3244
    # Test needs native arguments, an "is native" without args and an empty body
    sub abs(int32 --> int32) is native {};
    is abs(-1), 1, "optimizer doesn't inline the native sub's original body";
}

{ # https://github.com/rakudo/rakudo/issues/1576
    (my $dir := make-temp-dir).add('Foo.pm6').spurt: ｢
        use NativeCall;
        sub NCstrlen(Str --> int32) is native(｣
          ~ './00-misc'.IO.absolute.raku
        ~ ｢) is export {}
        BEGIN say NCstrlen '123';
        say NCstrlen '12345';
    ｣;
    is-run ｢say NCstrlen '1234567'; say NCstrlen '123456789'｣,
        :compiler-args[«-I "$dir.absolute()" -MFoo»], :out("3\n5\n7\n9\n"),
    'no segfaults when using NC routine after using it during precomp';
}

done-testing();

# vim: expandtab shiftwidth=4
