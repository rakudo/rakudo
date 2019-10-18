use lib <t/packages/  t/04-nativecall  lib>;
use NativeCall;
use Test;
use Test::Helpers;
use CompileTestLib;
compile_test_lib '00-misc';

plan 3;

{ # https://github.com/rakudo/rakudo/issues/3235
    role Foo {
        sub calloc(size_t, size_t --> Pointer) is native(Str) { !!! };
        method test() {
            calloc(1, 1)
        }
    };

    isa-ok Foo.test, Pointer;

    my &calloc := BEGIN {
        sub calloc(size_t, size_t --> Pointer) is native(Str) { !!! };
    };

    isa-ok calloc(1, 1), Pointer;
}

{ # https://github.com/rakudo/rakudo/issues/1576
    (my $dir := make-temp-dir).add('Foo.pm6').spurt: ｢
        use NativeCall;
        sub NCstrlen(Str --> int32) is native(｣
          ~ './00-misc'.IO.absolute.perl
        ~ ｢) is export {}
        BEGIN say NCstrlen '123';
        say NCstrlen '12345';
    ｣;
    is-run ｢say NCstrlen '1234567'; say NCstrlen '123456789'｣,
        :compiler-args[«-I "$dir.absolute()" -MFoo»], :out("3\n5\n7\n9\n"),
    'no segfaults when using NC routine after using it during precomp';
}
