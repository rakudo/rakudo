use lib <t/packages/  t/04-nativecall  lib>;
use NativeCall;
use Test;
use Test::Helpers;
use CompileTestLib;
compile_test_lib '00-misc';

plan 3;

{ # https://github.com/rakudo/rakudo/issues/3235
    role Foo {
        sub NCstrlen(Str --> int32) is native('./00-misc') { !!! };
        method test() {
            NCstrlen '123'
        }
    };

    is Foo.test, 3;

    my &NCstrlen := BEGIN {
        sub NCstrlen(Str --> int32) is native('./00-misc') { !!! };
    };

    is NCstrlen('123'), 3;
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
