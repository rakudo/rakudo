use lib <t/packages/  t/04-nativecall  lib>;
use NativeCall;
use Test;
use Test::Helpers;
use CompileTestLib;
compile_test_lib '00-misc';

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
