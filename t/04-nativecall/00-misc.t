use lib <t/packages/>;
use Test;
use Test::Helpers;

plan 1;

{ # https://github.com/rakudo/rakudo/issues/1576
    (my $dir := make-temp-dir).add('Foo.pm6').spurt: ｢
        use NativeCall;
        sub strlen(Str --> int32) is native is export {}
        BEGIN say strlen '123';
        say strlen '12345';
    ｣;
    is-run ｢say strlen '1234567'; say strlen '123456789'｣,
        :compiler-args[«-I "$dir.absolute()" -MFoo»], :out("3\n5\n7\n9\n"),
    'no segfaults when using NC routine after using it during precomp';
}
