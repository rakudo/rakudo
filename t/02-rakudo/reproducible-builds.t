my constant $lib = $*TMPDIR.child("rakudo-lib" ~ (^2**128).pick.base(36));
BEGIN {
    $lib.child('NativeCall').mkdir;
    $lib.child('NativeCall').child('Compiler').mkdir;
    'lib'.IO.child('NativeCall.rakumod').copy: $lib.child('NativeCall.rakumod');
    'lib'.IO.child('NativeCall').child('Types.rakumod').copy: $lib.child('NativeCall').child('Types.rakumod');
    'lib'.IO.child('NativeCall').child('Compiler').child('GNU.rakumod').copy: $lib.child('NativeCall').child('Compiler').child('GNU.rakumod');
    'lib'.IO.child('NativeCall').child('Compiler').child('MSVC.rakumod').copy: $lib.child('NativeCall').child('Compiler').child('MSVC.rakumod');
}
use lib $lib;

use Test;
use NativeCall; # precompile dependencies


my $store = CompUnit::PrecompilationStore::File.new(
    :prefix($*TMPDIR.child("rakudo-precomp" ~ (^2**128).pick.base(36)))
);
my $precompilation-repository = CompUnit::PrecompilationRepository::Default.new(:$store);
my @checksums;
my @units;
my $compiler-id = CompUnit::PrecompilationId.new-without-check($*PERL.compiler.id);
my constant $id = CompUnit::PrecompilationId.new-without-check('6B7A1AECF02807F30DDAD99C02C34440CA036AF6');
for ^2 -> $run {
    $precompilation-repository.precompile(
        'lib/NativeCall.rakumod'.IO,
        $id,
        :force,
    );
    @units.push: my $unit = $store.load-unit($compiler-id, $id);
    @checksums.push: $unit.checksum;
    $unit.bytecode; # read in bytecode
    $unit.close;
    $store.remove-from-cache($id);
    $store.delete-by-compiler($compiler-id);
}
$store.prefix.child('.lock').unlink;
$store.prefix.child('CACHEDIR.TAG').unlink;
$store.prefix.rmdir;

is @checksums[1], @checksums[0], 'Both precompilation runs resulted in the same checksum'
    or do {
        for :before(@units[0]), :after(@units[1]) {
            my $bytecode = $_.value.bytecode;
            $_.value.save-to($_.key().IO);
            spurt("$_.key().bc", $bytecode);
            shell("moar --dump $_.key().bc > $_.key().dump");
            shell("hexdump -C $_.key() > $_.key().hex");
        }
        my $proc = shell("diff before.dump after.dump");
        $proc = shell("diff before.hex after.hex");
    }

done-testing;

# vim: expandtab shiftwidth=4
