use lib 'lib';
use Test;
use NativeCall; # precompile dependencies

my $store = CompUnit::PrecompilationStore::File.new(
    :prefix($*TMPDIR.child("rakudo-precomp" ~ (^2**128).pick.base(36)))
);
my $precompilation-repository = CompUnit::PrecompilationRepository::Default.new(:$store);
my @checksums;
my @units;
my $compiler-id = CompUnit::PrecompilationId.new-without-check($*PERL.compiler.id);
my $id = CompUnit::PrecompilationId.new('000');
for ^2 -> $run {
    $precompilation-repository.precompile(
        'lib/NativeCall.pm6'.IO,
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
$store.prefix.rmdir;

is @checksums[1], @checksums[0], 'Both precompilation runs resulted in the same checksum'
    or do {
        for :before(@units[0]), :after(@units[1]) {
            my $bytecode = $_.value.bytecode;
            $_.value.save-to($_.key().IO);
            spurt("$_.key().bc", $bytecode, :bin);
            shell("moar --dump $_.key().bc > $_.key().dump");
            shell("hexdump -C $_.key() > $_.key().hex");
        }
        my $proc = shell("diff before.dump after.dump");
        $proc = shell("diff before.hex after.hex");
    }

done-testing;

# vim: ft=perl6
