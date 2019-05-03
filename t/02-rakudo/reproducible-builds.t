use Test;
use NativeCall; # precompile dependencies

my $store = CompUnit::PrecompilationStore::File.new(
    :prefix($*TMPDIR.child("rakudo-precomp" ~ (^2**128).pick.base(36)))
);
my $precompilation-repository = CompUnit::PrecompilationRepository::Default.new(:$store);
my @checksums;
my $compiler-id = CompUnit::PrecompilationId.new-without-check($*PERL.compiler.id);
my $id = CompUnit::PrecompilationId.new('000');
for ^2 -> $run {
    $precompilation-repository.precompile(
        'lib/NativeCall.pm6'.IO,
        $id,
        :force,
    );
    @checksums.push: $store.load-unit($compiler-id, $id).checksum;
    $store.remove-from-cache($id);
    $store.delete-by-compiler($compiler-id);
}
$store.prefix.child('.lock').unlink;
$store.prefix.rmdir;

is @checksums[1], @checksums[0], 'Both precompilation runs resulted in the same checksum';

done-testing;

# vim: ft=perl6
