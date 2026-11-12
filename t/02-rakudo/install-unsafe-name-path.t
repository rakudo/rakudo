use lib <lib t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 18;

# Installing a distribution must never write wrappers outside of the
# bin/ directory of the repository, whatever names its "files" use

my $dist-dir = make-temp-dir;
$dist-dir.add('bin').mkdir;
$dist-dir.add('bin').add('x').spurt: "say 42\n";

sub dist(%files) {
    state $ver = 0;
    Distribution::Hash.new: {
        :name<Unsafe-Names>, :ver(++$ver), :auth<test>, :api(''),
        :provides{}, :%files
    }, :prefix($dist-dir)
}

my $outer = make-temp-dir;
my $repo  = CompUnit::Repository::Installation.new:
  :prefix($outer.add('repo'));

for 'bin/../../ESCAPED', 'bin/../ESCAPED', 'bin/../dist/ESCAPED' -> $name {
    throws-like { $repo.install: dist({ $name => 'bin/x' }) },
      Exception, message => /outside/,
      "refuses to install '$name'";
}

ok $repo.prefix.add('resources').dir.elems == 0,
  'nothing was installed for refused distributions';

# \ is a directory separator on Windows only: elsewhere this is just an
# odd file name inside bin/
my $bslash = '..\\..\\ESCAPED';
if $*DISTRO.is-win {
    throws-like {
        $repo.install: dist({ "bin/$bslash" => 'bin/x' }), :!precompile
    }, Exception, message => /outside/,
      "refuses to install 'bin/$bslash' on Windows";
    skip 'a \\ in a name is a directory separator on Windows', 1;
}
else {
    lives-ok {
        $repo.install: dist({ "bin/$bslash" => 'bin/x' }), :!precompile
    }, "installs 'bin/$bslash' as a plain file name";
    ok $repo.prefix.add('bin').add($bslash).f
      && $repo.prefix.add('bin').add("$bslash.raku").f,
      'its wrappers are files with that literal name inside bin/';
}

nok $outer.dir.first(*.basename.starts-with('ESCAPED'))
  || $repo.prefix.dir.first(*.basename.starts-with('ESCAPED'))
  || $repo.prefix.add('dist').dir.first(*.basename.starts-with('ESCAPED')),
  'nothing was written outside of bin/';

# a wrapper must not be written through a symlink pointing outside
$repo.prefix.add('bin').mkdir;
$outer.add('OUTSIDE').spurt: 'original';
if try $outer.add('OUTSIDE').symlink:
     $repo.prefix.add('bin').add('evil.raku') {
    throws-like { $repo.install: dist({ 'bin/evil' => 'bin/x' }), :!precompile },
      Exception, message => /outside/,
      'refuses to write a wrapper through a symlink';
    is $outer.add('OUTSIDE').slurp, 'original',
      'nothing was written through the symlink';

    # writing through a dangling symlink would create its target
    $outer.add('NEW').symlink: $repo.prefix.add('bin').add('dangling.raku');
    throws-like {
        $repo.install: dist({ 'bin/dangling' => 'bin/x' }), :!precompile
    }, Exception, message => /outside/,
      'refuses to write a wrapper through a dangling symlink';
    nok $outer.add('NEW').e,
      'nothing was created through the dangling symlink';

    # every link in a chain of absolute symlinks must be followed
    my $bin = $repo.prefix.add('bin').absolute.IO;
    $outer.add('CHAINED').spurt: 'original';
    $outer.add('CHAINED').absolute.IO.symlink: $bin.add('chain2');
    $bin.add('chain2').symlink: $bin.add('chain.raku');
    throws-like {
        $repo.install: dist({ 'bin/chain' => 'bin/x' }), :!precompile
    }, Exception, message => /outside/,
      'refuses to write a wrapper through a chain of absolute symlinks';
    is $outer.add('CHAINED').slurp, 'original',
      'nothing was written through the chain of symlinks';

    # a symlink loop cannot be resolved, so must be refused up front
    $bin.add('loop.raku').symlink: $bin.add('loop.raku');
    throws-like {
        $repo.install: dist({ 'bin/loop' => 'bin/x' }), :!precompile
    }, Exception, message => /outside/,
      'refuses to write a wrapper through a symlink loop';
}
else {
    skip 'cannot create symlinks', 7;
}

# resources/ names are only stored under content ids, and existing
# distributions use names like this
lives-ok {
    $repo.install: dist({ 'resources/../lib/Foo.rakumod' => 'bin/x' }),
      :!precompile
}, 'resources/ names with .. still install';

if $*DISTRO.is-win {
    skip 'file names with quotes are not valid on Windows', 3;
}
else {
    my $name = 'a"b{1}$(c)$(d)';
    lives-ok { $repo.install: dist({ "bin/$name" => 'bin/x' }), :!precompile },
      'a script name with quotes, braces and dollars installs fine';

    ok $repo.prefix.add('bin').add("$name.raku").slurp
         .contains('run-script("a\"b\{1}\$(c)\$(d)")'),
      'the script name is quoted in the generated Raku wrapper';

    ok $repo.prefix.add('bin').add($name).slurp
         .contains('"${DIR}/a\"b{1}\$(c)\$(d).raku"'),
      'the script name is escaped in the generated shell wrapper';
}

# vim: expandtab shiftwidth=4
