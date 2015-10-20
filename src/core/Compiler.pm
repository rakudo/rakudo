class Compiler does Systemic {
    has Str $.id;
    has Str $.release;
    has Str $!build-date;
    has Str $.codename;

    submethod BUILD (
      :$!name      = 'rakudo',
      :$!auth      = 'The Perl Foundation',
      :$version,
      :$release,
      :$build-date,
      :$codename,
    ) {
# XXX Various issues with this stuff on JVM
        my Mu $compiler := nqp::getcurhllsym('$COMPILER_CONFIG');
        $!id = nqp::p6box_s( nqp::existskey($compiler,'id')
          ?? nqp::atkey($compiler,'id')
          !! nqp::getcomp('perl6').compilation-id );
        $!version = Version.new(
          $version // nqp::p6box_s(nqp::atkey($compiler, 'version')) );
        $!release =
          $release // nqp::p6box_s(nqp::atkey($compiler, 'release-number'));
        $!build-date =
          $build-date // nqp::p6box_s(nqp::atkey($compiler, 'build-date'));
        $!codename =
          $codename // nqp::p6box_s(nqp::atkey($compiler, 'codename'));
    }

    method build-date() {
        DateTime.new($!build-date)
    }
}

# vim: ft=perl6 expandtab sw=4
