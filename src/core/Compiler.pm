class Compiler does Systemic {
    has Str $.release;
    has DateTime $.build-date;
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
        $!version = Version.new(
          $version // nqp::p6box_s(nqp::atkey($compiler, 'version')) );
        $!release =
          $release // nqp::p6box_s(nqp::atkey($compiler, 'release-number'));
        $!build-date = DateTime.new(
          $build-date // nqp::p6box_s(nqp::atkey($compiler, 'build-date')) );
        $!codename =
          $codename // nqp::p6box_s(nqp::atkey($compiler, 'codename'));
    }
}

multi sub postcircumfix:<{ }> (Compiler $d, "name" )   {
    DEPRECATED('$*PERL.compiler.name', |<2014.06 2015.06>, :what('$*PERL<compiler><name>') );
    $d.name
}
multi sub postcircumfix:<{ }> (Compiler $d, "ver" )   {
    DEPRECATED('$*PERL.compiler.version', |<2014.06 2015.06>, :what('$*PERL<compiler><ver>') );
    $d.version
}
multi sub postcircumfix:<{ }> (Compiler $d, "release-number" )   {
    DEPRECATED('$*PERL.compiler.release', |<2014.06 2015.06>, :what('$*PERL<compiler><release-number>') );
    $d.release
}
multi sub postcircumfix:<{ }> (Compiler $d, "build-date" )   {
    DEPRECATED('$*PERL.compiler.build-date', |<2014.06 2015.06>, :what('$*PERL<compiler><build-date>') );
    $d.build-date
}
multi sub postcircumfix:<{ }> (Compiler $d, "codename" )   {
    DEPRECATED('$*PERL.compiler.codename', |<2014.06 2015.06>, :what('$*PERL<compiler><codename>') );
    $d.build-date
}

# vim: ft=perl6 expandtab sw=4
