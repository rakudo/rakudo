class Compiler does Systemic {
    has Str $.release;
    has DateTime $.build-date;
    has Str $.codename;

    submethod BUILD (
      :$!name      = 'rakudo',
      :$!auth      = 'unknown',
      :$version,
      :$!signature = Blob,
      :$!desc      = "",
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

multi postcircumfix:<{ }> (Compiler $d, "name" )   {
    DEPRECATED('$*PERL.compiler.name', :what('$*PERL<compiler><name>') );
    $d.name
}
multi postcircumfix:<{ }> (Compiler $d, "ver" )   {
    DEPRECATED('$*PERL.compiler.version', :what('$*PERL<compiler><ver>') );
    $d.version
}
multi postcircumfix:<{ }> (Compiler $d, "release-number" )   {
    DEPRECATED('$*PERL.compiler.release', :what('$*PERL<compiler><release-number>') );
    $d.release
}
multi postcircumfix:<{ }> (Compiler $d, "build-date" )   {
    DEPRECATED('$*PERL.compiler.build-date', :what('$*PERL<compiler><build-date>') );
    $d.build-date
}
multi postcircumfix:<{ }> (Compiler $d, "codename" )   {
    DEPRECATED('$*PERL.compiler.codename', :what('$*PERL<compiler><codename>') );
    $d.build-date
}
