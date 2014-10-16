class Perl does Systemic {
    has Compiler $.compiler;

    submethod BUILD (
      :$!name      = 'Perl 6',
      :$!auth      = "The Perl Foundation",
      :$!version   = Version.new("unknown"),
      :$!compiler  = Compiler.new,
    ) { }

    method VMnames { <moar jvm parrot> }

    method DISTROnames {
        (
#?if parrot
        <macosx linux freebsd mswin32 mingw msys cygwin solaris haiku openbsd netbsd>
#?endif
#?if jvm
        <macosx linux mswin32>
#?endif
#?if moar
        <macosx linux freebsd mswin32 openbsd netbsd>
#?endif
        )
    }

    method KERNELnames { <darwin linux freebsd openbsd netbsd win32> }
}

multi sub INITIALIZE_DYNAMIC('$*PERL') {
    PROCESS::<$PERL> := Perl.new;
}
multi postcircumfix:<{ }> (Perl $d, "name" )   {
    DEPRECATED('$*PERL.name', |<2014.06 2015.06>, :what('$*PERL<name>') );
    $d.name
}
multi postcircumfix:<{ }> (Perl $d, "compiler" )   {
    # allow this silently, as we will catch it on accessing the Compiler object
    $d.compiler
}

# vim: ft=perl6 expandtab sw=4
