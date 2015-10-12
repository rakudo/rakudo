class Perl does Systemic {
    has Compiler $.compiler;

    submethod BUILD (
      :$!name      = 'Perl 6',
      :$!auth      = "The Perl Foundation",
      :$!version   = Version.new("6b"),
      :$!compiler  = Compiler.new,
    ) { }

    method VMnames { <moar jvm > }

    method DISTROnames {
        (
#?if jvm
        <macosx linux freebsd mswin32 openbsd netbsd>
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
multi sub postcircumfix:<{ }> (Perl $d, "compiler" )   {
    # allow this silently, as we will catch it on accessing the Compiler object
    $d.compiler
}

# vim: ft=perl6 expandtab sw=4
