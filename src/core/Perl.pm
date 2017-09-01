class Perl does Systemic {
    has Compiler $.compiler;

    submethod BUILD(
      :$!name      = 'Perl 6',
      :$!auth      = "The Perl Foundation",
      :$!version   = Version.new(nqp::getcomp('perl6').language_version()),
      :$!compiler  = Compiler.new
      --> Nil
    ) { }

    method VMnames { <moar jvm > }

    method DISTROnames {
        (
#?if jvm
        <macosx linux freebsd mswin32 openbsd dragonfly netbsd>
#?endif
#?if moar
        <macosx linux freebsd mswin32 openbsd dragonfly netbsd>
#?endif
        )
    }

    method KERNELnames { <darwin linux freebsd openbsd netbsd dragonfly win32> }
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*PERL', {
    PROCESS::<$PERL> := Perl.new;
}

# vim: ft=perl6 expandtab sw=4
