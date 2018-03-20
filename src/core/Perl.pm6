class Perl does Systemic {
    has Compiler $.compiler;

    submethod BUILD(
      :$!name      = 'Perl 6',
      :$!auth      = "The Perl Foundation",
      :$!version   = Version.new(nqp::p6box_s(nqp::getcomp('perl6').language_version())),
      :$!compiler  = Compiler.new,
      --> Nil
    ) { }

    method VMnames { <moar jvm js> }

    method DISTROnames { <macosx linux freebsd mswin32 openbsd dragonfly netbsd> }
    method KERNELnames { <darwin linux freebsd openbsd netbsd  dragonfly win32>  }
}

# vim: ft=perl6 expandtab sw=4
