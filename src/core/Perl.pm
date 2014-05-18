class Perl does Systemic {
    has Compiler $.compiler;

    submethod BUILD (
      :$!name      = 'Perl 6',
      :$!auth      = "unknown",
      :$!version   = Version.new("unknown"),
      :$!signature = Blob,
      :$!desc      = "",
      :$!compiler  = Compiler.new,
    ) { }
}
PROCESS::<$PERL> := Perl.new;

multi postcircumfix:<{ }> (Perl $d, "name" )   {
    DEPRECATED('$*PERL.name', :what('$*PERL<name>') );
    $d.name
}
multi postcircumfix:<{ }> (Perl $d, "compiler" )   {
    # allow this silently, as we will catch it on accessing the Compiler object
    $d.compiler
}
