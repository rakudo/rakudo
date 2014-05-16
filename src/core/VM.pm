class VM does Systemic {
    has $.config;
    has $.precomp-ext;
    has $.precomp-target;

    submethod BUILD (:$!name, :$!config) {
        $!auth    = "unknown";
        $!version = Version.new($!config<version> // "unknown");
        $!precomp-ext =
#?if parrot
          "pir"
#?endif
#?if jvm
          "jar"
#?endif
#?if moar
          "moarvm"
#?endif
# add new backends here please
        ;
        $!precomp-target =
#?if parrot
          "pir"
#?endif
#?if jvm
          "jar"
#?endif
#?if moar
          "mbc"
#?endif
# add new backends here please
        ;
    }
}

multi postcircumfix:<{ }> (VM $d, "name" )   {
    DEPRECATED('$*VM.name', :what('$*VM<name>') );
    $d.name
}
multi postcircumfix:<{ }> (VM $d, "config" ) {
    DEPRECATED('$*VM.config', :what('$*VM<config>') );
    $d.config
}
PROCESS::<$VM> = VM.new( :name($*VM<name>), :config($*VM<config>) );
