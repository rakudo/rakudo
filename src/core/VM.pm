class VM does Application {
    has $.config;

    submethod BUILD (:$!name, :$!config) {
        $!auth    = "unknown";
        $!version = Version.new($!config<version> // "unknown");
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
