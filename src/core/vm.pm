class VM {
    has $.name;
    has $.ver;
    has $.config;

    submethod BUILD (:$!name, :$!config) {
        $!ver = $!config<version> // "unknown";
    }
    method gist { $!name ~ (" ($!ver)" if $!ver ne "unknown") }
    method Str  { $!name }

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
