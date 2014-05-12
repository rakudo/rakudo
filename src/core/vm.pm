class VM {
    has $.name;
    has $.version;
    has $.config;

    submethod BUILD (:$!name, :$!config) {
        $!version = $!config<version> // "unknown";
    }
    method gist { $!name ~ (" ($!version)" if $!version ne "unknown") }
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
