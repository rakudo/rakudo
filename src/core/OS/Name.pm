my class OS::Name {
    has Str $.name;
    has OS::Addr $.addr;
    #has Array of Str @.aliases; # jvm compile dies (during Test.pm)
                                 # with java.lang.RuntimeException: Invalid SC code index -1
    has @.aliases;
    #has Array of OS::Addr @.addrs; # jvm compile dies (during Test.pm)
                                    # with java.lang.RuntimeException: Invalid SC code index -1
    has @.addrs;

    method Str {
        return $.name;
    }
}
