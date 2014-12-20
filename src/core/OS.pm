sub gethostname( --> Str){
    return nqp::p6box_s(nqp::gethostname());
}

my class Proc::Status {
    has $.exit;
    has $.pid;
    has $.signal;
    
    #~ method exit()   { $!exit   }
    #~ method pid()    { $!pid    }
    #~ method signal() { $!signal }
    
    proto method status(|) { * }
    multi method status($new_status) {
        $!exit   = $new_status +> 8;
        $!signal = $new_status +& 0xFF;
    }
    multi method status(Proc::Status:D:)  { ($!exit +< 8) +| $!signal }
    multi method Numeric(Proc::Status:D:) { $!exit }
    multi method Bool(Proc::Status:D:)    { $!exit == 0 }
}

# vim: ft=perl6 expandtab sw=4
