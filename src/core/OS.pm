sub gethostname( --> Str){
    nqp::p6box_s(nqp::gethostname());
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
    multi method status() {
        ($!exit +< 8) +| $!signal
    }
    
    method Numeric {
        $!exit
    }
    
    method Bool {
        $!exit == 0
    }
}
