sub gethostname( --> Str){
    return nqp::p6box_s(nqp::gethostname());
}

my class Proc::Status {
    has $.exitcode = -1;  # distinguish uninitialized from 0 status
    has $.pid;
    has $.signal;

    #~ method exitcode() { $!exitcode   }
    #~ method pid()      { $!pid    }
    #~ method signal()   { $!signal }

    method exit {
        DEPRECATED('Proc::Status.exit', |<2015.04 2015.09>);
        $!exitcode;
    }

    proto method status(|) { * }
    multi method status($new_status) {
        $!exitcode = $new_status +> 8;
        $!signal   = $new_status +& 0xFF;
    }
    multi method status(Proc::Status:D:)  { ($!exitcode +< 8) +| $!signal }
    multi method Numeric(Proc::Status:D:) { $!exitcode }
    multi method Bool(Proc::Status:D:)    { $!exitcode == 0 }
}

# vim: ft=perl6 expandtab sw=4
