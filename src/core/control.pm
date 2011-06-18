my &THROW :=
    -> |$ {
        Q:PIR {
            .local pmc args, payload, type, severity, ex
            args = perl6_current_args_rpa
            payload  = args[0]
            type     = args[1]
            severity = args[2]
            unless null severity goto have_severity
            severity = box .EXCEPT_NORMAL
          have_severity:
            ex = root_new ['parrot';'Exception']
            setattribute ex, 'payload', payload
            setattribute ex, 'type', type
            setattribute ex, 'severity', severity
            throw ex
        };
        0
    };

my &return-rw := -> \$parcel { 
    THROW($parcel, pir::const::CONTROL_RETURN) 
};
my &return := -> \$parcel { 
    THROW(pir::perl6_decontainerize__PP($parcel), 
          pir::const::CONTROL_RETURN) 
};

my &take-rw := -> \$parcel { 
    THROW($parcel, pir::const::CONTROL_TAKE) 
};
my &take := -> \$parcel { 
    THROW(pir::perl6_decontainerize__PP($parcel), 
          pir::const::CONTROL_TAKE) 
};

sub die(*@msg) { pir::die(@msg.join('')) }
sub fail(*@msg) { pir::die(@msg.join('')) }
