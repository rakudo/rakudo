my &return-rw := -> \$parcel {
        Q:PIR {
            .include 'except_types.pasm'
            .include 'except_severity.pasm'
            .local pmc ex
            ex = root_new ['parrot';'Exception']
            $P0 = box .CONTROL_RETURN
            setattribute ex, 'type', $P0
            $P0 = find_lex '$parcel'
            setattribute ex, 'payload', $P0
            $P0 = box .EXCEPT_NORMAL
            setattribute ex, 'severity', $P0
            throw ex
        };
        0;
    };

# TODO: &return should decontainerize its parcel argument
my &return = &return-rw;


