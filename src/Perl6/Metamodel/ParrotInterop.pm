# Various bits of Parrot interoperability, including vtable overrides and specifying
# that an attribute delegates to a given Parrot vtable.
role Perl6::Metamodel::ParrotInterop {
    # Maps vtable names to vtable method overrides.
    has %!parrot_vtable_mapping;
    
    # Maps vtable names to attributes lookup info, so that an override can work by
    # delegation.
	has %!parrot_vtable_handler_mapping;
    
    method add_parrot_vtable_mapping($obj, $name, $meth) {
        if pir::defined(%!parrot_vtable_mapping{$name}) {
            pir::die("Class '" ~ self.name($obj) ~
                "' already has a Parrot v-table override for '" ~
                $name ~ "'");
        }
        %!parrot_vtable_mapping{$name} := $meth;
    }

    method add_parrot_vtable_handler_mapping($obj, $name, $attr_name) {
        if pir::defined(%!parrot_vtable_handler_mapping{$name}) {
            pir::die("Class '" ~ self.name($obj) ~
                "' already has a Parrot v-table handler for '" ~
                $name ~ "'");
        }
        %!parrot_vtable_handler_mapping{$name} := [ $obj, $attr_name ];
    }
    
    method publish_parrot_vtable_mapping($obj) {
        my %mapping;
        for self.mro($obj) {
            my %map := $_.HOW.parrot_vtable_mappings($_, :local(1));
            for %map {
                unless pir::exists(%mapping, $_.key) {
                    if !nqp::isnull($_.value) && $_.value {
                        %mapping{$_.key} := $_.value;
                    }
                    else {
                        %mapping{$_.key} := nqp::null();
                    }
                }
            }
        }
        if +%mapping {
            pir::stable_publish_vtable_mapping__vPP($obj, %mapping);
        }
    }

    method publish_parrot_vtable_handler_mapping($obj) {
        my %mapping;
        for self.mro($obj) {
            my %map := $_.HOW.parrot_vtable_handler_mappings($_, :local(1));
            for %map {
                unless pir::exists(%mapping, $_.key) {
                    if !nqp::isnull($_.value) && $_.value {
                        %mapping{$_.key} := $_.value;
                    }
                    else {
                        %mapping{$_.key} := nqp::null();
                    }
                }
            }
        }
        if +%mapping {
            pir::stable_publish_vtable_handler_mapping__vPP($obj, %mapping);
        }
    }
    
    method parrot_vtable_mappings($obj, :$local!) {
        %!parrot_vtable_mapping
    }

    method parrot_vtable_handler_mappings($obj, :$local!) {
        %!parrot_vtable_handler_mapping
    }
}
