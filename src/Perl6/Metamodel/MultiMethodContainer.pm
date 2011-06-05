role Perl6::Metamodel::MultiMethodContainer {
    # Set of multi-methods to incorporate. Not just the method handles;
    # each is a hash containing keys name and body.
    has @!multi_methods_to_incorporate;

    # We can't incorporate multis right away as we don't know all parents
    # yet, maybe, which influences whether we even can have multis, need to
    # generate a proto and so forth. So just queue them up in a todo list and
    # we handle it at class composition time.
    method add_multi_method($obj, $name, $code_obj) {
        my %todo;
        %todo<name> := $name;
        %todo<code> := $code_obj;
        @!multi_methods_to_incorporate[+@!multi_methods_to_incorporate] := %todo;
        $code_obj;
    }

    # Incorporates the multi candidates into the appropriate proto. Need to
    # implement proto incorporation yet.
    method incorporate_multi_candidates($obj) {
        my $num_todo := +@!multi_methods_to_incorporate;
        my $i := 0;
        while $i != $num_todo {
            # Get method name and code.
            my $name := @!multi_methods_to_incorporate[$i]<name>;
            my $code := @!multi_methods_to_incorporate[$i]<code>;

            # Do we have anything in the methods table already in
            # this class?
            my $dispatcher := (self.method_table($obj)){$name};
            if pir::defined($dispatcher) {
                # Yes. Only or dispatcher, though? If only, error. If
                # dispatcher, simply add new dispatchee.
                if $dispatcher.is_dispatcher {
                    $dispatcher.add_dispatchee($code);
                }
                else {
                    pir::die("Cannot have a multi candidate for '" ~ $name ~ 
                        "' when an only method is also in the package '" ~
                        self.name($obj) ~ "'");
                }
            }
            else {
                # Go hunting in the MRO for a proto.
                my @mro := self.mro($obj);
                my $j := 1;
                my $found := 0;
                while $j != +@mro && !$found {
                    my $parent := @mro[$j];
                    my %meths := $parent.HOW.method_table($parent);
                    my $dispatcher := %meths{$name};
                    if pir::defined($dispatcher) {
                        # Found a possible - make sure it's a dispatcher, not
                        # an only.
                        if $dispatcher.is_dispatcher {
                            # Clone it and install it in our method table.
                            my $copy := $dispatcher.clone();
                            $copy.add_dispatchee($code);
                            self.add_method($obj, $name, $copy);
                            $found := 1;
                        }
                        else {
                            pir::die("Could not find a proto for multi '" ~ $name ~
                                "' in package '" ~ self.name($obj) ~
                                "' (it may exist, but an only is hiding it if so)");
                        }
                    }
                    $j := $j + 1;
                }
                unless $found {
                    pir::die("Could not find a proto for multi '" ~ $name ~
                        "' in package '" ~ self.name($obj) ~
                        "', and proto generation is not yet implemented");
                }
            }
            $i := $i + 1;
        }
    }
}
