role Perl6::Metamodel::MultiMethodContainer {
    # Set of multi-methods to incorporate. Not just the method handles;
    # each is a hash containing keys name and body.
    has @!multi_methods_to_incorporate;
    
    # The proto we'll clone.
    my $autogen_proto;
    
    # Sets the proto we'll auto-gen based on.
    method set_autogen_proto($proto) {
        $autogen_proto := $proto
    }

    # We can't incorporate multis right away as we don't know all parents
    # yet, maybe, which influences whether we even can have multis, need to
    # generate a proto and so forth. So just queue them up in a todo list and
    # we handle it at class composition time.
    method add_multi_method($obj, $name, $code_obj) {
        # Represents a multi candidate to incorporate.
        my class MultiToIncorporate {
            has $!name;
            has $!code;
            method name() { $!name }
            method code() { $!code }
        }
        my $how := MultiToIncorporate.HOW.WHAT;
        my $todo := MultiToIncorporate.new( :name($name), :code($code_obj) );
        @!multi_methods_to_incorporate[+@!multi_methods_to_incorporate] := $todo;
        $code_obj;
    }

    # Gets the multi methods that are to be incorporated.
    method multi_methods_to_incorporate($obj) {
        @!multi_methods_to_incorporate
    }
    
    # Incorporates the multi candidates into the appropriate proto. Need to
    # implement proto incorporation yet.
    method incorporate_multi_candidates($obj) {
        my $num_todo := +@!multi_methods_to_incorporate;
        my $i := 0;
        while $i != $num_todo {
            # Get method name and code.
            my $name := @!multi_methods_to_incorporate[$i].name;
            my $code := @!multi_methods_to_incorporate[$i].code;

            # Do we have anything in the methods table already in
            # this class?
            my %meths := self.method_table($obj);
            if pir::exists(%meths, $name) {
                # Yes. Only or dispatcher, though? If only, error. If
                # dispatcher, simply add new dispatchee.
                my $dispatcher := %meths{$name};
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
                    if pir::exists(%meths, $name) {
                        # Found a possible - make sure it's a dispatcher, not
                        # an only.
                        my $dispatcher := %meths{$name};
                        if $dispatcher.is_dispatcher {
                            # Clone it and install it in our method table.
                            my $copy := $dispatcher.derive_dispatcher();
                            $copy.add_dispatchee($code);
                            self.add_method($obj, $name, $copy);
                            $found := 1;
                        }
                    }
                    $j := $j + 1;
                }
                unless $found {
                    # No proto found, so we'll generate one here.
                    unless $autogen_proto {
                        pir::die("Cannot auto-generate a proto method in the setting");
                    }
                    my $proto := $autogen_proto.instantiate_generic(
                        hash( T => $obj ));
                    $proto.set_name($name);
                    $proto.add_dispatchee($code);
                    self.add_method($obj, $name, $proto);
                }
            }
            $i := $i + 1;
        }
        @!multi_methods_to_incorporate := [];
    }
}
