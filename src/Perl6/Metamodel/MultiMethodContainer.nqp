role Perl6::Metamodel::MultiMethodContainer {
    # Set of multi-methods to incorporate. Not just the method handles;
    # each is a hash containing keys name and body.
    has @!multi_methods_to_incorporate;
    has %!multi_candidate_names;

    # The proto we'll clone.
    my $autogen_method_proto;
    my $autogen_submethod_proto;

    # Sets the proto we'll auto-gen based on.
    method set_autogen_proto($method_proto, $submethod_proto) {
        $autogen_method_proto := $method_proto;
        $autogen_submethod_proto := $submethod_proto;
    }

    # Represents a multi candidate to incorporate.
    my class MultiToIncorporate {
        has str $!name;
        has     $!code;

        method new(str $name, $code) {
            my $obj := nqp::create(self);

            nqp::bindattr_s($obj, MultiToIncorporate, '$!name', $name);
            nqp::bindattr(
              $obj, MultiToIncorporate, '$!code', nqp::decont($code)
            );

            $obj
        }
        method name() { $!name }
        method code() { $!code }
    }

    # We can't incorporate multis right away as we don't know all parents
    # yet, maybe, which influences whether we even can have multis, need to
    # generate a proto and so forth. So just queue them up in a todo list and
    # we handle it at class composition time.
    method add_multi_method($XXX, str $name, $code) {
        self.protect({
            my @methods := nqp::clone(@!multi_methods_to_incorporate);
            my %names   := nqp::clone(%!multi_candidate_names);

            nqp::push(@methods, MultiToIncorporate.new($name, $code));
            nqp::bindkey(%names, $name, 1);

            @!multi_methods_to_incorporate := @methods;
            %!multi_candidate_names        := %names;
        });

        $code
    }

    # Shortcut method to add multiple multis with the same name in one
    # fell swoop
    method add_multi_methods($XXX, str $name, @codes) {
        self.protect({
            my @methods := nqp::clone(@!multi_methods_to_incorporate);
            my %names   := nqp::clone(%!multi_candidate_names);

            my int $m := nqp::elems(@codes);
            my int $i;

            while $i < $m {
                nqp::push(@methods,
                  MultiToIncorporate.new($name, nqp::atpos(@codes, $i))
                );
                ++$i;
            }
            nqp::bindkey(%names, $name, 1);

            @!multi_methods_to_incorporate := @methods;
            %!multi_candidate_names        := %names;
        });
    }

    # Gets the multi methods that are to be incorporated.
    method multi_methods_to_incorporate($XXX?) {
        @!multi_methods_to_incorporate
    }

    # Incorporates the multi candidates into the appropriate proto. Need to
    # implement proto incorporation yet.
    method incorporate_multi_candidates($target) {
        my $num_todo := nqp::elems(@!multi_methods_to_incorporate);
        my int $i;
        my $submethod_type := Perl6::Metamodel::Configuration.submethod_type;
        my @new_protos;
        while $i != $num_todo {
            # Get method name and code.
            my $name := @!multi_methods_to_incorporate[$i].name;
            my $code := @!multi_methods_to_incorporate[$i].code;

            # Do we have anything in the methods table already in
            # this class?
            my $is_submethod  := nqp::istype(nqp::what($code), $submethod_type);
            my $method_table  := $is_submethod
                                    ?? 'submethod_table'
                                    !! 'method_table';
            my $autogen_proto := $is_submethod
                                    ?? $autogen_submethod_proto
                                    !! $autogen_method_proto;
            my %meths := nqp::hllize(self."$method_table"($target));
            if nqp::existskey(%meths, $name) {
                # Yes. Only or dispatcher, though? If only, error. If
                # dispatcher, simply add new dispatchee.
                my $dispatcher := %meths{$name};
                if $dispatcher.is_dispatcher {
                    $dispatcher.add_dispatchee($code);
                }
                else {
                    nqp::die("Cannot have a multi candidate for '" ~ $name ~
                        "' when an only method is also in the package '" ~
                        self.name($target) ~ "'");
                }
            }
            else {
                my $found := 0;
                unless $is_submethod {
                    # Go hunting in the MRO for a method proto. Note that we don't traverse MRO for submethods.
                    my @mro := self.mro($target);
                    my int $j := 1;
                    while $j != nqp::elems(@mro) && !$found {
                        my $parent := nqp::atpos(@mro, $j);
                        my %meths := nqp::hllize($parent.HOW."$method_table"($parent));
                        if nqp::existskey(%meths, $name) {
                            # Found a possible - make sure it's a dispatcher, not
                            # an only.
                            my $dispatcher := %meths{$name};
                            if $dispatcher.is_dispatcher {
                                # Clone it and install it in our method table.
                                my $copy := $dispatcher.derive_dispatcher();
                                $copy.add_dispatchee($code);
                                self.add_method($target, $name, $copy);
                                nqp::push(@new_protos, $copy);
                                $found := 1;
                            }
                        }
                        ++$j;
                    }
                }
                unless $found {
                    # No proto found, so we'll generate one here.
                    unless $autogen_proto {
                        nqp::die("Cannot auto-generate a proto method for '$name' in the setting");
                    }
                    my $proto := $autogen_proto.instantiate_generic(
                        nqp::hash('T', $target));
                    $proto.set_name($name);
                    $proto.add_dispatchee($code);
                    self.add_method($target, $name, $proto);
                    nqp::push(@new_protos, $proto);
                }
            }
            if nqp::can($code, 'apply_handles') && nqp::can(self, 'find_method_fallback') {
                $code.apply_handles($target);
            }
            $i := $i + 1;
        }
        for @new_protos {
            if nqp::can($_, 'sort_dispatchees') {
                $_.sort_dispatchees();
            }
        }
        @!multi_methods_to_incorporate := [];
        %!multi_candidate_names := nqp::hash();
    }

    method has_multi_candidate($target, $name) {
        %!multi_candidate_names{$name}
    }
}

# vim: expandtab sw=4
