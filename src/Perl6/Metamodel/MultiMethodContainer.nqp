#- Metamodel::MultiMethodContainer ---------------------------------------------
role Perl6::Metamodel::MultiMethodContainer {
    # Set of multi-methods to incorporate. Not just the method handles;
    # each is a hash containing keys name and body.
    has @!multi_methods_to_incorporate;
    has %!multi_candidate_names;

    # The proto we'll clone.
    my @autogen_proto := nqp::list;

    # Sets the proto we'll auto-gen based on.
    method set_autogen_proto($method_proto, $submethod_proto) {
        @autogen_proto := nqp::list($method_proto, $submethod_proto);
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
        my $submethod_type :=
          Perl6::Metamodel::Configuration.submethod_type;

        self.protect({
            my @methods := @!multi_methods_to_incorporate;
            my @new_protos;

            my int $m := nqp::elems(@methods);
            my int $i;
            while $i < $m {
                my $method := nqp::atpos(@methods, $i);

                # Get method name and code.
                my str $name          := $method.name;
                my     $code          := $method.code;
                my int $is_submethod  := nqp::istype($code, $submethod_type);

                # Do we have anything in the methods table already in
                # this class?
                my $method_table  := $is_submethod
                                        ?? 'submethod_table'
                                        !! 'method_table';
                my %methods := nqp::hllize(self."$method_table"($target));
                if nqp::existskey(%methods, $name) {
                    # Yes. Only or dispatcher, though? If only, error. If
                    # dispatcher, simply add new dispatchee.
                    (my $dispatcher := nqp::atkey(%methods,$name)).is_dispatcher
                      ?? $dispatcher.add_dispatchee($code)
                      !! nqp::die(
                           "Cannot have a multi candidate for '"
                           ~ $name
                           ~ "' when an only method is also in the package '"
                           ~ self.name($target)
                           ~ "'"
                         );
                }

                else {
                    my int $found;
                    unless $is_submethod {
                        # Go hunting in the MRO for a method proto. Note that
                        # we don't traverse MRO for submethods.
                        my @mro := self.mro($target);

                        my int $n := nqp::elems(@mro);
                        my int $j := 1;  # intentionally skip first
                        while $j < $n && nqp::not_i($found) {
                            my $parent  := nqp::atpos(@mro, $j);
                            my %methods := $parent.HOW."$method_table"($parent);

                            if nqp::existskey(%methods, $name) {
                                # Found a possible - make sure it's a
                                # dispatcher, not an only.
                                my $dispatcher := nqp::atkey(%methods, $name);
                                if $dispatcher.is_dispatcher {
                                    # Clone it and install it in our method
                                    # table.
                                    my $copy := $dispatcher.derive_dispatcher;
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
                        my $autogen_proto := nqp::atpos(
                          @autogen_proto, $is_submethod
                        );

                        if $autogen_proto {
                            my $proto := $autogen_proto.instantiate_generic(
                              nqp::hash('T', $target)
                            );
                            $proto.set_name($name);
                            $proto.add_dispatchee($code);
                            self.add_method($target, $name, $proto);
                            nqp::push(@new_protos, $proto);
                        }

                        # No proto found, so we'll generate one here.
                        else {
                            nqp::die(
                              "Cannot auto-generate a proto method for '"
                              ~ $name
                              ~ "' in the setting"
                            );
                        }
                    }
                }

                $code.apply_handles($target)
                  if nqp::can($code, 'apply_handles')
                  && nqp::can(self, 'find_method_fallback');

                ++$i;
            }

            $m := nqp::elems(@new_protos);
            $i := 0;
            while $i < $m {
                my $proto := nqp::atpos(@new_protos, $i);
                $proto.sort_dispatchees
                  if nqp::can($proto, 'sort_dispatchees');
                ++$i;
            }

            # These are now processed
            @!multi_methods_to_incorporate := nqp::list;
            %!multi_candidate_names        := nqp::hash;
        });
    }

    method has_multi_candidate($XXX, str $name) {
        nqp::atkey(%!multi_candidate_names, $name)
    }
}

# vim: expandtab sw=4
