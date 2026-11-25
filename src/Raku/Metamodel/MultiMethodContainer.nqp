#- Metamodel::MultiMethodContainer ---------------------------------------------
# All the logic related to multi methods
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

        my @method_table := nqp::list(
          self.method_table(   $target),
          self.submethod_table($target)
        );

        self.protect({
            my @methods := @!multi_methods_to_incorporate;
            my @new_protos;

            my int $m := nqp::elems(@methods);
            my int $i;
            while $i < $m {
                my $method := nqp::atpos(@methods, $i);

                # Get method name and code.
                my str $name := $method.name;
                my     $code := $method.code;

                # Get type of method flag and appropriate lookup table
                my int $is_submethod := nqp::istype($code, $submethod_type);
                my %methods := nqp::atpos(@method_table, $is_submethod);

                # Do we have anything in the methods table already in
                # this class?
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

                    # Helper sub to add proto and a dispatchee
                    sub add_proto_and_dispatchee($proto, $dispatchee) {
                        $proto.add_dispatchee($dispatchee);

                        # Add method and update appropriate local version as
                        # well
                        nqp::bindpos(
                          @method_table,
                          $is_submethod,
                          self.add_method($target, $name, $proto)
                        );
                        nqp::push(@new_protos, $proto);
                    }

                    my int $found; # flag, whether a dispatcher was found

                    # Submethods don't chase their MROs
                    unless $is_submethod {

                        # Go hunting in the MRO for a method proto
                        my @mro := self.mro($target);

                        my int $n := nqp::elems(@mro);
                        my int $j := 1;  # intentionally skip first
                        while $j < $n && nqp::not_i($found) {
                            my $parent  := nqp::atpos(@mro, $j);
                            my %methods := $parent.HOW.method_table($parent);

                            if nqp::existskey(%methods, $name) {
                                # Found a possible - make sure it's a
                                # dispatcher, not an only.
                                my $dispatcher := nqp::atkey(%methods, $name);

                                # Clone it and install it in our method
                                # table.
                                add_proto_and_dispatchee(
                                  $dispatcher.derive_dispatcher, $code
                                ) if $found := $dispatcher.is_dispatcher;
                            }
                            ++$j;
                        }
                    }

                    # Did not find a dispatcher, need to add one
                    unless $found {
                        my $autogen_proto := nqp::atpos(
                          @autogen_proto, $is_submethod
                        );

                        if $autogen_proto {
                            my $proto := $autogen_proto.instantiate_generic(
                              nqp::hash('T', $target)
                            );
                            $proto.set_name($name);
                            add_proto_and_dispatchee($proto, $code);
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

                # Process any "handles <foo bar>"
                $code.apply_handles($target)
                  if nqp::can($code, 'apply_handles')
                  && nqp::can(self, 'find_method_fallback');

                ++$i;
            }

            # Make sure any new protos have their dispatchees sorted
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
