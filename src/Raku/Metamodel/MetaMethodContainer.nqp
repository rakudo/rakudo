#- Metamodel::MetaMethodContainer ----------------------------------------------
role Perl6::Metamodel::MetaMethodContainer {
    has @!meta_methods;
    has %!meta_method_table;

    # Add a meta-method in a threadsafe manner
    method add_meta_method($target, str $name, $method) {
        nqp::die("Package '"
          ~ self.name($target)
          ~ "' already has a meta-method '$name'"
        ) if nqp::existskey(%!meta_method_table, $name);

        self.protect({
            my $list := nqp::clone(@!meta_methods);
            my $hash := nqp::clone(%!meta_method_table);

            nqp::push($list, $method);
            nqp::bindkey($hash, $name, $method);

            @!meta_methods      := $list;
            %!meta_method_table := $hash;
        });
    }

    # Get the meta-methods table: a hash of meta-methods added.
    method meta_method_table($XXX?) { %!meta_method_table }

    # Get the meta-methods in order of declaration.
    method meta_methods($XXX?) { @!meta_methods }

    # Applies the added meta-methods to the current meta-object instance by
    # building a role containing them, and mixing it in.
    method compose_meta_methods($target) {
        # Build flattened meta-methods set.
        my $seen  := nqp::hash;
        my $names := nqp::list_s;
        my $mro   := self.mro($target);

        my int $m := nqp::elems($mro);
        my int $i;
        while $i < $m {
            my $HOW := nqp::atpos($mro, $i).HOW;
            if nqp::can($HOW, 'meta_methods') {
                my $methods := $HOW.meta_methods;

                my int $n := nqp::elems($methods);
                my int $j;
                while $j < $n {
                    my $method   := nqp::atpos($methods, $j);
                    my str $name := $method.name;
                    unless nqp::existskey($seen, $name) {
                        nqp::bindkey($seen, $name, $method);
                        nqp::push_s($names, $name);
                    }

                    ++$j;
                }
            }

            ++$i;
        }

        # If we have any meta-methods, build a role for them to go in and
        # compose it into the meta-object..
        if nqp::elems($names) {
            self.protect({
                my $role := $?PACKAGE.HOW.new_type;
                my $HOW  := $role.HOW;

                my int $m := nqp::elems($names);
                my int $i;
                while $i < $m {
                    my str $name := nqp::atpos_s($names, $i);
                    $HOW.add_method($role, $name, nqp::atkey($seen, $name));
                    ++$i;
                }

                # Set mainline of role
                $role.HOW.set_body_block($role, sub ($class) {
                    nqp::list($role, nqp::hash('$?CLASS', $class))
                });

                $HOW.compose($role);
                self.HOW.mixin(self, $role);
            });
        }
    }
}

# vim: expandtab sw=4
