role Perl6::Metamodel::MetaMethodContainer {
    # Table of the methods.
    has %!meta_methods;
    has @!meta_methods;

    # Add a meta-method.
    method add_meta_method($obj, $name, $code_obj) {
        if nqp::existskey(%!meta_methods, $name) {
            nqp::die("Package '" ~ self.name($obj)
                ~ "' already has a meta-method '$name'");
        }
        %!meta_methods{$name} := $code_obj;
        nqp::push(@!meta_methods, $code_obj);
    }

    # Get the meta-methods table: a hash of meta-methods added.
    method meta_method_table($obj) {
        %!meta_methods
    }

    # Get the meta-methods in order of declaration.
    method meta_methods($obj) {
        @!meta_methods
    }

    # Applies the added meta-methods to the current meta-object instance by
    # building a role containing them, and mixing it in.
    method compose_meta_methods($obj) {
        # Build flattened meta-methods set.
        my %meta;
        my @meta;
        for self.mro($obj) {
            if nqp::can($_.HOW, 'meta_method_table') {
                for $_.HOW.meta_methods($obj) -> $method {
                    my str $name := $method.name;
                    unless nqp::existskey(%meta, $name) {
                        %meta{$name} := $method;
                        nqp::push(@meta, $name);
                    }
                }
            }
        }

        # If we have any meta-methods, build a role for them to go in and
        # compose it into the meta-object..
        if %meta {
            my $role := $?PACKAGE.HOW.new_type();
            for @meta -> $key {
                $role.HOW.add_method($role, $key, %meta{$key});
            }
            $role.HOW.set_body_block($role, sub ($class) {
                nqp::list($role, nqp::hash('$?CLASS', $class))
            });
            $role.HOW.compose($role);
            self.HOW.mixin(self, $role);
        }
    }
}

# vim: expandtab sw=4
