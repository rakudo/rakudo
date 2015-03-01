role Perl6::Metamodel::MetaMethodContainer {
    # Table of the methods.
    has %!meta_methods;

    # Add a meta-method.
    method add_meta_method($obj, $name, $code_obj) {
        if nqp::existskey(%!meta_methods, $name) {
            nqp::die("Package '" ~ self.name($obj)
                ~ "' already has a meta-method '$name'");
        }
        %!meta_methods{$name} := $code_obj;
    }

    # Get the meta-methods table: a hash of meta-methods added.
    method meta_method_table($obj) {
        %!meta_methods
    }

    # Applies the added meta-methods to the current meta-object instance by
    # building a role containing them, and mixing it in.
    method compose_meta_methods($obj) {
        if %!meta_methods {
            my $role := $?PACKAGE.HOW.new_type();
            for %!meta_methods {
                $role.HOW.add_method($role, $_.key, $_.value);
            }
            $role.HOW.set_body_block($role, -> $class {
                nqp::list($role, nqp::hash('$?CLASS', $class))
            });
            $role.HOW.compose($role);
            self.HOW.mixin(self, $role);
        }
    }
}
