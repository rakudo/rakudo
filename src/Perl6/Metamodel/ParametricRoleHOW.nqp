my $concrete := Perl6::Metamodel::ConcreteRoleHOW;
my $currier := Perl6::Metamodel::CurriedRoleHOW;
class Perl6::Metamodel::ParametricRoleHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::Documenting
    does Perl6::Metamodel::Versioning
    does Perl6::Metamodel::MethodContainer
    does Perl6::Metamodel::PrivateMethodContainer
    does Perl6::Metamodel::MultiMethodContainer
    does Perl6::Metamodel::AttributeContainer
    does Perl6::Metamodel::RoleContainer
    does Perl6::Metamodel::MultipleInheritance
    does Perl6::Metamodel::Stashing
    does Perl6::Metamodel::TypePretense
    does Perl6::Metamodel::RolePunning
    does Perl6::Metamodel::ArrayType
{
    has $!composed;
    has $!body_block;
    has $!in_group;
    has $!group;
    has $!signatured;
    has @!role_typecheck_list;

    my $archetypes := Perl6::Metamodel::Archetypes.new( :nominal(1), :composable(1), :inheritalizable(1), :parametric(1) );
    method archetypes() {
        $archetypes
    }
    
    method new(*%named) {
        nqp::findmethod(NQPMu, 'BUILDALL')(nqp::create(self), |%named)
    }

    method new_type(:$name = '<anon>', :$ver, :$auth, :$repr, :$signatured, *%extra) {
        my $metarole := self.new(:signatured($signatured));
        my $type := nqp::settypehll(nqp::newtype($metarole, 'Uninstantiable'), 'perl6');
        $metarole.set_name($type, $name);
        $metarole.set_ver($type, $ver) if $ver;
        $metarole.set_auth($type, $auth) if $auth;
        $metarole.set_pun_repr($type, $repr) if $repr;
        if nqp::existskey(%extra, 'group') {
            $metarole.set_group($type, %extra<group>);
        }
        self.add_stash($type);
    }
    
    method parameterize($obj, *@pos_args, *%named_args) {
        $currier.new_type($obj, |@pos_args, |%named_args)
    }
    
    method set_body_block($obj, $block) {
        $!body_block := $block
    }
    
    method body_block($obj) {
        $!body_block
    }
    
    method signatured($obj) {
        $!signatured
    }
    
    method set_group($obj, $group) {
        $!group := $group;
        $!in_group := 1;
    }
    
    method group($obj) {
        $!in_group ?? $!group !! $obj
    }
    
    method compose($obj) {
        my @rtl;
        if $!in_group {
            @rtl.push($!group);
        }
        for self.roles_to_compose($obj) {
            @rtl.push($_);
            for $_.HOW.role_typecheck_list($_) {
                @rtl.push($_);
            }
        }
        @!role_typecheck_list := @rtl;
        $!composed := 1;
        $obj
    }
    
    method is_composed($obj) {
        $!composed
    }
    
    method roles($obj, :$transitive) {
        if $transitive {
            my @result;
            for self.roles_to_compose($obj) {
                @result.push($_);
                for $_.HOW.roles($_, :transitive(1)) {
                    @result.push($_)
                }
            }
            @result
        }
        else {
            self.roles_to_compose($obj)
        }
    }
    
    method role_typecheck_list($obj) {
        @!role_typecheck_list
    }
    
    method type_check($obj, $checkee) {
        my $decont := nqp::decont($checkee);
        if $decont =:= $obj.WHAT {
            return 1;
        }
        if $!in_group && $decont =:= $!group {
            return 1;
        }
        for self.prentending_to_be() {
            if $decont =:= nqp::decont($_) {
                return 1;
            }
        }
        for self.roles_to_compose($obj) {
            if nqp::istype($checkee, $_) {
                return 1;
            }
        }
        0
    }
    
    method specialize($obj, *@pos_args, *%named_args) {
        # Run the body block to get the type environment (we know
        # the role in this csae).
        my $type_env;
        my $error;
        try {
            my @result := $!body_block(|@pos_args, |%named_args);
            $type_env := @result[1];
            CATCH {
                $error := $!
            }
        }
        if $error {
            nqp::die("Could not instantiate role '" ~ self.name($obj) ~ "':\n$error")
        }
        
        # Use it to build concrete role.
        self.specialize_with($obj, $type_env, @pos_args)
    }
    
    method specialize_with($obj, $type_env, @pos_args) {
        # Create a concrete role.
        my $conc := $concrete.new_type(:roles([$obj]), :name(self.name($obj)));
        
        # Go through attributes, reifying as needed and adding to
        # the concrete role.
        for self.attributes($obj, :local(1)) {
            $conc.HOW.add_attribute($conc,
                $_.is_generic ?? $_.instantiate_generic($type_env) !! $_);
        }
        
        # Go through methods and instantiate them; we always do this
        # unconditionally, since we need the clone anyway.
        for self.method_table($obj) {
            $conc.HOW.add_method($conc, $_.key, $_.value.instantiate_generic($type_env))
        }
        for self.submethod_table($obj) {
            $conc.HOW.add_method($conc, $_.key, $_.value.instantiate_generic($type_env))
        }
        for self.private_method_table($obj) {
            $conc.HOW.add_private_method($conc, $_.key, $_.value.instantiate_generic($type_env));
        }
        for self.multi_methods_to_incorporate($obj) {
            $conc.HOW.add_multi_method($conc, $_.name, $_.code.instantiate_generic($type_env))
        }
        
        # Roles done by this role need fully specializing also; all
        # they'll be missing is the target class (e.g. our first arg).
        for self.roles_to_compose($obj) {
            my $r := $_;
            if $_.HOW.archetypes.generic {
                $r := $r.HOW.instantiate_generic($r, $type_env);
                $conc.HOW.add_to_role_typecheck_list($conc, $r);
            }
            $conc.HOW.add_role($conc, $r.HOW.specialize($r, @pos_args[0]));
        }
        
        # Pass along any parents that have been added, resolving them in
        # the case they're generic (role Foo[::T] is T { })
        for self.parents($obj, :local(1)) {
            my $p := $_;
            if $_.HOW.archetypes.generic {
                $p := $p.HOW.instantiate_generic($p, $type_env);
            }
            $conc.HOW.add_parent($conc, $p);
        }
        
        # Resolve any array type being passed along (only really used in the
        # punning case, since roles are the way we get generic types).
        if self.is_array_type($obj) {
            my $at := self.array_type($obj);
            if $at.HOW.archetypes.generic {
                $at := $at.HOW.instantiate_generic($at, $type_env);
            }
            $conc.HOW.set_array_type($conc, $at);
        }
        
        $conc.HOW.compose($conc);
        return $conc;
    }
    
    method mro($obj) {
        [$obj]
    }
}
