# Sometimes, we see references to roles that provide parameters but
# do not fully resolve them. For example, in:
# 
#   class C does R[T] { }
#
# We need to represent R[T], but we cannot yet fully specialize the
# role because we don't have the first parameter to hand. We may also
# run into the issue where we have things like:
# 
#   sub foo(R[T] $x) { ... }
#   if $x ~~ R[T] { ... }
# 
# Where we clearly want to talk about a partial parameterization of a
# role and actually want to do so in a way distinct from a particular
# instantiation of it. This meta-object represents those "partial types"
# as both a way to curry on your way to a full specialization, but also
# as a way to do type-checking or punning.
class Perl6::Metamodel::CurriedRoleHOW
    does Perl6::Metamodel::RolePunning
    does Perl6::Metamodel::TypePretense
{
    has $!curried_role;
    has @!pos_args;
    has %!named_args;

    my $archetypes_g := Perl6::Metamodel::Archetypes.new( :composable(1), :inheritalizable(1), :parametric(1), :generic(1) );
    my $archetypes_ng := Perl6::Metamodel::Archetypes.new( :nominal(1), :composable(1), :inheritalizable(1), :parametric(1) );
    method archetypes() {
        if nqp::isconcrete(self) {
            for @!pos_args {
                if $_.HOW.archetypes.generic {
                    return $archetypes_g;
                }
            }
            for %!named_args {
                if $_.value.HOW.archetypes.generic {
                    return $archetypes_g;
                }
            }
        }
        $archetypes_ng
    }
    
    method new(*%named) {
        nqp::findmethod(NQPMu, 'BUILDALL')(nqp::create(self), |%named)
    }
    
    method new_type($curried_role, *@pos_args, *%named_args) {
        my $meta := self.new(:curried_role($curried_role), :pos_args(@pos_args),
            :named_args(%named_args));
        my $type := nqp::settypehll(nqp::newtype($meta, 'Uninstantiable'), 'perl6');
        nqp::settypecheckmode($type, 2)
    }
    
    method instantiate_generic($obj, $type_env) {
        my @new_pos;
        my %new_named;
        for @!pos_args {
            @new_pos.push($_.HOW.archetypes.generic ??
                $_.HOW.instantiate_generic($_, $type_env) !!
                $_);
        }
        for %!named_args {
            %new_named{$_.key} := $_.value.HOW.archetypes.generic ??
                $_.value.HOW.instantiate_generic($_.value, $type_env) !!
                $_.value;
        }
        self.new_type($!curried_role, |@new_pos, |%new_named)
    }
    
    method specialize($obj, $first_arg) {
        $!curried_role.HOW.specialize($!curried_role, $first_arg,
            |@!pos_args, |%!named_args);
    }
    
    method name($obj) {
        $!curried_role.HOW.name($!curried_role)
    }
    
    method curried_role($obj) {
        $!curried_role
    }
    
    method role_arguments($obj) {
        @!pos_args
    }
    
    method roles($obj, :$transitive) {
        $!curried_role.HOW.roles($obj, :transitive($transitive))
    }
    
    method role_typecheck_list($obj) {
        $!curried_role.HOW.role_typecheck_list($obj)
    }
    
    method type_check($obj, $checkee) {
        $!curried_role.HOW.type_check($!curried_role, $checkee)
    }
    
    method accepts_type($obj, $checkee) {
        # First, we locate candidate curryings to check against. If
        # the checkee is itself a curried role, it also goes in. Note
        # that we only want those that have the same parametric role
        # as us.
        my @cands;
        my $crdc := nqp::decont($!curried_role);
        if nqp::istype($checkee.HOW, self.WHAT) {
            if nqp::decont($checkee.HOW.curried_role($checkee)) =:= $crdc {
                @cands.push($checkee);
            }
        }
        if nqp::can($checkee.HOW, 'role_typecheck_list') {
            for $checkee.HOW.role_typecheck_list($checkee) {
                if nqp::istype($_.HOW, self.WHAT) && !$_.HOW.archetypes.generic {
                    if nqp::decont($_.HOW.curried_role($_)) =:= $crdc {
                        @cands.push($_);
                    }
                }
            }
        }
        
        # Provided we have some candidates, check the arguments.
        my $num_args := +@!pos_args;
        if @cands {
            for @cands {
                my @try_args := $_.HOW.role_arguments($_);
                if +@try_args == $num_args {
                    my $i := 0;
                    my $ok := 1;
                    while $i < +$num_args {
                        if !@!pos_args[$i].ACCEPTS(@try_args[$i]) {
                            $ok := 0;
                            $i := $num_args;
                        }
                        $i := $i + 1;
                    }
                    if $ok {
                        return 1;
                    }
                }
            }
        }
        
        0;
    }
}
