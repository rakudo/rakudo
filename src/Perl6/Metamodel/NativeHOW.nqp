class Perl6::Metamodel::NativeHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::Documenting
    does Perl6::Metamodel::Versioning
    does Perl6::Metamodel::Stashing
    does Perl6::Metamodel::MultipleInheritance
    does Perl6::Metamodel::C3MRO
    does Perl6::Metamodel::MROBasedMethodDispatch
    does Perl6::Metamodel::MROBasedTypeChecking
{
    has $!nativesize;
    has int $!unsigned;
    has $!composed;

    my $archetypes := Perl6::Metamodel::Archetypes.new( :nominal(1) );
    method archetypes() {
        $archetypes
    }

    method new(*%named) {
        nqp::findmethod(NQPMu, 'BUILDALL')(nqp::create(self), |%named)
    }

    method new_type(:$name = '<anon>', :$repr = 'P6opaque', :$ver, :$auth, :$api) {
        my $metaclass := self.new();
        my $obj := nqp::settypehll(nqp::newtype($metaclass, $repr), 'Raku');
        $metaclass.set_name($obj, $name);
        $metaclass.set_ver($obj, $ver);
        $metaclass.set_auth($obj, $auth) if $auth;
        $metaclass.set_api($obj, $api) if $api;
        self.add_stash($obj);
    }

    method compose($the-obj, :$compiler_services) {
        my $obj := nqp::decont($the-obj);

        self.compute_mro($obj);
        self.publish_method_cache($obj);
        self.publish_type_cache($obj);
        if !$!composed && ($!nativesize || $!unsigned) {
            my $info := nqp::hash();
            $info<integer> := nqp::hash();
            $info<integer><unsigned> := 1 if $!unsigned;
            $info<float> := nqp::hash();
            if nqp::objprimspec($!nativesize) {
                $info<integer><bits> := $!nativesize;
                $info<float><bits>   := $!nativesize;
            }
            else {
                if $!nativesize {
                    $info<integer><bits> := nqp::unbox_i($!nativesize);
                    $info<float><bits>   := nqp::unbox_i($!nativesize);
                }
            }
            nqp::composetype($obj, $info);
        }
        $!composed := 1;
    }

    method is_composed($obj) {
        $!composed
    }

    method set_ctype($obj, $ctype) {
        if $ctype eq 'char' {
            $!nativesize := nqp::const::C_TYPE_CHAR;
        }
        elsif $ctype eq 'short' {
            $!nativesize := nqp::const::C_TYPE_SHORT;
        }
        elsif $ctype eq 'int' {
            $!nativesize := nqp::const::C_TYPE_INT;
        }
        elsif $ctype eq 'long' {
            $!nativesize := nqp::const::C_TYPE_LONG;
        }
        elsif $ctype eq 'longlong' {
            $!nativesize := nqp::const::C_TYPE_LONGLONG;
        }
        elsif $ctype eq 'float' {
            $!nativesize := nqp::const::C_TYPE_FLOAT;
        }
        elsif $ctype eq 'double' {
            $!nativesize := nqp::const::C_TYPE_DOUBLE;
        }
        elsif $ctype eq 'longdouble' {
            $!nativesize := nqp::const::C_TYPE_LONGDOUBLE;
        }
        elsif $ctype eq 'bool' {
            $!nativesize := nqp::const::C_TYPE_BOOL;
        }
        elsif $ctype eq 'size_t' {
            $!nativesize := nqp::const::C_TYPE_SIZE_T;
        }
        elsif $ctype eq 'atomic' {
            $!nativesize := nqp::const::C_TYPE_ATOMIC_INT;
        }
        else {
            nqp::die("Unhandled C type '$ctype'")
        }
    }

    method set_nativesize($obj, $nativesize) {
        $!nativesize := $nativesize;
    }

    method nativesize($obj) {
        $!nativesize
    }

    method set_unsigned($obj, $unsigned) {
        $!unsigned := $unsigned ?? 1 !! 0
    }

    method unsigned($obj) {
        $!unsigned
    }

    method method_table($obj) {
        nqp::hash('new',
          nqp::getstaticcode(sub (*@_, *%_) {
            nqp::die('Cannot instantiate a native type')
          }))
    }
    method submethod_table($obj) { nqp::hash() }
}

# vim: expandtab sw=4
