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
    has int $!ctype;
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
        my $obj := nqp::settypehll(nqp::newtype($metaclass, $repr), 'perl6');
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
        if !$!composed && ($!ctype || $!nativesize || $!unsigned) {
            my $info := nqp::hash();
            $info<integer>           := nqp::hash();
            $info<float>             := nqp::hash();
            if nqp::isconcrete($!ctype) {
                $info<integer><nativetype> := $!ctype;
                $info<float><nativetype>   := $!ctype;
            }
            if nqp::objprimspec($!nativesize) {
                $info<integer><bits> := $!nativesize;
                $info<float><bits>   := $!nativesize;
            }
            elsif $!nativesize {
                $info<integer><bits> := nqp::unbox_i($!nativesize);
                $info<float><bits>   := nqp::unbox_i($!nativesize);
            }
            if $!unsigned {
                $info<integer><unsigned> := $!unsigned;
            }
            nqp::composetype($obj, $info);
        }
        $!composed := 1;
    }

    method is_composed($obj) {
        $!composed
    }

    method set_ctype($obj, $ctype) {
        my str $reprname := nqp::reprname($obj);
        if $reprname eq 'P6int' {
            if $ctype eq 'char' {
                $!ctype := nqp::const::P6INT_C_TYPE_CHAR;
            }
            elsif $ctype eq 'short' {
                $!ctype := nqp::const::P6INT_C_TYPE_SHORT;
            }
            elsif $ctype eq 'int' {
                $!ctype := nqp::const::P6INT_C_TYPE_INT;
            }
            elsif $ctype eq 'long' {
                $!ctype := nqp::const::P6INT_C_TYPE_LONG;
            }
            elsif $ctype eq 'longlong' {
                $!ctype := nqp::const::P6INT_C_TYPE_LONGLONG;
            }
            elsif $ctype eq 'bool' {
                $!ctype := nqp::const::P6INT_C_TYPE_BOOL;
            }
            elsif $ctype eq 'size_t' {
                $!ctype := nqp::const::P6INT_C_TYPE_SIZE_T;
            }
            elsif $ctype eq 'atomic' {
                $!ctype := nqp::const::P6INT_C_TYPE_ATOMIC_INT;
            }
            elsif $ctype eq 'wchar_t' {
                $!ctype := nqp::const::P6INT_C_TYPE_WCHAR_T;
            }
            elsif $ctype eq 'wint_t' {
                $!ctype := nqp::const::P6INT_C_TYPE_WINT_T;
            }
            elsif $ctype eq 'char16_t' {
                $!ctype := nqp::const::P6INT_C_TYPE_CHAR16_T;
            }
            elsif $ctype eq 'char32_t' {
                $!ctype := nqp::const::P6INT_C_TYPE_CHAR32_T;
            }
            else {
                nqp::die("Unhandled C type '$ctype'");
            }
        }
        elsif $reprname eq 'P6num' {
            if $ctype eq 'float' {
                $!ctype := nqp::const::P6NUM_C_TYPE_FLOAT;
            }
            elsif $ctype eq 'double' {
                $!ctype := nqp::const::P6NUM_C_TYPE_DOUBLE;
            }
            elsif $ctype eq 'longdouble' {
                $!ctype := nqp::const::P6NUM_C_TYPE_LONGDOUBLE;
            }
            else {
                nqp::die("Unhandled C type '$ctype'");
            }
        }
        else {
            nqp::die("Unhandled C type '$ctype'")
        }
    }

    method ctype($obj) {
        my str $reprname := nqp::reprname($obj);
        if $reprname eq 'P6int' {
            if $!ctype == nqp::const::P6INT_C_TYPE_CHAR {
                'char'
            }
            elsif $!ctype == nqp::const::P6INT_C_TYPE_SHORT {
                'short'
            }
            elsif $!ctype == nqp::const::P6INT_C_TYPE_INT {
                'int'
            }
            elsif $!ctype == nqp::const::P6INT_C_TYPE_LONG {
                'long'
            }
            elsif $!ctype == nqp::const::P6INT_C_TYPE_LONGLONG {
                'longlong'
            }
            elsif $!ctype == nqp::const::P6INT_C_TYPE_BOOL {
                'bool'
            }
            elsif $!ctype == nqp::const::P6INT_C_TYPE_SIZE_T {
                'size_t'
            }
            elsif $!ctype == nqp::const::P6INT_C_TYPE_ATOMIC_INT {
                'atomic'
            }
            elsif $!ctype == nqp::const::P6INT_C_TYPE_WCHAR_T {
                'wchar_t'
            }
            elsif $!ctype == nqp::const::P6INT_C_TYPE_WINT_T {
                'wint_t'
            }
            elsif $!ctype == nqp::const::P6INT_C_TYPE_CHAR16_T {
                'char16_t'
            }
            elsif $!ctype == nqp::const::P6INT_C_TYPE_CHAR32_T {
                'char32_t'
            }
            else {
                ''
            }
        }
        elsif $reprname eq 'P6num' {
            if $!ctype == nqp::const::P6NUM_C_TYPE_FLOAT {
                'float'
            }
            elsif $!ctype == nqp::const::P6NUM_C_TYPE_DOUBLE {
                'double'
            }
            elsif $!ctype == nqp::const::P6NUM_C_TYPE_LONGDOUBLE {
                'longdouble'
            }
            else {
                ''
            }
        }
        else {
            ''
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
