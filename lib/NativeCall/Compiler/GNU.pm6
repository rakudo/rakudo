unit class NativeCall::Compiler::GNU;

use NativeCall::Types;

our sub mangle_cpp_symbol(Routine $r, $symbol) {
    $r.signature.set_returns($r.package)
        if $r.name eq 'new' && !$r.signature.has_returns && $r.package !~~ GLOBAL;

    my $mangled = '_Z'
                ~ ($r.package.REPR eq 'CPPStruct' ?? 'N' !! '')
                ~ $symbol.split('::').map({$_ eq 'new' ?? 'C1' !! $_.chars ~ $_}).join('')
                ~ ($r.package.REPR eq 'CPPStruct' ?? 'E' !! '');
    my @params  = $r.signature.params;
    if $r ~~ Method {
        @params.shift;
        @params.pop if @params[*-1].name eq '%_';
    }

    my $params = join '', @params.map: {
        my $R = $_.?cpp-ref                 ?? 'R' !! ''; # reference
        my $P = .rw                         ?? 'P' !! ''; # pointer
        my $K = ($R || $P) && $_.?cpp-const ?? 'K' !! ''; # const
        cpp_param_letter(.type, :$R, :$P, :$K)
    };
    $mangled ~= $params || 'v';
}

sub cpp_param_letter($type, :$R = '', :$P = '', :$K = '') {
    given $type {
        when NativeCall::Types::void {
            $R ~ $P ~ $K ~ 'v'
        }
        when Bool {
            $R ~ $P ~ $K ~ 'b'
        }
        when int8 {
            $R ~ $P ~ $K ~ 'c'
        }
        when int16 {
            $R ~ $P ~ $K ~ 's'
        }
        when int32 {
            $R ~ $P ~ $K ~ 'i'
        }
        when NativeCall::Types::long {
            $R ~ $P ~ $K ~ 'l'
        }
        when NativeCall::Types::longlong {
            $R ~ $P ~ $K ~ 'x'
        }
        when num32 {
            $R ~ $P ~ $K ~ 'f'
        }
        when num64 {
            $R ~ $P ~ $K ~ 'd'
        }
        when Str {
            'Pc'
        }
        when NativeCall::Types::CArray | NativeCall::Types::Pointer {
            'P' ~ $K ~ cpp_param_letter(.of);
        }
        default {
            my $name  = .^name;
            $R ~ $P ~ $K ~ $name.chars ~ $name;
        }
    }
}
