class NativeCall::Compiler::GNU;

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
        my $R = '';                               # reference
        my $P = .rw                 ?? 'P' !! ''; # pointer
        my $K = $P && $_.?cpp-const ?? 'K' !! ''; # const
        cpp_param_letter(.type, :$R, :$P, :$K)
    };
    $mangled ~= $params || 'v';
}

sub cpp_param_letter($type, :$R = '', :$P = '', :$K = '') {
    given $type {
        when NativeCall::Types::void {
            $R ~ $K ~ 'v'
        }
        when Bool {
            $R ~ $K ~ 'b'
        }
        when int8 {
            $R ~ $K ~ 'c'
        }
        when int16 {
            $R ~ $K ~ 's'
        }
        when int32 {
            $P ~ $K ~ 'i'
        }
        when NativeCall::Types::long {
            $R ~ $K ~ 'l'
        }
        when NativeCall::Types::longlong {
            $R ~ 'x'
        }
        when num32 {
            $R ~ $K ~ 'f'
        }
        when num64 {
            $R ~ $K ~ 'd'
        }
        when Str {
            'Pc'
        }
        when NativeCall::Types::CArray | NativeCall::Types::Pointer {
            'P' ~ $K ~ cpp_param_letter(.of);
        }
        default {
            my $name  = .^name;
            $P ~ $K ~ $name.chars ~ $name;
        }
    }
}
