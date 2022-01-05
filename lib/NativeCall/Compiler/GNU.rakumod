unit class NativeCall::Compiler::GNU;

use NativeCall::Types;

our sub mangle_cpp_symbol(Routine $r, $symbol) {
    $r.signature.set_returns($r.package)
        if $r.name eq 'new' && !$r.signature.has_returns && $r.package !~~ GLOBAL;

    my $is-cpp-struct = $r.package.REPR eq 'CPPStruct';
    my @parts         = $symbol.split: '::';
    my $mangled       = '_Z';
    $mangled ~= 'N' if $is-cpp-struct;
    $mangled ~= 'K' if $r.?cpp-const;
    $mangled ~= .chars ~ $_ for @parts[0..*-2];
    if +@parts >= 2 && (@parts.tail eq 'new' || @parts[*-2] eq @parts[*-1]) {
        $mangled ~= 'C1';
    } else {
        $mangled ~= @parts.tail.chars ~ @parts.tail
    }
    $mangled ~= 'E' if $is-cpp-struct;

    my @params = $r.signature.params;
    if $r ~~ Method {
        @params.shift;
        @params.pop if @params.tail.name eq '%_';
    }

    my $params = join '', @params.map: {
        my $R = .?cpp-ref                 ?? 'R' !! ''; # reference
        my $P = .rw                       ?? 'P' !! ''; # pointer
        $P ~= 'P' if .type ~~ Str | NativeCall::Types::Pointer | NativeCall::Types::CArray;
        my $K = ($R || $P) && .?cpp-const ?? 'K' !! ''; # const
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
        when uint8 {
            $R ~ $P ~ $K ~ 'h'
        }
        when int16 {
            $R ~ $P ~ $K ~ 's'
        }
        when uint16 {
            $R ~ $P ~ $K ~ 't'
        }
        when int32 {
            $R ~ $P ~ $K ~ 'i'
        }
        when uint32 {
            $R ~ $P ~ $K ~ 'j'
        }
        when NativeCall::Types::long {
            $R ~ $P ~ $K ~ 'l'
        }
        when NativeCall::Types::ulong {
            $R ~ $P ~ $K ~ 'm'
        }
        when int64 {
            $R ~ $P ~ $K ~ 'x'
        }
        when NativeCall::Types::longlong {
            $R ~ $P ~ $K ~ 'x'
        }
        when uint64 {
            $R ~ $P ~ $K ~ 'y'
        }
        when NativeCall::Types::ulonglong {
            $R ~ $P ~ $K ~ 'y'
        }
        when num32 {
            $R ~ $P ~ $K ~ 'f'
        }
        when num64 {
            $R ~ $P ~ $K ~ 'd'
        }
        when Str {
            $P ~ $K ~ 'c'
        }
        when NativeCall::Types::CArray | NativeCall::Types::Pointer {
            $P ~ $K ~ cpp_param_letter(.of);
        }
        default {
            my $name  = .^name;
            $R ~ $P ~ $K ~ $name.chars ~ $name;
        }
    }
}

# vim: expandtab shiftwidth=4
