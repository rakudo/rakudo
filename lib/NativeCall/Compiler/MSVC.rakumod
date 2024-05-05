unit class NativeCall::Compiler::MSVC;

use NativeCall::Types;

our sub mangle_cpp_symbol(Routine $r, $symbol) {
    $r.signature.set_returns($r.package)
        if $r.name eq 'new' && !$r.signature.has_returns && $r.package !~~ GLOBAL;

    my $mangled = '?';
    if $r ~~ Method {
        $mangled ~= $symbol.split('::').reverse.map({$_ eq 'new' ?? '?0' !! "$_@"}).join('')
                  ~ '@'
                  ~ ($r.name eq 'new' ?? 'QE' !! 'UE');
    }
    else {
        $mangled ~= $symbol.split('::').reverse.map({"$_@"}).join('')
                  ~ '@'
                  ~ 'Y'
                  ~ 'A' # http://en.wikipedia.org/wiki/Visual_C%2B%2B_name_mangling#Function_Property
                  ~ ($r.signature.has_returns ?? cpp_param_letter($r.returns) !! 'X');
    }

    my @params  = $r.signature.params;
    if $r ~~ Method {
        @params.shift;
        @params.pop if @params.tail.name eq '%_';
    }

    my $params = join '', @params.map: {
        my str $P = .rw ?? 'PE'                           !! ''; # pointer
        my str $K = $P  ?? ($_.?cpp-const ?? 'B'  !! 'A') !! ''; # const
        cpp_param_letter(.type, $P ~ $K)
    };
    if $r ~~ Method {
        $mangled ~= 'AA';
        $mangled ~= $r.signature.has_returns && $r.name ne 'new'
          ?? cpp_param_letter($r.returns)
          !! '';
        $mangled ~= $params;
        $mangled ~= '@' if $params || $r.name eq 'new';
        $mangled ~= $params ?? 'Z' !! 'XZ';
    }
    else {
        $mangled ~= $params || 'X';
        $mangled ~= '@' if $r.package.REPR eq 'CPPStruct';
        $mangled ~= 'Z';
    }
    $mangled
}

sub cpp_param_letter($type, str $PK = '') {
    given $type {
        when NativeCall::Types::void {
            $PK ~ 'X'
        }
        when Bool {
            $PK ~ '_N'
        }
        when int8 {
            $PK ~ 'D'
        }
        when uint8 {
            $PK ~ 'E'
        }
        when int16 {
            $PK ~ 'F'
        }
        when uint16 {
            $PK ~ 'G'
        }
        when int32 {
            $PK ~ 'H'
        }
        when uint32 {
            $PK ~ 'I'
        }
        when NativeCall::Types::long {
            $PK ~ 'J'
        }
        when NativeCall::Types::ulong {
            $PK ~ 'K'
        }
        when int64 {
            '_J'
        }
        when NativeCall::Types::longlong {
            '_J'
        }
        when uint64 {
            '_K'
        }
        when NativeCall::Types::ulonglong {
            '_K'
        }
        when num32 {
            $PK ~ 'M'
        }
        when num64 {
            $PK ~ 'N'
        }
        when Str {
            'PEAD'
        }
        when NativeCall::Types::CArray {
            'QEA' ~ cpp_param_letter(.of);
        }
        when NativeCall::Types::Pointer {
            'PEA' ~ cpp_param_letter(.of);
        }
        default {
            my $name  = .^name;
            $PK ~ $name.chars ~ $name;
        }
    }
}

# vim: expandtab shiftwidth=4
