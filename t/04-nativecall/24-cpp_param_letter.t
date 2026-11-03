use lib <lib>;
use NativeCall::Compiler::GNU;
use NativeCall::Compiler::MSVC;
use NativeCall::Types;
use Test;

# Original implementation of GNU's cpp_param_letter
sub GNU_cpp_param_letter($type, :$R = '', :$P = '', :$K = '') {
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
            $P ~ $K ~ GNU_cpp_param_letter(.of);
        }
        default {
            my $name  = .^name;
            $R ~ $P ~ $K ~ $name.chars ~ $name;
        }
    }
}

# Original implementation of MSVC's cpp_param_letter
sub MSVC_cpp_param_letter($type, :$R = '', :$P = '', :$K = '') {
    given $type {
        when NativeCall::Types::void {
            $R ~ $K ~ 'X'
        }
        when Bool {
            $R ~ $K ~ '_N'
        }
        when int8 {
            $R ~ $K ~ 'D'
        }
        when uint8 {
            $R ~ $K ~ 'E'
        }
        when int16 {
            $R ~ $K ~ 'F'
        }
        when uint16 {
            $R ~ $K ~ 'G'
        }
        when int32 {
            $P ~ $K ~ 'H'
        }
        when uint32 {
            $P ~ $K ~ 'I'
        }
        when NativeCall::Types::long {
            $R ~ $K ~ 'J'
        }
        when NativeCall::Types::ulong {
            $R ~ $K ~ 'K'
        }
        when int64 {
            $R ~ '_J'
        }
        when NativeCall::Types::longlong {
            $R ~ '_J'
        }
        when uint64 {
            $R ~ '_K'
        }
        when NativeCall::Types::ulonglong {
            $R ~ '_K'
        }
        when num32 {
            $R ~ $K ~ 'M'
        }
        when num64 {
            $R ~ $K ~ 'N'
        }
        when Str {
            'PEAD'
        }
        when NativeCall::Types::CArray {
            'QEA' ~ MSVC_cpp_param_letter(.of);
        }
        when NativeCall::Types::Pointer {
            'PEA' ~ MSVC_cpp_param_letter(.of);
        }
        default {
            my $name  = .^name;
            $P ~ $K ~ $name.chars ~ $name;
        }
    }
}

my constant @types = 
  Bool,
  int16,
  int32,
  int64,
  int8,
  NativeCall::Types::CArray[int],
  NativeCall::Types::long,
  NativeCall::Types::longlong,
  NativeCall::Types::Pointer[int],
  NativeCall::Types::ulong,
  NativeCall::Types::ulonglong,
  NativeCall::Types::void,
  num32,
  num64,
  Str,
  uint16,
  uint32,
  uint64,
  uint8,
;

plan 4 * @types;

# GNU
{
    my constant &cpp_param_letter =
      &NativeCall::Compiler::GNU::cpp_param_letter;

    for @types -> $type {
        my ($R, $P, $K) = ("a".."z").pick(3);

        # Apparently R is never set for these types
        $R = '' if $type ~~ Str
                          | NativeCall::Types::CArray
                          | NativeCall::Types::Pointer;

        my $expected := GNU_cpp_param_letter($type, :$R, :$P, :$K);
        isa-ok $expected, Str;
        is cpp_param_letter($type, "$R$P$K"), $expected,
           "test GNU $type.^name() with :$R, :$P, :$K";
    }
}

# MSVC
{
    my constant &cpp_param_letter =
      &NativeCall::Compiler::MSVC::cpp_param_letter;

    for @types -> $type {
        my ($R, $P, $K) = ("a".."z").pick(3);

        my $expected := MSVC_cpp_param_letter($type, :$R, :$P, :$K);
        isa-ok $expected, Str;
        is cpp_param_letter($type, :$R, :$P, :$K), $expected,
           "test MSVC $type.^name() with :$R, :$P, :$K";
    }
}

# vim: expandtab shiftwidth=4
