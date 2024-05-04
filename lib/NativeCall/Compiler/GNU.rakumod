use nqp;
unit class NativeCall::Compiler::GNU;

use NativeCall::Types;

#- lookups ---------------------------------------------------------------------
my constant $type2letter = nqp::hash(
  'Bool',                         'b',
  'int16',                        's',
  'int32',                        'i',
  'int64',                        'x',
  'int8',                         'c',
  'NativeCall::Types::CArray',    '',   # recurse into .of
  'NativeCall::Types::long',      'l',
  'NativeCall::Types::longlong',  'x',
  'NativeCall::Types::Pointer',   '',   # recurse into .of
  'NativeCall::Types::ulong',     'm',
  'NativeCall::Types::ulonglong', 'y',
  'NativeCall::Types::void',      'v',
  'num32',                        'f',
  'num64',                        'd',
  'Str',                          'c',
  'uint16',                       't',
  'uint32',                       'j',
  'uint64',                       'y',
  'uint8',                        'h',
);

#- helper sub ------------------------------------------------------------------
my sub cpp_param_letter($type, str :$R = '', str :$P = '', str :$K = '') {
    my str $name = $type.^name;

    $R ~ $P ~ $K ~ nqp::ifnull(
      (nqp::atkey($type2letter, $name) || cpp_param_letter($type.of)),
      (nqp::chars($name) ~ $name)
    )
}

#- mangle_cpp_symbol -----------------------------------------------------------
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

# vim: expandtab shiftwidth=4
