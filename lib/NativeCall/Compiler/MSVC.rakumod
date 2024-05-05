use nqp;
unit class NativeCall::Compiler::MSVC;

use NativeCall::Types;

#- lookups ---------------------------------------------------------------------
my constant $type2letter = nqp::hash(
  'Bool',                         '_N',
  'int16',                        'F',
  'int32',                        'H',
  'int64',                        '_J',
  'int8',                         'c',
  'NativeCall::Types::CArray',    'QEA*',   # recurse into .of
  'NativeCall::Types::long',      'J',
  'NativeCall::Types::longlong',  '_J',
  'NativeCall::Types::Pointer',   'PEA*',   # recurse into .of
  'NativeCall::Types::ulong',     'K',
  'NativeCall::Types::ulonglong', '_K',
  'NativeCall::Types::void',      'X',
  'num32',                        'M',
  'num64',                        'N',
  'Str',                          'PEAD',
  'uint16',                       'G',
  'uint32',                       'I',
  'uint64',                       '_K',
  'uint8',                        'E',
);

#- helper sub ------------------------------------------------------------------
my sub cpp_param_letter($type, str $PK = '') {
    my str $name   = $type.^name;
    my str $letter = $PK ~ nqp::ifnull(
      nqp::atkey($type2letter, $name),
      (nqp::chars($name) ~ $name)
    );

    $letter.ends-with('*')
      ?? nqp::substr($letter, 0, nqp::chars($letter) - 1)
           ~  cpp_param_letter($type.of)
      !! $letter
}

#- mangle_cpp_symbol -----------------------------------------------------------
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

# vim: expandtab shiftwidth=4
