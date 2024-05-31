use nqp;
unit class NativeCall::Compiler::MSVC;

use NativeCall::Types;

#- lookups ---------------------------------------------------------------------
my constant $type2letter = nqp::hash(
  'Bool',                         '_N',
  'int16',                        'F',
  'int32',                        'H',
  'int64',                        '_J',
  'int8',                         'D',
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
our sub mangle_cpp_symbol(Routine:D $routine, Str:D $symbol) {
    my     $package    := $routine.package;
    my     $signature  := $routine.signature;
    my int $is-method   = nqp::istype($routine, Method);
    my int $is-new      = $routine.name eq 'new';
    my int $has-returns = $signature.has_returns;

    if $is-new && nqp::not_i($has-returns) && !($package ~~ GLOBAL) {
        $signature.set_returns($package);
        $has-returns = 1;
    }

    my str @mangled = '?';
    if $is-method {
        nqp::push_s(
          @mangled,
          $symbol.split('::').reverse.map({
              $_ eq 'new' ?? '?0' !! "$_@"
          }).join
        );
        nqp::push_s(@mangled, '@');
        nqp::push_s(@mangled, $is-new ?? 'QE' !! 'UE');
    }
    else {
        nqp::push_s(
          @mangled,
          $symbol.split('::').reverse.map({"$_@"}).join
        );
        # http://en.wikipedia.org/wiki/Visual_C%2B%2B_name_mangling#Function_Property
        nqp::push_s(@mangled, '@YA');
        nqp::push_s(
          @mangled,
          $has-returns ?? cpp_param_letter($routine.returns) !! 'X'
        );
    }

    # Get parameters that matter
    my @params = $signature.params;
    if $is-method {
        @params.shift;  # self
        @params.pop if @params.tail.name eq '%_';
    }

    # Get any letters for parameters
    my str $params = @params.map({
        my str $P = .rw ?? 'PE'                           !! ''; # pointer
        my str $K = $P  ?? ($_.?cpp-const ?? 'B'  !! 'A') !! ''; # const
        cpp_param_letter(.type, $P ~ $K)
    }).join;

    # Embed any parameters
    if $is-method {
        nqp::push_s(@mangled, 'AA');
        nqp::push_s(@mangled, cpp_param_letter($routine.returns))
          if $has-returns && nqp::not_i($is-new);

        if nqp::chars($params) {
            nqp::push_s(@mangled, $params);
            nqp::push_s(@mangled, $is-new ?? '@Z' !! 'Z');
        }
        else {
            nqp::push_s(@mangled, $is-new ?? '@XZ' !! 'XZ');
        }
    }

    # Not a method
    else {
        nqp::push_s(@mangled, $params || 'X');
        nqp::push_s(@mangled, $package.REPR eq 'CPPStruct' ?? '@Z' !! 'Z');
    }

    nqp::join('', @mangled)
}

# vim: expandtab shiftwidth=4
