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
our sub cpp_param_letter($type is raw, str $RPK = '') {

    if nqp::istype($type,NativeCall::Types::CArray)
      || nqp::istype($type,NativeCall::Types::Pointer) {
        $RPK ~ cpp_param_letter($type.of)
    }
    else {
        my str $name = $type.^name;

        $RPK ~ nqp::ifnull(
          nqp::atkey($type2letter, $name),
          (nqp::chars($name) ~ $name)
        )
    }
}

#- mangle_cpp_symbol -----------------------------------------------------------
our sub mangle_cpp_symbol(Routine:D $routine, Str:D $symbol) {
    my $package   := $routine.package;
    my $signature := $routine.signature;

    $signature.set_returns($package)
      if $routine.name eq 'new'
      && !$signature.has_returns
      && !($package ~~ GLOBAL);

    my $is-cpp-struct := $package.REPR eq 'CPPStruct';
    my str @parts      = $symbol.split: '::';
    my int $last       = nqp::elems(@parts) - 1;

    my str @mangled = '_Z';
    nqp::push_s(@mangled, "N") if $is-cpp-struct;
    nqp::push_s(@mangled, "K") if $routine.?cpp-const;

    # Handle all name parts except the last
    my str $part;
    my int $i;
    nqp::while(
      $i < $last,
      nqp::stmts(
        ($part = nqp::atpos_s(@parts, $i++)),
        nqp::push_s(@mangled, nqp::concat(nqp::chars($part), $part))
      )
    );

    # Handle the final name part
    nqp::push_s(
      @mangled,
      nqp::stmts(
        ($part = nqp::atpos_s(@parts, $last)),
        nqp::if(
          nqp::elems(@parts) >= 2
            && ($part eq 'new' || nqp::atpos_s(@parts, $last - 1) eq $part),
          'C1',
          nqp::concat(nqp::chars($part), $part)
        )
      )
    );
    nqp::push_s(@mangled, "E") if $is-cpp-struct;

    # Get parameters that matter
    my @params = $signature.params;
    if nqp::istype($routine, Method) {
        @params.shift;  # self
        @params.pop if @params.tail.name eq '%_';
    }

    # Add any letters for parameters
    nqp::push_s(
      @mangled,
      @params.map({
          my str $R = .?cpp-ref ?? 'R' !! ''; # reference
          my str $P = .rw       ?? 'P' !! ''; # pointer

          my $type := .type;
          $P ~= 'P'
            if nqp::istype($type, Str)
            || nqp::istype($type, NativeCall::Types::Pointer)
            || nqp::istype($type, NativeCall::Types::CArray);

          my str $K = ($R || $P) && .?cpp-const ?? 'K' !! ''; # const
          cpp_param_letter($type, $R ~ $P ~ $K)
      }).join || 'v'
    );

    nqp::join('', @mangled)
}

# vim: expandtab shiftwidth=4
