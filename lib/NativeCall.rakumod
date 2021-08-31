use nqp;
use QAST:from<NQP>;

use NativeCall::Types;
use NativeCall::Compiler::GNU;
use NativeCall::Compiler::MSVC;

my constant $repr_map = nqp::hash(
  "CArray",    "carray",
  "CPPStruct", "cppstruct",
  "CPointer",  "cpointer",
  "CStruct",   "cstruct",
  "CUnion",    "cunion",
  "VMArray",   "vmarray",
);

module NativeCall {

my constant long          is export(:types, :DEFAULT) = NativeCall::Types::long;
my constant longlong      is export(:types, :DEFAULT) = NativeCall::Types::longlong;
my constant ulong         is export(:types, :DEFAULT) = NativeCall::Types::ulong;
my constant ulonglong     is export(:types, :DEFAULT) = NativeCall::Types::ulonglong;
my constant bool          is export(:types, :DEFAULT) = NativeCall::Types::bool;
my constant size_t        is export(:types, :DEFAULT) = NativeCall::Types::size_t;
my constant ssize_t       is export(:types, :DEFAULT) = NativeCall::Types::ssize_t;
my constant void          is export(:types, :DEFAULT) = NativeCall::Types::void;
my constant CArray        is export(:types, :DEFAULT) = NativeCall::Types::CArray;
my constant Pointer       is export(:types, :DEFAULT) = NativeCall::Types::Pointer;
my constant OpaquePointer is export(:types, :DEFAULT) = NativeCall::Types::Pointer;


# Role for carrying extra calling convention information.
my role NativeCallingConvention[$name] {
    method native_call_convention() { $name };
}

# Role for carrying extra string encoding information.
my role NativeCallEncoded[$name] {
    method native_call_encoded() { $name };
}

my role NativeCallMangled[$name] {
    method native_call_mangled() { $name }
}


# Throwaway type just to get us some way to get at the NativeCall
# representation.
my class native_callsite is repr('NativeCall') { }

# Maps a chosen string encoding to a type recognized by the native call engine.
sub string_encoding_to_nci_type(\encoding) {
    my str $enc = encoding;
    nqp::iseq_s($enc,"utf8")
      ?? "utf8str"
      !! nqp::iseq_s($enc,"ascii")
        ?? "asciistr"
        !! nqp::iseq_s($enc,"utf16")
          ?? "utf16str"
          !! die "Unknown string encoding for native call: $enc"
}

# Builds a hash of type information for the specified parameter.
sub param_hash_for(Parameter $p) {
    my Mu $result := nqp::hash();
    my $type := $p.type();
    nqp::bindkey($result, 'typeobj', nqp::decont($type));
    nqp::bindkey($result, 'rw', nqp::unbox_i(1)) if $p.rw;
    if $type ~~ Str {
        my $enc := $p.?native_call_encoded() || 'utf8';
        nqp::bindkey($result, 'type', nqp::unbox_s(string_encoding_to_nci_type($enc)));
        nqp::bindkey($result, 'free_str', nqp::unbox_i(1));
    }
    elsif $type ~~ Callable {
        nqp::bindkey($result, 'type', nqp::unbox_s(type_code_for($type)));
        my $info := param_list_for($p.sub_signature);
        nqp::unshift($info, return_hash_for($p.sub_signature, :with-typeobj));
        nqp::bindkey($result, 'callback_args', $info);
    }
    else {
        nqp::bindkey($result, 'type', nqp::unbox_s(type_code_for($type)));
    }
    $result
}

# Builds the list of parameter information for a callback argument.
sub param_list_for(Signature $sig, &r?) {
    my $params   := nqp::getattr($sig.params,List,'$!reified');
    my int $elems = nqp::elems($params);

    # not sending Method's default slurpy *%_ (which is always last)
    --$elems
      if nqp::istype(&r,Method)
      && nqp::iseq_s(nqp::atpos($params,$elems - 1).name,'%_');

    # build list
    my $result := nqp::setelems(nqp::list,$elems);
    my int $i   = -1;
    nqp::bindpos($result,$i,
      param_hash_for(nqp::atpos($params,$i))
    ) while nqp::islt_i($i = nqp::add_i($i,1),$elems);

    $result
}

# Builds a hash of type information for the specified return type.
sub return_hash_for(Signature $s, &r?, :$with-typeobj, :$entry-point, :$resolve-libname, :$resolve-libname-arg) {
    my Mu $result := nqp::hash();
    my $returns := $s.returns;
    nqp::bindkey($result, 'typeobj',     nqp::decont($returns))     if $with-typeobj;
    nqp::bindkey($result, 'entry_point', nqp::decont($entry-point)) if $entry-point;
    nqp::bindkey($result, 'resolve_lib_name_arg', nqp::decont($resolve-libname-arg)) if $resolve-libname-arg;
    nqp::bindkey($result, 'resolve_lib_name', nqp::getattr(nqp::decont($resolve-libname), Code, '$!do')) if $resolve-libname;
    if $returns ~~ Str {
        my $enc := &r.?native_call_encoded() || 'utf8';
        nqp::bindkey($result, 'type', nqp::unbox_s(string_encoding_to_nci_type($enc)));
        nqp::bindkey($result, 'free_str', nqp::unbox_i(0));
    }
    # TODO: If we ever want to handle function pointers returned from C, this
    # bit of code needs to handle that.
    else {
        nqp::bindkey($result, 'type',
            $returns =:= Mu ?? 'void' !! nqp::unbox_s(type_code_for($returns)));
    }
    $result
}

sub nativesizeof($obj) is export(:DEFAULT) {
    nqp::nativecallsizeof($obj)
}

my constant $signed_ints_by_size =
  nqp::list_s( "", "char", "short", "", "int", "", "", "", "longlong" );

# Gets the NCI type code to use based on a given Raku type.
my constant $type_map = nqp::hash(
  "Bool",       nqp::atpos_s($signed_ints_by_size,nativesizeof(bool)),
  "bool",       nqp::atpos_s($signed_ints_by_size,nativesizeof(bool)),
  "Callable",   "callback",
  "Int",        "longlong",
  "int",        "long",
  "int16",      "short",
  "int32",      "int",
  "int64",      "longlong",
  "int8",       "char",
  "long",       "long",
  "longdouble", "longdouble",
  "longlong",   "longlong",
  "Num",        "double",
  "num",        "double",
  "num32",      "float",
  "num64",      "double",
  "size_t",     nqp::atpos_s($signed_ints_by_size,nativesizeof(size_t)),
  "ssize_t",    nqp::atpos_s($signed_ints_by_size,nativesizeof(ssize_t)),
  "uint",       "ulong",
  "uint16",     "ushort",
  "uint32",     "uint",
  "uint64",     "ulonglong",
  "uint8",      "uchar",
  "ulong",      "ulong",
  "ulonglong",  "ulonglong",
);

sub type_code_for(Mu ::T) {
    if nqp::atkey($type_map,T.^shortname) -> $type {
        $type
    }
    elsif nqp::atkey($repr_map,T.REPR) -> $type {
        $type
    }
    # the REPR of a Buf or Blob type object is Uninstantiable, so
    # needs an extra special case here that isn't covered in the
    # hash lookup above.
    elsif nqp::istype(T,Blob) {
        "vmarray"
    }
    elsif nqp::istype(T,Pointer) {
        "cpointer"
    }
    else {
        die
"Unknown type {T.^name} used in native call.\n" ~
"If you want to pass a struct, be sure to use the CStruct or\n" ~
"CPPStruct representation.\n" ~
"If you want to pass an array, be sure to use the CArray type.";
    }
}

sub gen_native_symbol(Routine $r, $name, :$cpp-name-mangler) {
    if ! $r.?native_call_mangled {
        # Native symbol or name is said to be already mangled
        $r.?native_symbol // $name;
    } elsif $r.package.REPR eq 'CPPStruct' {
        # Mangle C++ classes
        $cpp-name-mangler($r, $r.?native_symbol // ($r.package.^name ~ '::' ~ $name));
    } else {
        # Mangle C
        $cpp-name-mangler($r, $r.?native_symbol // $name)
    }
}

multi sub map_return_type(Mu $type) { Mu }
multi sub map_return_type($type) {
    nqp::istype($type, Int) ?? Int
                            !! nqp::istype($type, Num) ?? Num !! $type;
}

my role NativeCallSymbol[Str $name] {
    method native_symbol()  { $name }
}

multi guess_library_name(IO::Path $lib) is export(:TEST) {
    guess_library_name($lib.absolute)
}
multi guess_library_name(Distribution::Resource $lib) is export(:TEST) {
    $lib.platform-library-name.Str;
}
multi guess_library_name(Callable $lib) is export(:TEST) {
    $lib();
}
multi guess_library_name(List $lib) is export(:TEST) {
    guess_library_name($lib[0], $lib[1])
}
multi guess_library_name(Str $libname, $apiversion='') is export(:TEST) {
    $libname.DEFINITE
        ?? $libname ~~ /[\.<.alpha>+ | \.so [\.<.digit>+]+ ] $/
            ?? $libname #Already a full name?
            !! $*VM.platform-library-name(
                $libname.IO, :version($apiversion || Version)).Str
        !! ''
}

my %lib;
my @cpp-name-mangler =
    &NativeCall::Compiler::MSVC::mangle_cpp_symbol,
    &NativeCall::Compiler::GNU::mangle_cpp_symbol,
;

sub guess-name-mangler(Routine $r, $name, Str $libname) {
    if $r.package.REPR eq 'CPPStruct' {
        my $sym = $r.?native_symbol // ($r.package.^name ~ '::' ~ $name);
        for @cpp-name-mangler -> &mangler {
            return &mangler if try cglobal($libname, mangler($r, $sym), Pointer)
        }
        die "Don't know how to mangle symbol '$sym' for library '$libname'"
    }
    elsif $r.?native_call_mangled {
        my $sym = $r.?native_symbol // $name;
        for @cpp-name-mangler -> &mangler {
            return &mangler if try cglobal($libname, mangler($r, $sym), Pointer)
        }
        die "Don't know how to mangle symbol '$sym' for library '$libname'"
    }
}

my Lock $setup-lock .= new;

sub resolve-libname($libname) {
    CATCH { default { note $_ } }
    $libname.platform-library-name.Str
}

# This role is mixed in to any routine that is marked as being a
# native call.
our role Native[Routine $r, $libname where Str|Callable|List|IO::Path|Distribution::Resource] {
    has native_callsite $!call is box_target;
    has Mu $!rettype;
    has $!cpp-name-mangler;
    has Pointer $!entry-point;
    has int $!arity;
    has int $!any-optionals;
    has Mu $!optimized-body;
    has Mu $!jit-optimized-body;
    has $!name;

    method !setup() {
        $setup-lock.protect: {
            nqp::neverrepossess(self);
            return if nqp::unbox_i($!call);

            # Make sure that C++ methods are treated as mangled (unless set otherwise)
            if self.package.REPR eq 'CPPStruct' and not self.does(NativeCallMangled) {
              self does NativeCallMangled[True];
            }

            my $guessed_libname = guess_library_name($libname);
            if self.does(NativeCallMangled) and $r.?native_call_mangled {
              # if needed, try to guess mangler
              $!cpp-name-mangler  = %lib{$guessed_libname} //
                  (%lib{$guessed_libname} = guess-name-mangler($r, $!name, $guessed_libname));
            }
            my Mu $arg_info := param_list_for($r.signature, $r);
            my $conv = self.?native_call_convention || '';

            $!rettype := nqp::decont(map_return_type($r.returns)) unless $!rettype;
            $!arity = $r.signature.arity;
            $!any-optionals = self!any-optionals;

            my $jitted = nqp::buildnativecall(self,
                nqp::unbox_s($guessed_libname),                           # library name
                nqp::unbox_s(gen_native_symbol($r, $!name, :$!cpp-name-mangler)), # symbol to call
                nqp::unbox_s($conv),        # calling convention
                $arg_info,
                ($libname and $libname ~~ Distribution::Resource)
                    ?? return_hash_for(
                        $r.signature,
                        $r,
                        :$!entry-point,
                        :&resolve-libname,
                        :resolve-libname-arg($libname),
                    )
                    !! return_hash_for($r.signature, $r, :$!entry-point));

            my $body := $jitted ?? $!jit-optimized-body !! $!optimized-body;
            if $body {
                nqp::bindattr(
                    self,
                    Code,
                    '$!do',
                    nqp::getattr(nqp::hllizefor($body, 'Raku'), ForeignCode, '$!do')
                );
            }

        }
    }

    method !any-optionals() {
        for $r.signature.params -> $p {
            return True if $p.optional
        }
        return False
    }

    method !decont-for-type($type) {
           $type ~~ Str ?? 'decont_s'
        !! $type ~~ Int ?? 'decont_i'
        !! $type ~~ Num ?? 'decont_n'
        !! 'decont';
    }

    method !create-jit-compiled-function-body(Routine $r) {
        my $block := QAST::Block.new(:name($!name), :arity($!arity), :blocktype('declaration_static'));
        my $locals = 0;
        my $args = 0;
        my (@params, @assigns);
        for $r.signature.params {
            next if nqp::istype($r, Method) && ($_.name // '') eq '%_';
            $args++;
            my $name = $_.name || '__anonymous_param__' ~ $++;
            my $lowered_param_name = '__lowered_param__' ~ $locals;
            my $lowered_name = '__lowered__' ~ $locals++;
            $block.push: QAST::Var.new(
                :name($lowered_name),
                :scope<local>,
                :decl<var>,
                :returns(
                       $_.type ~~ Str ?? nqp::bootstr()
                    !! $_.type ~~ Int ?? nqp::bootint()
                    !! $_.type ~~ Num ?? nqp::bootnum()
                    !! $_.type
                ),
            );
            @params.push: QAST::Var.new(:scope<local>, :name($lowered_name));
            $block.push: QAST::Var.new(
                :name($lowered_param_name),
                :scope<local>,
                :decl<param>,
                :slurpy($_.slurpy ?? 1 !! 0),
            );
            if $_.rw and nqp::objprimspec($_.type) > 0 {
                $block.push: QAST::Var.new(
                    :name($name),
                    :scope<lexicalref>,
                    :decl<var>,
                    :returns($_.type),
                );
                $block.push:
                    QAST::Op.new(
                        :op<bind>,
                        QAST::Var.new(:scope<lexicalref>, :name($name)),
                        QAST::Var.new(:scope<local>, :name($lowered_param_name)),
                    );
            }
            $block.push: QAST::Op.new(
                :op<if>,
                QAST::Op.new(
                    :op<isconcrete>,
                    QAST::Var.new(:scope<local>, :name($lowered_param_name)),
                ),
                QAST::Op.new(
                    :op<bind>,
                    QAST::Var.new(:scope<local>, :name($lowered_name)),
                    QAST::Op.new(
                        :op(self!decont-for-type($_.type)),
                        QAST::Var.new(:scope<local>, :name($lowered_param_name)),
                    ),
                ),
                QAST::Op.new(
                    :op<bind>,
                    QAST::Var.new(:scope<local>, :name($lowered_name)),
                       $_.type ~~ Str ?? Str
                    !! $_.type ~~ Int ?? QAST::IVal.new(:value(0))
                    !! $_.type ~~ Num ?? QAST::NVal.new(:value(0e0))
                    !! QAST::IVal.new(:value(0))
                ),
            );

            if $_.rw and nqp::objprimspec($_.type) > 0 {
                @assigns.push: QAST::Op.new(
                    :op<assign>,
                    QAST::Var.new(:scope<lexicalref>, :name($name)),
                    QAST::Op.new(:op<getarg_i>, QAST::IVal.new(:value($args - 1))),
                );
            }
        }
        $!rettype := nqp::decont(map_return_type($r.returns)) unless $!rettype;
        my $invoke_op := QAST::Op.new(
            :op<nativeinvoke>,
            QAST::WVal.new(:value(self)),
            QAST::WVal.new(:value($!rettype)),
        );
        $invoke_op.push: nqp::decont($_) for @params;
        if @assigns {
            $block.push: QAST::Op.new(
                :op<bind>,
                QAST::Var.new(
                    :name<return_value>,
                    :scope<local>,
                    :decl<var>,
                ),
                $invoke_op
            );
            $block.push: nqp::decont($_) for @assigns;
            $block.push: QAST::Var.new(:name<return_value>, :scope<local>);
        }
        else {
            $block.push: $invoke_op;
        }
        $block
    }

    method !create-function-body(Routine $r) {
        my $block := QAST::Block.new(:name($!name), :arity($!arity), :blocktype('declaration_static'));
        my $arglist := QAST::Op.new(:op<list>);
        my $locals = 0;
        for $r.signature.params {
            next if nqp::istype($r, Method) && ($_.name // '') eq '%_';
            my $name = $_.name || '__anonymous_param__' ~ $++;
            my $decont = self!decont-for-type($_.type);
            if $_.rw and nqp::objprimspec($_.type) > 0 {
                $block.push: QAST::Var.new(
                    :name($name),
                    :scope<lexicalref>,
                    :decl<var>,
                    :returns($_.type),
                );
                my $lowered_name = '__lowered_param__' ~ $locals++;
                $block.push: QAST::Var.new(
                    :name($lowered_name),
                    :scope<local>,
                    :decl<param>,
                    QAST::Op.new(
                        :op<bind>,
                        QAST::Var.new(:scope<lexicalref>, :name($name)),
                        QAST::Var.new(:scope<local>, :name($lowered_name)),
                    ),
                );
                $arglist.push: QAST::Var.new(:scope<lexicalref>, :name($name));
            }
            else {
                my $lowered_name = '__lowered__' ~ $locals++;
                $block.push: QAST::Var.new(
                    :name($lowered_name),
                    :scope<local>,
                    :decl<param>,
                    :slurpy($_.slurpy ?? 1 !! 0),
                );
                $block.push: QAST::Op.new(
                    :op<bind>,
                    QAST::Var.new(:scope<local>, :name($lowered_name)),
                    QAST::Op.new(
                        :op<if>,
                        QAST::Op.new(
                            :op<isconcrete_nd>,
                            QAST::Var.new(:scope<local>, :name($lowered_name)),
                        ),
                        QAST::Op.new(
                            :op(self!decont-for-type($_.type)),
                            QAST::Var.new(:scope<local>, :name($lowered_name)),
                        ),
                        QAST::Var.new(:scope<local>, :name($lowered_name)),
                    ),
                );
                $arglist.push: QAST::Var.new(:scope<local>, :name($lowered_name));
            }
        }
        $!rettype := nqp::decont(map_return_type($r.returns)) unless $!rettype;
        $block.push: QAST::Op.new(
            :op<nativecallinvoke>,
            QAST::WVal.new(:value($!rettype)),
            QAST::WVal.new(:value(self)),
            $arglist,
        );
        $block
    }

    method !compile-function-body(Mu $block) {
        my $compiler := nqp::getcomp("Raku");
        my $body := $compiler.compile($block, :from<optimize>);

        $*W.add_object($body) if $*W;

        nqp::setcodename(nqp::getattr($body, ForeignCode, '$!do'), $!name);
        $body
    }

    method create-optimized-call() {
        unless $!optimized-body {
            $setup-lock.protect: {
                nqp::neverrepossess(self);
                unless nqp::defined(nqp::getobjsc(self)) {
                    if $*W {
                        $*W.add_object(self);
                    }
                    else {
                        my $sc := nqp::createsc('NativeCallSub' ~ nqp::objectid(self));
                        nqp::setobjsc(self, $sc);
                        my int $idx = nqp::scobjcount($sc);
                        nqp::scsetobj($sc, $idx, self);
                    }
                }

                my $optimized-body     := self!create-function-body($r);
                $optimized-body.annotate('code_object', self);
                $optimized-body.code_object(self);
                my $stub := nqp::freshcoderef(nqp::getattr(sub (*@args, *%named) { die "stub called" }, Code, '$!do'));
                nqp::setcodename($stub, self.name);
                nqp::markcodestatic($stub);
                nqp::markcodestub($stub);
                nqp::bindattr(self, $?CLASS, '$!optimized-body', $stub);
                my $jit-optimized-body := self!create-jit-compiled-function-body($r);
                $jit-optimized-body.annotate('code_object', self);
                $jit-optimized-body.code_object(self);
                nqp::bindattr(self, $?CLASS, '$!jit-optimized-body', $stub);
                my $fixups := QAST::Stmts.new();
                if $*W {
                    $*W.add_root_code_ref($stub, $optimized-body);
                    $*W.add_root_code_ref($stub, $jit-optimized-body);
                    $*W.add_object($?CLASS);
                    $*UNIT.push($optimized-body);
                    $*UNIT.push($jit-optimized-body);
                    $fixups.push(QAST::Op.new(:op<neverrepossess>, QAST::WVal.new(:value(self))));
                    $fixups.push($*W.set_attribute(self, $?CLASS, '$!optimized-body',
                        QAST::BVal.new( :value($optimized-body) )));
                    $fixups.push($*W.set_attribute(self, $?CLASS, '$!jit-optimized-body',
                        QAST::BVal.new( :value($jit-optimized-body) )));
                    $*W.add_fixup_task(:deserialize_ast($fixups), :fixup_ast($fixups));
                    Nil
                }
                else {
                    $!optimized-body     := self!compile-function-body(self!create-function-body($r));
                    $!jit-optimized-body := self!compile-function-body(self!create-jit-compiled-function-body($r));
                }
            }
        }
    }

    method !arity-error(\args) {
        X::TypeCheck::Argument.new(
            :objname($.name),
            :arguments(args.list.map(*.^name)),
            :signature(try $r.signature.gist),
        ).throw
    }

    method setup-nativecall() {
        $!name = self.name;

        # finish compilation of the original routine so our changes won't
        # become undone right afterwards
        $*W.unstub_code_object(self, Code) if $*W;

        my $replacement := -> |args {
            self.create-optimized-call() unless
                $!optimized-body # Already have the optimized body
                or $!any-optionals # the compiled code doesn't support optional parameters yet
                or try $*W;    # Avoid issues with compiling specialized version during BEGIN time
            self!setup() unless nqp::unbox_i($!call);

            my Mu $args := nqp::getattr(nqp::decont(args), Capture, '@!list');
            self!arity-error(args) if nqp::elems($args) != $!arity;

            nqp::nativecall($!rettype, self, $args)
        };

        my $do := nqp::getattr($replacement, Code, '$!do');
        nqp::bindattr(self, Code, '$!do', $do);
        nqp::setcodename($do, $!name);
    }

    method soft(--> True) {} # prevent inlining of the original function body
}

multi sub postcircumfix:<[ ]>(CArray:D \array, $pos) is raw is export(:DEFAULT, :types) is default {
    nqp::istype($pos, Iterable) ?? $pos.map: { array.AT-POS($_) } !! array.AT-POS($pos);
}
multi sub postcircumfix:<[ ]>(CArray:D \array, *@pos) is raw is export(:DEFAULT, :types) {
    @pos.map: { array.AT-POS($_) };
}
multi sub postcircumfix:<[ ]>(CArray:D \array, Callable:D $block) is raw is export(:DEFAULT, :types) {
    nqp::stmts(
      (my $*INDEX = 'Effective index'),
      array[$block.POSITIONS(array)]
    )
}
multi sub postcircumfix:<[ ]>(CArray:D \array) is export(:DEFAULT, :types) {
    array.ZEN-POS
}
multi sub postcircumfix:<[ ]>(CArray:D \array, Whatever:D) is export(:DEFAULT, :types) {
    array[^array.elems]
}
multi sub postcircumfix:<[ ]>(CArray:D \array, HyperWhatever:D) is export(:DEFAULT, :types) {
    X::NYI.new(feature => 'HyperWhatever in CArray index').throw;
}

multi trait_mod:<is>(Routine $r, :$symbol!) is export(:DEFAULT, :traits) {
    $r does NativeCallSymbol[$symbol];
}

# Specifies the calling convention to use for a native call.
multi trait_mod:<is>(Routine $r, :$nativeconv!) is export(:DEFAULT, :traits) {
    $r does NativeCallingConvention[$nativeconv];
}

# Ways to specify how to marshall strings.
multi trait_mod:<is>(Parameter $p, :$encoded!) is export(:DEFAULT, :traits) {
    $p does NativeCallEncoded[$encoded];
}
multi trait_mod:<is>(Routine $p, :$encoded!) is export(:DEFAULT, :traits) {
    $p does NativeCallEncoded[$encoded];
}

multi trait_mod:<is>(Routine $p, :$mangled!) is export(:DEFAULT, :traits) {
    $p does NativeCallMangled[$mangled === True ?? 'C++' !! $mangled];
}

role ExplicitlyManagedString {
    has $.cstr is rw;
}

multi explicitly-manage(Str $x, :$encoding = 'utf8') is export(:DEFAULT,
:utils) {
    $x does ExplicitlyManagedString;
    my $class = class CStr is repr('CStr') { method encoding() { $encoding; } };
    $x.cstr = nqp::box_s(nqp::unbox_s($x), nqp::decont($class));
}

role CPPConst {
    method cpp-const(--> 1) { }
}
multi trait_mod:<is>(Routine $p, :cpp-const($)!) is export(:DEFAULT, :traits) {
    $p does CPPConst;
}
multi trait_mod:<is>(Parameter $p, :cpp-const($)!) is export(:DEFAULT, :traits) {
    $p does CPPConst;
}

role CPPRef {
    method cpp-ref(--> 1) { }
}
multi trait_mod:<is>(Parameter $p, :cpp-ref($)!) is export(:DEFAULT, :traits) {
    $p does CPPRef;
}

multi refresh($obj --> 1) is export(:DEFAULT, :utils) {
    nqp::nativecallrefresh($obj);
}

multi sub nativecast(Signature $target-type, $source) is export(:DEFAULT) {
    my $r := sub { };
    $r does Native[$r, Str];
    $r.setup-nativecall;
    nqp::bindattr($r, Code, '$!signature', nqp::decont($target-type));
    nqp::bindattr($r, $r.WHAT, '$!entry-point', $source);
    $r
}

multi sub nativecast(Int $target-type, $source) is export(:DEFAULT) {
    nqp::nativecallcast(nqp::decont($target-type),
        Int, nqp::decont($source));
}

multi sub nativecast(Num $target-type, $source) is export(:DEFAULT) {
    nqp::nativecallcast(nqp::decont($target-type),
        Num, nqp::decont($source));
}

multi sub nativecast($target-type, $source) is export(:DEFAULT) {
    nqp::nativecallcast(nqp::decont($target-type),
        nqp::decont($target-type), nqp::decont($source));
}

sub cglobal($libname, $symbol, $target-type) is export is rw {
    Proxy.new(
        FETCH => -> $ {
            nqp::nativecallglobal(
                nqp::unbox_s(guess_library_name($libname)),
                nqp::unbox_s($symbol),
                nqp::decont($target-type),
                nqp::decont(map_return_type($target-type)))
        },
        STORE => -> | { die "Writing to C globals NYI" }
    )
}

}

sub check_routine_sanity(Routine $r) is export(:TEST) {
    #Maybe this should use the hash already existing?
    sub validnctype (Mu ::T) {
      return True if nqp::existskey($repr_map,T.REPR) && T.REPR ne 'CArray' | 'CPointer';
      return True if T.^name eq 'Str' | 'str' | 'Bool';
      return False if T.REPR eq 'P6opaque';
      return False if T.HOW.^can("nativesize") && !nqp::defined(T.^nativesize); #to disting int and int32 for example
      return validnctype(T.of) if T.REPR eq 'CArray' | 'CPointer' and T.^can('of');
      return True;
    }
    my $sig = $r.signature;
    for @($sig.params).kv -> $i, $param {
        next if $r ~~ Method and ($i < 1 or $i == $sig.params.elems - 1); #Method have two extra parameters
        if $param.type ~~ Callable {
          # We probably want to check the given routine type too here. but I don't know how
          next;
        }
        next unless $param.type ~~ Buf | Blob #Buf are Uninstantiable, make this buggy
        || $param.type.^can('gist'); #FIXME, it's to handle case of class A { sub foo(A) is native) }, the type is not complete
        if !validnctype($param.type) {
           warn "In '{$r.name}' routine declaration - Not an accepted NativeCall type"
            ~ " for parameter [{$i + 1}] {$param.name ?? $param.name !! ''} : {$param.type.^name}\n"
            ~ " --> For Numerical type, use the appropriate int32/int64/num64...";
        }
    }
    return True if $r.returns.REPR eq 'CPointer' | 'CStruct' | 'CPPStruct'; #Meh fix but 'imcomplete' type are a pain
    if $r.returns.^name ne 'Mu' && !validnctype($r.returns) {
        warn "The returning type of '{$r.name}' --> {$r.returns.^name} is erroneous."
            ~ " You should not return a non NativeCall supported type (like Int inplace of int32),"
            ~ " truncating errors can appear with different architectures";
    }
}

sub EXPORT(|) {
    my @routines_to_setup;
    if $*W {
        my $block := {
            for @routines_to_setup {
                .create-optimized-call;
                CATCH { default { note $_ } }
            }
        };
        $*W.add_object($block);
        my $op := $*W.add_phaser(Mu, 'CHECK', $block, class :: { method cuid { (^2**128).pick }});
    }
    # Specifies that the routine is actually a native call, into the
    # current executable (platform specific) or into a named library
    my $native_trait := multi trait_mod:<is>(Routine $r, :$native!) {
        check_routine_sanity($r);
        $r does NativeCall::Native[$r, $native === True ?? Str !! $native];
        $r.setup-nativecall;
        @routines_to_setup.push: $r;
    };
    Map.new(
        '&trait_mod:<is>' => $native_trait.dispatcher,
    );
}

# vim: expandtab shiftwidth=4
