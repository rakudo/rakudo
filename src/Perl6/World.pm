use NQPHLL;
use Perl6::ModuleLoader;

# Binder constants.
# XXX Want constant syntax in NQP really.
my $SIG_ELEM_BIND_CAPTURE        := 1;
my $SIG_ELEM_BIND_PRIVATE_ATTR   := 2;
my $SIG_ELEM_BIND_PUBLIC_ATTR    := 4;
my $SIG_ELEM_SLURPY_POS          := 8;
my $SIG_ELEM_SLURPY_NAMED        := 16;
my $SIG_ELEM_SLURPY_LOL          := 32;
my $SIG_ELEM_INVOCANT            := 64;
my $SIG_ELEM_MULTI_INVOCANT      := 128;
my $SIG_ELEM_IS_RW               := 256;
my $SIG_ELEM_IS_COPY             := 512;
my $SIG_ELEM_IS_PARCEL           := 1024;
my $SIG_ELEM_IS_OPTIONAL         := 2048;
my $SIG_ELEM_ARRAY_SIGIL         := 4096;
my $SIG_ELEM_HASH_SIGIL          := 8192;
my $SIG_ELEM_DEFAULT_FROM_OUTER  := 16384;
my $SIG_ELEM_IS_CAPTURE          := 32768;
my $SIG_ELEM_UNDEFINED_ONLY      := 65536;
my $SIG_ELEM_DEFINED_ONLY        := 131072;
my $SIG_ELEM_METHOD_SLURPY_NAMED := 262144;
my $SIG_ELEM_NOMINAL_GENERIC     := 524288;
my $SIG_ELEM_DEFAULT_IS_LITERAL  := 1048576;
my $SIG_ELEM_NATIVE_INT_VALUE    := 2097152;
my $SIG_ELEM_NATIVE_NUM_VALUE    := 4194304;
my $SIG_ELEM_NATIVE_STR_VALUE    := 8388608;

# This builds upon the SerializationContextBuilder to add the specifics
# needed by Rakudo Perl 6.
class Perl6::World is HLL::World {
    # The stack of lexical pads, actually as PAST::Block objects. The
    # outermost frame is at the bottom, the latest frame is on top.
    has @!BLOCKS;
    
    # The stack of code objects; phasers get attached to the top one.
    has @!CODES;
    
    # Mapping of sub IDs to their proto code objects; used for fixing
    # up in dynamic compilation.
    has %!sub_id_to_code_object;
    
    # Mapping of sub IDs to their static lexpad objects.
    has %!sub_id_to_static_lexpad;
    
    # Mapping of sub IDs to SC indexes of code stubs.
    has %!sub_id_to_sc_idx;
    
    # Array of stubs to check and the end of compilation.
    has @!stub_check;
    
    # Cached constants that we've built.
    has %!const_cache;
    
    # List of CHECK blocks to run.
    has @!CHECKs;
    
    # Creates a new lexical scope and puts it on top of the stack.
    method push_lexpad($/) {
        # Create pad, link to outer and add to stack.
        my $pad := PAST::Block.new( PAST::Stmts.new(), :node($/) );
        if +@!BLOCKS {
            $pad<outer> := @!BLOCKS[+@!BLOCKS - 1];
        }
        @!BLOCKS[+@!BLOCKS] := $pad;
        $pad
    }
    
    # Pops a lexical scope off the stack.
    method pop_lexpad() {
        @!BLOCKS.pop()
    }
    
    # Gets the top lexpad.
    method cur_lexpad() {
        @!BLOCKS[+@!BLOCKS - 1]
    }
    
    # Gets (and creates if needed) the static lexpad object for a PAST block.
    method get_static_lexpad($pad) {
        my $pad_id := $pad.subid();
        if pir::exists(%!sub_id_to_static_lexpad, $pad_id) {
            return %!sub_id_to_static_lexpad{$pad_id};
        }
        
        # Create it a static lexpad object.
        my $slp_type_obj := self.find_symbol(['StaticLexPad']);
        my $slp          := nqp::create($slp_type_obj);
        nqp::bindattr($slp, $slp_type_obj, '%!static_values', nqp::hash());
        nqp::bindattr($slp, $slp_type_obj, '%!flags', nqp::hash());
        
        # Deserialization and fixup need to associate static lex pad with the
        # low-level LexInfo.
        self.add_object($slp);
        my $fixup := PAST::Op.new(
            :pasttype('callmethod'), :name('set_static_lexpad'),
            PAST::Val.new( :value($pad), :returns('LexInfo')),
            self.get_ref($slp));
        self.add_fixup_task(:deserialize_past($fixup), :fixup_past($fixup));
        
        # Stash it under the PAST block sub ID.
        %!sub_id_to_static_lexpad{$pad.subid()} := $slp;
        
        $slp
    }
    
    # Marks the current lexpad as being a signatured block.
    method mark_cur_lexpad_signatured() {
        @!BLOCKS[+@!BLOCKS - 1]<signatured> := 1;
    }
    
    # Finds the nearest signatured block and checks if it declares
    # a certain symbol.
    method nearest_signatured_block_declares($symbol) {
        my $i := +@!BLOCKS;
        while $i > 0 {
            $i := $i - 1;
            if @!BLOCKS[$i]<signatured> {
                return +@!BLOCKS[$i].symbol($symbol);
            }
        }
    }
    
    # Pushes a stub on the "stubs to check" list.
    method add_stub_to_check($stub) {
        @!stub_check[+@!stub_check] := $stub;
    }
    
    # Checks for any stubs that weren't completed.
    method assert_stubs_defined($/) {
        my @incomplete;
        for @!stub_check {
            unless $_.HOW.is_composed($_) {
                @incomplete.push($_.HOW.name($_));
            }
        }
        if +@incomplete {
            self.throw($/, 'X::Package::Stubbed', packages => @incomplete);
        }
    }
    
    # Loads a setting.
    method load_setting($/, $setting_name) {
        # Do nothing for the NULL setting.
        if $setting_name ne 'NULL' {    
            # Load it immediately, so the compile time info is available.
            # Once it's loaded, set it as the outer context of the code
            # being compiled.
            my $setting := %*COMPILING<%?OPTIONS><outer_ctx>
                        := Perl6::ModuleLoader.load_setting($setting_name);
            
            # Add a fixup and deserialization task also.
            my $fixup := PAST::Stmt.new(
                self.perl6_module_loader_code(),
                PAST::Op.new(
                    :pasttype('callmethod'), :name('set_outer_ctx'),
                    PAST::Val.new( :value($*UNIT_OUTER) ),
                    PAST::Op.new(
                        :pasttype('callmethod'), :name('load_setting'),
                        PAST::Var.new( :name('ModuleLoader'), :namespace([]), :scope('package') ),
                        $setting_name
                    )
                )
            );
            self.add_load_dependency_task(:deserialize_past($fixup), :fixup_past($fixup));
            
            return pir::getattribute__PPs($setting, 'lex_pad');
        }
    }
    
    # Loads a module immediately, and also makes sure we load it
    # during the deserialization.
    method load_module($/, $module_name, $cur_GLOBALish) {
        # Immediate loading.
        my $module := Perl6::ModuleLoader.load_module($module_name, $cur_GLOBALish);
        
        # During deserialization, ensure that we get this module loaded.
        if self.is_precompilation_mode() {
            self.add_load_dependency_task(:deserialize_past(PAST::Stmts.new(
                self.perl6_module_loader_code(),
                PAST::Op.new(
                   :pasttype('callmethod'), :name('load_module'),
                   PAST::Var.new( :name('ModuleLoader'), :namespace([]), :scope('package') ),
                   $module_name
                ))));
        }

        return pir::getattribute__PPs($module, 'lex_pad');
    }
    
    # Uses the NQP module loader to load Perl6::ModuleLoader, which
    # is a normal NQP module.
    method perl6_module_loader_code() {
        PAST::Stmt.new(
            PAST::Op.new(
                :pirop('load_bytecode vs'), 'ModuleLoader.pbc'
            ),
            PAST::Op.new(
                :pasttype('callmethod'), :name('load_module'),
                PAST::Var.new( :scope('keyed_int'),
                    PAST::Var.new( :scope('keyed'),
                        PAST::Var.new( :scope('keyed'),
                            PAST::Op.new( :pirop('get_root_namespace P') ),
                            'nqp' ),
                        'ModuleLoader'),
                    1),
                'Perl6::ModuleLoader'
            ))
    }
    
    # Imports symbols from the specified package into the current lexical scope.
    method import($package) {
        # We'll do this in two passes, since at the start of CORE.setting we import
        # StaticLexPad, which of course we need to use when importing. Since we still
        # keep the authoritative copy of stuff from the compiler's view in PAST::Block's
        # .symbol(...) hash we get away with this for now.
        my %stash := $package.WHO;
        my $target := self.cur_lexpad();
        
        # First pass: PAST::Block symbol table installation. Also detect any
        # outright conflicts, and handle any situations where we need to merge.
        my %to_install;
        for %stash {
            if $target.symbol($_.key) {
                # XXX TODO: Merge handling.
                pir::die("Cannot import symbol '" ~ $_.key ~ "', since it already exists in the lexpad");
            }
            else {
                $target.symbol($_.key, :scope('lexical_6model'), :value($_.value));
                $target[0].push(PAST::Var.new( :scope('lexical_6model'), :name($_.key), :isdecl(1) ));
                %to_install{$_.key} := $_.value;
            }
        }
        
        # Second pass: stick everything we still need to install in the
        # actual static lexpad.
        my $slp := self.get_static_lexpad($target);
        for %to_install {
            $slp.add_static_value($_.key, $_.value, 0, 0);
        }

        1;
    }
    
    # Installs something package-y in the right place, creating the nested
    # pacakges as needed.
    method install_package($/, @name_orig, $scope, $pkgdecl, $package, $outer, $symbol) {
        if $scope eq 'anon' { return 1 }
        my @parts := pir::clone(@name_orig);
        my $name  := @parts.pop();
        my $create_scope := $scope;
        my $cur_pkg := $package;
        my $cur_lex := $outer;
        
        # Can only install packages as our or my scope.
        unless $create_scope eq 'my' || $create_scope eq 'our' {
            self.throw($/, 'X::Declaration::Scope',
                scope       => $*SCOPE,
                declaration => $pkgdecl,
            );
        }
        
        # If we have a multi-part name, see if we know the opening
        # chunk already. If so, use it for that part of the name.
        if +@parts {
            try {
                $cur_pkg := $*W.find_symbol([@parts[0]]);
                $cur_lex := 0;
                $create_scope := 'our';
                @parts.shift();
            }
        }
        
        # Chase down the name, creating stub packages as needed.
        while +@parts {
            my $part := @parts.shift;
            if pir::exists($cur_pkg.WHO, $part) {
                $cur_pkg := ($cur_pkg.WHO){$part};
            }
            else {
                my $new_pkg := self.pkg_create_mo($/, %*HOW<package>, :name($part));
                self.pkg_compose($new_pkg);
                if $create_scope eq 'my' || $cur_lex {
                    self.install_lexical_symbol($cur_lex, $part, $new_pkg);
                }
                if $create_scope eq 'our' {
                    self.install_package_symbol($cur_pkg, $part, $new_pkg);
                }
                $cur_pkg := $new_pkg;
                $create_scope := 'our';
                $cur_lex := 0;
            }
        }
        
        # Install final part of the symbol.
        if $create_scope eq 'my' || $cur_lex {
            self.install_lexical_symbol($cur_lex, $name, $symbol);
        }
        if $create_scope eq 'our' {
            if pir::exists($cur_pkg.WHO, $name) {
                self.steal_WHO($symbol, ($cur_pkg.WHO){$name});
            }
            self.install_package_symbol($cur_pkg, $name, $symbol);
        }
        
        1;
    }
    
    # If we declare class A::B { }, then class A { }, then A.WHO must be the
    # .WHO we already created for the stub package A.
    method steal_WHO($thief, $victim) {
        pir::set_who__vP($thief, $victim.WHO);
    }
    
    # Installs a lexical symbol. Takes a PAST::Block object, name and
    # the object to install. Does an immediate installation in the
    # compile-time block symbol table, and ensures that the installation
    # gets fixed up at runtime too.
    method install_lexical_symbol($block, $name, $obj, :$clone) {
        # Install the object directly as a block symbol.
        unless $block.symbol($name) {
            $block[0].push(PAST::Var.new( :scope('lexical_6model'), :name($name), :isdecl(1) ));
        }
        $block.symbol($name, :scope('lexical_6model'), :value($obj));
        
        # Add a clone if needed.
        if $clone {
            $block[0].push(PAST::Op.new(
                :pasttype('bind_6model'),
                PAST::Var.new( :name($name), :scope('lexical_6model') ),
                PAST::Op.new(
                    :pasttype('callmethod'), :name('clone'),
                    PAST::Var.new( :name($name), :scope('lexical_6model') )
                )));
        }
        
        # Add to static lexpad.
        my $slp := self.get_static_lexpad($block);
        $slp.add_static_value(~$name, $obj, 0, 0);

        1;
    }
    
    # Installs a lexical symbol. Takes a PAST::Block object, name and
    # the type of container to install.
    method install_lexical_container($block, $name, %cont_info, $descriptor, :$state) {
        # Add to block, if needed. Note that it doesn't really have
        # a compile time value.
        my $var;
        unless $block.symbol($name) {
            $var := PAST::Var.new( :scope('lexical_6model'), :name($name),
                :isdecl(1), :type(%cont_info<bind_constraint>) );
            $block[0].push($var);
        }
        $block.symbol($name, :scope('lexical_6model'), :type(%cont_info<bind_constraint>), :descriptor($descriptor));
            
        # If it's a native type, we're done - no container
        # as we inline natives straight into registers. Do
        # need to take care of initial value though.
        my $prim := pir::repr_get_primitive_type_spec__IP($descriptor.of);
        if $prim {
            if $state { pir::die("Natively typed state variables not yet implemented") }
            if $var {
                if $prim == 1    { $var.viviself(PAST::Val.new( :value(0) )) }
                elsif $prim == 2 { $var.viviself(PAST::Op.new( :pirop('set__Ns'), 'NaN' )) }
                elsif $prim == 3 { $var.viviself(PAST::Val.new( :value('') )) }
            }
            return 1;
        }
        
        # Build container.
        my $cont := nqp::create(%cont_info<container_type>);
        nqp::bindattr($cont, %cont_info<container_base>, '$!descriptor', $descriptor);
        if pir::exists(%cont_info, 'default_value') {
            nqp::bindattr($cont, %cont_info<container_base>, '$!value',
                %cont_info<default_value>);
        }
        $block.symbol($name, :value($cont));
        
        # Add container to static lexpad.
        my $slp := self.get_static_lexpad($block);
        $slp.add_static_value(~$name, $cont, 1, ($state ?? 1 !! 0));

        1;
    }
    
    # Builds PAST that constructs a container.
    method build_container_past(%cont_info, $descriptor) {
        # Create container.
        my $cont_code := PAST::Op.new(
            :pirop('repr_instance_of PP'),
            self.get_ref(%cont_info<container_type>)
        );
        
        # Set container descriptor.
        $cont_code := PAST::Op.new(
            :pirop('setattribute 0PPsP'),
            $cont_code, self.get_ref(%cont_info<container_base>),
            '$!descriptor', self.get_ref($descriptor));
        
        # Default contents, if applicable (note, slurpy param as we can't
        # use definedness here, as it's a type object we'd be checking).
        if pir::exists(%cont_info, 'default_value') {
            $cont_code := PAST::Op.new(
                :pirop('setattribute 0PPsP'),
                $cont_code, self.get_ref(%cont_info<container_base>),
                '$!value', self.get_ref(%cont_info<default_value>));
        }
        
        $cont_code
    }
    
    # Hunts through scopes to find the type of a lexical.
    method find_lexical_container_type($name) {
        my $i := +@!BLOCKS;
        while $i > 0 {
            $i := $i - 1;
            my %sym := @!BLOCKS[$i].symbol($name);
            if +%sym {
                if pir::exists(%sym, 'type') {
                    return %sym<type>;
                }
                else {
                    $i := 0;
                }
            }
        }
        pir::die("Could not find container descriptor for $name");
    }
    
    # Installs a symbol into the package.
    method install_package_symbol($package, $name, $obj) {
        ($package.WHO){$name} := $obj;
        1;
    }
    
    # Creates a parameter object.
    method create_parameter(%param_info) {
        # Create parameter object now.
        my $par_type  := self.find_symbol(['Parameter']);
        my $parameter := nqp::create($par_type);
        self.add_object($parameter);
        
        # Calculate flags.
        my $flags := 0;
        if %param_info<optional> {
            $flags := $flags + $SIG_ELEM_IS_OPTIONAL;
        }
        if %param_info<is_invocant> {
            $flags := $flags + $SIG_ELEM_INVOCANT;
        }
        if %param_info<is_multi_invocant> {
            $flags := $flags + $SIG_ELEM_MULTI_INVOCANT;
        }
        if %param_info<is_copy> {
            $flags := $flags + $SIG_ELEM_IS_COPY;
        }
        if %param_info<is_parcel> {
            $flags := $flags + $SIG_ELEM_IS_PARCEL;
        }
        if %param_info<is_capture> {
            $flags := $flags + $SIG_ELEM_IS_CAPTURE;
        }
        if %param_info<undefined_only> {
            $flags := $flags + $SIG_ELEM_UNDEFINED_ONLY;
        }
        if %param_info<defined_only> {
            $flags := $flags + $SIG_ELEM_DEFINED_ONLY;
        }
        if %param_info<pos_slurpy> {
            $flags := $flags + $SIG_ELEM_SLURPY_POS;
        }
        if %param_info<named_slurpy> {
            $flags := $flags + $SIG_ELEM_SLURPY_NAMED;
        }
        if %param_info<is_method_named_slurpy> {
            $flags := $flags + $SIG_ELEM_METHOD_SLURPY_NAMED;
        }
        if %param_info<pos_lol> {
            $flags := $flags + $SIG_ELEM_SLURPY_LOL;
        }
        if %param_info<bind_attr> {
            $flags := $flags + $SIG_ELEM_BIND_PRIVATE_ATTR;
        }
        if %param_info<bind_accessor> {
            $flags := $flags + $SIG_ELEM_BIND_PUBLIC_ATTR;
        }
        if %param_info<sigil> eq '@' {
            $flags := $flags + $SIG_ELEM_ARRAY_SIGIL;
        }
        elsif %param_info<sigil> eq '%' {
            $flags := $flags + $SIG_ELEM_HASH_SIGIL;
        }
        if %param_info<default_from_outer> {
            $flags := $flags + $SIG_ELEM_DEFAULT_FROM_OUTER;
        }
        if %param_info<nominal_generic> {
            $flags := $flags + $SIG_ELEM_NOMINAL_GENERIC;
        }
        if %param_info<default_is_literal> {
            $flags := $flags + $SIG_ELEM_DEFAULT_IS_LITERAL;
        }
        my $primspec := pir::repr_get_primitive_type_spec__IP(%param_info<nominal_type>);
        if $primspec == 1 {
            $flags := $flags + $SIG_ELEM_NATIVE_INT_VALUE;
        }
        elsif $primspec == 2 {
            $flags := $flags + $SIG_ELEM_NATIVE_NUM_VALUE;
        }
        elsif $primspec == 3 {
            $flags := $flags + $SIG_ELEM_NATIVE_STR_VALUE;
        }
        
        # Populate it.
        if pir::exists(%param_info, 'variable_name') {
            nqp::bindattr_s($parameter, $par_type, '$!variable_name', %param_info<variable_name>);
        }
        nqp::bindattr($parameter, $par_type, '$!nominal_type', %param_info<nominal_type>);
        nqp::bindattr_i($parameter, $par_type, '$!flags', $flags);
        if %param_info<named_names> {
            my @names := %param_info<named_names>;
            nqp::bindattr($parameter, $par_type, '$!named_names', @names);
        }
        if %param_info<type_captures> {
            my @type_names := %param_info<type_captures>;
            nqp::bindattr($parameter, $par_type, '$!type_captures', @type_names);
        }
        if %param_info<post_constraints> {
            nqp::bindattr($parameter, $par_type, '$!post_constraints',
                %param_info<post_constraints>);
        }
        if pir::exists(%param_info, 'default_value') {
            nqp::bindattr($parameter, $par_type, '$!default_value', %param_info<default_value>);
        }
        if pir::exists(%param_info, 'container_descriptor') {
            nqp::bindattr($parameter, $par_type, '$!container_descriptor', %param_info<container_descriptor>);
        }
        if pir::exists(%param_info, 'attr_package') {
            nqp::bindattr($parameter, $par_type, '$!attr_package', %param_info<attr_package>);
        }
        if pir::exists(%param_info, 'sub_signature') {
            nqp::bindattr($parameter, $par_type, '$!sub_signature', %param_info<sub_signature>);
        }

        # Return created parameter.
        $parameter
    }
    
    # Creates a signature object from a set of parameters.
    method create_signature(@parameters) {
        # Create signature object now.
        my $sig_type  := self.find_symbol(['Signature']);
        my $signature := nqp::create($sig_type);
        self.add_object($signature);
        
        # Set parameters.
        nqp::bindattr($signature, $sig_type, '$!params', @parameters);
        
        # Return created signature.
        $signature
    }
    
    # Creates a code object of the specified type, attached the passed signature
    # object and sets up dynamic compilation thunk.
    method create_code_object($code_past, $type, $signature, $is_dispatcher = 0, :$yada) {
        my $code := self.stub_code_object($type);
        self.attach_signature($code, $signature);
        self.finish_code_object($code, $code_past, $is_dispatcher, :yada($yada));
        $code
    }
    
    # Stubs a code object of the specified type.
    method stub_code_object($type) {
        my $type_obj := self.find_symbol([$type]);
        my $code     := nqp::create($type_obj);
        @!CODES[+@!CODES] := $code;
        self.add_object($code);
        $code
    }
    
    # Attaches a signature to a code object.
    method attach_signature($code, $signature) {
        my $code_type := self.find_symbol(['Code']);
        nqp::bindattr($code, $code_type, '$!signature', $signature);
    }
    
    # Takes a code object and the PAST::Block for its body.
    method finish_code_object($code, $code_past, $is_dispatcher = 0, :$yada) {
        my $fixups := PAST::Stmts.new();
        my $des    := PAST::Stmts.new();
        
        # Remove it from the code objects stack.
        @!CODES.pop();
        
        # Handle any phasers.
        self.add_phasers_handling_code($code, $code_past);
                
        # Locate various interesting symbols.
        my $code_type    := self.find_symbol(['Code']);
        my $routine_type := self.find_symbol(['Routine']);
        my $slp_type     := self.find_symbol(['StaticLexPad']);
        
        # Attach code object to PAST node.
        $code_past<code_object> := $code;
        
        # Stash it under the PAST block sub ID.
        %!sub_id_to_code_object{$code_past.subid()} := $code;
        
        # For now, install stub that will dynamically compile the code if
        # we ever try to run it during compilation.
        my $precomp;
        my $compiler_thunk := {
            # Fix up GLOBAL.
            my $rns    := pir::get_root_namespace__P();
            my $p6_pns := $rns{'perl6'};
            $p6_pns{'GLOBAL'} := $*GLOBALish;
            
            # Compile the block.
            $precomp := self.compile_in_context($code_past, $code_type, $slp_type);

            # Also compile the candidates if this is a proto.
            if $is_dispatcher {
                for nqp::getattr($code, $routine_type, '$!dispatchees') {
                    my $stub := nqp::getattr($_, $code_type, '$!do');
                    my $past := pir::getprop__PsP('PAST_BLOCK', $stub);
                    if $past {
                        self.compile_in_context($past, $code_type, $slp_type);
                    }
                }
            }
        };
        my $stub := pir::nqp_fresh_stub__PP(sub (*@pos, *%named) {
            unless $precomp {
                $compiler_thunk();
            }
            $precomp(|@pos, |%named);
        });
        pir::setprop__vPsP($stub, 'COMPILER_THUNK', $compiler_thunk);
        pir::set__vPS($stub, $code_past.name);
        nqp::bindattr($code, $code_type, '$!do', $stub);
        
        # Tag it as a static code ref and add it to the root code refs set.
        pir::setprop__vPsP($stub, 'STATIC_CODE_REF', $stub);
        pir::setprop__vPsP($stub, 'COMPILER_STUB', $stub);
        my $code_ref_idx := self.add_root_code_ref($stub, $code_past);
        %!sub_id_to_sc_idx{$code_past.subid()} := $code_ref_idx;
        
        # If we clone the stub, need to mark it as a dynamic compilation
        # boundary.
        if self.is_precompilation_mode() {
            my $clone_handler := sub ($orig, $clone) {
                my $do := nqp::getattr($clone, $code_type, '$!do');
                pir::setprop__vPsP($do, 'COMPILER_STUB', $do);
                pir::setprop__vPsP($do, 'CLONE_CALLBACK', $clone_handler);
            };
            pir::setprop__vPsP($stub, 'CLONE_CALLBACK', $clone_handler);
        }
        
        # Fixup will install the real thing, unless we're in a role, in
        # which case pre-comp will have sorted it out.
        unless $*PKGDECL eq 'role' {
            unless self.is_precompilation_mode() {
                $fixups.push(PAST::Stmts.new(
                    self.set_attribute($code, $code_type, '$!do', PAST::Val.new( :value($code_past) )),
                    PAST::Op.new(
                        :pirop('perl6_associate_sub_code_object vPP'),
                        PAST::Val.new( :value($code_past) ),
                        self.get_ref($code)
                    )));
                
                # If we clone the stub, then we must remember to do a fixup
                # of it also.
                pir::setprop__vPsP($stub, 'CLONE_CALLBACK', sub ($orig, $clone) {
                    self.add_object($clone);
                    $fixups.push(PAST::Stmts.new(
                        PAST::Op.new( :pasttype('bind'),
                            PAST::Var.new( :name('$P0'), :scope('register') ),
                            PAST::Op.new( :pirop('clone PP'), PAST::Val.new( :value($code_past) ) )
                        ),
                        self.set_attribute($clone, $code_type, '$!do',
                            PAST::Var.new( :name('$P0'), :scope('register') )),
                        PAST::Op.new(
                            :pirop('perl6_associate_sub_code_object vPP'),
                            PAST::Var.new( :name('$P0'), :scope('register') ),
                            self.get_ref($clone)
                        )));
                });
            }

			# Attach the PAST block to the stub.
			pir::setprop__vPsP($stub, 'PAST_BLOCK', $code_past);
        }
        
        # If this is a dispatcher, install dispatchee list that we can
        # add the candidates too.
        if $is_dispatcher {
            nqp::bindattr($code, $routine_type, '$!dispatchees', []);
        }
        
        # Set yada flag if needed.
        if $yada {
            nqp::bindattr_i($code, $routine_type, '$!yada', 1);
        }

        # Deserialization also needs to give the Parrot sub its backlink.
        if self.is_precompilation_mode() {
            $des.push(PAST::Op.new(
                :pirop('perl6_associate_sub_code_object vPP'),
                PAST::Val.new( :value($code_past) ),
                self.get_ref($code)));
        }

        # If it's a routine, flag that it needs fresh magicals.
        # Also store the namespace, which makes backtraces nicer.
        if nqp::istype($code, $routine_type) {
            self.get_static_lexpad($code_past).set_fresh_magicals();
            nqp::bindattr($code, $routine_type, '$!package', $*PACKAGE);
        }
            
        self.add_fixup_task(:deserialize_past($des), :fixup_past($fixups));
        $code;
    }

    method add_quasi_fixups($quasi_ast, $block) {
        $quasi_ast := pir::nqp_decontainerize__PP($quasi_ast);
        self.add_object($quasi_ast);
        unless $quasi_ast.is_quasi_ast {
            return "";
        }
        my $fixups := PAST::Op.new(:name<set_outer_ctx>, :pasttype<callmethod>,
                                   PAST::Val.new(:value($block)),
                                   PAST::Op.new(
                                        :pirop<perl6_get_outer_ctx__PP>,
                                        PAST::Var.new(
                                            :scope<attribute_6model>,
                                            :name<$!quasi_context>,
                                            self.get_ref($quasi_ast),
                                            self.get_ref(self.find_symbol(['AST']))
                                        )
                                   )
                        );
        self.add_fixup_task(:fixup_past($fixups));
    }
    
    # Adds any extra code needing for handling phasers.
    method add_phasers_handling_code($code, $code_past) {
        my $block_type := self.find_symbol(['Block']);
        if nqp::istype($code, $block_type) {
            sub run_phasers_code($type) {
                PAST::Op.new(
                    :pasttype('for'),
                    PAST::Var.new(
                        :scope('keyed'),
                        PAST::Var.new(
                            :scope('attribute_6model'), :name('$!phasers'),
                            $*W.get_ref($code),
                            $*W.get_ref($block_type)
                        ),
                        $type
                    ),
                    PAST::Block.new(
                        :blocktype('immediate'),
                        PAST::Op.new(
                            :pasttype('call'),
                            PAST::Var.new( :scope('parameter'), :name('$_') )
                        )))
            }
            my %phasers := nqp::getattr($code, $block_type, '$!phasers');
            if pir::exists(%phasers, 'PRE') {
                $code_past[0].push(PAST::Op.new( :pirop('perl6_set_checking_pre v') ));
                $code_past[0].push(run_phasers_code('PRE'));
                $code_past[0].push(PAST::Op.new( :pirop('perl6_clear_checking_pre v') ));
            }
            if pir::exists(%phasers, 'FIRST') {
                $code_past[0].push(PAST::Op.new(
                    :pasttype('if'),
                    PAST::Op.new( :pirop('perl6_take_block_first_flag i') ),
                    run_phasers_code('FIRST')));
            }
            if pir::exists(%phasers, 'ENTER') {
                $code_past[0].push(run_phasers_code('ENTER'));
            }
            if pir::exists(%phasers, '!LEAVE-ORDER') || pir::exists(%phasers, 'POST') {
                $code_past[+@($code_past) - 1] := PAST::Op.new(
                    :pirop('perl6_returncc__0P'),
                    $code_past[+@($code_past) - 1]);
            }
        }
    }
    
    # Adds a multi candidate to a proto/dispatch.
    method add_dispatchee_to_proto($proto, $candidate) {
        $proto.add_dispatchee($candidate);
    }
    
    # Derives a proto to get a dispatch.
    method derive_dispatcher($proto) {
        # Immediately do so and add to SC.
        my $derived := $proto.derive_dispatcher();
        self.add_object($derived);
        return $derived;
    }
    
    # Creates a new container descriptor and adds it to the SC.
    method create_container_descriptor($of, $rw, $name) {
        my $cd_type := self.find_symbol(['ContainerDescriptor']);
        my $cd      := pir::perl6_create_container_descriptor__PPPis($cd_type, $of, $rw, $name);
        self.add_object($cd);
        $cd
    }
    
    # Helper to make PAST for setting an attribute to a value. Value should
    # be a PAST tree.
    method set_attribute($obj, $class, $name, $value_past) {
        PAST::Op.new(
            :pasttype('bind_6model'),
            PAST::Var.new(
                :name($name), :scope('attribute_6model'),
                self.get_ref($obj), 
                self.get_ref($class)
            ),
            $value_past
        )
    }

    # Wraps a value in a scalar container
    method scalar_wrap($obj) {
        my $scalar_type := self.find_symbol(['Scalar']);
        my $scalar      := nqp::create($scalar_type);
        self.add_object($scalar);
        nqp::bindattr($scalar, $scalar_type, '$!value', $obj);
        $scalar;
    }
    
    # Takes a PAST::Block and compiles it for running during "compile time".
    # We need to do this for BEGIN but also for things that get called in
    # the compilation process, like user defined traits.
    method compile_in_context($past, $code_type, $slp_type) {
        # Ensure that we have the appropriate op libs loaded and correct
        # HLL.
        my $wrapper := PAST::Block.new(PAST::Stmts.new(), $past);
        self.add_libs($wrapper);
        $wrapper.hll('perl6');
        $wrapper.namespace('');
        
        # Create outer lexical contexts with all symbols visible. Maybe 
        # we can be a bit smarter here some day. But for now we just make a
        # single frame and copy all the visible things into it.
        my %seen;
        my $slp       := nqp::create($slp_type);
        my $mu        := try { self.find_symbol(['Mu']) };
        my $cur_block := $past;
        while $cur_block {
            my %symbols := $cur_block.symtable();
            for %symbols {
                unless %seen{$_.key} {
                    # Add slot for symbol.
                    $wrapper[0].push(PAST::Var.new(
                        :name($_.key), :scope('lexical_6model'), :isdecl(1) ));
                    $wrapper.symbol($_.key, :scope('lexical_6model'));
                    
                    # Make static lexpad entry.
                    my %sym := $_.value;
                    $slp.add_static_value($_.key,
                        (pir::exists(%sym, 'value') ?? %sym<value> !! $mu),
                        0, (%sym<state> ?? 1 !! 0));
                }
                %seen{$_.key} := 1;
            }
            $cur_block := $cur_block<outer>;
        }
        
        # Compile it, set wrapper's static lexpad, then invoke the wrapper,
        # which fixes up the lexicals.
        my $p6comp  := pir::compreg__Ps('perl6');
        my $post    := $p6comp.post($wrapper);
        my $pir     := $p6comp.pir($post);
        my $precomp := $p6comp.evalpmc($pir);
        $precomp[0].get_lexinfo.set_static_lexpad($slp);
        $precomp();
        
        # Fix up Code object associations (including nested blocks).
        # We un-stub any code objects for already-compiled inner blocks
        # to avoid wasting re-compiling them, and also to help make
        # parametric role outer chain work out. Also set up their static
        # lexpads, if they have any.
        my $num_subs := nqp::elems($precomp);
        my $i := 0;
        while $i < $num_subs {
            my $subid := $precomp[$i].get_subid();
            if pir::exists(%!sub_id_to_code_object, $subid) {
                pir::perl6_associate_sub_code_object__vPP($precomp[$i],
                    %!sub_id_to_code_object{$subid});
                nqp::bindattr(%!sub_id_to_code_object{$subid}, $code_type, '$!do', $precomp[$i]);
            }
            if pir::exists(%!sub_id_to_static_lexpad, $subid) {
                $precomp[$i].get_lexinfo.set_static_lexpad(%!sub_id_to_static_lexpad{$subid});
            }
            if pir::exists(%!sub_id_to_sc_idx, $subid) {
                pir::setprop__vPsP($precomp[$i], 'STATIC_CODE_REF', $precomp[$i]);
                self.update_root_code_ref(%!sub_id_to_sc_idx{$subid}, $precomp[$i]);
            }
            $i := $i + 1;
        }
        
        # Flag block as dynamically compiled.
        $past<DYNAMICALLY_COMPILED> := 1;
        
        # Return the Parrot Sub that maps to the thing we were originally
        # asked to compile.
        $precomp[1]
    }
    
    # Adds a constant value to the constants table. Returns PAST to do
    # the lookup of the constant.
    method add_constant($type, $primitive, :$nocache, *@value, *%named) {
        # If we already built this, find it in the cache and
        # just return that.
        my $cache_key;
        if !$nocache {
            my $namedkey := '';
            for %named {
                $namedkey := $namedkey ~ $_.key ~ ',' ~ $_.value ~ ';'
                    if pir::defined($_.value);
            }
            if $primitive eq 'bigint' {
                $cache_key := "$type,bigint," ~ nqp::tostr_I(@value[0]);
            } else {
                $cache_key := "$type,$primitive,"
                    ~ pir::join(',', @value)
                    ~ $namedkey;
            }
            if pir::exists(%!const_cache, $cache_key) {
                my $past := self.get_slot_past_for_object(%!const_cache{$cache_key});
                $past<has_compile_time_value> := 1;
                $past<compile_time_value> := %!const_cache{$cache_key};
                return $past;
            }
        }
        
        # Find type object for the box typed we'll create.
        my $type_obj := self.find_symbol(pir::split('::', $type));
        
        # Go by the primitive type we're boxing. Need to create
        # the boxed value and also code to produce it.
        my $constant;
        if $primitive eq 'int' {
            $constant := pir::repr_box_int__PiP(@value[0], $type_obj);
        }
        elsif $primitive eq 'str' {
            $constant := pir::repr_box_str__PsP(@value[0], $type_obj);
        }
        elsif $primitive eq 'num' {
            $constant := pir::repr_box_num__PnP(@value[0], $type_obj);
        }
        elsif $primitive eq 'bigint' {
            $constant := @value[0];
        }
        elsif $primitive eq 'type_new' {
            $constant := $type_obj.new(|@value, |%named);
        }
        else {
            pir::die("Don't know how to build a $primitive constant");
        }
        
        # Add to SC.
        self.add_object($constant);
        
        # Build PAST for getting the boxed constant from the constants
        # table, but also annotate it with the constant itself in case
        # we need it. Add to cache.
        my $past := self.get_slot_past_for_object($constant);
        $past<has_compile_time_value> := 1;
        $past<compile_time_value> := $constant;
        if !$nocache {
            %!const_cache{$cache_key} := $constant;
        }
        return $past;
    }
    
    # Adds a numeric constant value (int or num) to the constants table.
    # Returns PAST to do  the lookup of the constant.
    method add_numeric_constant($type, $value) {
        if $type eq 'Int' && pir::typeof__SP($value) eq 'Int' {
            if nqp::isbig_I($value) {
                # cannot unbox to int without loss of information
                my $past := self.add_constant('Int', 'bigint', $value);
                $past<has_compile_time_value> := 1;
                $past<compile_time_value>     := $value;
                return $past;
            }
            # since Int doesn't have any vtables yet (at least while compiling
            # the setting), it is inconvenient to work with, so unbox
            $value := nqp::unbox_i($value);
        }
        my $const := self.add_constant($type, nqp::lc($type), $value);
        my $tflag := $type eq 'Int' ?? 'Ii' !! 'Nn';
        my $past  := PAST::Want.new($const, $tflag,
            $value eq 'Inf' || $value eq '-Inf' || $value eq 'NaN' ??
                PAST::Op.new( :pirop('set Ns'), ~$value ) !!
                $value);
        $past<has_compile_time_value> := 1;
        $past<compile_time_value>     := $const<compile_time_value>;
        if $type eq 'Int' {
            $past<boxable_native> := 1;
        }
        elsif $type eq 'Num' {
            $past<boxable_native> := 2;
        }
        $past;
    }
    
    # Adds a string constant value to the constants table.
    # Returns PAST to do the lookup of the constant.
    method add_string_constant($value) {
        my $const := self.add_constant('Str', 'str', $value);
        my $past  := PAST::Want.new($const, 'Ss', $value);
        $past<has_compile_time_value> := 1;
        $past<compile_time_value>     := $const<compile_time_value>;
        $past<boxable_native>         := 3;
        $past;
    }
    
    # Adds the result of a constant folding operation to the SC and
    # returns a reference to it.
    method add_constant_folded_result($r) {
        self.add_object($r);
        self.get_ref($r);
    }

    # Creates a meta-object for a package, adds it to the root objects and
    # returns the created object.
    method pkg_create_mo($/, $how, :$name, :$repr, *%extra) {
        # Create the meta-object and add to root objects.
        my %args;
        if pir::defined($name) { %args<name> := ~$name; }
        if pir::defined($repr) { %args<repr> := ~$repr; }
        if pir::exists(%extra, 'base_type') {
            %args<base_type> := %extra<base_type>;
        }
        if pir::exists(%extra, 'group') {
            %args<group> := %extra<group>;
        }
        if pir::exists(%extra, 'signatured') {
            %args<signatured> := %extra<signatured>;
        }
        my $mo := $how.new_type(|%args);
        self.add_object($mo);

        # Result is just the object.
        return $mo;
    }
    
    # Constructs a meta-attribute and adds it to a meta-object. Expects to
    # be passed the meta-attribute type object, a set of literal named
    # arguments to pass and a set of name to object mappings to pass also
    # as named arguments, but where these passed objects also live in a
    # serialization context. The type would be passed in this way.
    method pkg_add_attribute($/, $obj, $meta_attr, %lit_args, %obj_args,
            %cont_info, $descriptor) {
        # Build container.
        my $cont := nqp::create(%cont_info<container_type>);
        nqp::bindattr($cont, %cont_info<container_base>, '$!descriptor', $descriptor);
        if pir::exists(%cont_info, 'default_value') {
            nqp::bindattr($cont, %cont_info<container_base>, '$!value',
                %cont_info<default_value>);
        }
        
        # Create meta-attribute instance and add right away. Also add
        # it to the SC.
        my $attr := $meta_attr.new(:auto_viv_container($cont), |%lit_args, |%obj_args);
        $obj.HOW.add_attribute($obj, $attr);
        self.add_object($attr);
        
        # Return attribute that was built.
        $attr
    }
    
    # Adds a method to the meta-object.
    method pkg_add_method($/, $obj, $meta_method_name, $name, $code_object) {
        $obj.HOW."$meta_method_name"($obj, $name, $code_object);
    }
    
    # Handles setting the body block code for a role.
    method pkg_set_role_body_block($/, $obj, $code_object, $past) {
        # Add it to the compile time meta-object.
        $obj.HOW.set_body_block($obj, $code_object);

        # Compile it immediately (we always compile role bodies as
        # early as possible, but then assume they don't need to be
        # re-compiled and re-fixed up at startup).
        self.compile_in_context($past, self.find_symbol(['Code']),
            self.find_symbol(['StaticLexPad']));
    }
    
    # Adds a possible role to a role group.
    method pkg_add_role_group_possibility($/, $group, $role) {
        $group.HOW.add_possibility($group, $role);
    }
    
    # Composes the package, and stores an event for this action.
    method pkg_compose($obj) {
        $obj.HOW.compose($obj);
    }
    
    # Builds a curried role based on a parsed argument list.
    method parameterize_type($role, $arglist, $/) {
        # Build a list of compile time arguments to the role; whine if
        # we find something without one.
        my @pos_args;
        my %named_args;
        for @($arglist[0].ast) {
            unless $_<has_compile_time_value> {
                $/.CURSOR.panic("Cannot use '" ~ $arglist[0].Str ~
                    "' as an argument to a parametric role as its value is not " ~
                    "known at compile time");
            }
            if $_.named {
                %named_args{$_.named} := $_<compile_time_value>;
            }
            else {
                @pos_args.push($_<compile_time_value>);
            }
        }
        
        self.parameterize_type_with_args($role, @pos_args, %named_args);
    }
    
    # Curries a role with the specified arguments.
    method parameterize_type_with_args($role, @pos_args, %named_args) {
        # Make the curry right away and add it to the SC.
        my $curried := $role.HOW.parameterize($role, |@pos_args, |%named_args);
        self.add_object($curried);
        return $curried;
    }
    
    # Creates a subset type meta-object/type object pair.
    method create_subset($how, $refinee, $refinement, :$name) {
        # Create the meta-object and add to root objects.
        my %args := hash(:refinee($refinee), :refinement($refinement));
        if pir::defined($name) { %args<name> := $name; }
        my $mo := $how.new_type(|%args);
        self.add_object($mo);
        return $mo;
    }
    
    # Adds a value to an enumeration.
    method create_enum_value($enum_type_obj, $key, $value) {
        # Create directly.
        my $val       := pir::repr_box_int__PiP($value, $enum_type_obj);
        my $base_type := ($enum_type_obj.HOW.parents($enum_type_obj, :local(1)))[0];
        nqp::bindattr($val, $enum_type_obj, '$!key', $key);
        nqp::bindattr($val, $enum_type_obj, '$!value',
            pir::repr_box_int__PiP($value, $base_type));
        self.add_object($val);
        
        # Add to meta-object.
        $enum_type_obj.HOW.add_enum_value($enum_type_obj, $val);

        # Result is the value.
        $val
    }
    
    # Applies a trait.
    method apply_trait($trait_sub_name, *@pos_args, *%named_args) {
        my $trait_sub := $*W.find_symbol([$trait_sub_name]);
        $trait_sub(|@pos_args, |%named_args);
    }
    
    # Some things get cloned many times with a lexical scope that
    # we never enter. This makes sure we capture them as needed.
    method create_lexical_capture_fixup() {
        # Create a list and put it in the SC.
        my class FixupList { has $!list }
        my $fixup_list := nqp::create(FixupList);
        self.add_object($fixup_list);
        nqp::bindattr($fixup_list, FixupList, '$!list', nqp::list());

        # Set up capturing code.
        my $capturer := self.cur_lexpad();
        $capturer[0].push(PAST::Op.new(
            :pirop('capture_all_outers vP'),
            PAST::Var.new(
                :name('$!list'), :scope('attribute_6model'),
                self.get_ref($fixup_list),
                self.get_ref(FixupList) )));
        
        # Return a PAST node that we can push the dummy closure
        return PAST::Op.new(
            :pirop('push vPP'),
            PAST::Var.new(
                :name('$!list'), :scope('attribute_6model'),
                self.get_ref($fixup_list),
                self.get_ref(FixupList) ));
    }
    
    # Handles addition of a phaser.
    method add_phaser($/, $phaser, $block, $phaser_past?) {
        if $phaser eq 'BEGIN' {
            # BEGIN phasers get run immediately.
            my $result := $block();
            return self.add_constant_folded_result($result);
        }
        elsif $phaser eq 'CHECK' {
            my $result_node := PAST::Stmt.new( PAST::Var.new( :name('Nil'), :scope('lexical_6model') ) );
            @!CHECKs := [] unless @!CHECKs;
            @!CHECKs.unshift([$block, $result_node]);
            return $result_node;
        }
        elsif $phaser eq 'INIT' {
            $*UNIT[0].push(PAST::Op.new(
                :pasttype('call'),
                self.get_ref($block)
            ));
            # XXX should keep value for r-value usage
            return PAST::Var.new(:name('Nil'), :scope('lexical_6model'));
        }
        elsif $phaser eq 'END' {
            $*UNIT[0].push(PAST::Op.new(
                :pasttype('callmethod'), :name('unshift'),
                PAST::Var.new( :name('@*END_PHASERS'), :scope('contextual') ),
                self.get_ref($block)
            ));
            return PAST::Var.new(:name('Nil'), :scope('lexical_6model'));
        }
        elsif $phaser eq 'START' {
            # Create a state variable to hold the phaser's result.
            my $pad := self.cur_lexpad();
            my $sym := $pad.unique('START_');
            my $mu := self.find_symbol(['Mu']);
            my $descriptor := self.create_container_descriptor($mu, 1, $sym);
            my %info;
            %info<container_type> := %info<container_base> := self.find_symbol(['Scalar']);
            %info<default_value> := %info<bind_constraint> := %info<value_type> := $mu;
            self.install_lexical_container($pad, $sym, %info, $descriptor, :state(1));
            
            # Generate code that runs the phaser the first time we init
            # the state block, or just evaluates to the existing value
            # in other cases.
            make PAST::Op.new(
                :pasttype('if'),
                PAST::Op.new( :pirop('perl6_state_needs_init I') ),
                PAST::Op.new(
                    :pirop('perl6_container_store__0PP'),
                    PAST::Var.new( :name($sym), :scope('lexical_6model') ),
                    PAST::Op.new( :pasttype('call'), $*W.get_ref($block) )
                ),
                PAST::Var.new( :name($sym), :scope('lexical_6model') ));
        }
        elsif $phaser eq 'PRE' || $phaser eq 'POST' {
            my $what := self.add_string_constant($phaser);
            $what.named('phaser');
            my $condition := self.add_string_constant(~$/<blorst>);
            $condition.named('condition');

            $phaser_past[1] := PAST::Op.new(
                :pasttype('unless'),
                $phaser_past[1],
                PAST::Op.new(
                    :pasttype('callmethod'), :name('throw'),
                    PAST::Op.new(
                        :pasttype('callmethod'), :name('new'),
                        self.get_ref(self.find_symbol(['X', 'Phaser', 'PrePost'])),
                        $what,
                        $condition,
                    )
                ),
            );
            
            if $phaser eq 'POST' {
                # Needs $_ that can be set to the return value.
                $phaser_past[0].unshift(PAST::Op.new( :pirop('bind_signature v') ));
                unless $phaser_past.symbol('$_') {
                    $phaser_past[0].unshift(PAST::Var.new( :name('$_'), :scope('lexical_6model'), :isdecl(1) ));
                }
                nqp::push(
                    nqp::getattr($block.signature, self.find_symbol(['Signature']), '$!params'),
                    self.create_parameter(hash(
                            variable_name => '$_', is_parcel => 1,
                            nominal_type => self.find_symbol(['Mu'])
                        )));
            }
            
            @!CODES[+@!CODES - 1].add_phaser($phaser, $block);
            return PAST::Var.new(:name('Nil'), :scope('lexical_6model'));
        }
        else {
            @!CODES[+@!CODES - 1].add_phaser($phaser, $block);
            return PAST::Var.new(:name('Nil'), :scope('lexical_6model'));
        }
    }
    
    # Runs the CHECK phasers and twiddles the PAST to look them up.
    method CHECK() {
        for @!CHECKs {
            my $result := $_[0]();
            $_[1][0] := self.add_constant_folded_result($result);
        }
    }
    
    # Adds required libraries to a compilation unit.
    method add_libs($comp_unit) {
        $comp_unit.loadlibs('nqp_group', 'nqp_ops', 'perl6_group', 'perl6_ops',
                            'bit_ops', 'math_ops', 'trans_ops', 'io_ops',
                            'obscure_ops', 'os', 'file', 'sys_ops',
                            'nqp_bigint_ops', 'nqp_dyncall_ops');
    }
    
    # Represents a longname after having parsed it.
    my class LongName {
        # The original text of the name.
        has str $!text;
        
        # Set of name components. Each one will be either a string
        # or a PAST node that represents an expresison to produce it.
        has @!components;
        
        # The colonpairs, if any.
        has @!colonpairs;
        
        # Flag for if the name ends in ::, meaning we need to emit a
        # .WHO on the end.
        has int $!get_who;
        
        # Gets the textual name of the value.
        method text() {
            $!text
        }
        
        # Gets the name, without any adverbs.
        method name() {
            my @parts := nqp::clone(@!components);
            @parts.shift() while self.is_pseudo_package(@parts[0]);
            pir::join('::', @parts)
        }
        
        # Gets the individual components, which may be PAST nodes for
        # unknown pieces.
        method components() {
            @!components
        }
        
        # Gets the individual components (which should be strings) but
        # taking a sigil and twigil and adding them to the last component.
        method variable_components($sigil, $twigil) {
            my @result;
            for @!components {
                @result.push($_);
            }
            @result[+@result - 1] := $sigil ~ $twigil ~ @result[+@result - 1];
            @result
        }
        
        # Checks if there is an indirect lookup required.
        method contains_indirect_lookup() {
            for @!components {
                if pir::can($_, 'isa') && $_.isa(PAST::Node) {
                    return 1;
                }
            }
            return 0;
        }
        
        # Fetches an array of components provided they are all known
        # or resolvable at compile time.
        method type_name_parts($dba, :$decl) {
            my @name;
            my $beyond_pp;
            if $decl && $!get_who {
                pir::die("Name $!text ends with '::' and cannot be used as a $dba");
            }
            for @!components {
                if pir::can($_, 'isa') && $_.isa(PAST::Node) {
                    if $_<has_compile_time_value> {
                        for nqp::split('::', ~$_<compile_time_value>) {
                            @name.push($_);
                        }
                    }
                    else {
                        pir::die("Name $!text is not compile-time known, and can not serve as a $dba");
                    }
                }
                elsif $beyond_pp || !self.is_pseudo_package($_) {
                    nqp::push(@name, $_);
                    $beyond_pp := 1;
                }
                else {
                    if $decl {
                        if $_ ne 'GLOBAL' {
                            pir::die("Cannot use pseudo-package $_ in a $dba");
                        }
                        elsif +@!components == 1 {
                            pir::die("Cannot declare pseudo-package GLOBAL");
                        }
                    }
                    else {
                        nqp::push(@name, $_);
                    }
                }
            }
            @name
        }
        
        method get_who() {
            $!get_who
        }

        # Checks if a name component is a pseudo-package.
        method is_pseudo_package($comp) {
            $comp eq 'CORE' || $comp eq 'SETTING' || $comp eq 'UNIT' ||
            $comp eq 'OUTER' || $comp eq 'MY' || $comp eq 'OUR' ||
            $comp eq 'PROCESS' || $comp eq 'GLOBAL' || $comp eq 'CALLER' ||
            $comp eq 'DYNAMIC' || $comp eq 'COMPILING' || $comp eq 'PARENT'
        }
    }
    
    # Takes a longname and turns it into an object representing the
    # name.
    method disect_longname($longname) {
        # Set up basic info about the long name.
        my $result := nqp::create(LongName);
        nqp::bindattr_s($result, LongName, '$!text', ~$longname);

        # Pick out the pieces of the name.
        my @components;
        my $name := $longname<name>;
        if $name<identifier> {
            @components.push(~$name<identifier>);
        }
        for $name<morename> {
            if $_<identifier> {
                @components.push(~$_<identifier>[0]);
            }
            elsif $_<EXPR> {
                my $EXPR := $_<EXPR>[0].ast;
                @components.push($EXPR);
            }
            else {
                # Either it's :: as a name entirely, in which case it's anon,
                # or we're ending in ::, in which case it implies .WHO.
                if +@components {
                    nqp::bindattr_i($result, LongName, '$!get_who', 1);
                }
            }
        }
        nqp::bindattr($result, LongName, '@!components', @components);
        
        # Stash colon pairs with names; incorporate non-named one into
        # the last part of the name (e.g. for infix:<+>). Need to be a
        # little cheaty when compiling the setting due to bootstrapping.
        my @pairs;
        for $longname<colonpair> {
            if $_<circumfix> && !$_<identifier> {
                my $value := $_.ast;
                if $value<has_compile_time_value> {
                    @components[+@components - 1] := @components[+@components - 1] ~
                        (%*COMPILING<%?OPTIONS><setting> ne 'NULL' ??
                        ':<' ~ ~$value<compile_time_value> ~ '>' !!
                        ~$_);
                }
                else {
                    pir::die(~$_ ~ ' cannot be resolved at compile time');
                }
            }
            else {
                @pairs.push($_);
            }
        }
        nqp::bindattr($result, LongName, '@!colonpairs', @pairs);
        
        $result
    }
    
    # Checks if a name starts with a pseudo-package.
    method is_pseudo_package($comp) {
        LongName.is_pseudo_package($comp)
    }
    
    # Checks if a given symbol is declared.
    method is_name(@name) {
        my $is_name := 0;
        if self.is_pseudo_package(@name[0]) {
            $is_name := 1;
        }
        else {
            try {
                # This throws if it's not a known name.
                self.find_symbol(@name);
                $is_name := 1;
            }
        }
        $is_name
    }
    
    # Checks if a given symbol is declared and a type object.
    method is_type(@name) {
        my $is_name := 0;
        try {
            # This throws if it's not a known name.
            $is_name := !nqp::isconcrete(self.find_symbol(@name))
        }
        $is_name
    }
    
    # Checks if a symbol has already been declared in the current
    # scope, and thus may not be redeclared.
    method already_declared($scope, $curpackage, $curpad, @name) {
        if $scope eq 'my' && +@name == 1 {
            my %sym := $curpad.symbol(@name[0]);
            if %sym {
                return %sym<value>.HOW.HOW.name(%sym<value>.HOW) ne 'Perl6::Metamodel::PackageHOW';
            }
            return 0;
        }
        else {
            # Does the current lexpad or package declare the first
            # part of the name? If not, we're in the clear.
            my $first_sym;
            if $curpad.symbol(@name[0]) {
                $first_sym := $curpad.symbol(@name[0])<value>;
            }
            elsif pir::exists($curpackage.WHO, @name[0]) {
                $first_sym := ($curpackage.WHO){@name[0]};
            }
            else {
                return 0;
            }
            
            # If we've more name, recursively check the next level
            # in the package. Otherwise, just go on if it's a
            # package or not.
            if +@name > 1 {
                my @restname := pir::clone(@name);
                @restname.shift;
                return self.already_declared('our', $first_sym, PAST::Block.new(), @restname);
            }
            else {
                return $first_sym.HOW.HOW.name($first_sym.HOW) ne 'Perl6::Metamodel::PackageHOW';
            }
        }
    }
    
    # Checks if there is a regex in scope.
    method regex_in_scope($name) {
        my $result := 0;
        try {
            my $maybe_regex := self.find_symbol([$name]);
            $result := nqp::istype($maybe_regex, self.find_symbol(['Regex']));
        }
        $result
    }
    
    # Finds a symbol that has a known value at compile time from the
    # perspective of the current scope. Checks for lexicals, then if
    # that fails tries package lookup.
    method find_symbol(@name) {
        # Make sure it's not an empty name.
        unless +@name { pir::die("Cannot look up empty name"); }

        # GLOBAL is current view of global.
        if +@name == 1 && @name[0] eq 'GLOBAL' {
            return $*GLOBALish;
        }

        # If it's a single-part name, look through the lexical
        # scopes.
        if +@name == 1 {
            my $final_name := @name[0];
            my $i := +@!BLOCKS;
            while $i > 0 {
                $i := $i - 1;
                my %sym := @!BLOCKS[$i].symbol($final_name);
                if +%sym {
                    if pir::exists(%sym, 'value') {
                        return %sym<value>;
                    }
                    else {
                        pir::die("No compile-time value for $final_name");
                    }
                }
            }
        }
        
        # If it's a multi-part name, see if the containing package
        # is a lexical somewhere. Otherwise we fall back to looking
        # in GLOBALish.
        my $result := $*GLOBALish;
        if +@name >= 2 {
            my $first := @name[0];
            my $i := +@!BLOCKS;
            while $i > 0 {
                $i := $i - 1;
                my %sym := @!BLOCKS[$i].symbol($first);
                if +%sym {
                    if pir::exists(%sym, 'value') {
                        $result := %sym<value>;
                        @name := pir::clone__PP(@name);
                        @name.shift();
                        $i := 0;
                    }
                    else {
                        pir::die("No compile-time value for $first");
                    }                    
                }
            }
        }
        
        # Try to chase down the parts of the name.
        for @name {
            if pir::exists($result.WHO, ~$_) {
                $result := ($result.WHO){$_};
            }
            else {
                pir::die("Could not locate compile-time value for symbol " ~
                    pir::join('::', @name));
            }
        }
        
        $result;
    }
    
    # Takes a name and compiles it to a lookup for the symbol.
    method symbol_lookup(@name, $/, :$package_only = 0, :$lvalue = 0) {
        # Catch empty names and die helpfully.
        if +@name == 0 { $/.CURSOR.panic("Cannot compile empty name"); }
        my $orig_name := pir::join('::', @name);
        
        # Handle fetching GLOBAL.
        if +@name == 1 && @name[0] eq 'GLOBAL' {
            return PAST::Var.new( :name('GLOBAL'), :namespace([]), :scope('package') );
        }
        
        # Handle things starting with pseudo-package.
        if self.is_pseudo_package(@name[0]) && @name[0] ne 'GLOBAL' && @name[0] ne 'PROCESS' {
            my $lookup;
            for @name {
                if $lookup {
                    $lookup := PAST::Op.new( :pirop('get_who PP'), $lookup );
                }
                else {
                    # Lookups start at the :: root.
                    $lookup := PAST::Op.new(
                        :pasttype('callmethod'), :name('new'),
                        $*W.get_ref($*W.find_symbol(['PseudoStash']))
                    );
                }
                $lookup := PAST::Op.new(
                    :pasttype('callmethod'), :name('postcircumfix:<{ }>'),
                    $lookup,
                    self.add_string_constant($_));
            }
            return $lookup;
        }
        
        # If it's a single item, then go hunting for it through the
        # block stack.
        if +@name == 1 && !$package_only {
            my $i := +@!BLOCKS;
            while $i > 0 {
                $i := $i - 1;
                my %sym := @!BLOCKS[$i].symbol(@name[0]);
                if +%sym {
                    return PAST::Var.new( :name(@name[0]), :scope(%sym<scope>) );
                }
            }
        }
        
        # The final lookup will always be just a keyed index (cheap)
        # for non-lvalue case, or at_key call on a Stash.
        my $final_name := @name.pop();
        my $lookup := $lvalue ??
            PAST::Op.new(
                :pasttype('callmethod'), :name('at_key'),
                self.add_constant('Str', 'str', $final_name)) !!
            PAST::Var.new( :scope('keyed'), ~$final_name);
        
        # If there's no explicit qualification, then look it up in the
        # current package, and fall back to looking in GLOBAL.
        if +@name == 0 {
            $lookup.unshift(PAST::Op.new(
                :pirop('get_who PP'),
                PAST::Var.new( :name('$?PACKAGE'), :scope('lexical_6model') )
            ));
            $lookup.isa(PAST::Var) && $lookup.viviself(PAST::Var.new(
                :scope('keyed'),
                :viviself(self.lookup_failure($orig_name)),
                PAST::Op.new(
                    :pirop('get_who PP'),
                    PAST::Var.new( :name('GLOBAL'), :namespace([]), :scope('package') )
                ),
                ~$final_name
            ));
        }
        
        # Otherwise, see if the first part of the name is lexically
        # known. If not, it's in GLOBAL. Also, if first part is GLOBAL
        # then strip it off.
        else {
            my $path := self.is_lexical(@name[0]) ??
                PAST::Var.new( :name(@name.shift()), :scope('lexical_6model') ) !!
                PAST::Var.new( :name('GLOBAL'), :namespace([]), :scope('package') );
            if @name[0] eq 'GLOBAL' {
                @name := pir::clone__PP(@name);
                @name.shift();
            }
            for @name {
                $path := PAST::Op.new(
                    :pirop('perl6_get_package_through_who PPs'),
                    $path, ~$_);
            }
            $lookup.unshift(PAST::Op.new(:pirop('get_who PP'), $path));
        }
        
        # Failure object if we can't find the name.
        if $lookup.isa(PAST::Var) && !$lookup.viviself {
            $lookup.viviself(self.lookup_failure($orig_name));
        }
        
        return $lookup;
    }
    
    method lookup_failure($orig_name) {
        my $msg := "Could not find symbol '$orig_name'";
        return PAST::Op.new(
            :pasttype('call'), :name('&die'),
            self.add_constant('Str', 'str', $msg)
        );
    }

    # Checks if the given name is known anywhere in the lexpad
    # and with lexical scope.
    method is_lexical($name) {
        my $i := +@!BLOCKS;
        while $i > 0 {
            $i := $i - 1;
            my %sym := @!BLOCKS[$i].symbol($name);
            if +%sym {
                return %sym<scope> eq 'lexical_6model' ||
                       %sym<scope> eq 'lexical';
            }
        }
        0;
    }
    
    # Checks if the symbol is really an alias to an attribute.
    method is_attr_alias($name) {
        my $i := +@!BLOCKS;
        while $i > 0 {
            $i := $i - 1;
            my %sym := @!BLOCKS[$i].symbol($name);
            if +%sym {
                return %sym<attr_alias>;
            }
        }
    }

    # Generates a series of PAST operations that will build this context if
    # it doesn't exist, and fix it up if it already does.
    method to_past() {
        if self.is_precompilation_mode() {
            my $load_tasks := PAST::Stmts.new();
            for self.load_dependency_tasks() {
                $load_tasks.push(PAST::Stmt.new($_));
            }
            my $fixup_tasks := PAST::Stmts.new();
            for self.fixup_tasks() {
                $fixup_tasks.push(PAST::Stmt.new($_));
            }
            return PAST::Stmts.new(
                PAST::Op.new( :pirop('nqp_dynop_setup v') ),
                PAST::Op.new( :pirop('nqp_bigint_setup v') ),
                PAST::Op.new( :pirop('nqp_native_call_setup v') ),
                PAST::Op.new( :pirop('rakudo_dynop_setup v') ),
                PAST::Op.new(
                    :pasttype('callmethod'), :name('hll_map'),
                    PAST::Op.new( :pirop('getinterp P') ),
                    PAST::Op.new( :pirop('get_class Ps'), 'LexPad' ),
                    PAST::Op.new( :pirop('get_class Ps'), 'Perl6LexPad' )
                ),
                PAST::Op.new(
                    :pasttype('bind_6model'),
                    PAST::Var.new( :name('cur_sc'), :scope('register'), :isdecl(1) ),
                    PAST::Op.new( :pirop('nqp_create_sc Ps'), self.handle() )
                ),
                PAST::Op.new(
                    :pasttype('callmethod'), :name('set_description'),
                    PAST::Var.new( :name('cur_sc'), :scope('register') ),
                    self.sc.description
                ),
                $load_tasks,
                self.serialize_and_produce_deserialization_past('cur_sc'),
                $fixup_tasks
            )
        }
        else {
            my $tasks := PAST::Stmts.new();
            for self.load_dependency_tasks() {
                $tasks.push(PAST::Stmt.new($_));
            }
            for self.fixup_tasks() {
                $tasks.push(PAST::Stmt.new($_));
            }
            return $tasks
        }
    }

    # throws a typed exception
    method throw($/, $ex_type, *%opts) {
        # TODO: provide context
        my $type_found := 1;
        my $ex := try {
            CATCH { $type_found := 0 };
            self.find_symbol(pir::does($ex_type, 'array') ?? $ex_type !! nqp::split('::', $ex_type));
        };

        if $type_found {
             %opts<line>     := HLL::Compiler.lineof($/.orig, $/.from);
            for %opts -> $p {
                if pir::does($p.value, 'array') {
                    my @a := [];
                    for $p.value {
                        nqp::push(@a, pir::perl6ize_type__PP($_));
                    }
                    %opts{$p.key} := pir::perl6ize_type__PP(@a);
                }
                else {
                    %opts{$p.key} := pir::perl6ize_type__PP($p.value);
                }
            }
            my $file        := pir::find_caller_lex__ps('$?FILES');
            %opts<filename> := nqp::box_s(
                (pir::isnull($file) ?? '<unknown file>' !! $file),
                self.find_symbol(['Str'])
            );
            $ex.new(|%opts).throw;
        } else {
            my @err := ['Error while compiling, type ', nqp::join('::', $ex_type),  "\n"];
            for %opts -> $key {
                @err.push: '  ';
                @err.push: $key;
                @err.push: ': ';
                @err.push: %opts{$key};
                @err.push: "\n";
            }
            $/.CURSOR.panic(nqp::join('', @err));
        }
    }
}
