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

# This builds upon the SerializationContextBuilder to add the specifics
# needed by Rakudo Perl 6.
class Perl6::SymbolTable is HLL::Compiler::SerializationContextBuilder {
    # The stack of lexical pads, actually as PAST::Block objects. The
    # outermost frame is at the bottom, the latest frame is on top.
    has @!BLOCKS;
    
    # Mapping of sub IDs to their proto code objects; used for fixing
    # up in dynamic compilation.
    has %!sub_id_to_code_object;
    
    # Array of stubs to check and the end of compilation.
    has @!stub_check;
    
    # Cached constants that we've built.
    has %!const_cache;
    
    # Creates a new lexical scope and puts it on top of the stack.
    method push_lexpad($/) {
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
    method assert_stubs_defined() {
        my @incomplete;
        for @!stub_check {
            unless $_.HOW.is_composed($_) {
                @incomplete.push($_.HOW.name($_));
            }
        }
        if +@incomplete {
            pir::die("The following packages were stubbed but not defined:\n    " ~
                pir::join("\n    ", @incomplete) ~ "\n");
        }
    }
    
    # Loads a setting.
    method load_setting($setting_name) {
        # Do nothing for the NULL setting.
        if $setting_name ne 'NULL' {    
            # Load it immediately, so the compile time info is available.
            # Once it's loaded, set it as the outer context of the code
            # being compiled.
            my $setting := %*COMPILING<%?OPTIONS><outer_ctx>
                        := Perl6::ModuleLoader.load_setting($setting_name);
            
            # Do load in code.
            my $fixup := PAST::Stmts.new(
                PAST::Op.new(
                    :pirop('load_bytecode vs'), 'blib/Perl6/ModuleLoader.pbc'
                ),
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
            self.add_event(:deserialize_past($fixup), :fixup_past($fixup));
            
            return pir::getattribute__PPs($setting, 'lex_pad');
        }
    }
    
    # Loads a module immediately, and also makes sure we load it
    # during the deserialization.
    method load_module($module_name, $cur_GLOBALish) {
        # Immediate loading.
        my $module := Perl6::ModuleLoader.load_module($module_name, $cur_GLOBALish);
        
        # Make sure we do the loading during deserialization.
        self.add_event(:deserialize_past(PAST::Stmts.new(
            PAST::Op.new(
                :pirop('load_bytecode vs'), 'blib/Perl6/ModuleLoader.pbc'
            ),
            PAST::Op.new(
               :pasttype('callmethod'), :name('load_module'),
               PAST::Var.new( :name('ModuleLoader'), :namespace([]), :scope('package') ),
               $module_name,
               self.get_slot_past_for_object($cur_GLOBALish)
            ))));
            
        return pir::getattribute__PPs($module, 'lex_pad');
    }
    
    # Implements a basic first cut of import. Works out what needs to
    # happen and builds code to do it at fixup/deserialization; also
    # does it so that things are available at compile time. Note that
    # import is done into the current lexpad.
    method import($package) {
        my %stash := $package.WHO;
        my $target := self.cur_lexpad();
        my $fixups := PAST::Stmts.new();
        for %stash {
            # Install the imported symbol directly as a block symbol.
            $target.symbol($_.key, :scope('lexical_6model'), :value($_.value));
            $target[0].push(PAST::Var.new( :scope('lexical_6model'), :name($_.key), :isdecl(1) ));
            
            # Add fixup/deserialize event to stick it in the static lexpad.
            $fixups.push(PAST::Op.new(
                :pasttype('callmethod'), :name('set_static_lexpad_value'),
                PAST::Op.new(
                    :pasttype('callmethod'), :name('get_lexinfo'),
                    PAST::Val.new( :value($target) )
                ),
                $_.key,
                PAST::Var.new(
                    :scope('keyed'),
                    PAST::Op.new(
                        :pirop('get_who PP'),
                        self.get_object_sc_ref_past($package)
                    ),
                    $_.key
                ),
                0, 0
            ));
        }
        self.add_event(:deserialize_past($fixups), :fixup_past($fixups));
        1;
    }
    
    # Factors out deciding where to install package-y things.
    method install_package($/, $longname, $scope, $pkgdecl, $package, $outer, $symbol) {
        if $scope eq 'my' {
            if +$longname<name><morename> == 0 {
                self.install_lexical_symbol($outer, ~$longname<name>, $symbol);
            }
            else {
                $/.CURSOR.panic("Cannot use multi-part package name with 'my' scope");
            }
        }
        elsif $scope eq 'our' {
            self.install_package_symbol($package, ~$longname<name>, $symbol);
            if +$longname<name><morename> == 0 {
                self.install_lexical_symbol($outer, ~$longname<name>, $symbol);
            }
        }
        else {
            $/.CURSOR.panic("Cannot use $*SCOPE scope with $pkgdecl");
        }
        1;
    }
    
    # Installs a lexical symbol. Takes a PAST::Block object, name and
    # the object to install. Does an immediate installation in the
    # compile-time block symbol table, and ensures that the installation
    # gets fixed up at runtime too.
    method install_lexical_symbol($block, $name, $obj) {
        # Install the object directly as a block symbol.
        $block.symbol($name, :scope('lexical_6model'), :value($obj));
        $block[0].push(PAST::Var.new( :scope('lexical_6model'), :name($name), :isdecl(1) ));
        
        # Fixup and deserialization task is the same.
        my $fixup := PAST::Op.new(
            :pasttype('callmethod'), :name('set_static_lexpad_value'),
            PAST::Op.new(
                :pasttype('callmethod'), :name('get_lexinfo'),
                PAST::Val.new( :value($block) )
            ),
            ~$name, self.get_object_sc_ref_past($obj),
            0, 0
        );
        self.add_event(:deserialize_past($fixup), :fixup_past($fixup));
        1;
    }
    
    # Installs a lexical symbol. Takes a PAST::Block object, name and
    # the type of container to install.
    method install_lexical_container($block, $name, $type_name, $descriptor, :$state, *@default_value) {
        # Add to block, if needed. Note that it doesn't really have
        # a compile time value.
        unless $block.symbol($name) {
            $block.symbol($name, :scope('lexical_6model'), :descriptor($descriptor));
            $block[0].push(PAST::Var.new( :scope('lexical_6model'), :name($name),
                :isdecl(1), :type($descriptor.of) ));
        }
            
        # If it's a native type, we're done - no container
        # as we inline natives straight into registers.
        if pir::repr_get_primitive_type_spec__IP($descriptor.of) {
            if $state { pir::die("Natively typed state variables not yet implemented") }
            return 1;
        }
        
        # Look up container type and create code to instantiate it.
        my $cont_code := self.build_container_past($type_name, $descriptor, |@default_value);
        
        # Fixup and deserialization task is the same - creating the
        # container type and put it in the static lexpad with a clone
        # flag set.
        my $fixup := PAST::Op.new(
            :pasttype('callmethod'), :name('set_static_lexpad_value'),
            PAST::Op.new(
                :pasttype('callmethod'), :name('get_lexinfo'),
                PAST::Val.new( :value($block) )
            ),
            ~$name, $cont_code, 1, ($state ?? 1 !! 0)
        );
        self.add_event(:deserialize_past($fixup), :fixup_past($fixup));
        1;
    }
    
    # Builds PAST that constructs a container.
    method build_container_past($type_name, $descriptor, *@default_value) {
        # Create container.
        my $type_obj := self.find_symbol([$type_name]);
        my $cont_code := PAST::Op.new(
            :pirop('repr_instance_of PP'),
            self.get_object_sc_ref_past($type_obj)
        );
        
        # Set container descriptor.
        $cont_code := PAST::Op.new(
            :pirop('setattribute 0PPsP'),
            $cont_code, self.get_object_sc_ref_past($type_obj),
            '$!descriptor', self.get_object_sc_ref_past($descriptor));
        
        # Default contents, if applicable (note, slurpy param as we can't
        # use definedness here, as it's a type object we'd be checking).
        if +@default_value {
            $cont_code := PAST::Op.new(
                :pirop('setattribute 0PPsP'),
                $cont_code, self.get_object_sc_ref_past($type_obj),
                '$!value', self.get_object_sc_ref_past(@default_value[0]));
        }
        
        $cont_code
    }
    
    # Hunts through scopes to find a container descriptor for a lexical.
    method find_lexical_container_descriptor($name) {
        my $i := +@!BLOCKS;
        while $i > 0 {
            $i := $i - 1;
            my %sym := @!BLOCKS[$i].symbol($name);
            if +%sym {
                if pir::exists(%sym, 'descriptor') {
                    return %sym<descriptor>;
                }
                else {
                    $i := 0;
                }
            }
        }
        pir::die("Could not find container descriptor for $name");
    }
    
    # Installs a symbol into the package. Does so immediately, and
    # makes sure this happens on deserialization also.
    method install_package_symbol($package, $symbol, $obj) {
        my @sym := pir::split('::', $symbol);
        my $name := ~@sym.pop();
        
        # Install symbol immediately.
        my $target := $package;
        for @sym {
            $target := pir::perl6_get_package_through_who__PPs($target, $_);
        }
        ($target.WHO){$name} := $obj;
        
        # Add deserialization installation of the symbol.
        my $path := self.get_slot_past_for_object($package);
        for @sym {
            $path := PAST::Op.new(:pirop('perl6_get_package_through_who PPs'), $path, ~$_);
        }
        self.add_event(:deserialize_past(PAST::Op.new(
            :pasttype('bind_6model'),
            PAST::Var.new(
                :scope('keyed'),
                PAST::Op.new( :pirop('get_who PP'), $path ),
                $name
            ),
            self.get_slot_past_for_object($obj)
        )));
        1;
    }
    
    # Creates a parameter object.
    method create_parameter(%param_info) {
        # Create parameter object now.
        my $par_type  := self.find_symbol(['Parameter']);
        my $parameter := pir::repr_instance_of__PP($par_type);
        my $slot      := self.add_object($parameter);
        
        # Create PAST to make it when deserializing.
        my $set_attrs := PAST::Stmts.new();
        self.add_event(:deserialize_past(PAST::Stmts.new(
            self.set_slot_past($slot, self.set_cur_sc(PAST::Op.new(
                :pirop('repr_instance_of PP'),
                self.get_object_sc_ref_past($par_type)
            ))),
            $set_attrs
        )));
        
        # Set name if there is one.
        if pir::exists(%param_info, 'variable_name') {
            pir::repr_bind_attr_str__vPPsS($parameter, $par_type, '$!variable_name', %param_info<variable_name>);
            $set_attrs.push(self.set_attribute_typed($parameter, $par_type,
                '$!variable_name', %param_info<variable_name>, str));
        }
        
        # Set nominal type.
        pir::setattribute__vPPsP($parameter, $par_type, '$!nominal_type', %param_info<nominal_type>);
        $set_attrs.push(self.set_attribute($parameter, $par_type, '$!nominal_type',
            self.get_object_sc_ref_past(%param_info<nominal_type>)));
        
        # Calculate and set flags.
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
        pir::repr_bind_attr_int__vPPsI($parameter, $par_type, '$!flags', $flags);
        $set_attrs.push(self.set_attribute_typed($parameter, $par_type,
            '$!flags', $flags, int));
        
        # Set named names up, for named parameters.
        if %param_info<named_names> {
            my @names := %param_info<named_names>;
            pir::setattribute__vPPsP($parameter, $par_type, '$!named_names', @names);
            $set_attrs.push(self.set_attribute($parameter, $par_type, '$!named_names',
                PAST::Op.new( :pasttype('list'), |@names )));
        }
        
        # Set type captures up.
        if %param_info<type_captures> {
            my @type_names := %param_info<type_captures>;
            pir::setattribute__vPPsP($parameter, $par_type, '$!type_captures', @type_names);
            $set_attrs.push(self.set_attribute($parameter, $par_type, '$!type_captures',
                PAST::Op.new( :pasttype('list'), |@type_names )));
        }
        
        # Post constraints.
        if %param_info<post_constraints> {
            pir::setattribute__vPPsP($parameter, $par_type, '$!post_constraints',
                %param_info<post_constraints>);
            $set_attrs.push(self.set_attribute($parameter, $par_type, '$!post_constraints',
                (my $con_list := PAST::Op.new( :pasttype('list') ))));
            for %param_info<post_constraints> {
                $con_list.push(self.get_object_sc_ref_past($_));
            }
        }
        
        # Set default value thunk up, if there is one.
        if pir::exists(%param_info, 'default_closure') {
            pir::setattribute__vPPsP($parameter, $par_type, '$!default_closure', %param_info<default_closure>);
            $set_attrs.push(self.set_attribute($parameter, $par_type, '$!default_closure',
                self.get_object_sc_ref_past(%param_info<default_closure>)));
        }
        
        # Set container descriptor, if there is one.
        if pir::exists(%param_info, 'container_descriptor') {
            pir::setattribute__vPPsP($parameter, $par_type, '$!container_descriptor', %param_info<container_descriptor>);
            $set_attrs.push(self.set_attribute($parameter, $par_type, '$!container_descriptor',
                self.get_object_sc_ref_past(%param_info<container_descriptor>)));
        }
        
        # Set attributive bind package up, if there is one.
        if pir::exists(%param_info, 'attr_package') {
            pir::setattribute__vPPsP($parameter, $par_type, '$!attr_package', %param_info<attr_package>);
            $set_attrs.push(self.set_attribute($parameter, $par_type, '$!attr_package',
                self.get_object_sc_ref_past(%param_info<attr_package>)));
        }
        
        # Return created parameter.
        $parameter
    }
    
    # Creates a signature object from a set of parameters.
    method create_signature(@parameters) {
        # Create signature object now.
        my $sig_type  := self.find_symbol(['Signature']);
        my $signature := pir::repr_instance_of__PP($sig_type);
        my $slot      := self.add_object($signature);
        
        # Set parameters.
        pir::setattribute__vPPsP($signature, $sig_type, '$!params', @parameters);
        
        # Create PAST to make it when deserializing.
        my $param_past := PAST::Op.new( :pasttype('list') );
        for @parameters {
            $param_past.push(self.get_slot_past_for_object($_));
        }
        self.add_event(:deserialize_past(PAST::Stmts.new(
            self.set_slot_past($slot, self.set_cur_sc(PAST::Op.new(
                :pirop('repr_instance_of PP'),
                self.get_object_sc_ref_past($sig_type)
            ))),
            self.set_attribute($signature, $sig_type, '$!params', $param_past)
        )));
        
        # Return created signature.
        $signature
    }
    
    # Creates a code object and ensures that it gets fixed up with the compiled
    # body at fixup time; during the deserialize we just set the already compiled
    # output right into place. If we get a request to run the code before we did
    # really compiling it, we can do that - we just dynamically compile it.
    method create_code_object($code_past, $type, $signature, $is_dispatcher = 0) {
        my $fixups := PAST::Stmts.new();
        my $des    := PAST::Stmts.new();
        
        # Create code object now.
        my $type_obj  := self.find_symbol([$type]);
        my $code_type := self.find_symbol(['Code']);
        my $code      := pir::repr_instance_of__PP($type_obj);
        my $slot      := self.add_object($code);
        
        # Stash it under the PAST block sub ID.
        %!sub_id_to_code_object{$code_past.subid()} := $code;
        
        # For now, install stub that will dynamically compile the code if
        # we ever try to run it during compilation.
        my $precomp;
        my $stub := sub (*@pos, *%named) {
            my $rns    := pir::get_root_namespace__P();
            my $p6_pns := $rns{'perl6'};
            $p6_pns{'GLOBAL'} := $*GLOBALish;
            unless $precomp {
                $precomp := self.compile_in_context($code_past, $code_type);
            }
            $precomp(|@pos, |%named);
        };
        pir::set__vPS($stub, $code_past.name);
        pir::setattribute__vPPsP($code, $code_type, '$!do', $stub);
        
        # Fixup will install the real thing, unless we're in a role, in
        # which case pre-comp will have sorted it out.
        unless $*PKGDECL eq 'role' {
            $fixups.push(PAST::Stmts.new(
                self.set_attribute($code, $code_type, '$!do', PAST::Val.new( :value($code_past) )),
                PAST::Op.new(
                    :pirop('perl6_associate_sub_code_object vPP'),
                    PAST::Val.new( :value($code_past) ),
                    self.get_object_sc_ref_past($code)
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
                        self.get_object_sc_ref_past($clone)
                    )));
            });
        }
        
        # Desserialization should do the actual creation and just put the right
        # code in there in the first place.
        $des.push(self.set_slot_past($slot, self.set_cur_sc(PAST::Op.new(
            :pirop('repr_instance_of PP'),
            self.get_object_sc_ref_past($type_obj)
        ))));
        $des.push(self.set_attribute($code, $code_type, '$!do', PAST::Val.new( :value($code_past) )));
        
        # Install signauture now and add to deserialization.
        pir::setattribute__vPPsP($code, $code_type, '$!signature', $signature);
        $des.push(self.set_attribute($code, $code_type, '$!signature', self.get_object_sc_ref_past($signature)));
        
        # If this is a dispatcher, install dispatchee list that we can
        # add the candidates too.
        if $is_dispatcher {
            pir::setattribute__vPPsP($code, $code_type, '$!dispatchees', []);
            $des.push(self.set_attribute($code, $code_type, '$!dispatchees',
                PAST::Op.new( :pasttype('list') )));
        }
        
        # Deserialization also needs to give the Parrot sub its backlink.
        $des.push(PAST::Op.new(
            :pirop('perl6_associate_sub_code_object vPP'),
            PAST::Val.new( :value($code_past) ),
            self.get_object_sc_ref_past($code)));

        # If it's a routine, flag that it needs fresh magicals.
        if pir::type_check__IPP($code, self.find_symbol(['Routine'])) {
            my $set := PAST::Op.new(
                :pasttype('callmethod'), :name('set_fresh_magicals'),
                PAST::Op.new(
                    :pasttype('callmethod'), :name('get_lexinfo'),
                    PAST::Val.new( :value($code_past) )));
            $des.push($set);
            $fixups.push($set);
        }
            
        self.add_event(:deserialize_past($des), :fixup_past($fixups));
        $code;
    }
    
    # Adds a multi candidate to a proto/dispatch.
    method add_dispatchee_to_proto($proto, $candidate) {
        # Add it to the list.
        my $code_type := self.find_symbol(['Code']);
        $proto.add_dispatchee($candidate);
        
        # Deserializatin code to add it.
        self.add_event(:deserialize_past(PAST::Op.new(
            :pasttype('callmethod'), :name('add_dispatchee'),
            self.get_object_sc_ref_past($proto),
            self.get_object_sc_ref_past($candidate)
        )));
    }
    
    # Derives a proto to get a dispatch.
    method derive_dispatcher($proto) {
        # Immediately do so and add to SC.
        my $derived := $proto.derive_dispatcher();
        my $slot    := self.add_object($derived);
        
        # Add deserialization action.
        my $des := PAST::Op.new(
            :pasttype('callmethod'), :name('derive_dispatcher'),
            self.get_object_sc_ref_past($proto));
        self.add_event(:deserialize_past(
            self.set_slot_past($slot, self.set_cur_sc($des))
        ));
        
        return $derived;
    }
    
    # Creates a new container descriptor and adds it to the SC.
    method create_container_descriptor($of, $rw, $name) {
        # Create descriptor object now.
        my $cd_type := self.find_symbol(['ContainerDescriptor']);
        my $cd      := pir::repr_instance_of__PP($cd_type);
        my $slot    := self.add_object($cd);
        
        # Create PAST to make it when deserializing.
        my $set_attrs := PAST::Stmts.new();
        self.add_event(:deserialize_past(PAST::Stmts.new(
            self.set_slot_past($slot, self.set_cur_sc(PAST::Op.new(
                :pirop('repr_instance_of PP'),
                self.get_object_sc_ref_past($cd_type)
            ))),
            $set_attrs
        )));
        
        # Set attributes.
        pir::setattribute__vPPsP($cd, $cd_type, '$!of', $of);
        $set_attrs.push(self.set_attribute($cd, $cd_type, '$!of',
            self.get_object_sc_ref_past($of)));
        pir::repr_bind_attr_int__vPPsI($cd, $cd_type, '$!rw', $rw);
        $set_attrs.push(self.set_attribute_typed($cd, $cd_type, '$!rw', $rw, int));
        pir::repr_bind_attr_str__vPPsS($cd, $cd_type, '$!name', $name);
        $set_attrs.push(self.set_attribute_typed($cd, $cd_type, '$!name', $name, str));
        
        $cd
    }
    
    # Helper to make PAST for setting an attribute to a value. Value should
    # be a PAST tree.
    method set_attribute($obj, $class, $name, $value_past) {
        PAST::Stmt.new(
            PAST::Op.new(
                :pasttype('bind_6model'),
                PAST::Var.new(
                    :name($name), :scope('attribute_6model'),
                    self.get_object_sc_ref_past($obj), 
                    self.get_object_sc_ref_past($class)
                ),
                $value_past
            )
        )
    }
    
    # Helper to make PAST for setting a typed attribute to a value. Value should
    # be a PAST tree.
    method set_attribute_typed($obj, $class, $name, $value_past, $type) {
        PAST::Stmt.new(
            PAST::Op.new(
                :pasttype('bind_6model'),
                PAST::Var.new(
                    :name($name), :scope('attribute_6model'), :type($type),
                    self.get_object_sc_ref_past($obj), 
                    self.get_object_sc_ref_past($class)
                ),
                $value_past
            )
        )
    }
    
    # Takes a PAST::Block and compiles it for running during "compile time".
    # We need to do this for BEGIN but also for things that get called in
    # the compilation process, like user defined traits.
    method compile_in_context($past, $code_type) {
        # Ensure that we have the appropriate op libs loaded and correct
        # HLL.
        my $wrapper := PAST::Block.new(PAST::Stmts.new(), $past);
        $wrapper.loadlibs('perl6_group', 'perl6_ops');
        $wrapper.hll('perl6');
        $wrapper.namespace('');
        
        # Create outer lexical contexts with all symbols visible.
        # XXX OK, so here the plan really is that we implement some kind
        # of static lexpads, which will then be a good lead on to having
        # stuff work in pre-comp too. However, that's a good day or two
        # of refactoring, so now we just shove all the compile-time known
        # symbols into the wrapper.
        my %seen;
        my $cur_block := $past;
        while $cur_block {
            my %symbols := $cur_block.symtable();
            for %symbols {
                unless %seen{$_.key} {
                    my %sym := $_.value;
                    if pir::exists(%sym, 'value') {
                        my $ref;
                        try {
                            $ref := $*ST.get_object_sc_ref_past(%sym<value>);
                        }
                        unless $ref {
                            try {
                                self.add_object(%sym<value>);
                                $ref := $*ST.get_object_sc_ref_past(%sym<value>);
                            }
                        }
                        if $ref {
                            $wrapper[0].push(PAST::Var.new(
                                :name($_.key), :scope('lexical_6model'), :isdecl(1),
                                :viviself($ref)
                            ));
                            $wrapper.symbol($_.key, :scope('lexical_6model'));
                        }
                    }
                }
                %seen{$_.key} := 1;
            }
            $cur_block := $cur_block<outer>;
        }
        
        # Compile it, then invoke the wrapper, which fixes up the
        # other lexicals.
        my $precomp := PAST::Compiler.compile($wrapper);
        $precomp();
        
        # Fix up Code object associations (including nested blocks).
        # We un-stub any code objects for already-compiled inner blocks
        # to avoid wasting re-compiling them, and also to help make
        # parametric role outer chain work out.
        my $num_subs := nqp::elems($precomp);
        my $i := 0;
        while $i < $num_subs {
            my $subid := $precomp[$i].get_subid();
            if pir::exists(%!sub_id_to_code_object, $subid) {
                pir::perl6_associate_sub_code_object__vPP($precomp[$i],
                    %!sub_id_to_code_object{$subid});
                nqp::bindattr(%!sub_id_to_code_object{$subid}, $code_type, '$!do', $precomp[$i]);
            }
            $i := $i + 1;
        }
        
        # Return the Parrot Sub that maps to the thing we were originally
        # asked to compile.
        $precomp[1]
    }
    
    # Adds a constant value to the constants table. Returns PAST to do
    # the lookup of the constant.
    method add_constant($type, $primitive, *@value) {
        # If we already built this, find it in the cache and
        # just return that.
        my $cache_key := "$type,$primitive," ~ pir::join(',', @value);
        if pir::exists(%!const_cache, $cache_key) {
            my $past := self.get_slot_past_for_object(%!const_cache{$cache_key});
            $past<has_compile_time_value> := 1;
            $past<compile_time_value> := %!const_cache{$cache_key};
            return $past;
        }
        
        # Find type object for the box typed we'll create.
        # On deserialization, we'll need to look it up too.
        my $type_obj := self.find_symbol([$type]);
        my $type_obj_lookup := self.get_object_sc_ref_past($type_obj);
        
        # Go by the primitive type we're boxing. Need to create
        # the boxed value and also code to produce it.
        my $constant;
        my $des;
        if $primitive eq 'int' {
            $constant := pir::repr_box_int__PiP(@value[0], $type_obj);
            $des := PAST::Op.new( :pirop('repr_box_int PiP'), @value[0], $type_obj_lookup );
        }
        elsif $primitive eq 'str' {
            $constant := pir::repr_box_str__PsP(@value[0], $type_obj);
            $des := PAST::Op.new( :pirop('repr_box_str PsP'), @value[0], $type_obj_lookup );
        }
        elsif $primitive eq 'num' {
            $constant := pir::repr_box_num__PnP(@value[0], $type_obj);
            $des := PAST::Op.new( :pirop('repr_box_num PnP'), @value[0], $type_obj_lookup );
        }
        elsif $primitive eq 'type_new' {
            $constant := $type_obj.new(|@value);
            $des := PAST::Op.new(
                :pasttype('callmethod'), :name('new'),
                $type_obj_lookup
            );
            nqp::push($des, self.get_object_sc_ref_past(nqp::shift(@value)))
                while @value;
        }
        else {
            pir::die("Don't know how to build a $primitive constant");
        }
        
        # Add to SC, finish up deserialization code.
        my $slot := self.add_object($constant);
        self.add_event(:deserialize_past(
            self.set_slot_past($slot, self.set_cur_sc($des))
        ));
        
        # Build PAST for getting the boxed constant from the constants
        # table, but also annotate it with the constant itself in case
        # we need it. Add to cache.
        my $past := self.get_slot_past_for_object($constant);
        $past<has_compile_time_value> := 1;
        $past<compile_time_value> := $constant;
        %!const_cache{$cache_key} := $constant;
        return $past;
    }
    
    # Adds a numeric constant value (int or num) to the constants table.
    # Returns PAST to do  the lookup of the constant.
    method add_numeric_constant($type, $value) {
        my $const := self.add_constant($type, nqp::lc($type), $value);
        my $past  := PAST::Want.new($const, 'IiNn', $value);
        $past<has_compile_time_value> := 1;
        $past<compile_time_value>     := $const<compile_time_value>;
        $past;
    }

    # Creates a meta-object for a package, adds it to the root objects and
    # stores an event for the action. Returns the created object.
    method pkg_create_mo($how, :$name, :$repr, *%extra) {
        # Create the meta-object and add to root objects.
        my %args;
        if pir::defined($name) { %args<name> := ~$name; }
        if pir::defined($repr) { %args<repr> := ~$repr; }
        if pir::exists(%extra, 'base_type') {
            %args<base_type> := %extra<base_type>;
        }
        my $mo := $how.new_type(|%args);
        my $slot := self.add_object($mo);
        
        # Add an event. There's no fixup to do, just a type object to create
        # on deserialization.
        my $setup_call := PAST::Op.new(
            :pasttype('callmethod'), :name('new_type'),
            self.get_object_sc_ref_past($how)
        );
        if pir::defined($name) {
            $setup_call.push(PAST::Val.new( :value(~$name), :named('name') ));
        }
        if pir::defined($repr) {
            $setup_call.push(PAST::Val.new( :value(~$repr), :named('repr') ));
        }
        if pir::exists(%extra, 'base_type') {
            $setup_call.push(my $ref := self.get_object_sc_ref_past(%extra<base_type>));
            $ref.named('base_type');
        }
        self.add_event(:deserialize_past(
            self.set_slot_past($slot, self.set_cur_sc($setup_call))));
        
        # Result is just the object.
        return $mo;
    }
    
    # Constructs a meta-attribute and adds it to a meta-object. Expects to
    # be passed the meta-attribute type object, a set of literal named
    # arguments to pass and a set of name to object mappings to pass also
    # as named arguments, but where these passed objects also live in a
    # serialization context. The type would be passed in this way.
    method pkg_add_attribute($obj, $meta_attr, %lit_args, %obj_args,
            $cont_type, $descriptor, *@default_value) {
        # Build container, and create container PAST for deserialize.
        my $cont_past := self.build_container_past($cont_type, $descriptor, |@default_value);
        my $cont_type_obj := self.find_symbol([$cont_type]);
        my $cont := pir::repr_instance_of__PP($cont_type_obj);
        pir::setattribute__vPPsP($cont, $cont_type_obj, '$!descriptor', $descriptor);
        if +@default_value {
            pir::setattribute__vPPsP($cont, $cont_type_obj, '$!value', @default_value[0]);
        }
        
        # Create meta-attribute instance and add right away. Also add
        # it to the SC.
        my $attr := $meta_attr.new(:auto_viv_container($cont), |%lit_args, |%obj_args);
        $obj.HOW.add_attribute($obj, $attr);
        my $slot := self.add_object($attr);
        
        # Emit code to create attribute deserializing.
        $cont_past.named('auto_viv_container');
        my $create_call := PAST::Op.new(
            :pasttype('callmethod'), :name('new'),
            self.get_object_sc_ref_past($meta_attr),
            $cont_past
        );
        for %lit_args {
            $create_call.push(PAST::Val.new( :value($_.value), :named($_.key) ));
        }
        for %obj_args {
            my $lookup := self.get_object_sc_ref_past($_.value);
            $lookup.named($_.key);
            $create_call.push($lookup);
        }
        self.add_event(:deserialize_past(
            self.set_slot_past($slot, self.set_cur_sc($create_call))));

        # Emit code to add attribute when deserializing.
        my $obj_slot_past := self.get_object_sc_ref_past($obj);
        self.add_event(:deserialize_past(PAST::Op.new(
            :pasttype('callmethod'), :name('add_attribute'),
            PAST::Op.new( :pirop('get_how PP'), $obj_slot_past ),
            $obj_slot_past,
            self.get_object_sc_ref_past($attr)
        )));
        
        # Return attribute that was built.
        $attr
    }
    
    # Adds a method to the meta-object, and stores an event for the action.
    method pkg_add_method($obj, $meta_method_name, $name, $code_object) {
        # Add it to the compile time meta-object.
        $obj.HOW."$meta_method_name"($obj, $name, $code_object);
        
        # Add call to add the method at deserialization time.
        my $slot_past := self.get_object_sc_ref_past($obj);
        self.add_event(
            :deserialize_past(PAST::Op.new(
                :pasttype('callmethod'), :name($meta_method_name),
                PAST::Op.new( :pirop('get_how PP'), $slot_past ),
                $slot_past,
                $name,
                self.get_object_sc_ref_past($code_object)
            )));
    }
    
    # Handles setting the body block code for a role.
    method pkg_set_role_body_block($obj, $code_object, $past) {
        # Add it to the compile time meta-object.
        $obj.HOW.set_body_block($obj, $code_object);
        
        # Add call to do it at deserialization time.
        my $slot_past := self.get_object_sc_ref_past($obj);
        self.add_event(:deserialize_past(PAST::Op.new(
            :pasttype('callmethod'), :name('set_body_block'),
            PAST::Op.new( :pirop('get_how PP'), $slot_past ),
            $slot_past,
            self.get_object_sc_ref_past($code_object)
        )));
        
        # Compile it immediately (we always compile role bodies as
        # early as possible, but then assume they don't need to be
        # re-compiled and re-fixed up at startup).
        self.compile_in_context($past, self.find_symbol(['Code']));
    }
    
    # Composes the package, and stores an event for this action.
    method pkg_compose($obj) {
        # Compose.
        $obj.HOW.compose($obj);
        
        # Emit code to do the composition when deserializing.
        my $slot_past := self.get_object_sc_ref_past($obj);
        self.add_event(:deserialize_past(PAST::Op.new(
            :pasttype('callmethod'), :name('compose'),
            PAST::Op.new( :pirop('get_how PP'), $slot_past ),
            $slot_past
        )));
    }
    
    # Builds a curried role with the specified arguments.
    method curry_role($curryhow, $role, $arglist, $/) {
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
        
        # Make the curry right away and add it to the SC.
        my $curried := $curryhow.new_type($role, |@pos_args, |%named_args);
        my $slot := self.add_object($curried);
        
        # Serialize call.
        my $setup_call := PAST::Op.new(
            :pasttype('callmethod'), :name('new_type'),
            self.get_object_sc_ref_past($curryhow),
            self.get_object_sc_ref_past($role)
        );
        for @pos_args {
            $setup_call.push(self.get_object_sc_ref_past($_));
        }
        for %named_args {
            $setup_call.push(my $arg := self.get_object_sc_ref_past($_.value));
            $arg.named($_.key);
        }
        self.add_event(:deserialize_past(
            self.set_slot_past($slot, self.set_cur_sc($setup_call))));
        
        return $curried;
    }
    
    # Creates a subset type meta-object/type object pair.
    method create_subset($how, $refinee, $refinement, :$name) {
        # Create the meta-object and add to root objects.
        my %args := hash(:refinee($refinee), :refinement($refinement));
        if pir::defined($name) { %args<name> := $name; }
        my $mo := $how.new_type(|%args);
        my $slot := self.add_object($mo);
        
        # Add an event. There's no fixup to do, just a type object to create
        # on deserialization.
        my $setup_call := PAST::Op.new(
            :pasttype('callmethod'), :name('new_type'),
            self.get_object_sc_ref_past($how),
            self.get_object_sc_ref_past($refinement),
            self.get_object_sc_ref_past($refinee)
        );
        $setup_call[1].named('refinement');
        $setup_call[2].named('refinee');
        if pir::defined($name) {
            $setup_call.push(PAST::Val.new( :value($name), :named('name') ));
        }
        self.add_event(:deserialize_past(
            self.set_slot_past($slot, self.set_cur_sc($setup_call))));
        
        # Result is just the object.
        return $mo;
    }
    
    # Adds a value to an enumeration.
    method create_enum_value($enum_type_obj, $key, $value) {
        # Create directly.
        my $val       := pir::repr_box_int__PiP($value, $enum_type_obj);
        my $base_type := ($enum_type_obj.HOW.parents($enum_type_obj, :local(1)))[0];
        pir::setattribute__vPPsP($val, $enum_type_obj, '$!key', $key);
        pir::setattribute__vPPsP($val, $enum_type_obj, '$!value',
            pir::repr_box_int__PiP($value, $base_type));
        my $slot := self.add_object($val);
        
        # Add to meta-object.
        $enum_type_obj.HOW.add_enum_value($enum_type_obj, $val);
        
        # Generate deserialization code.
        my $enum_type_obj_ref := self.get_object_sc_ref_past($enum_type_obj);
        my $base_type_ref := self.get_object_sc_ref_past($base_type);
        my $key_ref := self.get_object_sc_ref_past($key);
        my $val_ref := self.get_object_sc_ref_past($val);
        self.add_event(:deserialize_past(PAST::Stmts.new(            
            self.set_slot_past($slot, self.set_cur_sc(
                PAST::Op.new( :pirop('repr_box_int PiP'), $value, $enum_type_obj_ref )
            )),
            PAST::Op.new(
                :pirop('setattribute vPPsP'),
                $val_ref, $enum_type_obj_ref, '$!key', $key_ref
            ),
            PAST::Op.new(
                :pirop('setattribute vPPsP'),
                $val_ref, $enum_type_obj_ref, '$!value',
                PAST::Op.new( :pirop('repr_box_int PiP'), $value, $base_type_ref )
            ),
            PAST::Op.new(
                :pasttype('callmethod'), :name('add_enum_value'),
                PAST::Op.new( :pirop('get_how PP'), $enum_type_obj_ref ),
                $enum_type_obj_ref, $val
            ))));
            
        # Result is the value.
        $val
    }
    
    # Applies a trait.
    method apply_trait($trait_sub_name, *@pos_args, *%named_args) {
        # Locate the trait sub to apply.
        my $trait_sub := $*ST.find_symbol([$trait_sub_name]);
        
        # Call it right away.
        $trait_sub(|@pos_args, |%named_args);
        
        # Serialize call to it.
        my $call_past := PAST::Op.new(
            :pasttype('call'),
            self.get_object_sc_ref_past($trait_sub));
        for @pos_args {
            $call_past.push(self.get_object_sc_ref_past($_));
        }
        for %named_args {
            my $lookup := self.get_object_sc_ref_past($_.value);
            $lookup.named($_.key);
            $call_past.push($lookup);
        }
        self.add_event(:deserialize_past($call_past));
    }
    
    # Some things get cloned many times with a lexical scope that
    # we never entered. This makes sure we capture them as needed.
    # Yes, it's evil...find a vodka before reading, kthx.
    method create_lexical_capture_fixup() {
        # Create an RPA and put it in the SC. Also code to build
        # one and install it.
        my $fixup_list := pir::new__Ps('ResizablePMCArray');
        my $slot := self.add_code($fixup_list);
        self.add_event(:deserialize_past(
            self.set_slot_past($slot, PAST::Op.new( :pasttype('list') ))));

        # Set up capturing code.
        my $capturer := self.cur_lexpad();
        $capturer[0].push(PAST::Op.new(
            :pirop('capture_all_outers vP'),
            self.get_slot_past_for_object($fixup_list)));
        
        # Return code that adds current context to re-capture
        # list.
        return PAST::Op.new(
            :pirop('push vPP'),
            self.get_slot_past_for_object($fixup_list),
            PAST::Op.new(
                :pirop('set PQPS'),
                PAST::Op.new( :pirop('getinterp P') ),
                'context'));
    }
    
    # Handles addition of a phaser.
    method add_phaser($/, $block, $phaser) {
        if $phaser eq 'BEGIN' {
            # BEGIN phasers get run immediately.
            $block();
        }
        elsif $phaser eq 'CHECK' {
            @*CHECK_PHASERS.unshift($block);
        }
        elsif $phaser eq 'INIT' {
            $*UNIT[0].push(PAST::Op.new(
                :pasttype('call'),
                self.get_object_sc_ref_past($block)
            ));
        }
        elsif $phaser eq 'END' {
            $*UNIT[0].push(PAST::Op.new(
                :pasttype('callmethod'), :name('unshift'),
                PAST::Var.new( :name('@*END_PHASERS'), :scope('contextual') ),
                self.get_object_sc_ref_past($block)
            ));
        }
        else {
            $/.CURSOR.panic("$phaser phaser not yet implemented");
        }
    }
    
    # Checks if a given symbol is declared and also a type object.
    method is_name(@name) {
        my $is_name := 0;
        try {
            # This throws if it's not a known name.
            self.find_symbol(@name);
            $is_name := 1;
        }
        $is_name
    }
    
    # Checks if a symbol has already been declared in the current
    # scope, and thus may not be redeclared.
    method already_declared($scope, $curpackage, $curpad, @name) {
        if $scope eq 'my' {
            my %sym := $curpad.symbol(@name[0]);
            if %sym { %sym<value>;
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
                return self.already_declared($scope, $first_sym, PAST::Block.new(), @restname);
            }
            else {
                return $first_sym.HOW.HOW.name($first_sym.HOW) ne 'Perl6::Metamodel::PackageHOW';
            }
        }
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
                        @name.shift();
                        last;
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
        
        # Handle special names.
        if +@name == 1 && @name[0] eq 'GLOBAL' {
            return PAST::Var.new( :name('GLOBAL'), :namespace([]), :scope('package') );
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
        my $des := PAST::Stmts.new();
        my $fix := PAST::Stmts.new();
        for self.event_stream() {
            $des.push(PAST::Stmt.new($_.deserialize_past())) if pir::defined($_.deserialize_past());
            $fix.push(PAST::Stmt.new($_.fixup_past())) if pir::defined($_.fixup_past());
        }
        make PAST::Op.new(
            :pasttype('if'),
            PAST::Op.new(
                :pirop('isnull IP'),
                PAST::Op.new( :pirop('nqp_get_sc Ps'), self.handle() )
            ),
            PAST::Stmts.new(
                PAST::Op.new( :pirop('nqp_dynop_setup v') ),
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
                $des
            ),
            $fix
        );
    }
}
