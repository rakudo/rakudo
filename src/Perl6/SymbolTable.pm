use NQPHLL;
use Perl6::ModuleLoader;

# Binder constants.
# XXX Want constant syntax in NQP really.
my $SIG_ELEM_BIND_CAPTURE       := 1;
my $SIG_ELEM_BIND_PRIVATE_ATTR  := 2;
my $SIG_ELEM_BIND_PUBLIC_ATTR   := 4;
my $SIG_ELEM_SLURPY_POS         := 8;
my $SIG_ELEM_SLURPY_NAMED       := 16;
my $SIG_ELEM_SLURPY_LOL         := 32;
my $SIG_ELEM_INVOCANT           := 64;
my $SIG_ELEM_MULTI_INVOCANT     := 128;
my $SIG_ELEM_IS_RW              := 256;
my $SIG_ELEM_IS_COPY            := 512;
my $SIG_ELEM_IS_PARCEL          := 1024;
my $SIG_ELEM_IS_OPTIONAL        := 2048;
my $SIG_ELEM_ARRAY_SIGIL        := 4096;
my $SIG_ELEM_HASH_SIGIL         := 8192;
my $SIG_ELEM_IS_CAPTURE         := 32768;
my $SIG_ELEM_UNDEFINED_ONLY     := 65536;
my $SIG_ELEM_DEFINED_ONLY       := 131072;

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
                0
            ));
        }
        self.add_event(:deserialize_past($fixups), :fixup_past($fixups));
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
            0
        );
        self.add_event(:deserialize_past($fixup), :fixup_past($fixup));
    }
    
    # Installs a lexical symbol. Takes a PAST::Block object, name and
    # the type of container to install.
    method install_lexical_container($block, $name, $type_name, $descriptor, *@default_value) {
        # Add to block. Note that it doesn't really have a compile time
        # value.
        $block.symbol($name, :scope('lexical_6model'), :descriptor($descriptor));
        $block[0].push(PAST::Var.new( :scope('lexical_6model'), :name($name),
            :isdecl(1), :type($descriptor.of) ));
            
        # If it's a native type, we're done - no container
        # as we inline natives straight into registers.
        if pir::repr_get_primitive_type_spec__IP($descriptor.of) {
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
            ~$name, $cont_code, 1
        );
        self.add_event(:deserialize_past($fixup), :fixup_past($fixup));
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
        # XXX Many more to do.
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
        if %param_info<pos_lol> {
            $flags := $flags + $SIG_ELEM_SLURPY_LOL;
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
        my $code      := pir::repr_instance_of__PP($type_obj);
        my $slot      := self.add_object($code);
        
        # Stash it under the PAST block sub ID.
        %!sub_id_to_code_object{$code_past.subid()} := $code;
        
        # For now, install stub that will dynamically compile the code if
        # we ever try to run it during compilation.
        my $precomp;
        my $stub := sub (*@pos, *%named) {
            unless $precomp {
                # Compile.
                $precomp := self.compile_in_context($code_past);
                
                # Fix up Code object associations (including nested blocks).
                my $num_subs := nqp::elems($precomp);
                my $i := 0;
                while $i < $num_subs {
                    my $subid := $precomp[$i].get_subid();
                    if pir::exists(%!sub_id_to_code_object, $subid) {
                        pir::perl6_associate_sub_code_object__vPP($precomp[$i],
                            %!sub_id_to_code_object{$subid});
                    }
                    $i := $i + 1;
                }
            }
            $precomp(|@pos, |%named);
        };
        pir::set__vPS($stub, $code_past.name);
        my $code_type := self.find_symbol(['Code']);
        pir::setattribute__vPPsP($code, $code_type, '$!do', $stub);
        
        # Fixup will install the real thing.
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

        self.add_event(:deserialize_past($des), :fixup_past($fixups));
        $code;
    }
    
    # Adds a multi candidate to a proto/dispatch.
    method add_dispatchee_to_proto($proto, $candidate) {
        # Add it to the list.
        my $code_type := self.find_symbol(['Code']);
        pir::getattribute__PPPs($proto, $code_type, '$!dispatchees').push($candidate);
        
        # Deserializatin code to add it.
        self.add_event(:deserialize_past(PAST::Op.new(
            :pirop('push vPP'),
            PAST::Var.new(
                :scope('attribute_6model'), :name('$!dispatchees'),
                self.get_object_sc_ref_past($proto),
                self.get_object_sc_ref_past($code_type)
            ),
            self.get_object_sc_ref_past($candidate)
        )));
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
    method compile_in_context($past) {
        # Ensure that we have the appropriate op libs loaded and correct
        # HLL.
        $past.loadlibs('perl6_group', 'perl6_ops');
        $past.hll('perl6');
        
        # Create outer lexical contexts with all symbols visible.
        # XXX TODO
        
        # Compile and return.
        my $pir := PAST::Compiler.compile($past, :target('pir'));
        PAST::Compiler.compile($past)
    }
    
    # Adds a constant value to the constants table. Returns PAST to do
    # the lookup of the constant.
    method add_constant($type, $primitive, $value) {
        # If we already built this, find it in the cache and
        # just return that.
        my $cache_key := "$type,$primitive,$value";
        if pir::exists(%!const_cache, $cache_key) {
            return %!const_cache{$cache_key};
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
            $constant := pir::repr_box_int__PiP($value, $type_obj);
            $des := PAST::Op.new( :pirop('repr_box_int PiP'), $value, $type_obj_lookup );
        }
        elsif $primitive eq 'str' {
            $constant := pir::repr_box_str__PsP($value, $type_obj);
            $des := PAST::Op.new( :pirop('repr_box_str PsP'), $value, $type_obj_lookup );
        }
        elsif $primitive eq 'num' {
            $constant := pir::repr_box_num__PnP($value, $type_obj);
            $des := PAST::Op.new( :pirop('repr_box_num PnP'), $value, $type_obj_lookup );
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
        %!const_cache{$cache_key} := $past;
        return $past;
    }
    
    # Creates a meta-object for a package, adds it to the root objects and
    # stores an event for the action. Returns the created object.
    method pkg_create_mo($how, :$name, :$repr) {
        # Create the meta-object and add to root objects.
        my %args;
        if pir::defined($name) { %args<name> := $name; }
        if pir::defined($repr) { %args<repr> := $repr; }
        my $mo := $how.new_type(|%args);
        my $slot := self.add_object($mo);
        
        # Add an event. There's no fixup to do, just a type object to create
        # on deserialization.
        my $setup_call := PAST::Op.new(
            :pasttype('callmethod'), :name('new_type'),
            self.get_object_sc_ref_past($how)
        );
        if pir::defined($name) {
            $setup_call.push(PAST::Val.new( :value($name), :named('name') ));
        }
        if pir::defined($repr) {
            $setup_call.push(PAST::Val.new( :value($repr), :named('repr') ));
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
        
        # Create meta-attribute isntance and add right away. Also add
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
    method pkg_set_role_body_block($obj, $sig, $code_object) {
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
    
    # Handles addition of a phaser.
    method add_phaser($/, $block, $phaser) {
        if $phaser eq 'BEGIN' {
            # BEGIN phasers get run immediately.
            $block();
        }
        elsif $phaser eq 'CHECK' {
            @*CHECK_PHASERS.push($block);
        }
        elsif $phaser eq 'INIT' {
            $*UNIT[0].push(PAST::Op.new(
                :pasttype('call'),
                self.get_object_sc_ref_past($block)
            ));
        }
        elsif $phaser eq 'END' {
            $*UNIT[0].push(PAST::Op.new(
                :pasttype('callmethod'), :name('push'),
                PAST::Var.new( :name('@*END_PHASERS'), :scope('contextual') ),
                self.get_object_sc_ref_past($block)
            ));
        }
        else {
            $/.CURSOR.panic("$phaser phaser not yet implemented");
        }
    }
    
    # Checks if a given symbol is declared and also a type object.
    method is_type(@name) {
        my $is_type := 0;
        try {
            $is_type := !pir::repr_defined__IP(self.find_symbol(@name))
        }
        $is_type
    }
    
    # Finds a symbol that has a known value at compile time from the
    # perspective of the current scope. Checks for lexicals, then if
    # that fails tries package lookup.
    method find_symbol(@name) {
        # Make sure it's not an empty name.
        unless +@name { pir::die("Cannot look up empty name"); }
        
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
    method symbol_lookup(@name, $/, :$package_only = 0) {
        # Catch empty names and die helpfully.
        if +@name == 0 { $/.CURSOR.panic("Cannot compile empty name"); }
        my $orig_name := pir::join('::', @name);
        
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
        
        # The final lookup will always be just a keyed access to a
        # symbol table.
        my $final_name := @name.pop();
        my $lookup := PAST::Var.new( :scope('keyed'), ~$final_name);
        
        # If there's no explicit qualification, then look it up in the
        # current package, and fall back to looking in GLOBAL.
        if +@name == 0 {
            $lookup.unshift(PAST::Op.new(
                :pirop('get_who PP'),
                PAST::Var.new( :name('$?PACKAGE'), :scope('lexical_6model') )
            ));
            $lookup.viviself(PAST::Var.new(
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
        unless $lookup.viviself {
            $lookup.viviself(self.lookup_failure($orig_name));
        }
        
        return $lookup;
    }
    
    method lookup_failure($orig_name) {
        my $msg := "Could not find symbol '$orig_name'";
        return PAST::Op.new(
            :pasttype('call'), :name('&fail'),
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
                return %sym<scope> eq 'lexical_6model';
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
