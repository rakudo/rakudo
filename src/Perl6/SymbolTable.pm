use NQPHLL;
use Perl6::ModuleLoader;

# This builds upon the SerializationContextBuilder to add the specifics
# needed by Rakudo Perl 6.
class Perl6::SymbolTable is HLL::Compiler::SerializationContextBuilder {
    # The stack of lexical pads, actually as PAST::Block objects. The
    # outermost frame is at the bottom, the latest frame is on top.
    has @!BLOCKS;
    
    # Array of stubs to check and the end of compilation.
    has @!stub_check;
    
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
                    :pirop('load_bytecode vs'), 'Perl6/ModuleLoader.pbc'
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
                :pirop('load_bytecode vs'), 'Perl6/ModuleLoader.pbc'
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
            say("# Importing " ~ $_.key);
            $target.symbol($_.key, :scope('lexical'), :value($_.value));
            $target[0].push(PAST::Var.new( :scope('lexical'), :name($_.key), :isdecl(1) ));
            
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
                )
            ));
        }
        self.add_event(:deserialize_past($fixups), :fixup_past($fixups));
    }
    
    # Installs a lexical symbol. Takes a PAST::Block object, name and
    # the object to install. Does an immediate installation in the
    # compile-time block symbol table, and ensures that the installation
    # gets fixed up at runtime too.
    method install_lexical_symbol($block, $name, $obj) {
        # Install the object directly as a block symbol.
        $block.symbol($name, :scope('lexical'), :value($obj));
        $block[0].push(PAST::Var.new( :scope('lexical'), :name($name), :isdecl(1) ));
        
        # Fixup and deserialization task is the same.
        my $fixup := PAST::Op.new(
            :pasttype('callmethod'), :name('set_static_lexpad_value'),
            PAST::Op.new(
                :pasttype('callmethod'), :name('get_lexinfo'),
                PAST::Val.new( :value($block) )
            ),
            ~$name, self.get_object_sc_ref_past($obj)
        );
        self.add_event(:deserialize_past($fixup), :fixup_past($fixup));
    }
    
    # Creates a parameter object.
    method create_parameter(%param_info) {
        # Create parameter object now.
        say("# Creating parameter object for " ~ %param_info<variable_name>);
        my $par_type  := self.find_symbol(['Parameter']);
        my $parameter := pir::repr_instance_of__PP($par_type);
        my $slot      := self.add_object($parameter);
        say("# ... slot $slot");
        
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
        
        # XXX Set up other various attribute values.
        
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
    method create_code_object($code_past, $type, $signature) {
        my $fixups := PAST::Stmts.new();
        my $des    := PAST::Stmts.new();
        
        # Create code object now.
        my $type_obj  := self.find_symbol([$type]);
        my $code      := pir::repr_instance_of__PP($type_obj);
        my $slot      := self.add_object($code);
        
        # For now, install stub that will dynamically compile the code if we ever try
        # to run it during compilation.
        my $precomp;
        my $stub := sub (*@pos, *%named) {
            unless $precomp {
                $precomp := self.compile_in_context($code_past);
            }
            $precomp(|@pos, |%named);
        };
        my $code_type := self.find_symbol(['Code']);
        pir::setattribute__vPPsP($code, $code_type, '$!do', $stub);
        
        # Fixup will install the real thing.
        $fixups.push(self.set_attribute($code, $code_type, '$!do', PAST::Val.new( :value($code_past) )));
        
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
        
        self.add_event(:deserialize_past($des), :fixup_past($fixups));
        $code;
    }
    
    # Helper to make PAST for setting an attribute to a value. Value should
    # be a PAST tree.
    method set_attribute($obj, $class, $name, $value_past) {
        PAST::Op.new(
            :pasttype('bind'),
            PAST::Var.new(
                :name($name), :scope('attribute_6model'),
                self.get_object_sc_ref_past($obj), 
                self.get_object_sc_ref_past($class)
            ),
            $value_past
        )
    }
    
    # Helper to make PAST for setting a typed attribute to a value. Value should
    # be a PAST tree.
    method set_attribute_typed($obj, $class, $name, $value_past, $type) {
        PAST::Op.new(
            :pasttype('bind'),
            PAST::Var.new(
                :name($name), :scope('attribute_6model'), :type($type),
                self.get_object_sc_ref_past($obj), 
                self.get_object_sc_ref_past($class)
            ),
            $value_past
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
        say($pir);
        PAST::Compiler.compile($past)
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
    method symbol_lookup(@name, $/) {
        # Catch empty names and die helpfully.
        if +@name == 0 { $/.CURSOR.panic("Cannot compile empty name"); }
        
        # If it's a single item, then go hunting for it through the
        # block stack.
        if +@name == 1 {
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
                PAST::Var.new( :name('$?PACKAGE'), :scope('lexical') )
            ));
            $lookup.viviself(PAST::Var.new(
                :scope('keyed'),
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
                PAST::Var.new( :name(@name.shift()), :scope('lexical') ) !!
                PAST::Var.new( :name('GLOBAL'), :namespace([]), :scope('package') );
            if @name[0] eq 'GLOBAL' {
                @name.shift();
            }
            for @name {
                $path := PAST::Op.new(
                    :pirop('nqp_get_package_through_who PPs'),
                    $path, ~$_);
            }
            $lookup.unshift(PAST::Op.new(:pirop('get_who PP'), $path));
        }
        
        return $lookup;
    }

    # Checks if the given name is known anywhere in the lexpad
    # and with lexical scope.
    method is_lexical($name) {
        my $i := +@!BLOCKS;
        while $i > 0 {
            $i := $i - 1;
            my %sym := @!BLOCKS[$i].symbol($name);
            if +%sym {
                return %sym<scope> eq 'lexical';
            }
        }
        0;
    }
    
    # XXX TODO
    method is_attr_alias($name) {
        0
    }
    
    # Generates a series of PAST operations that will build this context if
    # it doesn't exist, and fix it up if it already does.
    method to_past() {
        my $des := PAST::Stmts.new();
        my $fix := PAST::Stmts.new();
        for self.event_stream() {
            $des.push($_.deserialize_past()) if pir::defined($_.deserialize_past());
            $fix.push($_.fixup_past()) if pir::defined($_.fixup_past());
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
                    :pasttype('bind'),
                    PAST::Var.new( :name('cur_sc'), :scope('register'), :isdecl(1) ),
                    PAST::Op.new( :pirop('nqp_create_sc Ps'), self.handle() )
                ),
                $des
            ),
            $fix
        );
    }
}
