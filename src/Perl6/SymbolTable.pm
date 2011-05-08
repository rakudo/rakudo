use NQPHLL;
use Perl6::ModuleLoader;

# This builds upon the SerializationContextBuilder to add the specifics
# needed by Rakudo Perl 6.
class Perl6::SymbolTable is HLL::Compiler::SerializationContextBuilder {
    # The stack of lexical pads, actually as PAST::Block objects. The
    # outermost frame is at the bottom, the latest frame is on top.
    has @!BLOCKS;
    
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
    
    # Loads a setting.
    method load_setting($setting_name) {
        # Do nothing for the NULL setting.
        if $setting_name ne 'NULL' {    
            # Load it immediately, so the compile time info is available.
            # Once it's loaded, set it as the outer context of the code
            # being compiled.
            my $setting := %*COMPILING<%?OPTIONS><outer_ctx>
                        := Perl6::ModuleLoader.load_setting($setting_name);
            
            # Do load for pre-compiled situation.
            self.add_event(:deserialize_past(PAST::Stmts.new(
                PAST::Op.new(
                    :pirop('load_bytecode vs'), 'Perl6/ModuleLoader.pbc'
                ),
                PAST::Op.new(
                    :pasttype('callmethod'), :name('set_outer_ctx'),
                       PAST::Var.new( :name('block'), :scope('register') ),
                       PAST::Op.new(
                           :pasttype('callmethod'), :name('load_setting'),
                           PAST::Var.new( :name('ModuleLoader'), :namespace([]), :scope('package') ),
                           $setting_name
                       )
                )
            )));
            
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
    
    # Installs a lexical symbol. Takes a PAST::Block object, name and
    # the object to install. Does an immediate installation in the
    # compile-time block symbol table, and ensures that the installation
    # gets fixed up at runtime too.
    method install_lexical_symbol($block, $name, $obj) {
        # Install the object directly as a block symbol.
        $block.symbol($name, :scope('lexical'), :value($obj));
        $block[0].push(PAST::Var.new( :scope('lexical'), :name($name), :isdecl(1) ));
        
        # Fixup and deserialization task is the same.
        my $fixup := PAST::Stmts.new(
            PAST::Op.new(
                :pasttype('callmethod'), :name('set_static_lexpad_value'),
                PAST::Op.new(
                    :pasttype('callmethod'), :name('get_lexinfo'),
                    PAST::Val.new( :value($block) )
                ),
                ~$name, self.get_slot_past_for_object($obj)
            )
        );
        self.add_event(:deserialize_past($fixup), :fixup_past($fixup));
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
        my $slot_past := self.get_slot_past_for_object($obj);
        self.add_event(:deserialize_past(PAST::Op.new(
            :pasttype('callmethod'), :name('compose'),
            PAST::Op.new( :pirop('get_how PP'), $slot_past ),
            $slot_past
        )));
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
