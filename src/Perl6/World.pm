use NQPHLL;
use QAST;
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

sub p6ize_recursive($x) {
    if nqp::islist($x) {
        my @copy := [];
        for $x {
            nqp::push(@copy, p6ize_recursive($_));
        }
        return pir::perl6ize_type__PP(@copy);
    }
    elsif nqp::ishash($x) {
        my %copy := nqp::hash();
        for $x {
            %copy{$_.key} := p6ize_recursive($_.value);
        }
        return pir::perl6ize_type__PP(%copy).item;
    }
    pir::perl6ize_type__PP($x);
}

# this levenshtein implementation is used to suggest good alternatives
# when deriving from an unknown/typo'd class.
sub levenshtein($a, $b) {
    my %memo;
    my $alen := nqp::chars($a);
    my $blen := nqp::chars($b);

    return 0 if $alen eq 0 || $blen eq 0;

    # the longer of the two strings is an upper bound.
    my $bound := $alen < $blen ?? $blen !! $alen;

    sub levenshtein_impl($apos, $bpos, $estimate) {
        my $key := nqp::join(":", ($apos, $bpos));

        return %memo{$key} if nqp::existskey(%memo, $key);

        # if we're already worse off than the current best solution,
        # just give up with $BIGNUM
        if $estimate > $bound {
            return 1000 + $bound * $bound;
        }

        # if either cursor reached the end of the respective string,
        # the result is the remaining length of the other string.
        sub check($pos1, $len1, $pos2, $len2) {
            if $pos2 == $len2 {
                my $result := $estimate + $len1 - $pos1;
                $bound := $result if $result < $bound;
                return $result - $estimate;
            }
            return -1;
        }

        my $check := check($apos, $alen, $bpos, $blen);
        return $check unless $check eq -1;
        $check := check($bpos, $blen, $apos, $alen);
        return $check unless $check eq -1;

        my $cost := 0;
        $cost := 1 unless (nqp::substr($a, $apos, 1) eq nqp::substr($b, $bpos, 1)); # can we keep the current letter?

        my $ca := levenshtein_impl($apos+1, $bpos,   $estimate+1) + 1; # what if we remove the current letter from A?
        my $cb := levenshtein_impl($apos,   $bpos+1, $estimate+1) + 1; # what if we add the current letter from B?
        my $cc := levenshtein_impl($apos+1, $bpos+1, $estimate+$cost) + $cost; # what if we change/keep the current letter?

        # the result is the shortest of the three sub-tasks
        my $distance;
        $distance := $ca if $ca <= $cb && $ca <= $cc;
        $distance := $cb if $cb <= $ca && $cb <= $cc;
        $distance := $cc if $cc <= $ca && $cc <= $cb;

        %memo{$key} := $distance;
        return $distance;
    }

    my $result := levenshtein_impl(0, 0, 0);
    return $result;
}

# This builds upon the HLL::World to add the specifics needed by Rakudo Perl 6.
class Perl6::World is HLL::World {
    # The stack of lexical pads, actually as QAST::Block objects. The
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
    
    method BUILD(*%adv) {
        @!BLOCKS := [];
        @!CODES := [];
        @!stub_check := [];
        @!CHECKs := [];
        %!sub_id_to_code_object := {};
        %!sub_id_to_static_lexpad := {};
        %!sub_id_to_sc_idx := {};
        %!const_cache := {};
    }
    
    # Creates a new lexical scope and puts it on top of the stack.
    method push_lexpad($/) {
        # Create pad, link to outer and add to stack.
        my $pad := QAST::Block.new( QAST::Stmts.new(), :node($/) );
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
    
    # Gets (and creates if needed) the static lexpad object for a QAST block.
    method get_static_lexpad($pad) {
        my $pad_id := $pad.cuid();
        if nqp::existskey(%!sub_id_to_static_lexpad, $pad_id) {
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
        my $fixup := QAST::Op.new(
            :op('callmethod'), :name('set_static_lexpad'),
            QAST::VM.new(
                pir => '    .const "LexInfo" %r = "' ~ $pad.cuid() ~ '"'
            ),
            QAST::WVal.new( :value($slp) ));
        self.add_fixup_task(:deserialize_past($fixup), :fixup_past($fixup));
        
        # Stash it under the QAST block unique ID.
        %!sub_id_to_static_lexpad{$pad.cuid()} := $slp;
        
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
            my $fixup := QAST::Stmt.new(
                self.perl6_module_loader_code(),
                QAST::Op.new(
                    :op('callmethod'), :name('set_outer_ctx'),
                    QAST::BVal.new( :value($*UNIT_OUTER) ),
                    QAST::Op.new(
                        :op('callmethod'), :name('load_setting'),
                        QAST::VM.new(
                            pirop => 'get_hll_global Ps',
                            QAST::SVal.new( :value('ModuleLoader') )
                        ),
                        QAST::SVal.new( :value($setting_name) )
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
        my $line := HLL::Compiler.lineof($/.orig, $/.from);
        my $module := Perl6::ModuleLoader.load_module($module_name, $cur_GLOBALish, :$line);
        
        # During deserialization, ensure that we get this module loaded.
        if self.is_precompilation_mode() {
            self.add_load_dependency_task(:deserialize_past(QAST::Stmts.new(
                self.perl6_module_loader_code(),
                QAST::Op.new(
                   :op('callmethod'), :name('load_module'),
                   QAST::VM.new( pirop => 'get_hll_global Ps',
                        QAST::SVal.new( :value('ModuleLoader') ) ),
                   QAST::SVal.new( :value($module_name) ),
                   QAST::IVal.new(:value($line), :named('line'))
                ))));
        }

        return pir::getattribute__PPs($module, 'lex_pad');
    }
    
    # Uses the NQP module loader to load Perl6::ModuleLoader, which
    # is a normal NQP module.
    method perl6_module_loader_code() {
        QAST::Stmt.new(
            QAST::VM.new(
                pirop => 'load_bytecode vs',
                QAST::SVal.new( :value('ModuleLoader.pbc') )
            ),
            QAST::Op.new(
                :op('callmethod'), :name('load_module'),
                QAST::Op.new(
                    :op('atkey'),
                    QAST::Op.new(
                        :op('atkey'),
                        QAST::VM.new( pirop => 'get_root_namespace P' ),
                        QAST::SVal.new( :value('nqp') )
                    ),
                    QAST::SVal.new( :value('ModuleLoader') )
                ),
                QAST::SVal.new( :value('Perl6::ModuleLoader') )
            ))
    }
    
    # Imports symbols from the specified package into the current lexical scope.
    method import($/, $package, $source_package_name) {
        # We'll do this in two passes, since at the start of CORE.setting we import
        # StaticLexPad, which of course we need to use when importing. Since we still
        # keep the authoritative copy of stuff from the compiler's view in QAST::Block's
        # .symbol(...) hash we get away with this for now.
        my %stash := $package.WHO;
        my $target := self.cur_lexpad();
        
        # First pass: QAST::Block symbol table installation. Also detect any
        # outright conflicts, and handle any situations where we need to merge.
        my %to_install;
        my @clash;
        my @clash_onlystar;
        for %stash {
            if $target.symbol($_.key) -> %sym {
                # There's already a symbol. However, we may be able to merge
                # if both are multis and have onlystar dispatchers.
                my $installed := %sym<value>;
                my $foreign := $_.value;
                if nqp::can($installed, 'is_dispatcher') && $installed.is_dispatcher
                && nqp::can($foreign, 'is_dispatcher') && $foreign.is_dispatcher {
                    # Both dispatchers, but are they onlystar? If so, we can
                    # go ahead and merge them.
                    if $installed.onlystar && $foreign.onlystar {
                        # Replace installed one with a derived one, to avoid any
                        # weird action at a distance.
                        $installed := self.derive_dispatcher($installed);
                        self.install_lexical_symbol($target, $_.key, $installed, :clone(1));
                        
                        # Incorporate dispatchees of foreign proto, avoiding
                        # duplicates.
                        my %seen;
                        for $installed.dispatchees {
                            %seen{$_.static_id} := $_;
                        }
                        for $foreign.dispatchees {
                            unless nqp::existskey(%seen, $_.static_id) {
                                self.add_dispatchee_to_proto($installed, $_);
                            }
                        }
                    }
                    else {
                        nqp::push(@clash_onlystar, $_.key);
                    }
                }
                else {
                    nqp::push(@clash, $_.key);
                }
            }
            else {
                $target.symbol($_.key, :scope('lexical'), :value($_.value));
                $target[0].push(QAST::Var.new( :scope('lexical'), :name($_.key), :decl('var') ));
                %to_install{$_.key} := $_.value;
            }
        }

        if +@clash_onlystar {
            self.throw($/, 'X::Import::OnlystarProto',
                symbols             => @clash_onlystar,
                source-package-name => $source_package_name,
            );
        }

        if +@clash {
            self.throw($/, 'X::Import::Redeclaration',
                symbols             => @clash,
                source-package-name => $source_package_name,
            );
        }
        
        # Second pass: stick everything we still need to install in the
        # actual static lexpad.
        my $slp := self.get_static_lexpad($target);
        for %to_install {
            $slp.add_static_value($_.key, $_.value, 0, 0);
            my $categorical := match($_.key, /^ '&' (\w+) ':<' (.+) '>' $/);
            if $categorical {
                $/.CURSOR.add_categorical(~$categorical[0], ~$categorical[1],
                    ~$categorical[0] ~ ':sym<' ~$categorical[1] ~ '>',
                    $_.key, $_.value);
            }
        }

        1;
    }
    
    # Installs something package-y in the right place, creating the nested
    # pacakges as needed.
    method install_package($/, @name_orig, $scope, $pkgdecl, $package, $outer, $symbol) {
        if $scope eq 'anon' { return 1 }
        my @parts := nqp::clone(@name_orig);
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
            if nqp::existskey($cur_pkg.WHO, $part) {
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
            if nqp::existskey($cur_pkg.WHO, $name) {
                self.steal_WHO($symbol, ($cur_pkg.WHO){$name});
            }
            self.install_package_symbol($cur_pkg, $name, $symbol);
        }
        
        1;
    }
    
    # If we declare class A::B { }, then class A { }, then A.WHO must be the
    # .WHO we already created for the stub package A.
    method steal_WHO($thief, $victim) {
        nqp::setwho($thief, $victim.WHO);
    }
    
    # Installs a lexical symbol. Takes a QAST::Block object, name and
    # the object to install. Does an immediate installation in the
    # compile-time block symbol table, and ensures that the installation
    # gets fixed up at runtime too.
    method install_lexical_symbol($block, $name, $obj, :$clone) {
        # Install the object directly as a block symbol.
        unless $block.symbol($name) {
            $block[0].push(QAST::Var.new( :scope('lexical'), :name($name), :decl('var') ));
        }
        $block.symbol($name, :scope('lexical'), :value($obj));
        
        # Add a clone if needed.
        if $clone {
            $block[0].push(QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name($name), :scope('lexical') ),
                QAST::Op.new(
                    :op('callmethod'), :name('clone'),
                    QAST::Var.new( :name($name), :scope('lexical') )
                )));
        }
        
        # Add to static lexpad.
        my $slp := self.get_static_lexpad($block);
        $slp.add_static_value(~$name, $obj, 0, 0);

        1;
    }
    
    # Installs a lexical symbol. Takes a QAST::Block object, name and
    # the type of container to install.
    method install_lexical_container($block, $name, %cont_info, $descriptor, :$state) {
        # Add to block, if needed. Note that it doesn't really have
        # a compile time value.
        my $var;
        unless $block.symbol($name) {
            $var := QAST::Var.new( :scope('lexical'), :name($name),
                :decl('var'), :returns(%cont_info<bind_constraint>) );
            $block[0].push($var);
        }
        $block.symbol($name, :scope('lexical'), :type(%cont_info<bind_constraint>), :descriptor($descriptor));
            
        # If it's a native type, we're done - no container
        # as we inline natives straight into registers. Do
        # need to take care of initial value though.
        my $prim := pir::repr_get_primitive_type_spec__IP($descriptor.of);
        if $prim {
            if $state { nqp::die("Natively typed state variables not yet implemented") }
            if $var {
                if $prim == 1 {
                    $block[0].push(QAST::Op.new( :op('bind'),
                        QAST::Var.new( :scope('lexical'), :name($name) ),
                        QAST::IVal.new( :value(0) ) ))
                }
                elsif $prim == 2 {
                    $block[0].push(QAST::Op.new( :op('bind'),
                        QAST::Var.new( :scope('lexical'), :name($name) ),
                        QAST::VM.new(
                            :pirop('set__Ns'), QAST::SVal.new( :value('NaN') 
                        ))));
                }
                elsif $prim == 3 {
                    $block[0].push(QAST::Op.new( :op('bind'),
                        QAST::Var.new( :scope('lexical'), :name($name) ),
                        QAST::SVal.new( :value('') ) ))
                }
            }
            return 1;
        }
        
        # Build container.
        my $cont := nqp::create(%cont_info<container_type>);
        nqp::bindattr($cont, %cont_info<container_base>, '$!descriptor', $descriptor);
        if nqp::existskey(%cont_info, 'default_value') {
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
        # Create container and set descriptor.
        my $tmp := QAST::Node.unique('cont');
        my $cont_code := QAST::Stmts.new(
            :resultchild(0),
            QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name($tmp), :scope('local'), :decl('var') ),
                QAST::Op.new(
                    :op('create'),
                    QAST::WVal.new( :value(%cont_info<container_type>) ))),
            QAST::Op.new(
                :op('bindattr'),
                QAST::Var.new( :name($tmp), :scope('local') ),
                QAST::WVal.new( :value(%cont_info<container_base>) ),
                QAST::SVal.new( :value('$!descriptor') ),
                QAST::WVal.new( :value($descriptor) )));
        
        # Default contents, if applicable (note, slurpy param as we can't
        # use definedness here, as it's a type object we'd be checking).
        if nqp::existskey(%cont_info, 'default_value') {
            $cont_code.push(QAST::Op.new(
                :op('bindattr'),
                QAST::Var.new( :name($tmp), :scope('local') ),
                QAST::WVal.new( :value(%cont_info<container_base>) ),
                QAST::SVal.new( :value('$!value') ),
                QAST::WVal.new( :value(%cont_info<default_value>) )));
        }
        
        $cont_code
    }
    
    # Hunts through scopes to find the type of a lexical.
    method find_lexical_container_type($name) {
        my int $i := +@!BLOCKS;
        while $i > 0 {
            $i := $i - 1;
            my %sym := @!BLOCKS[$i].symbol($name);
            if +%sym {
                if nqp::existskey(%sym, 'type') {
                    return %sym<type>;
                }
                else {
                    $i := 0;
                }
            }
        }
        nqp::die("Could not find container descriptor for $name");
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
        my int $flags := 0;
        if %param_info<optional> {
            $flags := $flags + $SIG_ELEM_IS_OPTIONAL;
        }
        if %param_info<is_invocant> {
            $flags := $flags + $SIG_ELEM_INVOCANT;
        }
        if %param_info<is_multi_invocant> {
            $flags := $flags + $SIG_ELEM_MULTI_INVOCANT;
        }
        if %param_info<is_rw> {
            $flags := $flags + $SIG_ELEM_IS_RW;
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
        if nqp::existskey(%param_info, 'variable_name') {
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
        if nqp::existskey(%param_info, 'default_value') {
            nqp::bindattr($parameter, $par_type, '$!default_value', %param_info<default_value>);
        }
        if nqp::existskey(%param_info, 'container_descriptor') {
            nqp::bindattr($parameter, $par_type, '$!container_descriptor', %param_info<container_descriptor>);
        }
        if nqp::existskey(%param_info, 'attr_package') {
            nqp::bindattr($parameter, $par_type, '$!attr_package', %param_info<attr_package>);
        }
        if nqp::existskey(%param_info, 'sub_signature') {
            nqp::bindattr($parameter, $par_type, '$!sub_signature', %param_info<sub_signature>);
        }

        # Return created parameter.
        $parameter
    }
    
    # Creates a signature object from a set of parameters.
    method create_signature(%signature_info) {
        # Create signature object now.
        my $sig_type   := self.find_symbol(['Signature']);
        my $signature  := nqp::create($sig_type);
        my @parameters := %signature_info<parameters>;
        self.add_object($signature);
        
        # Set parameters.
        nqp::bindattr($signature, $sig_type, '$!params', @parameters);
        if nqp::existskey(%signature_info, 'returns') {
            nqp::bindattr($signature, $sig_type, '$!returns', %signature_info<returns>);
        }
        
        # Return created signature.
        $signature
    }

    method compile_time_evaluate($/, $ast) {
        return $ast.compile_time_value if $ast.has_compile_time_value;
        my $thunk := self.create_thunk($/, $ast);
        $thunk();
    }

    # Turn a QAST tree into a code object, to be called immediately.
    method create_thunk($/, $to_thunk) {
        my $block := self.push_lexpad($/);
        $block.push($to_thunk);
        self.pop_lexpad();
        self.create_simple_code_object($block, 'Code');
    }

    # Creates a simple code object with an empty signature
    method create_simple_code_object($block, $type) {
        self.cur_lexpad()[0].push($block);
        my $sig := self.create_signature(nqp::hash('parameters', []));
        return self.create_code_object($block, $type, $sig);
    }
    
    # Creates a code object of the specified type, attached the passed signature
    # object and sets up dynamic compilation thunk.
    method create_code_object($code_past, $type, $signature, $is_dispatcher = 0, :$yada) {
        my $code := self.stub_code_object($type);
        self.attach_signature($code, $signature);
        self.finish_code_object($code, $code_past, $is_dispatcher, :yada($yada));
        $code
    }

    method create_lazy($/, $code) {
        my $type      := self.find_symbol(['LazyScalar']);
        my $container := $type.new($code);
        self.add_object($container);
        QAST::WVal.new( :value($container) )
    }
    
    # Stubs a code object of the specified type.
    method stub_code_object($type) {
        my $type_obj := self.find_symbol([$type]);
        my $code     := nqp::create($type_obj);
        @!CODES[+@!CODES] := $code;
        self.add_object($code);
        $code
    }
    
    # Attaches a signature to a code object, and gives the
    # signature its backlink to the code object.
    method attach_signature($code, $signature) {
        my $code_type := self.find_symbol(['Code']);
        my $sig_type := self.find_symbol(['Signature']);
        nqp::bindattr($code, $code_type, '$!signature', $signature);
        nqp::bindattr($signature, $sig_type, '$!code', $code);
    }
    
    # Takes a code object and the QAST::Block for its body.
    method finish_code_object($code, $code_past, $is_dispatcher = 0, :$yada) {
        my $fixups := QAST::Stmts.new();
        my $des    := QAST::Stmts.new();
        
        # Remove it from the code objects stack.
        @!CODES.pop();
        
        # Handle any phasers.
        self.add_phasers_handling_code($code, $code_past);
                
        # Locate various interesting symbols.
        my $code_type    := self.find_symbol(['Code']);
        my $routine_type := self.find_symbol(['Routine']);
        my $slp_type     := self.find_symbol(['StaticLexPad']);
        
        # Attach code object to QAST node.
        $code_past<code_object> := $code;
        
        # Stash it under the QAST block unique ID.
        %!sub_id_to_code_object{$code_past.cuid()} := $code;
        
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
        %!sub_id_to_sc_idx{$code_past.cuid()} := $code_ref_idx;
        
        # If we clone the stub, need to mark it as a dynamic compilation
        # boundary.
        if self.is_precompilation_mode() {
            my $clone_handler := sub ($orig, $clone) {
                my $do := nqp::getattr($clone, $code_type, '$!do');
                pir::setprop__vPsP($do, 'COMPILER_STUB', $do);
                pir::setprop__0PsP($do, 'CLONE_CALLBACK', $clone_handler);
            };
            pir::setprop__0PsP($stub, 'CLONE_CALLBACK', $clone_handler);
        }
        
        # Fixup will install the real thing, unless we're in a role, in
        # which case pre-comp will have sorted it out.
        unless $*PKGDECL eq 'role' {
            unless self.is_precompilation_mode() {
                $fixups.push(QAST::Stmts.new(
                    self.set_attribute($code, $code_type, '$!do', QAST::BVal.new( :value($code_past) )),
                    QAST::VM.new(
                        :pirop('perl6_associate_sub_code_object vPP'),
                        QAST::BVal.new( :value($code_past) ),
                        QAST::WVal.new( :value($code) )
                    )));
                
                # If we clone the stub, then we must remember to do a fixup
                # of it also.
                pir::setprop__0PsP($stub, 'CLONE_CALLBACK', sub ($orig, $clone) {
                    my $tmp := $fixups.unique('tmp_block_fixup');
                    self.add_object($clone);
                    $fixups.push(QAST::Stmt.new(
                        QAST::Op.new(
                            :op('bind'),
                            QAST::Var.new( :name($tmp), :scope('local'), :decl('var') ),
                            QAST::Op.new( :op('clone'), QAST::BVal.new( :value($code_past) ) )
                        ),
                        self.set_attribute($clone, $code_type, '$!do',
                            QAST::Var.new( :name($tmp), :scope('local') )),
                        QAST::Op.new(
                            :op('p6assoccode'),
                            QAST::Var.new( :name($tmp), :scope('local') ),
                            QAST::WVal.new( :value($clone) )
                        )));
                });
            }

			# Attach the QAST block to the stub.
			pir::setprop__0PsP($stub, 'PAST_BLOCK', $code_past);
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
            $des.push(QAST::VM.new(
                :pirop('perl6_associate_sub_code_object vPP'),
                QAST::BVal.new( :value($code_past) ),
                QAST::WVal.new( :value($code) )));
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
        my $fixups := QAST::Op.new(:name<set_outer_ctx>, :op<callmethod>,
           QAST::BVal.new(:value($block)),
           QAST::Op.new(
                :op<p6getouterctx>,
                QAST::Var.new(
                    :scope<attribute>,
                    :name<$!quasi_context>,
                    QAST::WVal.new( :value($quasi_ast) ),
                    QAST::WVal.new( :value(self.find_symbol(['AST'])) )
                )
           )
        );
        self.add_fixup_task(:fixup_past($fixups), :deserialize_past($fixups));
    }
    
    # Generates code for running phasers.
    method run_phasers_code($code, $block_type, $type) {
        QAST::Op.new(
            :op('for'),
            QAST::Op.new(
                :op('atkey'),
                QAST::Var.new(
                    :scope('attribute'), :name('$!phasers'),
                    QAST::WVal.new( :value($code) ),
                    QAST::WVal.new( :value($block_type) )
                ),
                QAST::SVal.new( :value($type) )
            ),
            QAST::Block.new(
                :blocktype('immediate'),
                QAST::Op.new(
                    :op('call'),
                    QAST::Var.new( :scope('lexical'), :name('$_'), :decl('param') )
                )))
    }
    
    # Adds any extra code needing for handling phasers.
    method add_phasers_handling_code($code, $code_past) {
        my $block_type := self.find_symbol(['Block']);
        if nqp::istype($code, $block_type) {
            my %phasers := nqp::getattr($code, $block_type, '$!phasers');
            if nqp::existskey(%phasers, 'PRE') {
                $code_past[0].push(QAST::Op.new( :op('p6setpre') ));
                $code_past[0].push(self.run_phasers_code($code, $block_type, 'PRE'));
                $code_past[0].push(QAST::Op.new( :op('p6clearpre') ));
            }
            if nqp::existskey(%phasers, 'FIRST') {
                $code_past[0].push(QAST::Op.new(
                    :op('if'),
                    QAST::Op.new( :op('p6takefirstflag') ),
                    self.run_phasers_code($code, $block_type, 'FIRST')));
            }
            if nqp::existskey(%phasers, 'ENTER') {
                $code_past[0].push(self.run_phasers_code($code, $block_type, 'ENTER'));
            }
            if nqp::existskey(%phasers, '!LEAVE-ORDER') || nqp::existskey(%phasers, 'POST') {
                $code_past[+@($code_past) - 1] := QAST::Op.new(
                    :op('p6return'),
                    $code_past[+@($code_past) - 1]);
            }
        }
    }
    
    # Gives the current block what's needed for "let"/"temp" support.
    method give_cur_block_let($/) {
        my $block := self.cur_lexpad();
        unless $block.symbol('!LET-RESTORE') {
            self.setup_let_or_temp($/, '!LET-RESTORE', 'UNDO');
        }
    }
    method give_cur_block_temp($/) {
        my $block := self.cur_lexpad();
        unless $block.symbol('!TEMP-RESTORE') {
            self.setup_let_or_temp($/, '!TEMP-RESTORE', 'LEAVE');
        }
    }
    method setup_let_or_temp($/, $value_stash, $phaser) {
        # Add variable to current block.
        my $block := self.cur_lexpad();
        $block[0].push(QAST::Op.new(
            :op('bind'),
            QAST::Var.new( :name($value_stash), :scope('lexical'), :decl('var') ),
            QAST::Op.new( :op('list') )));
        $block.symbol($value_stash, :scope('lexical'));
        
        # Create a phaser block that will do the restoration.
        my $phaser_block := self.push_lexpad($/);
        self.pop_lexpad();
        $phaser_block.push(QAST::Op.new(
            :op('while'),
            QAST::Var.new( :name($value_stash), :scope('lexical') ),
            QAST::Op.new(
                :op('p6store'),
                QAST::Op.new(
                    :op('shift'),
                    QAST::Var.new( :name($value_stash), :scope('lexical') )
                ),
                QAST::Op.new(
                    :op('shift'),
                    QAST::Var.new( :name($value_stash), :scope('lexical') )
                ))));
        
        # Add as phaser.
        $block[0].push($phaser_block);
        self.add_phaser($/, $phaser,
            self.create_code_object($phaser_block, 'Code', self.create_signature(nqp::hash('parameters', []))));
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
        QAST::Op.new(
            :op('bind'),
            QAST::Var.new(
                :name($name), :scope('attribute'),
                QAST::WVal.new( :value($obj) ),
                QAST::WVal.new( :value($class) )
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
    
    # Takes a QAST::Block and compiles it for running during "compile time".
    # We need to do this for BEGIN but also for things that get called in
    # the compilation process, like user defined traits.
    method compile_in_context($past, $code_type, $slp_type) {
        # Ensure that we have the appropriate op libs loaded and correct
        # HLL.
        my $wrapper := QAST::Block.new(QAST::Stmts.new(), $past);
        self.add_libs($wrapper);
        
        # Create outer lexical contexts with all symbols visible. Maybe 
        # we can be a bit smarter here some day. But for now we just make a
        # single frame and copy all the visible things into it.
        my %seen;
        my $slp       := $slp_type.new();
        my $mu        := try { self.find_symbol(['Mu']) };
        my $cur_block := $past;
        while $cur_block {
            my %symbols := $cur_block.symtable();
            for %symbols {
                unless %seen{$_.key} {
                    # Add slot for symbol.
                    $wrapper[0].push(QAST::Var.new(
                        :name($_.key), :scope('lexical'), :decl('var') ));
                    $wrapper.symbol($_.key, :scope('lexical'));
                    
                    # Make static lexpad entry.
                    my %sym := $_.value;
                    $slp.add_static_value($_.key,
                        (nqp::existskey(%sym, 'value') ?? %sym<value> !! $mu),
                        0, (%sym<state> ?? 1 !! 0));
                }
                %seen{$_.key} := 1;
            }
            $cur_block := $cur_block<outer>;
        }
        
        # Compile it, set wrapper's static lexpad, then invoke the wrapper,
        # which fixes up the lexicals.
        my $compunit := QAST::CompUnit.new(
            :hll('perl6'),
            :sc(self.sc()),
            :compilation_mode(0),
            $wrapper
        );
        my $p6comp  := pir::compreg__Ps('perl6');
        my $post    := $p6comp.post($compunit);
        my $pir     := $p6comp.pir($post);
        my $precomp := $p6comp.evalpmc($pir);
        $precomp[0].get_lexinfo.set_static_lexpad($slp);
        $precomp();
        
        # Fix up Code object associations (including nested blocks).
        # We un-stub any code objects for already-compiled inner blocks
        # to avoid wasting re-compiling them, and also to help make
        # parametric role outer chain work out. Also set up their static
        # lexpads, if they have any.
        my int $num_subs := nqp::elems($precomp);
        my int $i := 0;
        while $i < $num_subs {
            my $subid := $precomp[$i].get_subid();
            if nqp::existskey(%!sub_id_to_code_object, $subid) {
                pir::perl6_associate_sub_code_object__vPP($precomp[$i],
                    %!sub_id_to_code_object{$subid});
                nqp::bindattr(%!sub_id_to_code_object{$subid}, $code_type, '$!do', $precomp[$i]);
            }
            if nqp::existskey(%!sub_id_to_static_lexpad, $subid) {
                $precomp[$i].get_lexinfo.set_static_lexpad(%!sub_id_to_static_lexpad{$subid});
            }
            if nqp::existskey(%!sub_id_to_sc_idx, $subid) {
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
        my str $cache_key;
        if !$nocache {
            my str $namedkey := '';
            for %named {
                $namedkey := $namedkey ~ $_.key ~ ',' ~ $_.value ~ ';'
                    if nqp::defined($_.value);
            }
            if $primitive eq 'bigint' {
                $cache_key := "$type,bigint," ~ nqp::tostr_I(@value[0]);
            } else {
                $cache_key := "$type,$primitive,"
                    ~ nqp::join(',', @value)
                    ~ $namedkey;
            }
            if nqp::existskey(%!const_cache, $cache_key) {
                my $value := %!const_cache{$cache_key};
                return QAST::WVal.new( :value($value), :returns($value.WHAT) );
            }
        }
        
        # Find type object for the box typed we'll create.
        my $type_obj := self.find_symbol(nqp::split('::', $type));
        
        # Go by the primitive type we're boxing. Need to create
        # the boxed value and also code to produce it.
        my $constant;
        if $primitive eq 'int' {
            $constant := nqp::box_i(@value[0], $type_obj);
        }
        elsif $primitive eq 'str' {
            $constant := nqp::box_s(@value[0], $type_obj);
        }
        elsif $primitive eq 'num' {
            $constant := nqp::box_n(@value[0], $type_obj);
        }
        elsif $primitive eq 'bigint' {
            $constant := @value[0];
        }
        elsif $primitive eq 'type_new' {
            $constant := $type_obj.new(|@value, |%named);
        }
        else {
            nqp::die("Don't know how to build a $primitive constant");
        }
        
        # Add to SC.
        self.add_object($constant);
        
        # Build QAST for getting the boxed constant from the constants
        # table, but also annotate it with the constant itself in case
        # we need it. Add to cache.
        my $qast := QAST::WVal.new( :value($constant), :returns($constant.WHAT) );
        if !$nocache {
            %!const_cache{$cache_key} := $constant;
        }
        return $qast;
    }
    
    # Adds a numeric constant value (int or num) to the constants table.
    # Returns PAST to do  the lookup of the constant.
    method add_numeric_constant($type, $value) {
        if $type eq 'Int' && pir::typeof__SP($value) eq 'Int' {
            if nqp::isbig_I($value) {
                # cannot unbox to int without loss of information
                return self.add_constant('Int', 'bigint', $value);
            }
            # since Int doesn't have any vtables yet (at least while compiling
            # the setting), it is inconvenient to work with, so unbox
            $value := nqp::unbox_i($value);
        }
        my $const := self.add_constant($type, nqp::lc($type), $value);
        my $past;
        if $type eq 'Int' {
            $past := QAST::Want.new($const, 'Ii', QAST::IVal.new( :value($value) ) );
        }
        else {
            $past := QAST::Want.new($const, 'Nn',
                $value eq 'Inf' || $value eq '-Inf' || $value eq 'NaN' ??
                    QAST::VM.new( :pirop('set Ns'), QAST::SVal.new( :value(~$value) ) ) !!
                    QAST::NVal.new( :value($value) ) );
        }
        $past.returns($const.returns);
        $past;
    }
    
    # Adds a string constant value to the constants table.
    # Returns PAST to do the lookup of the constant.
    method add_string_constant($value) {
        my $const := self.add_constant('Str', 'str', $value);
        QAST::Want.new(
            $const, :returns($const.returns),
            'Ss', QAST::SVal.new( :value($value) ));
    }
    
    # Adds the result of a constant folding operation to the SC and
    # returns a reference to it.
    method add_constant_folded_result($r) {
        self.add_object($r);
        QAST::WVal.new( :value($r) )
    }
    
    # Takes a data structure of non-Perl 6 objects and wraps them up
    # recursively.
    method p6ize_recursive($data) {
        p6ize_recursive($data)
    }
    
    method nibble_to_str($/, $ast, $mkerr) {
        if $ast.has_compile_time_value {
            return nqp::unbox_s($ast.compile_time_value);
        }
        elsif nqp::istype($ast[0], QAST::Op) && $ast[0].name eq '&infix:<,>' {
            my @pieces;
            for @($ast[0]) {
                if $_.has_compile_time_value {
                    nqp::push(@pieces, nqp::unbox_s($_.compile_time_value));
                }
                else {
                    $/.CURSOR.panic($mkerr());
                }
            }
            return nqp::join(' ', @pieces);
        }
        else {
            $/.CURSOR.panic($mkerr());
        }
    }
    
    method colonpair_nibble_to_str($/, $nibble) {
        self.nibble_to_str($/, $nibble.ast,
            -> { "Colon pair value '$nibble' too complex to use in name" })
    }

    # Creates a meta-object for a package, adds it to the root objects and
    # returns the created object.
    method pkg_create_mo($/, $how, :$name, :$repr, *%extra) {
        # Create the meta-object and add to root objects.
        my %args;
        if nqp::defined($name) { %args<name> := ~$name; }
        if nqp::defined($repr) { %args<repr> := ~$repr; }
        if nqp::existskey(%extra, 'base_type') {
            %args<base_type> := %extra<base_type>;
        }
        if nqp::existskey(%extra, 'group') {
            %args<group> := %extra<group>;
        }
        if nqp::existskey(%extra, 'signatured') {
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
        if nqp::existskey(%cont_info, 'default_value') {
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
        self.ex-handle($/, {
                $obj.HOW."$meta_method_name"($obj, $name, $code_object)
            }
        )
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
            my $val;
            if $_.has_compile_time_value {
                $val := $_.compile_time_value;
            }
            else {
                $val := self.compile_time_evaluate($/, $_);
            }
            if $_.named {
                %named_args{$_.named} := $val;
            }
            else {
                @pos_args.push($val);
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
        if nqp::defined($name) { %args<name> := $name; }
        my $mo := $how.new_type(|%args);
        self.add_object($mo);
        return $mo;
    }
    
    # Adds a value to an enumeration.
    method create_enum_value($enum_type_obj, $key, $value) {
        # Create directly.
        my $val := nqp::rebless(pir::repr_clone__PP($value), $enum_type_obj);
        nqp::bindattr($val, $enum_type_obj, '$!key', $key);
        nqp::bindattr($val, $enum_type_obj, '$!value', $value);
        self.add_object($val);
        
        # Add to meta-object.
        $enum_type_obj.HOW.add_enum_value($enum_type_obj, $val);

        # Result is the value.
        $val
    }
    
    # Applies a trait.
    method apply_trait($/, $trait_sub_name, *@pos_args, *%named_args) {
        my $trait_sub := self.find_symbol([$trait_sub_name]);
        my $ex;
        my $nok := 0;
        try {
            self.ex-handle($/, { $trait_sub(|@pos_args, |%named_args) });
            CATCH {
                $ex := $_;
                my $payload := nqp::getpayload($_);
                if nqp::istype($payload, self.find_symbol(["X", "Inheritance", "UnknownParent"])) {
                    my $Str-obj := self.find_symbol(["Str"]);
                    my %seen := nqp::hash();
                    # sort them into ranges of 0..0.1, 0.1..0.2 and 0.2..0.3
                    my @candidates := [[], [], []];

                    sub evaluator($name, $object) {
                        # only care about type objects
                        return 1 if nqp::isconcrete($object);

                        # difference in length is a good lower bound.
                        my $parlen := nqp::chars($payload.parent);
                        my $lendiff := nqp::chars($name) - $parlen;
                        $lendiff := -$lendiff if $lendiff < 0;
                        return 1 if $lendiff >= $parlen * 0.3;

                        return 1 if nqp::existskey(%seen, $name);

                        my $dist := levenshtein(nqp::unbox_s($payload.parent), $name) / $parlen;
                        %seen{$name} := 1;
                        my @target;
                        @target := @candidates[0] if $dist <= 0.1;
                        @target := @candidates[1] if 0.1 < $dist && $dist <= 0.2;
                        @target := @candidates[2] if 0.2 < $dist && $dist <= 0.35;
                        if nqp::defined(@target) {
                            my $name-str := nqp::box_s($name, $Str-obj);
                            nqp::push(@target, $name-str);
                        }
                        1;
                    }
                    self.walk_symbols(&evaluator);

                    # only take a few suggestions
                    my $to-add := 5;
                    for @candidates[0] {
                        $payload.suggestions.push($_) if $to-add > 0;
                        $to-add := $to-add - 1;
                    }
                    $to-add := $to-add - 1 if +@candidates[0] > 0;
                    for @candidates[1] {
                        $payload.suggestions.push($_) if $to-add > 0;
                        $to-add := $to-add - 1;
                    }
                    $to-add := $to-add - 2 if +@candidates[1] > 0;
                    for @candidates[2] {
                        $payload.suggestions.push($_) if $to-add > 0;
                        $to-add := $to-add - 1;
                    }
                }
                $nok := 1;
            }
        }
        if $nok {
            self.rethrow($/, $ex);
        }
    }

    # Some things get cloned many times with an outer lexical scope that
    # we never enter. This makes sure we capture them as needed.
    method create_lexical_capture_fixup() {
        # Create a list and put it in the SC.
        my class FixupList { has $!list }
        my $fixup_list := nqp::create(FixupList);
        self.add_object($fixup_list);
        nqp::bindattr($fixup_list, FixupList, '$!list', nqp::list());

        # Set up capturing code.
        my $capturer := self.cur_lexpad();
        $capturer[0].push(QAST::VM.new(
            :pirop('capture_all_outers vP'),
            QAST::Var.new(
                :name('$!list'), :scope('attribute'),
                QAST::WVal.new( :value($fixup_list) ),
                QAST::WVal.new( :value(FixupList) ))));
        
        # Return a PAST node that we can push the dummy closure
        return QAST::Op.new(
            :op('push'),
            QAST::Var.new(
                :name('$!list'), :scope('attribute'),
                QAST::WVal.new( :value($fixup_list) ),
                QAST::WVal.new( :value(FixupList) )));
    }
    
    # Handles addition of a phaser.
    method add_phaser($/, $phaser, $block, $phaser_past?) {
        if $phaser eq 'BEGIN' {
            # BEGIN phasers get run immediately.
            my $result := $block();
            return self.add_constant_folded_result($result);
        }
        elsif $phaser eq 'CHECK' {
            my $result_node := QAST::Stmt.new( QAST::Var.new( :name('Nil'), :scope('lexical') ) );
            @!CHECKs := [] unless @!CHECKs;
            @!CHECKs.unshift([$block, $result_node]);
            return $result_node;
        }
        elsif $phaser eq 'INIT' {
            unless $*UNIT.symbol('!INIT_VALUES') {
                my $mu := self.find_symbol(['Mu']);
                my %info;
                %info<container_type> := %info<container_base> := self.find_symbol(['Hash']);
                %info<bind_constraint> := self.find_symbol(['Associative']);
                %info<value_type> := $mu;
                self.install_lexical_container($*UNIT, '!INIT_VALUES', %info,
                    self.create_container_descriptor($mu, 1, '!INIT_VALUES'));
            }
            $*UNIT[0].push(QAST::Op.new(
                :op('callmethod'), :name('bind_key'),
                QAST::Var.new( :name('!INIT_VALUES'), :scope('lexical') ),
                QAST::SVal.new( :value($phaser_past.cuid) ),
                QAST::Op.new(
                    :op('call'),
                    QAST::WVal.new( :value($block) )
                )));
            return QAST::Op.new(
                :op('callmethod'), :name('at_key'),
                QAST::Var.new( :name('!INIT_VALUES'), :scope('lexical') ),
                QAST::SVal.new( :value($phaser_past.cuid) )
            );
        }
        elsif $phaser eq 'END' {
            $*UNIT[0].push(QAST::Op.new(
                :op('callmethod'), :name('unshift'),
                QAST::Var.new( :name('@*END_PHASERS'), :scope('contextual') ),
                QAST::WVal.new( :value($block) )
            ));
            return QAST::Var.new(:name('Nil'), :scope('lexical'));
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
            make QAST::Op.new(
                :op('if'),
                QAST::Op.new( :op('p6stateinit') ),
                QAST::Op.new(
                    :op('p6store'),
                    QAST::Var.new( :name($sym), :scope('lexical') ),
                    QAST::Op.new( :op('call'), QAST::WVal.new( :value($block) ) )
                ),
                QAST::Var.new( :name($sym), :scope('lexical') ));
        }
        elsif $phaser eq 'PRE' || $phaser eq 'POST' {
            my $what := self.add_string_constant($phaser);
            $what.named('phaser');
            my $condition := self.add_string_constant(~$/<blorst>);
            $condition.named('condition');

            $phaser_past[1] := QAST::Op.new(
                :op('unless'),
                $phaser_past[1],
                QAST::Op.new(
                    :op('callmethod'), :name('throw'),
                    QAST::Op.new(
                        :op('callmethod'), :name('new'),
                        QAST::WVal.new( :value(self.find_symbol(['X', 'Phaser', 'PrePost'])) ),
                        $what,
                        $condition,
                    )
                ),
            );
            
            if $phaser eq 'POST' {
                # Needs $_ that can be set to the return value.
                $phaser_past[0].unshift(QAST::Op.new( :op('p6bindsig') ));
                unless $phaser_past.symbol('$_') {
                    $phaser_past[0].unshift(QAST::Var.new( :name('$_'), :scope('lexical'), :decl('var') ));
                }
                nqp::push(
                    nqp::getattr($block.signature, self.find_symbol(['Signature']), '$!params'),
                    self.create_parameter(hash(
                            variable_name => '$_', is_parcel => 1,
                            nominal_type => self.find_symbol(['Mu'])
                        )));
            }
            
            @!CODES[+@!CODES - 1].add_phaser($phaser, $block);
            return QAST::Var.new(:name('Nil'), :scope('lexical'));
        }
        else {
            @!CODES[+@!CODES - 1].add_phaser($phaser, $block);
            return QAST::Var.new(:name('Nil'), :scope('lexical'));
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
        $comp_unit.push(QAST::VM.new(
            loadlibs => ['nqp_group', 'nqp_ops', 'perl6_group', 'perl6_ops',
                         'bit_ops', 'math_ops', 'trans_ops', 'io_ops',
                         'obscure_ops', 'os', 'file', 'sys_ops',
                         'nqp_bigint_ops', 'nqp_dyncall_ops' ]));
    }
    
    # Represents a longname after having parsed it.
    my class LongName {
        # a match object, so that error messages can get a proper line number
        has $!match;
        
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
            ~$!match
        }
        
        # Gets the name, by default without any adverbs.
        method name(:$decl, :$dba = '', :$with_adverbs) {
            my @parts := self.type_name_parts($dba, :$decl);
            unless $decl && $decl eq 'routine' {
                @parts.shift() while self.is_pseudo_package(@parts[0]);
            }
            nqp::join('::', @parts)
                ~ ($with_adverbs ?? nqp::join('', @!colonpairs) !! '');
        }

        # returns a QAST tree that represents the name
        # currently needed for 'require ::($modulename) <importlist>'
        # ignore adverbs for now
        method name_past() {
            if self.contains_indirect_lookup() {
                if @!components == 1 {
                    return @!components[0];
                }
                else {
                    my $past := QAST::Op.new(:op<call>, :name('&infix:<,>'));
                    for @!components {
                        $past.push: $_ ~~ QAST::Node ?? $_ !! QAST::SVal.new(:value($_));
                    }
                    return QAST::Op.new(:op<callmethod>, :name<join>,
                        $past,
                        QAST::SVal.new(:value<::>)
                    );
                }
            }
            else {
                my $value := nqp::join('::', @!components);
                QAST::SVal.new(:$value);
            }
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
                if nqp::istype($_, QAST::Node) {
                    return 1;
                }
            }
            return 0;
        }
        
        # Fetches an array of components provided they are all known
        # or resolvable at compile time.
        method type_name_parts($dba, :$decl) {
            my @name;
            my int $beyond_pp;
            if $decl && $!get_who {
                my $name := self.text;
                nqp::die("Name $name ends with '::' and cannot be used as a $dba");
            }
            if +@!components == 1 && self.is_pseudo_package(@!components[0]) {
                my $c := @!components[0];
                if !$decl || ($decl eq 'routine') {
                    nqp::push(@name, $c);
                    return @name;
                }
                if $c eq 'GLOBAL' {
                    nqp::die("Cannot declare pseudo-package GLOBAL");
                }
                $*W.throw($!match, 'X::PseudoPackage::InDeclaration',
                    pseudo-package  => $c,
                    action          => $dba,
                );
            }
            for @!components {
                if nqp::istype($_, QAST::Node) {
                    if $_.has_compile_time_value {
                        for nqp::split('::', ~$_.compile_time_value) {
                            @name.push($_);
                        }
                    }
                    else {
                        my $name := self.text;
                        nqp::die("Name $name is not compile-time known, and can not serve as a $dba");
                    }
                }
                elsif $beyond_pp || !self.is_pseudo_package($_) {
                    nqp::push(@name, $_);
                    $beyond_pp := 1;
                }
                else {
                    if $decl {
                        if $_ ne 'GLOBAL' {
                            $*W.throw($!match, 'X::PseudoPackage::InDeclaration',
                                pseudo-package  => $_,
                                action          => $dba,
                            );
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
        nqp::bindattr($result, LongName, '$!match', $longname);

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
                @components[+@components - 1] := @components[+@components - 1]
                        ~ (%*COMPILING<%?OPTIONS><setting> ne 'NULL'
                                ??  ':<' ~ ~$*W.compile_time_evaluate($_, $_.ast) ~ '>'
                                !!  ~$_
                           );
            }
            else {
                @pairs.push($_);
            }
        }
        nqp::bindattr($result, LongName, '@!colonpairs', @pairs);
        
        $result
    }
    method disect_deflongname($deflongname) {
        # deflongname has the same capture structure as longname
        self.disect_longname($deflongname);
    }
    
    # Checks if a name starts with a pseudo-package.
    method is_pseudo_package($comp) {
        LongName.is_pseudo_package($comp)
    }
    
    # Checks if a given symbol is declared.
    method is_name(@name) {
        my int $is_name := 0;
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
        $is_name || +@name == 1 && self.is_lexical(@name[0])
    }

    method symbol_has_compile_time_value(@name) {
        my $has_ctv := 0;
        try {
            self.find_symbol(@name);
            $has_ctv := 1;
        }
        $has_ctv;
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
            elsif nqp::existskey($curpackage.WHO, @name[0]) {
                $first_sym := ($curpackage.WHO){@name[0]};
            }
            else {
                return 0;
            }
            
            # If we've more name, recursively check the next level
            # in the package. Otherwise, just go on if it's a
            # package or not.
            if +@name > 1 {
                my @restname := nqp::clone(@name);
                @restname.shift;
                return self.already_declared('our', $first_sym, QAST::Block.new(), @restname);
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

    method walk_symbols($code) {
        # first, go through all lexical scopes
        sub walk_block($block) {
            my %symtable := $block.symtable();
            for %symtable {
                if nqp::existskey($_.value, 'value') {
                    my $val := $_.value<value>;
                    if nqp::istype($val, QAST::Block) {
                        say("recursing into " ~ $_.key);
                        walk_block($val);
                    } else {
                        $code($_.key, $val);
                    }
                }
            }
        }

        for @!BLOCKS {
            walk_block($_);
        }
        for $*GLOBALish.WHO {
            $code($_.key, $_.value);
        }
    }

    # Finds a symbol that has a known value at compile time from the
    # perspective of the current scope. Checks for lexicals, then if
    # that fails tries package lookup.
    method find_symbol(@name) {
        # Make sure it's not an empty name.
        unless +@name { nqp::die("Cannot look up empty name"); }

        # GLOBAL is current view of global.
        if +@name == 1 && @name[0] eq 'GLOBAL' {
            return $*GLOBALish;
        }

        # If it's a single-part name, look through the lexical
        # scopes.
        if +@name == 1 {
            my $final_name := @name[0];
            my int $i := +@!BLOCKS;
            while $i > 0 {
                $i := $i - 1;
                my %sym := @!BLOCKS[$i].symbol($final_name);
                if +%sym {
                    if nqp::existskey(%sym, 'value') {
                        return %sym<value>;
                    }
                    else {
                        nqp::die("No compile-time value for $final_name");
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
            my int $i := +@!BLOCKS;
            while $i > 0 {
                $i := $i - 1;
                my %sym := @!BLOCKS[$i].symbol($first);
                if +%sym {
                    if nqp::existskey(%sym, 'value') {
                        $result := %sym<value>;
                        @name := nqp::clone(@name);
                        @name.shift();
                        $i := 0;
                    }
                    else {
                        nqp::die("No compile-time value for $first");
                    }                    
                }
            }
        }
        
        # Try to chase down the parts of the name.
        for @name {
            if nqp::existskey($result.WHO, ~$_) {
                $result := ($result.WHO){$_};
            }
            else {
                nqp::die("Could not locate compile-time value for symbol " ~
                    nqp::join('::', @name));
            }
        }
        
        $result;
    }
    
    # Takes a name and compiles it to a lookup for the symbol.
    method symbol_lookup(@name, $/, :$package_only = 0, :$lvalue = 0) {
        # Catch empty names and die helpfully.
        if +@name == 0 { $/.CURSOR.panic("Cannot compile empty name"); }
        my $orig_name := nqp::join('::', @name);
        
        # Handle fetching GLOBAL.
        if +@name == 1 && @name[0] eq 'GLOBAL' {
            return QAST::VM.new( pirop => 'get_hll_global Ps',
                QAST::SVal.new( :value('GLOBAL') ) );
        }
        
        # Handle things starting with pseudo-package.
        if self.is_pseudo_package(@name[0]) && @name[0] ne 'GLOBAL' && @name[0] ne 'PROCESS' {
            my $lookup;
            for @name {
                if $lookup {
                    $lookup := QAST::Op.new( :op('who'), $lookup );
                }
                else {
                    # Lookups start at the :: root.
                    $lookup := QAST::Op.new(
                        :op('callmethod'), :name('new'),
                        QAST::WVal.new( :value($*W.find_symbol(['PseudoStash'])) )
                    );
                }
                $lookup := QAST::Op.new(
                    :op('callmethod'), :name('postcircumfix:<{ }>'),
                    $lookup,
                    self.add_string_constant($_));
            }
            return $lookup;
        }
        
        # If it's a single item, then go hunting for it through the
        # block stack.
        if +@name == 1 && !$package_only {
            my int $i := +@!BLOCKS;
            while $i > 0 {
                $i := $i - 1;
                my %sym := @!BLOCKS[$i].symbol(@name[0]);
                if +%sym {
                    return QAST::Var.new( :name(@name[0]), :scope(%sym<scope>) );
                }
            }
        }
        
        # The final lookup will always be just an at_key call on a Stash.
        my $final_name := @name.pop();
        my $lookup := QAST::Op.new(
            :op('callmethod'), :name('at_key'),
            self.add_constant('Str', 'str', $final_name));
        
        # If there's no explicit qualification, then look it up in the
        # current package, and fall back to looking in GLOBAL.
        if +@name == 0 {
            $lookup.unshift(QAST::Op.new(
                :op('who'),
                QAST::Var.new( :name('$?PACKAGE'), :scope('lexical') )
            ));
        }
        
        # Otherwise, see if the first part of the name is lexically
        # known. If not, it's in GLOBAL. Also, if first part is GLOBAL
        # then strip it off.
        else {
            my $path := self.is_lexical(@name[0]) ??
                QAST::Var.new( :name(@name.shift()), :scope('lexical') ) !!
                QAST::VM.new( pirop => 'get_hll_global Ps',
                    QAST::SVal.new( :value('GLOBAL') ) );
            if @name[0] eq 'GLOBAL' {
                @name := nqp::clone(@name);
                @name.shift();
            }
            for @name {
                $path := QAST::Op.new( :op('p6getpackage'),
                    $path, QAST::SVal.new( :value(~$_) ));
            }
            $lookup.unshift(QAST::Op.new(:op('who'), $path));
        }
        
        unless $lvalue {
            $lookup.push(QAST::WVal.new( 
                :value(self.find_symbol(['Bool', 'True'])),
                :named('global_fallback')
            ));
        }
        
        return $lookup;
    }

    # Checks if the given name is known anywhere in the lexpad
    # and with lexical scope.
    method is_lexical($name) {
        my int $i := +@!BLOCKS;
        while $i > 0 {
            $i := $i - 1;
            my %sym := @!BLOCKS[$i].symbol($name);
            if +%sym {
                return %sym<scope> eq 'lexical';
            }
        }
        0;
    }
    
    # Checks if the symbol is really an alias to an attribute.
    method is_attr_alias($name) {
        my int $i := +@!BLOCKS;
        while $i > 0 {
            $i := $i - 1;
            my %sym := @!BLOCKS[$i].symbol($name);
            if +%sym {
                return %sym<attr_alias>;
            }
        }
    }
    
    # Checks if a symbol is lexically visible relative to a given scope.
    # Returns 0 if it's not, 1 if it is, 2 if it's a type.
    method is_lexically_visible($name, $scope) {
        my $cur_block := $scope;
        while $cur_block {
            my %symbols := $cur_block.symtable();
            if nqp::existskey(%symbols, $name) {
                my %sym := %symbols{$name};
                return nqp::existskey(%sym, 'value') && 
                    !nqp::isconcrete(%sym<value>) ?? 2 !! 1;
            }
            $cur_block := $cur_block<outer>;
        }
    }

    # Adds various bits of initialization that must always be done early on.
    method add_initializations() {
        if self.is_precompilation_mode() {
            self.add_load_dependency_task(:deserialize_past(QAST::Stmts.new(
                QAST::VM.new( :pirop('nqp_dynop_setup v') ),
                QAST::VM.new( :pirop('nqp_bigint_setup v') ),
                QAST::VM.new( :pirop('nqp_native_call_setup v') ),
                QAST::VM.new( :pirop('rakudo_dynop_setup v') ),
                QAST::Op.new(
                    :op('callmethod'), :name('hll_map'),
                    QAST::VM.new( :pirop('getinterp P') ),
                    QAST::VM.new( :pirop('get_class Ps'), QAST::SVal.new( :value('LexPad') ) ),
                    QAST::VM.new( :pirop('get_class Ps'), QAST::SVal.new( :value('Perl6LexPad') ) )
                )
            )));
        }
    }

    # Constructs and immediately throws a typed exception. Note that if there
    # are extra sorrows or worries it will put them into a group.
    method throw($/, $ex_type, *%opts) {
        my $ex := self.typed_exception($/, $ex_type, |%opts);
        if @*SORROWS || @*WORRIES {
            $ex := self.group_exception($ex);
        }
        $ex.throw
    }
    
    # Builds an exception group.
    method group_exception(*@panic) {
        my %opts;
        %opts<panic> := @panic[0] if @panic;
        %opts<sorrows> := p6ize_recursive(@*SORROWS) if @*SORROWS;
        %opts<worries> := p6ize_recursive(@*WORRIES) if @*WORRIES;
        try {
            my $group_type := self.find_symbol(['X', 'Comp', 'Group']);
            return $group_type.new(|%opts);
            CATCH {
                nqp::print("Error while constructing error object:");
                nqp::say($_);
            }
        }
    }
    
    # Tries to construct a typed exception, incorporating all available compile
    # time information (such as about location). Returns it provided it is able
    # to construct it. If that fails, dies right away.
    method typed_exception($/, $ex_type, *%opts) {
        my int $type_found := 1;
        my $ex;
        my $x_comp;
        try {
            CATCH {
                $type_found := 0;
                nqp::print("Error while constructing error object:");
                nqp::say($_);
            };
            $ex := self.find_symbol(nqp::islist($ex_type) ?? $ex_type !! nqp::split('::', $ex_type));
            my $x_comp := self.find_symbol(['X', 'Comp']);
            unless nqp::istype($ex, $x_comp) {
                $ex := $ex.HOW.mixin($ex, $x_comp);
            }
        };

        if $type_found {
            # If the highwater is beyond the current position, force the cursor to
            # that location.
            my $c := $/.CURSOR;
            my @expected;
            if $c.'!highwater'() >= $c.pos() {
                my @raw_expected := $c.'!highexpect'();
                $c.'!cursor_pos'($c.'!highwater'());
                my %seen;
                for @raw_expected {
                    unless %seen{$_} {
                        nqp::push(@expected, $_);
                        %seen{$_} := 1;
                    }
                }
            }
            
            # Try and better explain "Confused".
            my @locprepost := self.locprepost($c);
            if $ex.HOW.name($ex) eq 'X::Syntax::Confused' {
                my $next := nqp::substr(@locprepost[1], 0, 1);
                if $next ~~ /\)|\]|\}|\»/ {
                    %opts<reason> := "Unexpected closing bracket";
                    @expected := [];
                }
                else {
                    my $expected_infix := 0;
                    for @expected {
                        if nqp::index($_, "infix") >= 0 {
                            $expected_infix := 1;
                            last;
                        }
                    }
                    if $expected_infix {
                        %opts<reason> := "Two terms in a row";
                    }
                }
            }
            
            # Build and throw exception object.
            %opts<line>            := HLL::Compiler.lineof($c.orig, $c.pos);
            %opts<modules>         := p6ize_recursive(@*MODULES);
            %opts<pre>             := @locprepost[0];
            %opts<post>            := @locprepost[1];
            %opts<highexpect>      := p6ize_recursive(@expected) if @expected;
            %opts<is-compile-time> := 1;
            for %opts -> $p {
                if nqp::islist($p.value) {
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
            my $file        := pir::find_caller_lex__Ps('$?FILES');
            %opts<filename> := nqp::box_s(
                (nqp::isnull($file) ?? '<unknown file>' !! $file),
                self.find_symbol(['Str'])
            );
            return $ex.new(|%opts);
        } else {
            my @err := ['Error while compiling, type ', nqp::join('::', $ex_type),  "\n"];
            for %opts -> $key {
                @err.push: '  ';
                @err.push: $key;
                @err.push: ': ';
                @err.push: %opts{$key};
                @err.push: "\n";
            }
            nqp::findmethod(HLL::Grammar, 'panic')($/.CURSOR, nqp::join('', @err));
        }
    }
    
    method locprepost($c) {
        my $pos  := $c.pos;
        my $orig := $c.orig;

        my $prestart := $pos - 40;
        $prestart := 0 if $prestart < 0;
        my $pre := nqp::substr($orig, $prestart, $pos - $prestart);
        $pre    := subst($pre, /.*\n/, "", :global);
        $pre    := '<BOL>' if $pre eq '';
        
        my $postchars := $pos + 40 > nqp::chars($orig) ?? nqp::chars($orig) - $pos !! 40;
        my $post := nqp::substr($orig, $pos, $postchars);
        $post    := subst($post, /\n.*/, "", :global);
        $post    := '<EOL>' if $post eq '';
        
        [$pre, $post]
    }

    method ex-handle($/, $code) {
        my $res;
        my $ex;
        my int $nok;
        try {
            $res := $code();
            CATCH {
                $nok := 1;
                $ex  := $_;
            }
        }
        if $nok {
            $*W.rethrow($/, $ex);
        } else {
            $res;
        }
    }

    method rethrow($/, $err) {
        my int $success := 0;
        my $coercer;
        try { $coercer := self.find_symbol(['&COMP_EXCEPTION']); ++$success; };
        nqp::rethrow($err) unless $success;
        my $p6ex := $coercer($err);
        unless nqp::can($p6ex, 'SET_FILE_LINE') {
            try {
                my $x_comp := self.find_symbol(['X', 'Comp']);
                $p6ex.HOW.mixin($p6ex, $x_comp).BUILD_LEAST_DERIVED(nqp::hash());
            }
        }
        if nqp::can($p6ex, 'SET_FILE_LINE') {
            $p6ex.SET_FILE_LINE(
                nqp::box_s(pir::find_caller_lex__Ps('$?FILES'),
                    self.find_symbol(['Str'])),
                nqp::box_i(HLL::Compiler.lineof($/.orig, $/.from),
                    self.find_symbol(['Int'])),
            );
        }
        $p6ex.rethrow();
    }
}
