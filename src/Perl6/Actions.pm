class Perl6::Actions is HLL::Actions;

our @BLOCK;
our @PACKAGE;
our $TRUE;

INIT {
    # initialize @BLOCK and @PACKAGE
    our @BLOCK := Q:PIR { %r = new ['ResizablePMCArray'] };
    our @PACKAGE := Q:PIR { %r = new ['ResizablePMCArray'] };
    @PACKAGE.unshift(Perl6::Compiler::Module.new());
    our $TRUE := PAST::Var.new( :name('true'), :scope('register') );

    # Tell PAST::Var how to encode Perl6Str and Str values
    my %valflags := 
        Q:PIR { %r = get_hll_global ['PAST';'Compiler'], '%valflags' };
    %valflags<Perl6Str> := 'e';
    %valflags<Str>      := 'e';
}

sub xblock_immediate($xblock) {
    $xblock[1] := block_immediate($xblock[1]);
    $xblock;
}

sub block_immediate($block) {
    $block.blocktype('immediate');
    $block;
}

sub sigiltype($sigil) {
    $sigil eq '%' 
    ?? 'Hash' 
    !! ($sigil eq '@' ?? 'Array' !! 'Perl6Scalar');
}

method deflongname($/) {
    make $<colonpair>
         ?? ~$<name> ~ ':<' ~ $<colonpair>[0]<circumfix><quote_EXPR>.ast.value ~ '>'
         !! ~$<name>;
}

method comp_unit($/) {
    # Create the block for the mainline code.
    my $mainline := @BLOCK.shift;
    $mainline.push($<statementlist>.ast);

    # Create a block for the entire compilation unit.
    our $?RAKUDO_HLL;
    my $unit := PAST::Block.new( :node($/), :hll($?RAKUDO_HLL) );

    # Executing the compilation unit causes the mainline to be executed.
    # We force a tailcall here, because we have other :load/:init blocks 
    # that have to be done at the end of the unit, and we don't want them 
    # executed by the mainline.
    $unit.push(
        PAST::Op.new(
            :pirop('tailcall'),
            PAST::Var.new( :name('!UNIT_START'), :namespace(''), :scope('package') ),
            $mainline,
            PAST::Var.new( :scope('parameter'), :name('@_'), :slurpy(1) )
        )
    );

    # CHECK time occurs at the end of the compilation unit, :load/:init.
    # (We can't # use the .loadinit property because that will generate 
    # the CHECK block too early.)
    $unit.push(
        PAST::Block.new( 
            :pirflags(':load :init'), :lexical(0), :namespace(''),
            PAST::Op.new( :name('!fire_phasers'), 'CHECK' )
        )
    );

    # If this unit is loaded via load_bytecode, we want it to automatically
    # execute the mainline code above after all other initializations have
    # occurred.
    $unit.push(
        PAST::Block.new(
            :pirflags(':load'), :lexical(0), :namespace(''),
            PAST::Op.new( 
                :name('!UNIT_START'), :pasttype('call'),
                PAST::Val.new( :value($unit) ),
            )
        )
    );

    make $unit;
}

method statementlist($/) {
    my $past := PAST::Stmts.new( :node($/) );
    if $<statement> {
        for $<statement> { 
            my $ast := $_.ast;
            if $ast.isa(PAST::Block) && !$ast.blocktype {
                $ast := block_immediate($ast);
            }
            $past.push( $ast ); 
        }
    }
    make $past;
}

method statement($/, $key?) {
    my $past;
    if $<EXPR> {
        my $mc := $<statement_mod_cond>[0];
        $past := $mc
              ?? PAST::Op.new($mc.ast, $<EXPR>.ast, :pasttype(~$mc<sym>), :node($/) )
              !! $<EXPR>.ast;
    }
    elsif $<statement_control> { $past := $<statement_control>.ast; }
    else { $past := 0; }
    make $past;
}

method xblock($/) {
    make PAST::Op.new( $<EXPR>.ast, $<pblock>.ast, :pasttype('if'), :node($/) );
}

method pblock($/) {
    my $block := $<blockoid>.ast;
    my $signature;
    if $<signature> { $signature := $<signature>.ast; }
    else {
        $signature := Perl6::Compiler::Signature.new();
        unless $block.symbol('$_') {
            if $*IMPLICIT {
                my $parameter := Perl6::Compiler::Parameter.new();
                $parameter.var_name('$_');
                $parameter.optional(1);
                $parameter.is_ref(1);
                $parameter.default_from_outer(1);
                $signature.add_parameter($parameter);
            }
            else {
                add_implicit_var($block, '$_', 1);
            }
        }
    }
    add_signature($block, $signature);
    make $block;
}

method block($/) {
    make $<blockoid>.ast;
}

method blockoid($/) {
    my $past := $<statementlist>.ast;
    my $BLOCK := @BLOCK.shift;
    $BLOCK.push($past);
    $BLOCK.node($/);
    make $BLOCK;
}

method newpad($/) {
    our @BLOCK;
    our @PACKAGE;
    my $new_block := PAST::Block.new( PAST::Stmts.new(
        PAST::Op.new(
            :inline("    .local pmc true\n    true = get_hll_global 'True'")
        )
    ));
    @BLOCK.unshift($new_block);
    unless @PACKAGE[0].block {
        @PACKAGE[0].block($new_block);
    }
}

method finishpad($/) {
    # Generate the $_, $/, and $! lexicals if they aren't already
    # declared.  For routines and methods, they're simply created as
    # undefs; for other blocks they initialize to their outer lexical.

    my $BLOCK := @BLOCK[0];
    my $outer := $*IN_DECL ne 'routine' && $*IN_DECL ne 'method';

    for <$_ $/ $!> { 
        # Generate the lexical variable except if...
        #   (1) the block already has one, or
        #   (2) the variable is '$_' and $*IMPLICIT is set
        #       (this case gets handled by getsig)
        unless $BLOCK.symbol($_) || ($_ eq '$_' && $*IMPLICIT) {
            add_implicit_var($BLOCK, $_, $outer);
        }
    }
}


## Statement control

method statement_control:sym<if>($/) {
    my $count := +$<xblock> - 1;
    my $past := xblock_immediate( $<xblock>[$count].ast );
    if $<else> {
        $past.push( block_immediate( $<else>[0].ast ) );
    }
    # build if/then/elsif structure
    while $count > 0 {
        $count--;
        my $else := $past;
        $past := xblock_immediate( $<xblock>[$count].ast );
        $past.push($else);
    }
    make $past;
}

method statement_control:sym<unless>($/) {
    my $past := xblock_immediate( $<xblock>.ast );
    $past.pasttype('unless');
    make $past;
}

method statement_control:sym<while>($/) {
    my $past := xblock_immediate( $<xblock>.ast );
    $past.pasttype(~$<sym>);
    make $past;
}

method statement_control:sym<repeat>($/) {
    my $pasttype := 'repeat_' ~ ~$<wu>;
    my $past;
    if $<xblock> { 
        $past := xblock_immediate( $<xblock>.ast );
        $past.pasttype($pasttype);
    }
    else {
        $past := PAST::Op.new( $<EXPR>.ast, block_immediate( $<pblock>.ast ),
                               :pasttype($pasttype), :node($/) );
    }
    make $past;
}

method statement_control:sym<for>($/) {
    my $past := $<xblock>.ast;
    $past.pasttype('for');
    my $block := $past[1];
    $block[0].push( PAST::Var.new( :name('$_'), :scope('parameter') ) );
    $block.symbol('$_', :scope('lexical') );
    $block.arity(1);
    $block.blocktype('immediate');
    make $past;
}

method statement_control:sym<use>($/) {
    if $<module_name> {
        @BLOCK[0][0].unshift(
            PAST::Op.new( :name('!use'), ~$<module_name>, :node($/) )
        );
    }
    make PAST::Stmts.new( :node($/) );
}

method statement_control:sym<return>($/) {
    make PAST::Op.new( $<EXPR>.ast, :pasttype('return'), :node($/) );
}

method statement_control:sym<given>($/) {
    my $past := $<xblock>.ast;
    $past.push($past.shift); # swap [0] and [1] elements
    $past.pasttype('call');
    make $past;
}

method statement_control:sym<when>($/) {
    # Get hold of the smartmatch expression and the block.
    my $xblock := $<xblock>.ast;
    my $sm_exp := $xblock.shift;
    my $block  := $xblock.shift;

    # Add exception handler to the block so we fall out of the enclosing block
    # after it's executed.
    $block.blocktype('immediate');
    when_handler_helper($block);

    # Handle the smart-match. XXX Need to handle syntactic cases too.
    my $match_past := PAST::Op.new( :pasttype('call'), :name('&infix:<~~>'),
        PAST::Var.new( :name('$_'), :scope('lexical') ),
        $sm_exp
    );

    # Use the smartmatch result as the condition for running the block.
    make PAST::Op.new( :pasttype('if'), :node( $/ ),
        $match_past, $block,
    );
}

method statement_control:sym<default>($/) {
    # We always execute this, so just need the block, however we also
    # want to make sure we break after running it.
    my $block := $<block>.ast;
    $block.blocktype('immediate');
    when_handler_helper($block);
    make $block;
}

# XXX BEGIN isn't correct here, but I'm adding it along with this
# note so that everyone else knows it's wrong too.  :-)
method statement_prefix:sym<BEGIN>($/) { add_phaser($/, 'BEGIN'); }
method statement_prefix:sym<CHECK>($/) { add_phaser($/, 'CHECK'); }
method statement_prefix:sym<INIT>($/)  { add_phaser($/, 'INIT'); }
method statement_prefix:sym<END>($/)   { add_phaser($/, 'END'); }

method statement_prefix:sym<do>($/) {
    my $past := $<blorst>.ast;
    $past.blocktype('immediate');
    make $past;
}

method statement_prefix:sym<try>($/) {
    my $block := $<blorst>.ast;
    $block.blocktype('immediate');
    my $past := PAST::Op.new( :pasttype('try'), $block );

    # On failure, capture the exception object into $!.
    $past.push(PAST::Op.new(
        :inline( '    .get_results (%r)',
                 '    $P0 = new ["Perl6Exception"]',
                 '    setattribute $P0, "$!exception", %r',
                 '    store_lex "$!", $P0' )
    ));

    # Otherwise, put a failure into $!.
    $past.push(PAST::Op.new( :pasttype('bind'),
        PAST::Var.new( :name('$!'), :scope('lexical') ),
        PAST::Op.new( :pasttype('call'), :name('!FAIL') )
    ));

    make $past;
}

method blorst($/) {
    my $block := $<block>
                 ?? $<block>.ast
                 !! PAST::Block.new( $<statement>.ast, :node($/) );
    $block.blocktype('declaration');
    make $block;
}

sub add_phaser($/, $bank) {
    @BLOCK[0].loadinit.push(
        PAST::Op.new( :pasttype('call'), :name('!add_phaser'), 
                      $bank, $<blorst>.ast, :node($/))
    );
    make PAST::Stmts.new(:node($/));
}

# Statement modifiers

method statement_mod_cond:sym<if>($/)     { make $<mod_expr>.ast; }
method statement_mod_cond:sym<unless>($/) { make $<mod_expr>.ast; }


## Terms

method term:sym<colonpair>($/)          { make $<colonpair>.ast; }
method term:sym<variable>($/)           { make $<variable>.ast; }
method term:sym<package_declarator>($/) { make $<package_declarator>.ast; }
method term:sym<scope_declarator>($/)   { make $<scope_declarator>.ast; }
method term:sym<routine_declarator>($/) { make $<routine_declarator>.ast; }
method term:sym<multi_declarator>($/)   { make $<multi_declarator>.ast; }
method term:sym<regex_declarator>($/)   { make $<regex_declarator>.ast; }
method term:sym<statement_prefix>($/)   { make $<statement_prefix>.ast; }
method term:sym<lambda>($/)             { make $<pblock>.ast; }

method name($/) { }

method module_name($/) {
    my @name := Perl6::Grammar::parse_name(~$<longname>);
    my $var := PAST::Var.new(
        :name(@name.pop),
        :namespace(@name),
        :scope('package')
    );
    if $<arglist> {
        my $past := $<arglist>[0].ast;
        $past.pasttype('callmethod');
        $past.name('postcircumfix:<[ ]>');
        $past.unshift($var);
        make $past;
    }
    else {
        make $var;
    }
}

method colonpair($/) {
    my $past := $<circumfix> 
                ?? $<circumfix>.ast 
                !! PAST::Val.new( :value( !$<not> ) );
    $past.named( ~$<identifier> );
    make $past;
}

method variable($/) {
    my $past;
    if $<postcircumfix> {
        $past := $<postcircumfix>.ast;
        $past.unshift( PAST::Var.new( :name('$/') ) );
    }
    else {
        $past := PAST::Var.new( :name(~$/) );
        if $<twigil>[0] eq '*' { 
            $past.scope('contextual'); 
            $past.viviself( PAST::Op.new( 'Contextual ' ~ ~$/ ~ ' not found', 
                                          :pirop('die') )
            );
        }
        elsif $<twigil>[0] eq '!' {
            $past.scope('attribute');
            $past.viviself( sigiltype( $<sigil> ) );
            $past.unshift(PAST::Var.new( :name('self'), :scope('lexical') ));
        }
        elsif $<twigil>[0] eq '.' && !$*IN_DECL {
            # Need to transform this to a method call.
            $past := PAST::Op.new(
                :pasttype('callmethod'), :name(~$<desigilname>),
                PAST::Var.new( :name('self'), :scope('lexical') )
            );
        }
    }
    make $past;
}

method package_declarator:sym<module>($/)  { make $<package_def>.ast; }
method package_declarator:sym<class>($/)   { make $<package_def>.ast; }
method package_declarator:sym<grammar>($/) { make $<package_def>.ast; }
method package_declarator:sym<role>($/)    { make $<package_def>.ast; }

method package_def($/, $key?) {
    our @PACKAGE;

    # Is this the opening of a new package?
    if $key eq 'open' {
        # Create the right kind of package compiler.
        my $pkg_compiler := %*PKGCOMPILER{$*PKGDECL};
        if pir::isa__IPS($pkg_compiler, 'Undef') { $pkg_compiler := Perl6::Compiler::Package; }
        my $package := $pkg_compiler.new();

        # Set HOW and other details.
        my $how := %*HOW{$*PKGDECL};
        unless $how { $/.CURSOR.panic("No HOW declared for package declarator $*PKGDECL"); }
        $package.how($how);
        $*SCOPE := $*SCOPE || 'our';
        $package.scope($*SCOPE);
        if $<def_module_name> {
            my $name := ~$<def_module_name>[0]<longname>;
            if $name ne '::' {
                $package.name($name);
                $/.CURSOR.add_my_name($name);
            }
            if $<def_module_name>[0]<signature> {
                $package.signature($<def_module_name>[0]<signature>[0].ast);
            }
        }

        # Add traits.
        for $<trait> {
            $package.traits.push($_.ast);
        }

        # Put on front of packages list. Note - nesting a package in a role is
        # not supported (gets really tricky in the parametric case - needs more
        # thought and consideration).
        if +@PACKAGE && pir::isa__IPS(@PACKAGE[0], 'Role') {
            $/.CURSOR.panic("Can not nest a package inside a role");
        }
        @PACKAGE.unshift($package);
    }
    else {
        # We just need to finish up the current package.
        my $package := @PACKAGE.shift;
        my $block;
        if $<block> { $block := $<block>.ast }
        else {
            $block := @BLOCK.shift;
            $block.push($<statementlist>.ast);
            $block.node($/);
        }
        make $package.finish($block);
    }
}

method scope_declarator:sym<my>($/)      { make $<scoped>.ast; }
method scope_declarator:sym<our>($/)     { make $<scoped>.ast; }
method scope_declarator:sym<has>($/)     { make $<scoped>.ast; }
method scope_declarator:sym<augment>($/) { make $<scoped>.ast; }

method declarator($/) {
    if    $<variable_declarator> { make $<variable_declarator>.ast }
    elsif $<routine_declarator>  { make $<routine_declarator>.ast  }
    elsif $<regex_declarator>    { make $<regex_declarator>.ast    }
    else {
        $/.CURSOR.panic('Unknown declarator type');
    }
}

method multi_declarator:sym<multi>($/) { make $<declarator> ?? $<declarator>.ast !! $<routine_def>.ast }
method multi_declarator:sym<proto>($/) { make $<declarator> ?? $<declarator>.ast !! $<routine_def>.ast }
method multi_declarator:sym<only>($/)  { make $<declarator> ?? $<declarator>.ast !! $<routine_def>.ast }
method multi_declarator:sym<null>($/)  { make $<declarator>.ast }

method scoped($/) {
    make $<DECL>.ast;
}

method variable_declarator($/) {
    my $past := $<variable>.ast;
    my $sigil := $<variable><sigil>;
    my $twigil := $<variable><twigil>[0];
    my $name := $past.name;

    if $*SCOPE eq 'has' {
        # Find the current package and add the attribute.
        my $attrname := ~$sigil ~ '!' ~ ~$<variable><desigilname>;
        our @PACKAGE;
        unless +@PACKAGE { $/.CURSOR.panic("Can not declare attribute outside of a package"); }
        my %attr_table := @PACKAGE[0].attributes;
        if %attr_table{$attrname} { $/.CURSOR.panic("Can not re-declare attribute " ~ $attrname); }
        %attr_table{$attrname} := Q:PIR { %r = new ['Hash'] };
        %attr_table{$attrname}<name>     := $attrname;
        %attr_table{$attrname}<accessor> := $twigil eq '.' ?? 1 !! 0;

        # If the trait is . then need an accessor method adding too.
        if $twigil eq '.' {
            # Create block to return either the attribute itself, or
            # wrapped to hide the rw-ness.
            my $var := PAST::Var.new(
                :name($attrname),
                :scope('attribute'),
                PAST::Var.new( :name('self'), :scope('lexical') )
            );
            unless $<trait> && has_compiler_trait_with_val($<trait>, '&trait_mod:<is>', 'rw') {
                $var := PAST::Op.new( :pirop('new PsP'), 'Perl6Scalar', $var);
            }
            my $meth := PAST::Block.new(
                :name(~$<variable><desigilname>),
                :nsentry(''),
                PAST::Stmts.new(
                    PAST::Var.new( :name('self'), :scope('lexical'), :isdecl(1) )
                ),
                $var
            );
            my $sig := Perl6::Compiler::Signature.new();
            $sig.add_invocant();
            add_signature($meth, $sig);

            # Wrap it in a Method handle, and install in methods table.
            my %meth_table := @PACKAGE[0].methods;
            my %meth_hash;
            %meth_hash<code_ref> := create_code_object($meth, 'Method', 0);
            %meth_table{~$<variable><desigilname>} := %meth_hash;
        }
        
        # Nothing to emit here.
        $past := PAST::Stmts.new( );
    }
    else {
        # Not an attribute - need to emit delcaration here. Check it's
        # not a duplicate.
        my $BLOCK := @BLOCK[0];
        if $BLOCK.symbol($name) {
            $/.CURSOR.panic("Redeclaration of symbol ", $name);
        }

        # First, create a container and give it a 'rw' property
        # Create the container, give it a 'rw' property
        my $cont := PAST::Op.new( sigiltype($sigil), :pirop('new Ps') );
        my $true := PAST::Var.new( :name('true'), :scope('register') );
        my $vivipast := PAST::Op.new( $cont, 'rw', $true, :pirop('setprop'));

        # If it's an array or hash, it flattens in list context.
        if $sigil eq '@' || $sigil eq '%' {
            $vivipast := PAST::Op.new($vivipast,'flatten',$true,:pirop('setprop'));
        }

        # For 'our' variables, we first bind or lookup in the namespace
        if $*SCOPE eq 'our' {
            $vivipast := PAST::Var.new( :name($name), :scope('package'), :isdecl(1),
                                         :lvalue(1), :viviself($vivipast), :node($/) );
        }

        # Now bind a lexical in the block
        my $decl := PAST::Var.new( :name($name), :scope('lexical'), :isdecl(1),
                                   :lvalue(1), :viviself($vivipast), :node($/) );
        $BLOCK.symbol($name, :scope('lexical'), :decl_node($decl) );
        $BLOCK[0].push($decl);

        # If we have traits, set up us the node to emit handlers into, then
        # emit them.
        if $<trait> || $*TYPENAME {
            my $trait_node := get_var_traits_node($BLOCK, $name);
            for $<trait> {
                my $trait := $_.ast;
                $trait.unshift(PAST::Var.new( :name('declarand'), :scope('register') ));
                if $trait.name() eq '&trait_mod:<of>' && $*TYPENAME {
                    $trait[1] := PAST::Op.new(
                        :pasttype('callmethod'), :name('postcircumfix:<[ ]>'),
                        $*TYPENAME, $trait[1]
                    );
                    $*TYPENAME := '';
                }
                $trait_node.push($trait);
            }
            if $*TYPENAME {
                $trait_node.push(PAST::Op.new(
                    :pasttype('call'), :name('&trait_mod:<of>'),
                    PAST::Var.new( :name('declarand'), :scope('register') ),
                    $*TYPENAME
                ));
            }
        }
    }

    make $past;
}

method routine_declarator:sym<sub>($/) { make $<routine_def>.ast; }
method routine_declarator:sym<method>($/) { make $<method_def>.ast; }

method routine_def($/) {
    my $past := $<blockoid>.ast;
    $past.blocktype('declaration');
    $past.control('return_pir');
    add_signature($past, $<signature> ?? $<signature>[0].ast !! Perl6::Compiler::Signature.new());
    if $<trait> {
        emit_routine_traits($past, $<trait>, 'Sub');
    }
    if $<deflongname> {
        # Set name.
        my $name := '&' ~ ~$<deflongname>[0].ast;
        $past.name($name);
        $past.nsentry('');

        # Wrap it in the correct routine type.
        my $multi_flag := PAST::Val.new( :value(0) );
        $past := create_code_object($past, 'Sub', $multi_flag);

        # Handle multi-ness, if any.
        my $symbol_holder := $*SCOPE eq 'our' ?? @PACKAGE[0].block !! @BLOCK[0];
        my $symbol := $symbol_holder.symbol($name);
        if $*MULTINESS eq 'only' {
            if $symbol {
                $/.CURSOR.panic('Can not declare only routine ' ~ $name ~
                    ' when another routine with this name was already declared');
            }
        }
        elsif $*MULTINESS || ($symbol && $symbol<multis>) {
            # If no multi declarator and no proto, error.
            if !$*MULTINESS && !$symbol<proto> {
                $/.CURSOR.panic('Can not re-declare sub ' ~ $name ~ ' without declaring it multi');
            }

            # If it's a proto, stash it away in the symbol entry.
            if $*MULTINESS eq 'proto' { $symbol_holder.symbol($name, :proto($past)) }

            # Otherwise, create multi container if we don't have one; otherwise,
            # just push this candidate onto it.
            if $symbol<multis> {
                $symbol<multis>.push($past);
                $past := 0;
            }
            else {
                $past := PAST::Op.new(
                    :pasttype('callmethod'),
                    :name('set_candidates'),
                    PAST::Op.new( :inline('    %r = new ["Perl6MultiSub"]') ),
                    $past
                );
                $symbol_holder.symbol($name, :multis($past))
            }
            $multi_flag.value(1);
        }

        # Install in lexical scope if it's not package scoped.
        if $*SCOPE ne 'our' {
            if $past {
                @BLOCK[0][0].push(PAST::Var.new( :name($name), :isdecl(1), 
                                      :viviself($past), :scope('lexical') ) );
                @BLOCK[0].symbol($name, :scope('lexical') );
            }
        }

        # Otherwise, package scoped; add something to loadinit to install them.
        else {
            if $past {
                if $symbol_holder.symbol($name)<multis> {
                    $past[0] := PAST::Var.new( :name($name), :scope('package'), :viviself($past[0]) );
                }
                @PACKAGE[0].block.loadinit.push(PAST::Op.new(
                    :pasttype('bind'),
                    PAST::Var.new( :name($name), :scope('package') ),
                    $past
                ));
            }
            @BLOCK[0].symbol($name, :scope('package') );
        }

        $past := PAST::Var.new( :name($name) );
    }
    elsif $*MULTINESS {
        $/.CURSOR.panic('Can not put ' ~ $*MULTINESS ~ ' on anonymous routine');
    }
    make $past;
}


method method_def($/) {
    my $past := $<blockoid>.ast;
    $past.blocktype('declaration');
    $past.control('return_pir');
    if $<trait> {
        emit_routine_traits($past, $<trait>, 'Method');
    }
    
    # Set signature and invocant handling set up.
    my $sig := $<signature> ?? $<signature>[0].ast !! Perl6::Compiler::Signature.new();
    $sig.add_invocant();
    add_signature($past, $sig);
    $past[0].unshift(PAST::Var.new( :name('self'), :scope('lexical'), :isdecl(1), :viviself(sigiltype('$')) ));
    $past.symbol('self', :scope('lexical'));

    # Method container.
    if $<longname> {
        # Set up us the name.
        my $name := $<longname>.Str;
        if $<specials> && ~$<specials>[0] eq '!' { $name := '!' ~ $name; }
        $past.name($name);
        $past.nsentry('');
        my $multi_flag := PAST::Val.new( :value(0) );
        $past := create_code_object($past, 'Method', $multi_flag);

        # Get hold of methods table.
        our @PACKAGE;
        unless +@PACKAGE { $/.CURSOR.panic("Can not declare method outside of a package"); }
        my %table := @PACKAGE[0].methods();
        unless %table{$name} { my %tmp; %table{$name} := %tmp; }

        # If it's an only and there's already a symbol, problem.
        if $*MULTINESS eq 'only' && %table{$name} {
            $/.CURSOR.panic('Can not declare only method ' ~ $name ~
                ' when another method with this name was already declared');
        }
        elsif $*MULTINESS || %table{$name}<multis> {
            # If no multi declarator and no proto, error.
            if !$*MULTINESS && !%table{$name}<proto> {
                $/.CURSOR.panic('Can not re-declare method ' ~ $name ~ ' without declaring it multi');
            }

            # If it's a proto, stash it away in the symbol entry.
            if $*MULTINESS eq 'proto' { %table{$name}<proto> := $past; }

            # Otherwise, create multi container if we don't have one; otherwise,
            # just push this candidate onto it.
            if %table{$name}<multis> {
                %table{$name}<multis>.push($past);
            }
            else {
                $past := PAST::Op.new(
                    :pasttype('callmethod'),
                    :name('set_candidates'),
                    PAST::Op.new( :inline('    %r = new ["Perl6MultiSub"]') ),
                    $past
                );
                %table{$name}<code_ref> := %table{$name}<multis> := $past;
            }
            $multi_flag.value(1);
        }
        else {
            %table{$name}<code_ref> := $past;
        }

        # Added via meta-class; needn't add anything.
        $past := PAST::Stmts.new();
    }
    elsif $*MULTINESS {
        $/.CURSOR.panic('Can not put ' ~ $*MULTINESS ~ ' on anonymous routine');
    }
    else {
        $past := create_code_object($past, 'Method', 0);
    }

    make $past;
}


method signature($/) {
    my $signature := Perl6::Compiler::Signature.new();
    for $<parameter> {
        my $param := $_.ast;
        $param.multi_invocant(1);
        $signature.add_parameter($param);
    }
    make $signature;
}

method parameter($/) { 
    my $quant := $<quant>;
    
    # Sanity checks.
    if $<default_value> { 
        if $quant eq '*' { 
            $/.CURSOR.panic("Can't put default on slurpy parameter");
        }
        if $quant eq '!' { 
            $/.CURSOR.panic("Can't put default on required parameter");
        }
    }

    # Set various flags on the parameter.
    $*PARAMETER.pos_slurpy( $quant eq '*' && $*PARAMETER.sigil eq '@' );
    $*PARAMETER.named_slurpy( $quant eq '*' && $*PARAMETER.sigil eq '%' );
    $*PARAMETER.optional( $quant eq '?' || $<default_value> || ($<named_param> && $quant ne '!') );
    if $<default_value> {
        $*PARAMETER.default( PAST::Block.new( $<default_value>[0]<EXPR>.ast ) );
    }

    # Handle traits.
    if $<trait> {
        # Handle built-in ones.
        my $read_type := trait_readtype($<trait>);
        if $read_type eq 'CONFLICT' {
            $/.CURSOR.panic('Can not apply more than one of: is ref, is copy, is rw, is readonly');
        }
        $*PARAMETER.is_rw( $read_type eq 'rw' );
        $*PARAMETER.is_ref( $read_type eq 'ref' );
        $*PARAMETER.is_copy( $read_type eq 'copy' );
    }

    make $*PARAMETER;
}

method param_var($/) {
    if $<signature> {
        if pir::defined__IP($*PARAMETER.sub_signature) {
            $/.CURSOR.panic('Can not have more than one sub-signature for a parameter');
        }
        $*PARAMETER.sub_signature( $<signature>.ast );
        if pir::substr(~$/, 0, 1) eq '[' {
            $*PARAMETER.var_name('@');
        }
    }
    else {
        $*PARAMETER.var_name(~$/);
    }
}

method named_param($/) {
    if $<name>               { $*PARAMETER.names.push(~$<name>); }
    if $<param_var><name>    { $*PARAMETER.names.push(~$<param_var><name>); }
}

method type_constraint($/) {
    if $<typename> {
        if pir::substr(~$<typename>, 0, 2) eq '::' {
            $*PARAMETER.type_captures.push(pir::substr(~$<typename>, 2));
        }
        else {
            if $*PARAMETER.nom_type {
                $/.CURSOR.panic('Parameter may only have one prefix type constraint');
            }
            $*PARAMETER.nom_type($<typename>.ast);
        }
    }
    else {
        $/.CURSOR.panic('Can not do non-typename cases of type_constraint yet');
    }
}

method post_constraint($/) {
    if $<signature> {
        if pir::defined__IP($*PARAMETER.sub_signature) {
            $/.CURSOR.panic('Can not have more than one sub-signature for a parameter');
        }
        $*PARAMETER.sub_signature( $<signature>.ast );
    }
    else {
        my $past := $<EXPR>.ast;
        unless $past.isa(PAST::Block) {
            $/.CURSOR.panic('Non-block anonymous sub-types su todo');
        }
        $*PARAMETER.cons_types.push($past);
    }
}

method trait($/) {
    my $past;
    if $<trait_mod> {
        $past := $<trait_mod>.ast;
    }
    elsif $<colonpair> {
        $/.panic('traits specified as colon pairs not yet understood');
    }
    make $past;
}

method trait_mod:sym<is>($/) {
    my $trait := PAST::Op.new( :pasttype('call'), :name('&trait_mod:<is>') );
    if $<circumfix> { $trait.push($<circumfix>[0].ast); }
    
    if $/.CURSOR.is_name(~$<longname>) {
        # It's a type - look it up and send it in as a positional, before
        # the parameter.
        my @name := Perl6::Grammar::parse_name(~$<longname>);
        $trait.unshift(PAST::Var.new(
            :scope('package'),
            :name(@name.pop()),
            :namespace(@name)
        ));
    }
    else {
        # Not a type name, so construct a named parameter with this name; it
        # is a named param so it has to go on the end.
        $trait.push(PAST::Var.new(
            :name('True'),
            :namespace('Bool'),
            :scope('package'),
            :named(~$<longname>)
        ));
    }
    
    $trait<is_name> := ~$<longname>;
    make $trait;
}

method trait_mod:sym<hides>($/) {
    make PAST::Op.new(
        :pasttype('call'),
        :name('&trait_mod:<hides>'),
        $<module_name>.ast
    );
}

method trait_mod:sym<does>($/) {
    make PAST::Op.new(
        :pasttype('call'),
        :name('&trait_mod:<does>'),
        $<module_name>.ast
    );
}

method trait_mod:sym<will>($/) {
    my $trait := PAST::Op.new(
        :pasttype('call'),
        :name('&trait_mod:will'),
        $<pblock>.ast
    );

    if $/.CURSOR.is_name(~$<identifier>) {
        # It's a type - look it up and send it in as a positional, before
        # the parameter.
        $trait.unshift(PAST::Var.new(
            :scope('package'),
            :name(~$<identifier>)
        ));
    }
    else {
        # Not a type name, so construct a named parameter with this name; it
        # is a named param so it has to go on the end.
        $trait.push(PAST::Val.new(
            :value(PAST::Var.new( :name('True'), :namespace('Bool'), :scope('package') )),
            :named(~$<identifier>)
        ));
    }

    make $trait;
}

method trait_mod:sym<of>($/) {
    make PAST::Op.new(
        :pasttype('call'),
        :name('&trait_mod:<of>'),
        $<typename>.ast
    );
}

method trait_mod:sym<as>($/) {
    make PAST::Op.new(
        :pasttype('call'),
        :name('&trait_mod:<as>'),
        $<typename>.ast
    );
}

method trait_mod:sym<returns>($/) {
    make PAST::Op.new(
        :pasttype('call'),
        :name('&trait_mod:<returns>'),
        $<typename>.ast
    );
}

method trait_mod:sym<handles>($/) {
    make PAST::Op.new(
        :pasttype('call'),
        :name('&trait_mod:<handles>'),
        $<term>.ast
    );
}

method regex_declarator($/, $key?) {
    my @MODIFIERS := Q:PIR {
        %r = get_hll_global ['Regex';'P6Regex';'Actions'], '@MODIFIERS'
    };
    my $name := ~$<deflongname>.ast;
    my $past;
    if $key eq 'open' {
        my %h;
        if $<sym> eq 'token' { %h<r> := 1; }
        if $<sym> eq 'rule'  { %h<r> := 1;  %h<s> := 1; }
        @MODIFIERS.unshift(%h);
        Q:PIR {
            $P0 = find_lex '$name'
            set_hll_global ['Regex';'P6Regex';'Actions'], '$REGEXNAME', $P0
        };
        @BLOCK[0].symbol('$¢', :scope('lexical'));
        @BLOCK[0].symbol('$/', :scope('lexical'));
        return 0;
    }
    elsif $<proto> {
        $past :=
            PAST::Stmts.new(
                PAST::Block.new( :name($name),
                    PAST::Op.new(
                        PAST::Var.new( :name('self'), :scope('register') ),
                        $name,
                        :name('!protoregex'),
                        :pasttype('callmethod'),
                    ),
                    :blocktype('method'),
                    :lexical(0),
                    :node($/)
                ),
                PAST::Block.new( :name('!PREFIX__' ~ $name),
                    PAST::Op.new(
                        PAST::Var.new( :name('self'), :scope('register') ),
                        $name,
                        :name('!PREFIX__!protoregex'),
                        :pasttype('callmethod'),
                    ),
                    :blocktype('method'),
                    :lexical(0),
                    :node($/)
                )
            );
    }
    else {
        my $rpast := $<p6regex>.ast;
        my %capnames := Regex::P6Regex::Actions::capnames($rpast, 0);
        %capnames{''} := 0;
        $rpast := PAST::Regex.new(
                     $rpast,
                     PAST::Regex.new( :pasttype('pass') ),
                     :pasttype('concat'),
                     :capnames(%capnames)
        );
        $past := @BLOCK.shift;
        $past.blocktype('method');
        $past.name($name);
        $past.push($rpast);
        @MODIFIERS.shift;
    }
    make $past;
}


method dotty($/) {
    my $past := $<args> ?? $<args>[0].ast !! PAST::Op.new( :node($/) );
    $past.name( ~$<identifier> );
    $past.pasttype('callmethod');
    make $past;
}

## Terms

method term:sym<self>($/) {
    make PAST::Var.new( :name('self') );
}

method term:sym<identifier>($/) {
    my $past := $<args>.ast;
    $past.name('&' ~ $<identifier>);
    make $past;
}

method term:sym<name>($/) {
    my $ns := Perl6::Grammar::parse_name(~$<longname>);
    $ns := pir::clone__PP($ns);
    my $name := $ns.pop;
    my $var := 
        PAST::Var.new( :name(~$name), :namespace($ns), :scope('package') );
    my $past := $var;
    if $<args> {
        $past := $<args>.ast;
        if $ns { $past.unshift($var); }
        else { $past.name('&' ~ $name); }
    }
    make $past;
}

method term:sym<pir::op>($/) {
    my $past := $<args> ?? $<args>[0].ast !! PAST::Op.new( :node($/) );
    my $pirop := ~$<op>;
    $pirop := Q:PIR {
        $P0 = find_lex '$pirop'
        $S0 = $P0
        $P0 = split '__', $S0
        $S0 = join ' ', $P0
        %r = box $S0
    };
    $past.pirop($pirop);
    $past.pasttype('pirop');
    make $past;
}

method term:sym<*>($/) {
    my @name := Perl6::Grammar::parse_name('Whatever');
    make PAST::Op.new(
        :pasttype('callmethod'), :name('new'), :node($/), :lvalue(1),
        PAST::Var.new( :name(@name.pop), :namespace(@name), :scope('package') )
    )
}

method args($/) { 
    my $past;
    if    $<semiarglist> { $past := $<semiarglist>.ast; }
    elsif $<arglist>     { $past := $<arglist>.ast; }
    else {
        $past := PAST::Op.new( :pasttype('call'), :node($/) );
    }
    make $past;
}

method semiarglist($/) { make $<arglist>.ast; }

method arglist($/) {
    my $past := PAST::Op.new( :pasttype('call'), :node($/) );
    if $<EXPR> {
        my $expr := $<EXPR>.ast;
        if $expr.name eq '&infix:<,>' && !$expr.named {
            for $expr.list { $past.push($_); }
        }
        else { $past.push($expr); }
    }
    make $past;
}


method term:sym<value>($/) { make $<value>.ast; }

method circumfix:sym<( )>($/) { make $<EXPR>.ast; }

method circumfix:sym<ang>($/) { make $<quote_EXPR>.ast; }

method circumfix:sym<{ }>($/) { make $<pblock>.ast; }

method circumfix:sym<[ ]>($/) {
    make PAST::Op.new( :name('&circumfix:<[ ]>'), $<EXPR>.ast, :node($/) );
}

method circumfix:sym<sigil>($/) {
    my $name := ~$<sigil> eq '@' ?? 'list' !!
                ~$<sigil> eq '%' ?? 'hash' !!
                                    'item';
    make PAST::Op.new( :pasttype('callmethod'), :name($name), $<semilist>.ast );
}

method semilist($/) { make $<statement>.ast }

method infixish($/) {
    if $<infix_postfix_meta_operator> {
        my $sym := ~$<infix><sym>;
        my $opsub := "&infix:<$sym=>";
        unless %*METAOPGEN{$opsub} {
            @BLOCK[0].loadinit.push(
                PAST::Op.new( :name('!gen_assign_metaop'), $sym, 
                              :pasttype('call') )
            );
            %*METAOPGEN{$opsub} := 1;
        }
        make PAST::Op.new( :name($opsub), :pasttype('call') );
    }
}

method postcircumfix:sym<[ ]>($/) {
    make PAST::Op.new( $<EXPR>.ast, :name('!postcircumfix:<[ ]>'),
                       :pasttype('call'), :node($/) );
}

method postcircumfix:sym<{ }>($/) {
    make PAST::Var.new( $<EXPR>.ast , :scope('keyed'),
                        :viviself('Undef'),
                        :vivibase('Hash') );
}

method postcircumfix:sym<ang>($/) {
    make PAST::Var.new( $<quote_EXPR>.ast, :scope('keyed'),
                        :viviself('Undef'),
                        :vivibase('Hash') );
}

method postcircumfix:sym<( )>($/) {
    make $<arglist>.ast;
}

method value:sym<quote>($/) {
    make $<quote>.ast;
}

method value:sym<number>($/) {
    make $<number>.ast;
}

method number:sym<rational>($/) {
    make PAST::Op.new(
        :pasttype('callmethod'), :name('new'),
        PAST::Var.new( :name('Rat'), :namespace(''), :scope('package') ),
        $<nu>.ast, $<de>.ast
    );
}

method number:sym<complex>($/) {
    make PAST::Op.new(
        :pasttype('callmethod'), :name('new'),
        PAST::Var.new( :name('Complex'), :namespace(''), :scope('package') ),
        ($<re> ?? $<re>.ast !! 0), $<im>.ast
    );
}

method number:sym<numish>($/) {
    make $<numish>.ast;
}

method numish($/) {
    if $<integer> { make PAST::Val.new( :value($<integer>.ast) ); }
    elsif $<dec_number> { make $<dec_number>.ast; }
    else {
        $/.CURSOR.panic('Number parsing not fully implemented yet');
    }
}

method dec_number($/) {
    my $int  := $<int> ?? $<int>.ast !! 0;
    my $frac := $<frac> ?? $<frac>.ast !! 0;
    my $base := Q:PIR {
        $P0 = find_lex '$/'
        $S0 = $P0['frac']
        $I1 = length $S0
        $I0 = 0
        $I2 = 1
      loop:
        unless $I0 < $I1 goto done
        $S1 = substr $S0, $I0, 1
        inc $I0
        if $S1 == '_' goto loop
        $I2 *= 10
        goto loop
      done:
        %r = box $I2
    };
    if $<escale> {
        my $exp := $<escale>[0]<decint>.ast;
        if $<escale>[0]<sign> eq '-' { $exp := -$exp; }
        make PAST::Val.new( 
            :value(($int * $base + $frac) / $base * 10 ** +$exp ) 
        );
    }
    else {
        make PAST::Op.new(
            :pasttype('callmethod'), :name('new'),
            PAST::Var.new( :name('Rat'), :namespace(''), :scope('package') ),
            $int * $base + $frac, $base, :node($/)
        );
    }
}

method typename($/) {
    my @name := Perl6::Grammar::parse_name($<longname>.Str);
    my $past := PAST::Var.new(
        :name(@name.pop),
        :namespace(@name),
        :scope('package')
    );
    make $past;
}

method quote:sym<apos>($/) { make $<quote_EXPR>.ast; }
method quote:sym<dblq>($/) { make $<quote_EXPR>.ast; }
method quote:sym<qq>($/)   { make $<quote_EXPR>.ast; }
method quote:sym<q>($/)    { make $<quote_EXPR>.ast; }
method quote:sym<Q>($/)    { make $<quote_EXPR>.ast; }
method quote:sym<Q:PIR>($/) {
    make PAST::Op.new( :inline( $<quote_EXPR>.ast.value ),
                       :pasttype('inline'),
                       :node($/) );
}

method quote_escape:sym<$>($/) { make $<variable>.ast; }

# overrides versions from HLL::Actions to handle Perl6Str
# and use &infix:<,> to build the parcel
method quote_EXPR($/) {
    my $past := $<quote_delimited>.ast;
    if HLL::Grammar::quotemod_check($/, 'w') {
        if !$past.isa(PAST::Val) {
            $/.CURSOR.panic("Can't form :w list from non-constant strings (yet)");
        }
        else {
            my @words := HLL::Grammar::split_words($/, $past.value);
            if +@words > 1 {
                $past := PAST::Op.new( :name('&infix:<,>'), :node($/) );
                for @words { $past.push($_); }
            }
        }
    }
    make $past;
}
     

method quote_delimited($/) {
    my @parts;
    my $lastlit := '';
    for $<quote_atom> {
        my $ast := $_.ast;
        if !HLL::Actions::isaPAST($ast) {
            $lastlit := $lastlit ~ $ast;
        }
        elsif $ast.isa(PAST::Val) {
            $lastlit := $lastlit ~ $ast.value;
        }
        else {
            if $lastlit gt '' {
                @parts.push(
                    PAST::Val.new( :value($lastlit), :returns('Perl6Str') ) 
                ); 
            }
            @parts.push($ast);
            $lastlit := '';
        }
    }
    if $lastlit gt '' || !@parts { 
        @parts.push(
            PAST::Val.new( :value($lastlit), :returns('Perl6Str') ) 
        ); 
    }
    my $past := @parts.shift;
    while @parts {
        $past := PAST::Op.new( $past, @parts.shift, :pirop('concat') );
    }
    make $past;
}


## Operators

method nulltermish($/) {
    make $<term> ?? $<term>.ast !! 0;
}

method postfix:sym<.>($/) { make $<dotty>.ast; }


class Perl6::RegexActions is Regex::P6Regex::Actions {

    method metachar:sym<:my>($/) {
        my $past := $<statement>.ast;
        make PAST::Regex.new( $past, :pasttype('pastnode') );
    }

    method metachar:sym<{ }>($/) { make $<codeblock>.ast; }

    method assertion:sym<{ }>($/) { make $<codeblock>.ast; }

    method codeblock($/) {
        my $block := $<block>.ast;
        $block.blocktype('immediate');
        my $past := 
            PAST::Regex.new(
                PAST::Stmts.new(
                    PAST::Op.new(
                        PAST::Var.new( :name('$/') ),
                        PAST::Op.new(
                            PAST::Var.new( :name('$¢') ),
                            :name('MATCH'),
                            :pasttype('callmethod')
                        ),
                        :pasttype('bind')
                    ),
                    $block
                ),
                :pasttype('pastnode')
            );
        make $past;
    }
}

# Takes a block and adds a signature ot it, as well as code to bind the call
# capture against the signature.
sub add_signature($block, $sig_obj) {
    # Add signature building code to the block.
    $block.loadinit.push($sig_obj.ast);
    $block.loadinit.push(PAST::Op.new( :inline('    setprop block, "$!signature", signature') ));

    # Add call to signature binder as well as lexical declarations
    # to the start of the block.
    $block[0].push(PAST::Var.new( :name('call_sig'), :scope('parameter'), :call_sig(1) ));
    my $decls := $sig_obj.get_declarations();
    for @($decls) { $block.symbol( $_.name, :scope('lexical') ) }
    $block[0].push($decls);
    $block[0].push(PAST::Op.new(
        :pirop('bind_signature vP'),
        PAST::Var.new( :name('call_sig'), :scope('lexical') )
    ));
}

# Wraps a sub up in a block type.
sub create_code_object($block, $type, $multiness) {
    my @name := Perl6::Grammar::parse_name($type);
    PAST::Op.new(
        :pasttype('callmethod'),
        :name('new'),
        PAST::Var.new( :name(@name.pop), :namespace(@name), :scope('package') ),
        $block,
        $multiness
    );
}

# This routine checks if the given list of traits contains one of the given
# name. If so, it marks it as compiler handled so no multi call will be
# emitted when we emit the traits. If there is such a trait, it returns it's
# AST.
sub has_compiler_trait($trait_list, $name) {
    if $trait_list {
        for $trait_list {
            my $ast := $_.ast;
            if $ast.name eq $name {
                $ast<trait_is_compiler_handled> := 1;
                return $ast;
            }
        }
    }
    return 0;
}


# This routine checks if the given list of traits contains one of the given
# names and also that it carries the given value as a named parameter. If so,
# it marks it as compiler handled so no multi call will be emitted when we emit
# the traits. If there is such a trait, it returns it's AST.
sub has_compiler_trait_with_val($trait_list, $name, $value) {
    if $trait_list {
        for $trait_list {
            my $ast := $_.ast;
            if $ast.name eq $name && $ast<is_name> eq $value {
                $ast<trait_is_compiler_handled> := 1;
                return $ast;
            }
        }
    }
    return 0;
}


# Emits routine traits into the loadinit for the routine.
sub emit_routine_traits($routine, @trait_list, $type) {
    $routine.loadinit.push(PAST::Op.new(
        :pasttype('bind'),
        PAST::Var.new( :name('trait_subject'), :scope('register'), :isdecl(1) ),
        create_code_object(PAST::Var.new( :name('block'), :scope('register') ), $type, 0)
    ));
    for @trait_list {
        my $ast := $_.ast;
        $ast.unshift(PAST::Var.new( :name('trait_subject'), :scope('register') ));
        $routine.loadinit.push($ast);
    }
}


# Finds out which readtype trait we have, and marks all of the relevant ones
# as compiler handled.
sub trait_readtype($traits) {
    my $readtype;
    if has_compiler_trait_with_val($traits, '&trait_mod:<is>', 'readonly') {
        $readtype := 'readonly';
    }
    if has_compiler_trait_with_val($traits, '&trait_mod:<is>', 'rw') {
        $readtype := $readtype ?? 'CONFLICT' !! 'rw';
    }
    if has_compiler_trait_with_val($traits, '&trait_mod:<is>', 'copy') {
        $readtype := $readtype ?? 'CONFLICT' !! 'copy';
    }
    if has_compiler_trait_with_val($traits, '&trait_mod:<is>', 'ref') {
        $readtype := $readtype ?? 'CONFLICT' !! 'ref';
    }
    $readtype;
}


# Handles trait node on a block and setting up the ContainerDeclarand.
sub get_var_traits_node($block, $name) {
    # Do we already have a traits node?
    my %symtab := $block.symbol($name);
    if %symtab<traits_node> {
        return %symtab<traits_node>;
    }

    # Create one, in the viviself.
    my $decl := %symtab<decl_node>;
    my @cd_name := Perl6::Grammar::parse_name('ContainerDeclarand');
    my $traits_node := PAST::Stmts.new();
    my $vivinode := PAST::Stmts.new(
        PAST::Op.new( :pasttype('bind'),
            PAST::Var.new( :name('$P0'), :scope('register') ),
            $decl.viviself()
        ),
        PAST::Op.new( :pasttype('bind'),
            PAST::Var.new( :name('declarand'), :scope('register'), :isdecl(1) ),
            PAST::Op.new(
                :pasttype('callmethod'), :name('new'),
                PAST::Var.new( :name(@cd_name.pop), :namespace(@cd_name), :scope('package') ),
                PAST::Var.new( :name('$P0'), :scope('register') ),
                PAST::Val.new( :value($name), :named('name') )
            )
        ),
        $traits_node,
        PAST::Op.new( :inline('    %r = $P0') )
    );
    $decl.viviself($vivinode);
    $block.symbol($name, :traits_node($traits_node));
    $traits_node;
}

sub add_implicit_var($block, $name, $outer) {
    my $base := $outer
                ?? PAST::Op.new( :inline("    %r = new ['Perl6Scalar'], %0"),
                       PAST::Op.new(:pirop('find_lex_skip_current Ps'), $name)
                   )
                !! PAST::Op.new( :inline("    %r = new ['Perl6Scalar']") );
    $base := PAST::Op.new( $base, 'rw', $TRUE, :pirop('setprop') );
    $block[0].push(
        PAST::Var.new( :name($name), :scope('lexical'), :isdecl(1),
                       :viviself($base) )
    );
    $block.symbol($name, :scope('lexical') );
}

sub when_handler_helper($block) {
    our @BLOCK;
    my $BLOCK := @BLOCK[0];
    # XXX TODO: This isn't quite the right way to check this...
    unless $BLOCK.handlers() {
        my @handlers;
        @handlers.push(
            PAST::Control.new(
                PAST::Op.new(
                    :pasttype('pirop'),
                    :pirop('return'),
                    PAST::Var.new(
                        :scope('keyed'),
                        PAST::Var.new( :name('exception'), :scope('register') ),
                        'payload',
                    ),
                ),
                :handle_types('BREAK')
            )
        );
        $BLOCK.handlers(@handlers);
    }

    # push a control exception throw onto the end of the block so we
    # exit the innermost block in which $_ was set.
    my $last := $block.pop();
    $block.push(
        PAST::Op.new(
            :pasttype('call'),
            :name('break'),
            $last
        )
    );

    # Push a handler onto the block to handle CONTINUE exceptions so we can
    # skip throwing the BREAK exception
    my @handlers;
    if $block.handlers() {
        @handlers := $block.handlers();
    }
    @handlers.push(
        PAST::Control.new(
            PAST::Op.new(
                :pasttype('pirop'),
                :pirop('return'),
            ),
            :handle_types('CONTINUE')
        )
    );
    $block.handlers(@handlers);
}
