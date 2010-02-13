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
         ?? ~$<name> ~ ':<' ~ ~$<colonpair>[0]<circumfix><quote_EXPR><quote_delimited><quote_atom>[0] ~ '>'
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

    # Make sure we have a clean @PACKAGE for next time.
    @PACKAGE.shift;
    @PACKAGE.unshift(Perl6::Compiler::Module.new());

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
            elsif $ast<past_block> && !$ast<past_block>.blocktype {
                $ast := block_immediate($ast<past_block>);
            }
            $past.push( $ast ); 
        }
    }
    make $past;
}

method semilist($/) {
    my $past := PAST::Stmts.new( :node($/) );
    if $<statement> {
        for $<statement> { $past.push($_.ast); }
    }
    else { $past.push( PAST::Op.new( :name('&Nil') ) ); }
    make $past;
}

method statement($/, $key?) {
    my $past;
    if $<EXPR> {
        my $mc := $<statement_mod_cond>[0];
        my $ml := $<statement_mod_loop>[0];
        $past := $<EXPR>.ast;
        if $mc {
            $past := PAST::Op.new($mc<cond>.ast, $past, PAST::Op.new(:name('&Nil')), 
                        :pasttype(~$mc<sym>), :node($/) );
        }
        if $ml {
            $past := PAST::Op.new($ml<cond>.ast, $past, :pasttype(~$ml<sym>), :node($/) );
        }
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
    if pir::defined__IP($block<placeholder_sig>) && $<signature> {
        $/.CURSOR.panic('Placeholder variable cannot override existing signature');
    }
    elsif pir::defined__IP($block<placeholder_sig>) { 
        $signature := $block<placeholder_sig>; 
    }
    elsif $<signature> { 
        $signature := $<signature>.ast; 
        $block.blocktype('declaration'); 
    }
    else {
        $signature := Perl6::Compiler::Signature.new();
        unless $block.symbol('$_') {
            if $*IMPLICIT {
                my $parameter := Perl6::Compiler::Parameter.new();
                $parameter.var_name('$_');
                $parameter.optional(1);
                $parameter.is_parcel(1);
                $parameter.default_from_outer(1);
                $signature.add_parameter($parameter);
            }
            else {
                add_implicit_var($block, '$_', 1);
            }
        }
    }
    if $<lambda> eq '<->' {
        $signature.set_rw_by_default();
    }
    add_signature($block, $signature, 0);
    if $<lambda> {
        prevent_null_return($block);
    }
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

method outerlex($/) {
    my $outer_ctx := %*COMPILING<%?OPTIONS><outer_ctx>;
    if pir::defined__IP($outer_ctx) {
        my $block := @BLOCK[0];
        my %lexinfo := Perl6::Compiler.get_lexinfo($outer_ctx);
        for %lexinfo { $block.symbol($_.key, :scope<lexical>); }
        my @ns := pir::getattribute__PPs($outer_ctx, 'current_namespace').get_name;
        @ns.shift;
        $block.namespace(@ns);
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
    my $past := xblock_immediate($<xblock>.ast);
    $past.pasttype('for');
    $past[0] := PAST::Op.new(:name('&eager'), $past[0]);
    $past.arity($past[1].arity || 1);
    make $past;
}

method statement_control:sym<loop>($/) {
    my $block := $<block>.ast;
    $block.blocktype('immediate');
    my $cond := $<e2> ?? $<e2>[0].ast !! 1;
    my $loop := PAST::Op.new( $cond, $block, :pasttype('while'), :node($/) );
    if $<e3> {
        $loop.push( $<e3>[0].ast );
    }
    if $<e1> {
        $loop := PAST::Stmts.new( $<e1>[0].ast, $loop, :node($/) );
    }
    make $loop;
}

method statement_control:sym<use>($/) {
    my $past := PAST::Stmts.new( :node($/) );
    if $<module_name> {
        if ~$<module_name> eq 'fatal' {
            declare_variable($/, PAST::Stmts.new(), '$', '*', 'FATAL', 0);
            $past := PAST::Op.new(
                :name('&infix:<=>'),
                :node($/),
                PAST::Op.new(
                    :name('!find_contextual'),
                    :pasttype('call'),
                    :lvalue(0),
                    '$*FATAL',
                ),
                PAST::Var.new(
                    :name('True'),
                    :namespace(['Bool']),
                    :scope('package'),
                ),
            );
        }
        else {
            @BLOCK[0][0].unshift(
                PAST::Op.new( :name('!use'), ~$<module_name>, :node($/) )
            );
        }
    }
    make $past;
}

method statement_control:sym<return>($/) {
    my $retval := $<EXPR> ?? $<EXPR>[0].ast !! PAST::Op.new( :name('&Nil') );
    make PAST::Op.new( $retval, :pasttype('return'), :node($/) );
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

method statement_control:sym<CATCH>($/) {
    my $block := $<block>.ast;
    push_block_handler($/, $block);
    @BLOCK[0].handlers()[0].handle_types_except('CONTROL');
    make PAST::Stmts.new(:node($/));
}

method statement_control:sym<CONTROL>($/) {
    my $block := $<block>.ast;
    push_block_handler($/, $block);
    @BLOCK[0].handlers()[0].handle_types('CONTROL');
    make PAST::Stmts.new(:node($/));
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

method statement_prefix:sym<gather>($/) {
    my $past := $<blorst>.ast;
    $past.blocktype('declaration');
    make PAST::Op.new( :pasttype('call'), :name('!GATHER'), $past );
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

method statement_mod_cond:sym<if>($/)     { make $<cond>.ast; }
method statement_mod_cond:sym<unless>($/) { make $<cond>.ast; }

method statement_mod_loop:sym<while>($/)  { make $<cond>.ast; }
method statement_mod_loop:sym<until>($/)  { make $<cond>.ast; }

## Terms

method term:sym<fatarrow>($/)           { make $<fatarrow>.ast; }
method term:sym<colonpair>($/)          { make $<colonpair>.ast; }
method term:sym<variable>($/)           { make $<variable>.ast; }
method term:sym<package_declarator>($/) { make $<package_declarator>.ast; }
method term:sym<scope_declarator>($/)   { make $<scope_declarator>.ast; }
method term:sym<routine_declarator>($/) { make $<routine_declarator>.ast; }
method term:sym<multi_declarator>($/)   { make $<multi_declarator>.ast; }
method term:sym<regex_declarator>($/)   { make $<regex_declarator>.ast; }
method term:sym<statement_prefix>($/)   { make $<statement_prefix>.ast; }
method term:sym<lambda>($/)             { make create_code_object($<pblock>.ast, 'Block', 0, ''); }

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

method fatarrow($/) {
    make make_pair($<key>.Str, $<val>.ast);
}

method colonpair($/) {
    if $*key {
        if $<var> {
            make make_pair($*key, make_variable($/, ~$<var>));
        }
        elsif $*value ~~ Regex::Match {
            make make_pair($*key, $*value.ast);
        }
        elsif $*value == 0 {
            make make_pair($*key, PAST::Var.new( :name('False'), :namespace('Bool'), :scope('package') ));
        }
        else {
            make make_pair($*key, PAST::Var.new( :name('True'), :namespace('Bool'), :scope('package') ));
        }
    }
    else {
        make $*value.ast;
    }
}

sub make_pair($key, $value) {
    my @name := Perl6::Grammar::parse_name('Pair');
    $value.named('value');
    PAST::Op.new(
        :pasttype('callmethod'),
        :returns('Pair'),
        :name('new'),
        PAST::Var.new( :name(@name.pop), :namespace(@name), :scope('package') ),
        PAST::Val.new( :value($key), :named('key') ),
        $value
    )
}

method variable($/) {
    my $past;
    if $<index> {
        $past := PAST::Op.new(
            :name('!postcircumfix:<[ ]>'),
            PAST::Var.new( :name('$/') ),
            +$<index>
        );
    }
    elsif $<postcircumfix> {
        $past := $<postcircumfix>.ast;
        $past.unshift( PAST::Var.new( :name('$/') ) );
    }
    else {
        $past := make_variable($/, ~$/);
    }
    make $past;
}

sub make_variable($/, $name) {
    my @name := Perl6::Grammar::parse_name($name);
    my $past := PAST::Var.new( :name(@name.pop) );
    if @name {
        $past.namespace(@name);
        $past.scope('package');
    }
    if $<twigil>[0] eq '*' { 
        $past := PAST::Op.new( $past.name(), :pasttype('call'), :name('!find_contextual'), :lvalue(0) );
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
    elsif $<twigil>[0] eq '^' || $<twigil>[0] eq ':' {
        $past := add_placeholder_parameter($<sigil>.Str, $<desigilname>.Str, :named($<twigil>[0] eq ':'));
    }
    elsif ~$/ eq '@_' {
        $past := add_placeholder_parameter('@', '_', :slurpy_pos(1));
    }
    elsif ~$/ eq '%_' {
        $past := add_placeholder_parameter('%', '_', :slurpy_named(1));
    }
    else {
        my $attr_alias := is_attr_alias($past.name);
        if $attr_alias {
            $past.name($attr_alias);
            $past.scope('attribute');
            $past.viviself( sigiltype( $<sigil> ) );
            $past.unshift(PAST::Var.new( :name('self'), :scope('lexical') ));
        }
    }
    $past
}

method package_declarator:sym<package>($/) { make $<package_def>.ast; }
method package_declarator:sym<module>($/)  { make $<package_def>.ast; }
method package_declarator:sym<class>($/)   { make $<package_def>.ast; }
method package_declarator:sym<grammar>($/) { make $<package_def>.ast; }
method package_declarator:sym<role>($/)    { make $<package_def>.ast; }

method package_declarator:sym<does>($/) {
    our @PACKAGE;
    @PACKAGE[0].traits.push(PAST::Op.new(
        :pasttype('call'),
        :name('&trait_mod:<does>'),
        $<typename>.ast
    ));
    make PAST::Stmts.new();
}

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
                $/.CURSOR.add_name($name);
                $package.name($name);
            }
            if $<def_module_name>[0]<signature> {
                $package.signature($<def_module_name>[0]<signature>[0].ast);
                $package.signature_text(~$<def_module_name>[0]<signature>[0]);
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
        if pir::substr__SSII($<block><blockoid><statementlist><statement>[0], 0, 3) eq '...' {
            # Just a stub, so don't do any more work.
            if $*SCOPE eq 'our' || $*SCOPE eq '' {
                %Perl6::Grammar::STUBCOMPILINGPACKAGES{~$<def_module_name>[0]<longname>} := 1;
            }
            @BLOCK[0].symbol(~$<def_module_name>[0]<longname>, :stub(1));
            make PAST::Stmts.new( );
        }
        else {
            my $block;
            if $<block> {
                $block := $<block>.ast;
            }
            else {
                $block := @BLOCK.shift;
                $block.push($<statementlist>.ast);
                $block.node($/);
            }
            make $package.finish($block);
        }
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
    elsif $<signature> {
        my $list  := PAST::Op.new( :pasttype('call'), :name('&infix:<,>') );
        my $decls := $<signature>.ast.get_declarations;
        for @($decls) {
            $list.push(declare_variable($/, $_, $_<sigil>, $_<twigil>, $_<desigilname>, $_<traits>));
        }
        make $list;
    }
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
    my $name := ~$sigil ~ ~$twigil ~ ~$<variable><desigilname>;
    if @BLOCK[0].symbol($name) {
        $/.CURSOR.panic("Redeclaration of symbol ", $name);
    }
    make declare_variable($/, $past, ~$sigil, ~$twigil, ~$<variable><desigilname>, $<trait>);
}

sub declare_variable($/, $past, $sigil, $twigil, $desigilname, $trait_list) {
    my $name  := $sigil ~ $twigil ~ $desigilname;
    my $BLOCK := @BLOCK[0];

    if $*SCOPE eq 'has' {
        # Find the current package and add the attribute.
        my $attrname := ~$sigil ~ '!' ~ $desigilname;
        our @PACKAGE;
        unless +@PACKAGE { $/.CURSOR.panic("Can not declare attribute outside of a package"); }
        my %attr_table := @PACKAGE[0].attributes;
        if %attr_table{$attrname} { $/.CURSOR.panic("Can not re-declare attribute " ~ $attrname); }
        %attr_table{$attrname} := Q:PIR { %r = new ['Hash'] };
        %attr_table{$attrname}<name>     := $attrname;
        %attr_table{$attrname}<accessor> := $twigil eq '.' ?? 1 !! 0;
        %attr_table{$attrname}<rw>       := $trait_list && has_compiler_trait_with_val($trait_list, '&trait_mod:<is>', 'rw') ?? 1 !! 0;

        # If no twigil, note $foo is an alias to $!foo.
        if $twigil eq '' {
            $BLOCK.symbol($name, :attr_alias($attrname));
        }

        # Nothing to emit here; just hand  back an empty node, but also
        # annotate it with the attribute table.
        $past := PAST::Stmts.new( );
        $past<attribute_data> := %attr_table{$attrname};
    }
    else {
        # Not an attribute - need to emit delcaration here.
        # First, create a container and give it a 'rw' property
        # Create the container, give it a 'rw' property
        my $cont := $sigil eq '%' ??
            PAST::Op.new( :name('&CREATE_HASH_LOW_LEVEL'), :pasttype('call') ) !!
            PAST::Op.new( sigiltype($sigil), :pirop('new Ps') );
        my $true := PAST::Var.new( :name('true'), :scope('register') );
        my $vivipast := PAST::Op.new( $cont, 'rw', $true, :pirop('setprop'));

        # If it's a scalar, mark it as scalar (non-flattening)
        if $sigil eq '$' {
            $vivipast := PAST::Op.new($vivipast,'scalar',$true,:pirop('setprop'));
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
        my $init_type := 0;
        if $trait_list || $*TYPENAME {
            my $trait_node := get_var_traits_node($BLOCK, $name);
            for $trait_list {
                my $trait := $_.ast;
                $trait.unshift(PAST::Var.new( :name('declarand'), :scope('register') ));
                if $trait.name() eq '&trait_mod:<of>' && $*TYPENAME {
                    $init_type := $trait[1] := PAST::Op.new(
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
                $init_type := $*TYPENAME;
            }
        }

        # If we've a type to init with and it's a scalar, do so.
        if $init_type && $sigil eq '$' {
            $cont.pirop('new PsP');
            $cont.push($init_type);
        }
    }

    return $past;
}

method routine_declarator:sym<sub>($/) { make $<routine_def>.ast; }
method routine_declarator:sym<method>($/) { make $<method_def>.ast; }
method routine_declarator:sym<submethod>($/) { make $<method_def>.ast; }

method routine_def($/) {
    my $past := $<blockoid>.ast;
    $past.blocktype('declaration');
    $past.control('return_pir');
    if pir::defined__IP($past<placeholder_sig>) && $<multisig> {
        $/.CURSOR.panic('Placeholder variable cannot override existing signature');
    }
    my $signature := $<multisig>                     ?? $<multisig>[0].ast    !! 
            pir::defined__IP($past<placeholder_sig>) ?? $past<placeholder_sig> !!
            Perl6::Compiler::Signature.new();
    $signature.set_default_parameter_type('Any');
    my $sig_setup_block := add_signature($past, $signature, 1);
    if $<trait> {
        emit_routine_traits($past, $<trait>, 'Sub');
    }
    if $<deflongname> {
        # Set name.
        my $name := '&' ~ ~$<deflongname>[0].ast;
        $past.name(~$<deflongname>[0].ast);
        $past.nsentry('');

        # Wrap it in the correct routine type.
        my $multi_flag := PAST::Val.new( :value(0) );
        $past := create_code_object($past, 'Sub', $multi_flag, $sig_setup_block);

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
            $multi_flag.value($*MULTINESS eq 'proto' ?? 2 !! 1);
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
    else {
        # Just wrap in a Sub.
        $past := create_code_object($past, 'Sub', 0, $sig_setup_block);
    }
    make $past;
}


method method_def($/) {
    my $past := $<blockoid>.ast;
    $past.blocktype('declaration');
    $past.control('return_pir');
    if $<trait> {
        emit_routine_traits($past, $<trait>, $*METHODTYPE);
    }
    
    # Get signature - or create - and sort out invocant handling.
    if pir::defined__IP($past<placeholder_sig>) {
        $/.CURSOR.panic('Placeholder variables cannot be used in a method');
    }
    my $sig := $<multisig> ?? $<multisig>[0].ast !! Perl6::Compiler::Signature.new();
    $sig.add_invocant();
    $sig.set_default_parameter_type('Any');

    # Add *%_ parameter if there's no other named slurpy and the package isn't hidden.
    my $need_slurpy_hash := !$sig.has_named_slurpy();
    if $need_slurpy_hash { # XXX ADD BACK: && !package_has_trait('hidden') {
        my $param := Perl6::Compiler::Parameter.new();
        $param.var_name('%_');
        $param.named_slurpy(1);
        $sig.add_parameter($param);
    }

    # Add signature to block.
    my $sig_setup_block := add_signature($past, $sig, 1);
    $past[0].unshift(PAST::Var.new( :name('self'), :scope('lexical'), :isdecl(1), :viviself(sigiltype('$')) ));
    $past.symbol('self', :scope('lexical'));

    # Method container.
    if $<longname> {
        # Set up us the name.
        my $name := $<longname>.Str;
        if $<specials> eq '!' { $name := '!' ~ $name; }
        $past.name($name);
        $past.nsentry('');
        my $multi_flag := PAST::Val.new( :value(0) );
        $past := create_code_object($past, $*METHODTYPE, $multi_flag, $sig_setup_block);

        # Get hold of methods table.
        our @PACKAGE;
        unless +@PACKAGE { $/.CURSOR.panic("Can not declare method outside of a package"); }
        my %table;
        if $<specials> eq '^' {
            %table := @PACKAGE[0].meta_methods();
        } 
        else {
            %table := @PACKAGE[0].methods();
        }
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
            $multi_flag.value($*MULTINESS eq 'proto' ?? 2 !! 1);
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
        $past := create_code_object($past, $*METHODTYPE, 0, $sig_setup_block);
    }

    make $past;
}

method regex_declarator($/, $key?) {
    if $key ne 'open' {
        # Create regex code object.
        # XXX TODO: token/regex/rule differences, signatures, traits.
        my $past := Regex::P6Regex::Actions::buildsub($<p6regex>.ast);
        $past := create_code_object($past, 'Regex', 0, '');

        # Install in lexpad or namespace. XXX Need & on start of name?
        my $name := ~$<deflongname>;
        if $*SCOPE ne 'our' {
            @BLOCK[0][0].push(PAST::Var.new( :name($name), :isdecl(1), 
                                             :viviself($past), :scope('lexical') ) );
            @BLOCK[0].symbol($name, :scope('lexical') );
        }

        # Otherwise, package scoped; add something to loadinit to install them.
        else {
            @PACKAGE[0].block.loadinit.push(PAST::Op.new(
                :pasttype('bind'),
                PAST::Var.new( :name($name), :scope('package') ),
                $past
            ));
            @BLOCK[0].symbol($name, :scope('package') );
        }

        make PAST::Var.new( :name($name) );
    }
}

method capterm($/) {
    # Construct a Parcel, and then call .Capture to coerce it to a capture.
    my $past := $<termish> ?? $<termish>.ast !!
                $<capture> ?? $<capture>[0].ast !!
                PAST::Op.new( :name('&infix:<,>') );
    unless $past.isa(PAST::Op) && $past.name() eq '&infix:<,>' {
        $past := PAST::Op.new( :name('&infix:<,>'), $past );
    }
    make PAST::Op.new( :pasttype('callmethod'), :name('Capture'), $past);
}

method capture($/) {
    make $<EXPR>.ast;
}

method multisig($/) {
    make $<signature>.ast;
}

method signature($/) {
    my $signature := Perl6::Compiler::Signature.new();
    my $cur_param := 0;
    my $is_multi_invocant := 1;
    for $<parameter> {
        my $param := $_.ast;
        $param.multi_invocant($is_multi_invocant);
        if ~@*seps[$cur_param] eq ':' {
            if $cur_param == 0 {
                $param.invocant(1);
            }
            else {
                $/.CURSOR.panic("Can not put ':' parameter seperator after first parameter");
            }
        }
        if @*seps[$cur_param] eq ';;' {
            $is_multi_invocant := 0;
        }
        $signature.add_parameter($param);
        $cur_param := $cur_param + 1;
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
    $*PARAMETER.is_parcel( $quant eq '\\' );
    $*PARAMETER.is_capture( $quant eq '|' );
    if $<default_value> {
        $*PARAMETER.default( PAST::Block.new( $<default_value>[0]<EXPR>.ast ) );
    }

    # Handle traits.
    $*PARAMETER.traits($<trait>);
    if $<trait> {
        # Handle built-in ones.
        my $read_type := trait_readtype($<trait>);
        if $read_type eq 'CONFLICT' {
            $/.CURSOR.panic('Can not apply more than one of: is copy, is rw, is readonly');
        }
        $*PARAMETER.is_rw( $read_type eq 'rw' );
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
        if $<name> {
            if @BLOCK[0].symbol(~$/) {
                $/.CURSOR.panic("Redeclaration of symbol ", ~$/);
            }
            @BLOCK[0].symbol(~$/, :scope($*SCOPE eq 'my' ?? 'lexical' !! 'package'));
        }
    }
}

method named_param($/) {
    if $<name>               { $*PARAMETER.names.push(~$<name>); }
    elsif $<param_var><name> { $*PARAMETER.names.push(~$<param_var><name>[0]); }
}

method type_constraint($/) {
    if $<typename> {
        if pir::substr(~$<typename>, 0, 2) eq '::' {
            my $desigilname := pir::substr(~$<typename>, 2);
            $*PARAMETER.type_captures.push($desigilname);
            my @BlOCK;
            @BLOCK[0].symbol($desigilname, :scope('lexical'));
        }
        else {
            if $*PARAMETER.nom_type {
                $/.CURSOR.panic('Parameter may only have one prefix type constraint');
            }
            $*PARAMETER.nom_type($<typename>.ast);
        }
    }
    elsif $<value> {
        if $*PARAMETER.nom_type {
            $/.CURSOR.panic('Parameter may only have one prefix type constraint');
        }
        $*PARAMETER.nom_type(PAST::Op.new( :pasttype('callmethod'), :name('WHAT'), $<value>.ast ));
        $*PARAMETER.cons_types.push($<value>.ast);
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
            $past := PAST::Block.new( :blocktype('declaration'),
                PAST::Stmts.new( ),
                PAST::Stmts.new(
                    PAST::Op.new( :pasttype('call'), :name('&infix:<~~>'),
                        PAST::Var.new( :name('$_'), :scope('lexical') ),
                        $past
                    )
                )
            );
            my $sig := Perl6::Compiler::Signature.new();
            my $param := Perl6::Compiler::Parameter.new();
            $param.var_name('$_');
            $sig.add_parameter($param);
            my $lazy_name := add_signature($past, $sig, 1);
            $past := create_code_object($past, 'Block', 0, $lazy_name);
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

method postop($/) {
    make $<postfix> ?? $<postfix>.ast !! $<postcircumfix>.ast;
}

method dotty:sym<.>($/) { make $<dottyop>.ast; }

method dotty:sym<.*>($/) {
    my $past := $<dottyop>.ast;
    $past.unshift($past.name);
    $past.name('!dispatch_' ~ $<sym>.Str);
    $past.pasttype('call');
    make $past;
}

method dottyop($/) {
    if $<methodop> {
        make $<methodop>.ast;
    } else {
        make $<postop>.ast;
    }
}

method privop($/) {
    my $past := $<methodop>.ast;
    if $<methodop><quote> {
        $past.name(PAST::Op.new( :pasttype('call'), :name('&infix:<~>'), '!', $past.name ));
    }
    else {
        $past.name( '!' ~ $past.name );
    }
    make $past;
}

method methodop($/) {
    my $past := $<args> ?? $<args>[0].ast !! PAST::Op.new( :node($/) );
    if $<identifier> {
        $past.name( ~$<identifier> );
    }
    elsif $<quote> {
        $past.name( $<quote>.ast );
    }
    $past.pasttype('callmethod');
    make $past;
}


method term:sym<self>($/) {
    make PAST::Var.new( :name('self'), :node($/) );
}

method term:sym<Nil>($/) {
    make PAST::Op.new(:name('&Nil'), :node($/) );
}

method term:sym<...>($/) {
    make PAST::Op.new( :pasttype('call'), :name('&fail'), 'Stub code executed', :node($/) );
}

method term:sym<???>($/) {
    make PAST::Op.new( :pasttype('call'), :name('&warn'), 'Stub code executed', :node($/) );
}

method term:sym<!!!>($/) {
    make PAST::Op.new( :pasttype('call'), :name('&die'), 'Stub code executed', :node($/) );
}

method term:sym<dotty>($/) {
    my $past := $<dotty>.ast;
    $past.unshift(PAST::Op.new(
        :pirop('descalarref PP'),
        PAST::Var.new( :name('$_'), :scope('lexical') )
    ));
    make $past;
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
    my $var;
    if is_lexical(~$<longname>) {
        $var := PAST::Var.new( :name(~$<longname>), :scope('lexical') );
    }
    else {
        $var := PAST::Var.new( :name(~$name), :namespace($ns), :scope('package') );
    }
    my $past := $var;
    if $<args> {
        $past := $<args>.ast;
        if $ns {
            $past.unshift($var);
            unless pir::substr($var.name, 0, 1) eq '&' {
                $var.name('&' ~ $var.name);
            }
        }
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

method term:sym<capterm>($/) {
    make $<capterm>.ast;
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
        if $expr.name eq '&infix:<,>' {
            for $expr.list { $past.push(handle_named_parameter($_)); }
        }
        else { $past.push(handle_named_parameter($expr)); }
    }
    make $past;
}

sub handle_named_parameter($arg) {
    if $arg ~~ PAST::Node && $arg.returns() eq 'Pair' {
        my $result := $arg[2];
        $result.named(~$arg[1].value());
        $result;
    }
    else {
        $arg;
    }
}

method term:sym<value>($/) { make $<value>.ast; }

method circumfix:sym<( )>($/) { make $<semilist>.ast; }

method circumfix:sym<ang>($/) { make $<quote_EXPR>.ast; }

method circumfix:sym<« »>($/) { make $<quote_EXPR>.ast; }

method circumfix:sym<{ }>($/) {
    # If it is completely empty or consists of a single list, the first
    # element of which is either a hash or a pair, it's a hash constructor.
    my $past := $<pblock>.ast;
    my $is_hash := 0;
    if +@($past) == 2 {
        if +@($past[1]) == 0 {
            # Empty block, so a hash.
            $is_hash := 1;
        }
        elsif +@($past[1]) == 1 && $past[1][0].isa(PAST::Op) {
            if $past[1][0].returns() eq 'Pair' || $past[1][0].name() eq '&infix:<=>>' {
                # Block with just one pair in it, so a hash.
                $is_hash := 1;
            }
            elsif $past[1][0].name() eq '&infix:<,>' {
                # List, but first elements must be...
                if $past[1][0][0].isa(PAST::Op) &&
                        ($past[1][0][0].returns() eq 'Pair' || $past[1][0][0].name() eq '&infix:<=>>') {
                    # ...a Pair
                    $is_hash := 1;
                }
                elsif $past[1][0][0].isa(PAST::Var) &&
                        pir::substr__SSII($past[1][0][0].name(), 0, 1) eq '%' {
                    # ...or a hash.
                    $is_hash := 1
                }
            }
        }
        elsif +@($past[1]) == 1 && $past[1][0].isa(PAST::Var) {
            if pir::substr__SSII($past[1][0].name(), 0, 1) eq '%' {
                $is_hash := 1;
            }
        }
    }
    if $is_hash {
        my @children := @($past[1]);
        $past := PAST::Op.new(
            :pasttype('call'),
            :name('&circumfix:<{ }>'),
            :node($/)
        );
        for @children {
            $past.push($_);
        }
    }
    else {
        $past := create_code_object($past, 'Block', 0, '');
    }
    make $past;
}

method circumfix:sym<[ ]>($/) {
    make PAST::Op.new( :name('&circumfix:<[ ]>'), $<semilist>.ast, :node($/) );
}

method circumfix:sym<sigil>($/) {
    my $name := ~$<sigil> eq '@' ?? 'list' !!
                ~$<sigil> eq '%' ?? 'hash' !!
                                    'item';
    make PAST::Op.new( :pasttype('callmethod'), :name($name), $<semilist>.ast );
}

## Expressions

method EXPR($/, $key?) {
    unless $key { return 0; }
    if $/<drop> { make PAST::Stmts.new() }
    my $past := $/.ast // $<OPER>.ast;
    if !$past && $<infix><sym> eq '.=' {
        make make_dot_equals($/[0].ast, $/[1].ast);
        return 1;
    }
    unless $past {
        $past := PAST::Op.new( :node($/) );
        if $<OPER><O><pasttype> { $past.pasttype( ~$<OPER><O><pasttype> ); }
        elsif $<OPER><O><pirop>    { $past.pirop( ~$<OPER><O><pirop> ); }
        unless $past.name {
            if $key eq 'LIST' { $key := 'infix'; }
            my $name := Q:PIR {
                $P0 = find_lex '$key'
                $S0 = $P0
                $S0 = downcase $S0
                %r = box $S0
            } ~ ':<' ~ $<OPER><sym> ~ '>';
            $past.name('&' ~ $name);
        }
    }
    if $key eq 'POSTFIX' { 
        $past.unshift(
            PAST::Op.ACCEPTS($past) && $past.pasttype eq 'callmethod'
            ?? PAST::Op.new( :pirop('descalarref PP'), $/[0].ast )
            !! $/[0].ast
        );
    }
    else {
        for $/.list { if $_.ast { $past.push($_.ast); } }
    }
    make $past;
}

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

method postfixish($/) {
    if $<postfix_prefix_meta_operator> {
        my $past := $<OPER>.ast;
        if $past.isa(PAST::Op) && $past.pasttype() eq 'call' {
            $past.unshift($past.name());
            $past.name('!dispatch_dispatcher_parallel');
        }
        elsif $past.isa(PAST::Op) && $past.pasttype() eq 'callmethod' {
            $past.unshift($past.name());
            $past.name('!dispatch_method_parallel');
            $past.pasttype('call');
        }
        else {
            $/.CURSOR.panic("Unimplemented or invalid use of parallel dispatch");
        }
        make $past;
    }
}

method postcircumfix:sym<[ ]>($/) {
    make PAST::Op.new( $<EXPR>.ast, :name('!postcircumfix:<[ ]>'),
                       :pasttype('call'), :node($/) );
}

method postcircumfix:sym<{ }>($/) {
    make PAST::Op.new( $<EXPR>.ast, :name('!postcircumfix:<{ }>'),
                       :pasttype('call'), :node($/) );
}

method postcircumfix:sym<ang>($/) {
    make PAST::Op.new( $<quote_EXPR>.ast, :name('!postcircumfix:<{ }>'),
                       :pasttype('call'), :node($/) );
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
        make PAST::Var.new( :name(~$/), :namespace(''), :scope('package') );
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
            :value(($int * $base + $frac) / $base * 10 ** +$exp ) ,
            :returns('Num')
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
    my $past;

    if is_lexical($<longname>.Str) {
        # We need to build a thunk.
        $past := PAST::Block.new(
            PAST::Var.new( :name('$_'), :scope('parameter'), :isdecl(1) ),
            PAST::Op.new( :pasttype('callmethod'), :name('ACCEPTS'),
                PAST::Var.new( :name($<longname>.Str), :scope('lexical') ),
                PAST::Var.new( :name('$_'), :scope('lexical') )
            )
        );
        my $sig := Perl6::Compiler::Signature.new();
        my $param := Perl6::Compiler::Parameter.new();
        $param.var_name('$_');
        $sig.add_parameter($param);
        add_signature($past, $sig, 0);
        $past := create_code_object($past, 'Block', 0, '');
    }
    else {
        my @name := Perl6::Grammar::parse_name($<longname>.Str);
        $past := PAST::Var.new(
            :name(@name.pop),
            :namespace(@name),
            :scope('package')
        );
    }

    # Parametric type?
    if $<typename> {
        $past := PAST::Op.new(
            :pasttype('callmethod'), :name('postcircumfix:<[ ]>'),
            $past, $<typename>[0].ast
        );
    }

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
method quote:sym<qx>($/) {
    make PAST::Op.new( :name('!qx'), :pasttype('call'),
        $<quote_EXPR>.ast
    );
}
method quote:sym<qqx>($/)  {
    make PAST::Op.new( :name('!qx'), :pasttype('call'),
        $<quote_EXPR>.ast
    );
}
method quote:sym</ />($/) {
    my $past := Regex::P6Regex::Actions::buildsub($<p6regex>.ast);
    make create_code_object($past, 'Regex', 0, '');
}
method quote:sym<rx>($/) {
    my $past := Regex::P6Regex::Actions::buildsub($<p6regex>.ast);
    make create_code_object($past, 'Regex', 0, '');
}
method quote:sym<m>($/) {
    my $past := Regex::P6Regex::Actions::buildsub($<p6regex>.ast);
    make create_code_object($past, 'Regex', 0, '');
}

method quote_escape:sym<$>($/) { make $<variable>.ast; }
method quote_escape:sym<{ }>($/) {
    make PAST::Op.new(
        :pirop('set S*'), block_immediate($<block>.ast), :node($/)
    );
}

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
        if !PAST::Node.ACCEPTS($ast) {
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
    my $past := @parts ?? @parts.shift !! '';
    while @parts {
        $past := PAST::Op.new( $past, @parts.shift, :pirop('concat') );
    }
    make $past;
}

## Operators

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
# capture against the signature. Returns the name of the signature setup block.
sub add_signature($block, $sig_obj, $lazy) {
    # Set arity.
    $block.arity($sig_obj.arity);

    # Add call to signature binder as well as lexical declarations
    # to the start of the block.
    $block[0].push(PAST::Var.new( :name('call_sig'), :scope('parameter'), :call_sig(1) ));
    my $decls := $sig_obj.get_declarations();
    for @($decls) {
        $_.isdecl(1);
        $block.symbol( $_.name, :scope('lexical') );
    }
    $block[0].push($decls);
    $block[0].push(PAST::Op.new(
        :pirop('bind_signature vP'),
        PAST::Var.new( :name('call_sig'), :scope('lexical') )
    ));

    # If lazy, make and push signature setup block.
    if $lazy {
        my $sig_setup_block_name := $block.unique('!sig_setup_' ~ pir::time__N() ~ '_');
        $block[0].push(PAST::Block.new(
            :name($sig_setup_block_name),
            :blocktype('declaration'),
            $sig_obj.ast
        ));
        $sig_setup_block_name
    }
    else {
        $block.loadinit.push($sig_obj.ast);
        $block.loadinit.push(PAST::Op.new( :inline('    setprop block, "$!signature", signature') ));
    }
}

# Adds a placeholder parameter to this block's signature.
sub add_placeholder_parameter($sigil, $ident, :$named, :$slurpy_pos, :$slurpy_named) {
    our @BLOCK;
    my $block := @BLOCK[0];

    # Add entry to the block signature.
    my $placeholder_sig := $block<placeholder_sig>;
    unless pir::defined__IP($placeholder_sig) {
        $block<placeholder_sig> := $placeholder_sig := Perl6::Compiler::Signature.new();
    }
    my $param := Perl6::Compiler::Parameter.new();
    $param.var_name(~$sigil ~ ~$ident);
    $param.pos_slurpy($slurpy_pos);
    $param.named_slurpy($slurpy_named);
    if $named { $param.names.push($ident) }
    $placeholder_sig.add_placeholder_parameter($param);

    # Just want a lookup of the variable here.
    return PAST::Var.new( :name(~$sigil ~ ~$ident), :scope('lexical') );
}

# Wraps a sub up in a block type.
sub create_code_object($block, $type, $multiness, $lazy_init) {
    my @name := Perl6::Grammar::parse_name($type);
    my $past := PAST::Op.new(
        :pasttype('callmethod'),
        :name('new'),
        PAST::Var.new( :name(@name.pop), :namespace(@name), :scope('package') ),
        $block,
        $multiness,
        $lazy_init
    );
    $past<past_block> := $block;
    $past
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
        create_code_object(PAST::Var.new( :name('block'), :scope('register') ), $type, 0, '')
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
            :name('&succeed'),
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

sub make_dot_equals($thingy, $call) {
    $call.unshift($call.name);
    $call.unshift($thingy);
    $call.name('!dispatch_.=');
    $call.pasttype('call');
    $call;
}

# XXX This isn't quite right yet... need to evaluate these semantics
sub push_block_handler($/, $block) {
    unless @BLOCK[0].handlers() {
        @BLOCK[0].handlers([]);
    }
    $block.blocktype('declaration');
    $block := PAST::Block.new(
        :blocktype('declaration'),
        PAST::Var.new( :scope('parameter'), :name('$_') ),
        PAST::Op.new( :pasttype('bind'),
            PAST::Var.new( :scope('lexical'), :name('$_') ),
            PAST::Op.new(
                :pasttype('callmethod'),
                :name('new'),
                PAST::Var.new(
                    :name('Exception'),
                    :namespace([]),
                    :scope('package'),
                ),
                PAST::Var.new( :scope('lexical'), :name('$_') ),
            ),
        ),
        PAST::Op.new( :pasttype('bind'),
            PAST::Var.new( :scope('lexical'), :name('$!'), :isdecl(1) ),
            PAST::Var.new( :scope('lexical'), :name('$_') ),
        ),
        PAST::Op.new( :pasttype('call'),
            $block,
        ),
    );
    $block.symbol('$_', :scope('lexical'));
    $block.symbol('$!', :scope('lexical'));
    $block := PAST::Stmts.new(
        PAST::Op.new( :pasttype('call'),
            $block,
            PAST::Var.new( :scope('register'), :name('exception') ),
        ),
        # XXX Rakudo needs to set this when $! is inspected
        # We just cheat for now.  Call .rethrow() if you want it rethrown.
        PAST::Op.new( :pasttype('bind'),
            PAST::Var.new( :scope('keyed'),
                PAST::Var.new( :scope('register'), :name('exception')),
                'handled'
            ),
            1
        )
    );

    @BLOCK[0].handlers.unshift(
        PAST::Control.new(
            :node($/),
            $block,
        )
    );
}

# Makes the closure for the RHS of has $.answer = 42.
sub make_attr_init_closure($init_value) {
    # Need to not just build the closure, but new_closure it; otherwise, we
    # run into trouble if our initialization value involves a parameter from
    # a parametric role.
    my $block := PAST::Block.new(
        :blocktype('declaration'),
        PAST::Stmts.new( ),
        PAST::Stmts.new( $init_value )
    );
    $block[0].unshift(PAST::Var.new( :name('self'), :scope('lexical'), :isdecl(1), :viviself(sigiltype('$')) ));
    my $sig := Perl6::Compiler::Signature.new();
    my $parameter := Perl6::Compiler::Parameter.new();
    $parameter.var_name('$_');
    $sig.add_parameter($parameter);
    $sig.add_invocant();
    my $lazy_name := add_signature($block, $sig, 1);
    create_code_object(PAST::Op.new( :pirop('newclosure PP'), $block ), 'Method', 0, $lazy_name);
}

# Looks through the lexpads and sees if we recognize the symbol as a lexical.
sub is_lexical($name) {
    our @BLOCK;
    for @BLOCK {
        my %entry := $_.symbol($name);
        if %entry && %entry<scope> eq 'lexical' {
            return 1;
        }
    }
    return 0;
}

# Looks to see if a variable has been set up as an alias to an attribute.
sub is_attr_alias($name) {
    our @BLOCK;
    for @BLOCK {
        my %entry := $_.symbol($name);
        if %entry {
            return %entry<attr_alias>;
        }
    }
    return "";
}

# Gives a block a Nil to return if it has no statements, to prevent Null
# PMCs being handed back.
sub prevent_null_return($block) {
    if +@($block[1]) == 0 {
        $block[1].push(PAST::Op.new( :name('&Nil') ));
    }
}
