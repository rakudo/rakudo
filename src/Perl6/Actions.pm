class Perl6::Actions is HLL::Actions;

our @BLOCK;
our @PACKAGE;
our $TRUE;
our %BEGINDONE;

INIT {
    # initialize @BLOCK and @PACKAGE
    our @BLOCK := Q:PIR { %r = root_new ['parrot';'ResizablePMCArray'] };
    our @PACKAGE := Q:PIR { %r = root_new ['parrot';'ResizablePMCArray'] };
    our $TRUE := PAST::Var.new( :name('true'), :scope('register') );
    our %BEGINDONE := Q:PIR { %r = root_new ['parrot';'Hash'] };

    # Tell PAST::Var how to encode Perl6Str and Str values
    my %valflags :=
        Q:PIR { %r = get_hll_global ['PAST';'Compiler'], '%valflags' };
    %valflags<Perl6Str> := 'e';
    %valflags<Str>      := 'e';
}

sub xblock_immediate($xblock) {
    $xblock[1] := pblock_immediate($xblock[1]);
    $xblock;
}

sub pblock_immediate($pblock) {
    block_immediate($pblock);
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

method comp_unit($/, $key?) {
    our $?RAKUDO_HLL;
    
    # Get the block for the unit mainline code.
    my $unit := @BLOCK.shift;
    my $mainline := $<statementlist>.ast;

    # Get the block for the entire compilation unit.
    my $outer := @BLOCK.shift;
    $outer.node($/);
    $outer.hll($?RAKUDO_HLL);
    
    # If it's the setting, just need to run the mainline.
    if $*SETTING_MODE {
        $unit.push($mainline);
        $unit.hll($?RAKUDO_HLL);
        $unit.pirflags(':init :load');
        make $unit;
        return 1;
    }

    my $mainparam := PAST::Var.new(:name('$MAIN'), :scope('parameter'),
                         :viviself( PAST::Val.new( :value(0) ) ) );
    $unit.symbol('$MAIN', :scope<lexical>);
    # If the unit defines &MAIN, add a &MAIN_HELPER.
    if $unit.symbol('&MAIN') {
        $mainline := 
            PAST::Op.new(
                :pasttype('call'),
                :name('&MAIN_HELPER'),
                $mainline,
                $mainparam
            );
    }
    else {
        $unit.push($mainparam);
    }
    $unit.push( self.CTXSAVE() );
    $unit.push($mainline);

    # Executing the compilation unit causes the mainline to be executed.
    # We force a return here, because we have other :load/:init blocks
    # that have to be done at the end of the unit, and we don't want them
    # executed by the mainline.
    $outer.push(
        PAST::Op.new(
            :pirop('return'),
            PAST::Op.new( :pasttype<call>,
                PAST::Var.new( :name('!UNIT_START'), :namespace(''), :scope('package') ),
                $unit,
                PAST::Var.new( :scope('parameter'), :name('@_'), :slurpy(1) )
            )
        )
    );

    # CHECK time occurs at the end of the compilation unit, :load/:init.
    # (We can't # use the .loadinit property because that will generate
    # the CHECK block too early.)
    $outer.push(
        PAST::Block.new(
            :pirflags(':load :init'), :lexical(0), :namespace(''),
            PAST::Op.new( :name('!fire_phasers'), 'CHECK' )
        )
    );

    # If this unit is loaded via load_bytecode, we want it to automatically
    # execute the mainline code above after all other initializations have
    # occurred.
    $outer.push(
        PAST::Block.new(
            :pirflags(':load'), :lexical(0), :namespace(''),
            PAST::Op.new(
                :name('!UNIT_START'), :pasttype('call'),
                PAST::Val.new( :value($outer) ),
            )
        )
    );

    # Add file annotation.
    my $file := pir::find_caller_lex__ps('$?FILES');
    unless pir::isnull($file) {
        $outer.unshift(PAST::Op.new(:inline(".annotate 'file', '" ~ $file ~ "'")));
    }

    # Remove the outer module package.
    @PACKAGE.shift;

    make $outer;
}

method unitstart($/) {
    # Create a block for the compilation unit.
    self.newpad($/);
    # Use SET_BLOCK_OUTER_CTX (inherited from HLL::Actions)
    # to set dynamic outer lexical context and namespace details
    # for the compilation unit.
    self.SET_BLOCK_OUTER_CTX(@BLOCK[0]);

    self.newpad($/);
    self.finishpad($/);

    # set up initial package and $*UNITPAST
    @PACKAGE.unshift(Perl6::Compiler::Module.new());
    @PACKAGE[0].block(@BLOCK[0]);
    $*UNITPAST := @BLOCK[0];
}

method statementlist($/) {
    my $past := PAST::Stmts.new( :node($/) );
    if $<statement> {
        for $<statement> {
            my $ast := $_.ast;
            if $ast {
                if $ast<bareblock> {
                    $ast := PAST::Op.new(
                                :pirop<setprop__0PsP>,
                                block_immediate($ast<block_past>),
                                '$!lazysig',
                                $ast[2]);
                }
                elsif $ast.isa(PAST::Block) && !$ast.blocktype {
                    $ast := block_immediate($ast);
                }
                $past.push( $ast );
            }
        }
    }
    $past.push(PAST::Op.new(:name('&Nil'))) if +$past.list < 1;
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
            $mc.ast.push($past);
            $mc.ast.push(PAST::Op.new(:name('&Nil')));
            $past := $mc.ast;
        }
        if $ml {
            my $cond := $ml<smexpr>.ast;
            if ~$ml<sym> eq 'given' {
                $past := PAST::Op.new(
                    :pasttype('call'),
                    PAST::Block.new(
                        :blocktype('declaration'),
                        PAST::Var.new( :name('$_'), :scope('parameter'), :isdecl(1) ),
                        $past
                    ),
                    $cond
                );
            }
            elsif ~$ml<sym> eq 'for' {
                $past := PAST::Block.new( :blocktype('immediate'),
                    PAST::Var.new( :name('$_'), :scope('parameter'), :isdecl(1) ),
                    $past);
                $cond := PAST::Op.new(:name('&flat'), $cond);
                $past := PAST::Op.new($cond, $past, :pasttype(~$ml<sym>), :node($/) );
            }
            else {
                $past := PAST::Op.new($cond, $past, :pasttype(~$ml<sym>), :node($/) );
            }
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
                $signature.add_parameter(Perl6::Compiler::Parameter.new(
                    :var_name('$_'), :optional(1),
                    :is_parcel(1), :default_from_outer(1)
                ));
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
    # We ought to find a way to avoid this, but it seems necessary for now.
    $block.loadinit.push(
        PAST::Op.new( :pirop<setprop__vPsP>,
            PAST::Val.new( :value($block) ),
            '$!lazysig',
            $block<lazysig> )
    );
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
        ),
        PAST::Var.new(
            :name('__CANDIDATE_LIST__'), :scope('lexical'), :isdecl(1)
        )
    ));
    @BLOCK.unshift($new_block);
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
        $past.push( pblock_immediate( $<else>[0].ast ) );
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
        $past := PAST::Op.new( $<EXPR>.ast, pblock_immediate( $<pblock>.ast ),
                               :pasttype($pasttype), :node($/) );
    }
    make $past;
}

method statement_control:sym<for>($/) {
    my $past := xblock_immediate($<xblock>.ast);
    $past.pasttype('for');
    $past[0] := PAST::Op.new(:name('&flat'), $past[0]);
    $past.arity($past[1].arity || 1);
    make $past;
}

method statement_control:sym<loop>($/) {
    my $block := block_immediate($<block>.ast);
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

method statement_control:sym<need>($/) {
    my $past := PAST::Stmts.new( :node($/) );
    for $<module_name> {
        need($_);
    }
    make $past;
}

sub need($module_name) {
    # Build up adverbs hash if we have them. Note that we need a hash
    # for now (the compile time call) and an AST that builds said hash
    # for the runtime call once we've compiled the module.
    my $name := $module_name<longname><name>.Str;
    my %adverbs;
    my $adverbs_ast := PAST::Op.new(
        :name('&circumfix:<{ }>'), PAST::Op.new( :name('&infix:<,>') )
    );
    if $module_name<longname><colonpair> {
        for $module_name<longname><colonpair> {
            my $ast := $_.ast;
            $adverbs_ast[0].push($ast);
            %adverbs{$ast[1].value()} := $ast[2].value();
        }
    }

    # Need to immediately load module and get lexicals stubbed in.
    Perl6::Module::Loader.need($name, %adverbs);

    # Also need code to do the actual loading emitting (though need
    # won't repeat its work if already carried out; we mainly need
    # this for pre-compilation to PIR to work).
    my @ns := pir::split__PSS('::', 'Perl6::Module');
    @BLOCK[0].loadinit.push(
        PAST::Op.new( :pasttype('callmethod'), :name('need'),
            PAST::Var.new( :name('Loader'), :namespace(@ns), :scope('package') ),
            $name,
            PAST::Op.new( :pirop('getattribute PPS'), $adverbs_ast, '$!storage' )
        ));
}

method statement_control:sym<import>($/) {
    my $past := PAST::Stmts.new( :node($/) );
    import($/);
    make $past;
}

sub import($/) {
    my $name := $<module_name><longname><name>.Str;
    Perl6::Module::Loader.stub_lexical_imports($name, @BLOCK[0]);
    my @ns := pir::split__PSS('::', 'Perl6::Module');
    @BLOCK[0].push(
        PAST::Op.new( :pasttype('callmethod'), :name('import'),
            PAST::Var.new( :name('Loader'), :namespace(@ns), :scope('package') ),
            $name
        ));
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
        elsif ~$<module_name> eq 'MONKEY_TYPING' {
            $*MONKEY_TYPING := 1;
        }
        elsif ~$<module_name> eq 'SETTING_MODE' {
            $*SETTING_MODE := 1;
        }
        elsif ~$<module_name> eq 'FORBID_PIR' {
            $*FORBID_PIR := 1;
        }
        else {
            need($<module_name>);
            import($/);
        }
    }
    make $past;
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
    my $pblock := $xblock.shift;

    # Add exception handler to the block so we fall out of the enclosing block
    # after it's executed.
    $pblock := pblock_immediate($pblock);
    when_handler_helper($pblock);

    # Handle the smart-match. XXX Need to handle syntactic cases too.
    my $match_past := PAST::Op.new( :pasttype('call'), :name('&infix:<~~>'),
        PAST::Var.new( :name('$_'), :scope('lexical') ),
        $sm_exp
    );

    # Use the smartmatch result as the condition for running the block.
    make PAST::Op.new( :pasttype('if'), :node( $/ ),
        $match_past, $pblock,
    );
}

method statement_control:sym<default>($/) {
    # We always execute this, so just need the block, however we also
    # want to make sure we break after running it.
    my $block := block_immediate($<block>.ast);
    when_handler_helper($block);
    make $block;
}

method statement_control:sym<CATCH>($/) {
    my $block := $<block>.ast;
    push_block_handler($/, @BLOCK[0], $block);
    @BLOCK[0].handlers()[0].handle_types_except('CONTROL');
    make PAST::Stmts.new(:node($/));
}

method statement_control:sym<CONTROL>($/) {
    my $block := $<block>.ast;
    push_block_handler($/, @BLOCK[0], $block);
    @BLOCK[0].handlers()[0].handle_types('CONTROL');
    make PAST::Stmts.new(:node($/));
}

method statement_prefix:sym<BEGIN>($/) {
    # BEGIN is kinda tricky. We actually need to run the code in it with
    # immediate effect, and then have the BEGIN block evaluate to what
    # was produced at runtime. But if we're in a pre-compiled module, we
    # need to run the lot. Thus we keep a "already computed BEGIN values"
    # hash and don't re-run if the value is in that. Of course, we still
    # don't handle looking at things in outer lexical scopes here.
    our %BEGINDONE;
    our $?RAKUDO_HLL;
    my $past := $<blorst>.ast;
    $past.blocktype('declaration');
    $past := PAST::Block.new(
        :hll($?RAKUDO_HLL),
        PAST::Op.new( :pasttype('call'), :name('!YOU_ARE_HERE'), $past )
    );
    my $compiled := PAST::Compiler.compile($past);
    my $begin_id := $past.unique('BEGINDONE_');
    %BEGINDONE{$begin_id} := $compiled();
    @BLOCK[0].loadinit.push(PAST::Op.new(
        :pasttype('call'), :name('!begin_unless_begun'),
        $begin_id, $past
    ));
    make PAST::Var.new( :scope('keyed'),
        PAST::Var.new( :name('%BEGINDONE'), :namespace(pir::split('::', 'Perl6::Actions')), :scope('package') ),
        PAST::Val.new( :value($begin_id) )
    );
}

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

method statement_prefix:sym<sink>($/) {
    my $blast := $<blorst>.ast;
    $blast.blocktype('immediate');
    make PAST::Stmts.new(
        PAST::Op.new( :name('&eager'), $blast ),
        PAST::Op.new( :name('&Nil') ),
        :node($/)
    );
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

method modifier_expr($/) { make $<EXPR>.ast; }

method statement_mod_cond:sym<if>($/)     { 
    make PAST::Op.new( :pasttype<if>, $<modifier_expr>.ast, :node($/) );
}

method statement_mod_cond:sym<unless>($/) {
    make PAST::Op.new( :pasttype<unless>, $<modifier_expr>.ast, :node($/) );
}

method statement_mod_cond:sym<when>($/) {
    make PAST::Op.new( :pasttype<if>,
        PAST::Op.new( :name('&infix:<~~>'),
                      PAST::Var.new( :name('$_') ),
                      $<modifier_expr>.ast ),
        :node($/)
    );
}

method statement_mod_loop:sym<while>($/)  { make $<smexpr>.ast; }
method statement_mod_loop:sym<until>($/)  { make $<smexpr>.ast; }
method statement_mod_loop:sym<for>($/)    { make $<smexpr>.ast; }
method statement_mod_loop:sym<given>($/)  { make $<smexpr>.ast; }

## Terms

method term:sym<fatarrow>($/)           { make $<fatarrow>.ast; }
method term:sym<colonpair>($/)          { make $<colonpair>.ast; }
method term:sym<variable>($/)           { make $<variable>.ast; }
method term:sym<package_declarator>($/) { make $<package_declarator>.ast; }
method term:sym<scope_declarator>($/)   { make $<scope_declarator>.ast; }
method term:sym<routine_declarator>($/) { make $<routine_declarator>.ast; }
method term:sym<multi_declarator>($/)   { make $<multi_declarator>.ast; }
method term:sym<regex_declarator>($/)   { make $<regex_declarator>.ast; }
method term:sym<type_declarator>($/)    { make $<type_declarator>.ast; }
method term:sym<statement_prefix>($/)   { make $<statement_prefix>.ast; }
method term:sym<lambda>($/)             { make block_closure($<pblock>.ast, 'Block', 0); }
method term:sym<sigterm>($/)            { make $<sigterm>.ast; }

method term:sym<YOU_ARE_HERE>($/) {
    my $past := PAST::Block.new(
        :name('!YOU_ARE_HERE'),
        PAST::Op.new(
            :inline(
                '$P0 = getinterp',
                '$P0 = $P0["context"]',
                '$P0 = getattribute $P0, "outer_ctx"',
                '%0."set_outer_ctx"($P0)',
                '%r = %0(%1)'
            ),
            PAST::Var.new( :name('mainline'), :scope('parameter') ),
            PAST::Var.new( :name('$MAIN'), :scope('parameter'),
                           :viviself( PAST::Val.new( :value(0) ) ) )
        )
    );
    @BLOCK[0][0].push(PAST::Var.new(
        :name('!YOU_ARE_HERE'), :isdecl(1), :viviself($past), :scope('lexical')
    ));
    make PAST::Op.new( :pasttype('call'),
        PAST::Var.new( :name('!YOU_ARE_HERE'), :scope('lexical') ),
        PAST::Block.new( )
    );
}

method name($/) { }

method module_name($/) {
    my @name := Perl6::Grammar::parse_name(~$<longname>);
    my $var := PAST::Var.new(
        :name(@name.pop),
        :namespace(@name),
        :scope(is_lexical(~$<longname>) ?? 'lexical' !! 'package')
    );
    if $<arglist> {
        my $past := $<arglist>[0].ast;
        $past.pasttype('callmethod');
        $past.name('!select');
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
            make make_pair($*key, make_variable($/<var>, ~$<var>));
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
    elsif $<fakesignature> {
        make $<fakesignature>.ast.ast;   # XXX: Huh?
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
    elsif $<infixish> {
        $past := PAST::Op.new( :pirop('find_sub_not_null__Ps'), '&infix:<' ~ $<infixish>.Str ~ '>' );
    }
    else {
        $past := make_variable($/, ~$/);
    }
    make $past;
}

sub make_variable($/, $name) {
    my @name := Perl6::Grammar::parse_name($name);
    my $past := PAST::Var.new( :name(@name.pop), :node($/));
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
    elsif $<twigil>[0] eq '.' && $*IN_DECL ne 'variable' {
        # Need to transform this to a method call.
        $past := $<arglist> ?? $<arglist>[0].ast !! PAST::Op.new();
        $past.pasttype('callmethod');
        $past.name(~$<desigilname>);
        $past.unshift(PAST::Var.new( :name('self'), :scope('lexical') ));
    }
    elsif $<twigil>[0] eq '^' || $<twigil>[0] eq ':' {
        $past := add_placeholder_parameter($<sigil>.Str, $<desigilname>.Str, :named($<twigil>[0] eq ':'));
    }
    elsif ~$/ eq '@_' {
        unless get_nearest_signature().declares_symbol('@_') {
            $past := add_placeholder_parameter('@', '_', :slurpy_pos(1));
        }
    }
    elsif ~$/ eq '%_' {
        unless get_nearest_signature().declares_symbol('%_') {
            $past := add_placeholder_parameter('%', '_', :slurpy_named(1));
        }
    }
    else {
        my $attr_alias := is_attr_alias($past.name);
        if $attr_alias {
            $past.name($attr_alias);
            $past.scope('attribute');
            $past.viviself( sigiltype( $<sigil> ) );
            $past.unshift(PAST::Var.new( :name('self'), :scope('lexical') ));
        }
        elsif $<sigil> eq '&' && !@name {
            $past := PAST::Op.new(:pirop('find_sub_not_null__Ps'), $past.name);
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
            my $name := ~$<def_module_name>[0]<longname><name>;
            if $name ne '::' {
                $/.CURSOR.add_name($name, 1);
                $package.name($name);
            }
            if $<def_module_name>[0]<signature> {
                $package.signature($<def_module_name>[0]<signature>[0].ast);
                $package.signature_text(~$<def_module_name>[0]<signature>[0]);
            }
            if $<def_module_name>[0]<longname><colonpair> {
                for $<def_module_name>[0]<longname><colonpair> {
                    $package.name_adverbs.push($_.ast);
                }
            }
        }

        # Add traits.
        for $<trait> {
            $package.traits.push($_.ast);
        }

        # Claim currently open block as the package's block.
        $package.block(@BLOCK[0]);

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
        if pir::substr__SSII($<blockoid><statementlist><statement>[0], 0, 3) eq '...' {
            # Just a stub, so don't do any more work.
            if $*SCOPE eq 'our' || $*SCOPE eq '' {
                %Perl6::Grammar::STUBCOMPILINGPACKAGES{~$<def_module_name>[0]<longname>} := 1;
            }
            @BLOCK[0].symbol(~$<def_module_name>[0]<longname>, :stub(1));
            make PAST::Stmts.new( );
        }
        else {
            my $block;
            if $<blockoid> {
                $block := $<blockoid>.ast;
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
method scope_declarator:sym<anon>($/)    { make $<scoped>.ast; }
method scope_declarator:sym<augment>($/) { make $<scoped>.ast; }

method declarator($/) {
    if    $<variable_declarator> { make $<variable_declarator>.ast }
    elsif $<routine_declarator>  { make $<routine_declarator>.ast  }
    elsif $<regex_declarator>    { make $<regex_declarator>.ast    }
    elsif $<signature> {
        my $list  := PAST::Op.new( :pasttype('call'), :name('&infix:<,>') );
        my $decls := $<signature>.ast.get_declarations;
        for @($decls) {
            if $_.isa(PAST::Var) {
                my $decl := declare_variable($/, $_, $_<sigil>, $_<twigil>, $_<desigilname>, $_<traits>);
                unless $decl.isa(PAST::Op) && $decl.pasttype() eq 'null' {
                    $list.push($decl);
                }
            }
            else {
                $list.push($_);
            }
        }
        $list<signature_from_declarator> := $<signature>.ast;
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
        unless +@PACKAGE {
            $/.CURSOR.panic("Can not declare an attribute outside of a package");
        }
        if @PACKAGE[0].has_attribute($attrname) {
            $/.CURSOR.panic("Can not re-declare attribute " ~ $attrname);
        }
        my %attr_info;
        %attr_info<name>      := $attrname;
        %attr_info<type>      := $*TYPENAME;
        %attr_info<accessor>  := $twigil eq '.' ?? 1 !! 0;
        if $trait_list && has_compiler_trait_with_val($trait_list, '&trait_mod:<is>', 'rw') {
            %attr_info<rw> := 1;
        }
        if $trait_list && has_compiler_trait_with_val($trait_list, '&trait_mod:<is>', 'readonly') {
            %attr_info<rw> := 0;
        }
        my $has_handles := has_compiler_trait($trait_list, '&trait_mod:<handles>');
        if $has_handles {
            %attr_info<handles> := $has_handles[0];
        }
        @PACKAGE[0].attributes.push(%attr_info);

        # If no twigil, note $foo is an alias to $!foo.
        if $twigil eq '' {
            $BLOCK.symbol($name, :attr_alias($attrname));
        }

        # Nothing to emit here; just hand  back an empty node, but also
        # annotate it with the attribute table.
        $past := PAST::Op.new( :pasttype('null') );
        $past<attribute_data> := %attr_info;
    }
    else {
        # Not an attribute - need to emit delcaration here.
        # Create the container
        my $cont := $sigil eq '%' ??
            PAST::Op.new( :name('&CREATE_HASH_FROM_LOW_LEVEL'), :pasttype('call') ) !!
            PAST::Op.new( sigiltype($sigil), :pirop('new Ps') );
        
        # Give it a 'rw' property unless it's explicitly readonly.
        my $readtype := trait_readtype($trait_list);
        if $readtype eq 'CONFLICT' {
            $/.CURSOR.panic('Can not apply more than one of: is copy, is rw, is readonly');
        }
        if $readtype eq 'copy' {
            $/.CURSOR.panic("'is copy' trait not valid on variable declaration");
        }
        my $true := PAST::Var.new( :name('true'), :scope('register') );
        my $vivipast := $readtype ne 'readonly' ??
            PAST::Op.new( $cont, 'rw', $true, :pirop('setprop')) !!
            $cont;

        # If it's a scalar, mark it as scalar (non-flattening)
        if $sigil eq '$' || $sigil eq '&' {
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
                unless $trait<trait_is_compiler_handled> {
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

        # For arrays, need to transform_to_p6opaque. XXX Find a neater way
        # to do this.
        if $sigil eq '@' {
            get_var_traits_node($BLOCK, $name).push(PAST::Op.new(
                :pirop('transform_to_p6opaque vP'),
                PAST::Var.new( :name('$P0'), :scope('register') )
            ));
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
    my $block := $<blockoid>.ast;
    $block.blocktype('declaration');
    $block.control('return_pir');
    if pir::defined__IP($block<placeholder_sig>) && $<multisig> {
        $/.CURSOR.panic('Placeholder variable cannot override existing signature');
    }
    my $signature := $<multisig>                     ?? $<multisig>[0].ast    !!
            pir::defined__IP($block<placeholder_sig>) ?? $block<placeholder_sig> !!
            Perl6::Compiler::Signature.new();
    $signature.set_default_parameter_type('Any');
    add_signature($block, $signature, 1);
    if $<trait> {
        emit_routine_traits($block, $<trait>, 'Sub');
    }
    my $past;
    if $<deflongname> {
        # Set name.
        my $name := '&' ~ ~$<deflongname>[0].ast;
        $block.name(~$<deflongname>[0].ast);
        $block.nsentry('');

        # Create a code object for the routine
        my $symbol := @BLOCK[0].symbol($name);

        # Check for common error conditions.
        if $symbol {
            if $*MULTINESS eq 'only' {
                $/.CURSOR.panic('Can not declare only routine ' ~ $name ~
                    ' when another routine with this name was already declared');
            }
            if !$symbol<proto> && !$*MULTINESS {
                $/.CURSOR.panic('Can not re-declare sub ' ~ $name ~ ' without declaring it multi');
            }
        }
        else { 
            $symbol := @BLOCK[0].symbol($name, :scope<lexical>); 
        }

        # Create a code object for use in the block
        my $multiflag := $*MULTINESS eq 'proto' ?? 2 !! $*MULTINESS eq 'multi';
        my $code := block_closure($block, 'Sub', $multiflag);
        # Bind the code object to a unique register
        $code := PAST::Var.new( :name($block.unique('code_')), :viviself($code),
                                :scope<register>, :isdecl(1) );
        # ..and use the code object as the result for this node.
        $past := PAST::Var.new( :name($code.name), :scope<register> );

        # Handle multisubs...
        if $multiflag || $symbol<proto> {
            # If this is a proto, stash that information.
            if $*MULTINESS eq 'proto' { $symbol<proto> := $code; }

            # If we already have a multi candidate, just add to it.
            if $symbol<multi> { 
                $symbol<multi>.push($code); $code := 0 ; 
            }
            else {
                # Otherwise, built a multi candidate.
                $symbol<multi> := PAST::Op.new( :pasttype<callmethod>,
                                      :name('set_candidates'),
                                      PAST::Op.new( :pirop<new__Ps>, 'Perl6MultiSub'),
                                      $code );
                # Merge it with outer (lexical) or existing (package) candidates
                $code := PAST::Op.new( :pasttype<callmethod>,
                             :name<merge_candidates>,
                             $symbol<multi>,
                             $*SCOPE eq 'our'
                                 ?? PAST::Var.new( :name($name), :scope('package') )
                                 !! PAST::Op.new( :pirop<find_lex_skip_current__Ps>, $name ) );
            }
        }

        # Bind the block code or multisub object
        if $code {
            # If it's package scoped, also bind into the package
            if $*SCOPE eq 'our' {
                $code := PAST::Op.new( :pasttype<bind>, 
                             PAST::Var.new( :name($name), :scope('package'), :isdecl(1) ),
                             $code);
            }
            # Always bind lexically (like 'our' variables do)
            @BLOCK[0][0].push( 
                PAST::Var.new( :name($name), :scope('lexical'), :isdecl(1),
                               :lvalue(1), :viviself($code), :node($/) ) );
        }

        # If it's package scoped, we also need a separate compile-time binding into the package
        if $*SCOPE eq 'our' {
            my $code := block_code($block, 'Sub', $multiflag);
            $symbol := @PACKAGE[0].block.symbol($name);
            if $multiflag || $symbol<pkgproto> {
                # If this is a proto, stash that information.
                if $*MULTINESS eq 'proto' { $symbol<pkgproto> := $code; }

                if $symbol<pkgmulti> { 
                    $symbol<pkgmulti>.push($code); 
                    $code := 0; 
                }
                else {
                    $symbol<pkgmulti> := PAST::Op.new( :pasttype<callmethod>,
                                          :name('set_candidates'),
                                          PAST::Op.new( :pirop<new__Ps>, 'Perl6MultiSub'),
                                          $code );
                    $code := PAST::Op.new( :pasttype<callmethod>,
                                 :name<merge_candidates>,
                                 $symbol<pkgmulti>,
                                 PAST::Var.new( :name($name), :scope('package') ) );
                }
            }
            if $code {
                @PACKAGE[0].block.loadinit.push(
                    PAST::Op.new( :pasttype<bind>,
                        PAST::Var.new( :name($name), :scope('package') ),
                        $code) );
            }
        }
    }
    elsif $*MULTINESS {
        $/.CURSOR.panic('Can not put ' ~ $*MULTINESS ~ ' on anonymous routine');
    }
    else {
        # Just wrap in a Sub.
        $past := block_closure($block, 'Sub', 0);
    }
    make $past;
}


method method_def($/) {
    my $past := $<blockoid>.ast;
    $past.blocktype('declaration');
    $past.control('return_pir');

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
    add_signature($past, $sig, 1);
    $past[0].unshift(PAST::Var.new( :name('self'), :scope('lexical'), :isdecl(1), :viviself(sigiltype('$')) ));
    $past.symbol('self', :scope('lexical'));

    # Emit traits.
    if $<trait> {
        emit_routine_traits($past, $<trait>, $*METHODTYPE);
    }

    # Method container.
    if $<longname> {
        # Set up us the name.
        my $name := $<longname>.Str;
        if $<specials> eq '!' { $name := '!' ~ $name; }
        $past.name($name);
        $past.nsentry('');
        my $multi_flag := $*MULTINESS eq 'proto' ?? 2 !! 
                          $*MULTINESS eq 'multi' ?? 1 !!
                          0;
        
        # Create code object using a reference to $past.
        my $code := block_code($past, $*METHODTYPE, $multi_flag);

        # Get hold of the correct table to install it in, and install.
        our @PACKAGE;
        unless +@PACKAGE { $/.CURSOR.panic("Can not declare method outside of a package"); }
        my %table;
        if $<specials> eq '^' {
            %table := @PACKAGE[0].meta_methods();
        }
        else {
            %table := @PACKAGE[0].methods();
        }
        install_method($/, $code, $name, %table);
    }
    elsif $*MULTINESS {
        $/.CURSOR.panic('Can not put ' ~ $*MULTINESS ~ ' on anonymous routine');
    }
    else {
        $past := block_closure($past, $*METHODTYPE, 0);
    }

    make $past;
}

sub install_method($/, $code, $name, %table) {
    my $installed;
    
    # Create method table entry if we need one.
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
        if $*MULTINESS eq 'proto' { %table{$name}<proto> := $code; }

        # Create multi container if we don't have one; otherwise, just push 
        # this candidate onto it.
        if %table{$name}<multis> {
            %table{$name}<multis>.push($code);
        }
        else {
            $code := PAST::Op.new(
                :pasttype('callmethod'),
                :name('set_candidates'),
                PAST::Op.new( :inline('    %r = new ["Perl6MultiSub"]') ),
                $code
            );
            %table{$name}<code_ref> := %table{$name}<multis> := $installed := $code;
        }
    }
    else {
        %table{$name}<code_ref> := $installed := $code;
    }

    # If we did install something (we maybe didn't need to if this is a multi),
    # we may need to also pop it in other places.
    if $installed {
        if $*SCOPE eq 'my' {
            @BLOCK[0][0].push(PAST::Var.new( :name('&' ~ $name), :isdecl(1),
                    :viviself($installed), :scope('lexical') ));
            @BLOCK[0].symbol($name, :scope('lexical') );
        }
        elsif $*SCOPE eq 'our' {
            @PACKAGE[0].block.loadinit.push(PAST::Op.new(
                :pasttype('bind'),
                PAST::Var.new( :name('&' ~ $name), :scope('package') ),
                $installed
            ));
        }
    }
}

our %REGEX_MODIFIERS;
method regex_declarator:sym<regex>($/, $key?) {
    if ($key) {
        my %h;
        %REGEX_MODIFIERS := %h;
    } else {
        make $<regex_def>.ast;
    }
}

method regex_declarator:sym<token>($/, $key?) {
    if ($key) {
        my %h;
        %h<r> := 1;
        %REGEX_MODIFIERS := %h;
    } else {
        make $<regex_def>.ast;
    }
}

method regex_declarator:sym<rule>($/, $key?) {
    if ($key) {
        my %h;
        %h<r> := 1; %h<s> :=1;
        %REGEX_MODIFIERS := %h;
    } else {
        make $<regex_def>.ast;
    }
}

method regex_def($/, $key?) {
    my $name := ~$<deflongname>[0];
    my @MODIFIERS := Q:PIR {
        %r = get_hll_global ['Regex';'P6Regex';'Actions'], '@MODIFIERS'
    };
    
    my $past;
    if $key eq 'open' {
        @MODIFIERS.unshift(%REGEX_MODIFIERS);
        # The following is so that <sym> can work
        Q:PIR {
            $P0 = find_lex '$name'
            set_hll_global ['Regex';'P6Regex';'Actions'], '$REGEXNAME', $P0
        };
        return 0;
    } elsif $*MULTINESS eq 'proto' {
        # Need to build code for setting up a proto-regex.
        @MODIFIERS.shift;
        @BLOCK.shift;
        unless ($name) {
            $/.CURSOR.panic('proto ' ~ ~$<sym> ~ 's cannot be anonymous');
        }
        our @PACKAGE;
        unless +@PACKAGE {
            $/.CURSOR.panic("Can not declare named " ~ ~$<sym> ~ " outside of a package");
        }
        my %table;
        %table := @PACKAGE[0].methods();
        unless %table{$name} { my %tmp; %table{$name} := %tmp; }
        if %table{$name} {
            $/.CURSOR.panic('Cannot declare proto ' ~ ~$<sym> ~ ' ' ~ $name ~
                ' when another with this name was already declared');
        }
        %table{$name}<code_ref> :=
            block_closure(
                PAST::Block.new( :name($name),
                    PAST::Op.new(
                        PAST::Var.new( :name('self'), :scope('register') ),
                        $name,
                        :name('!protoregex'),
                        :pasttype('callmethod')
                    ),
                    :lexical(0),
                    :blocktype('method'),
                    :pirflags(':anon'),
                    :node($/)
                ),
                'Regex', 0);
        %table{'!PREFIX__' ~ $name}<code_ref> :=
            block_closure(
                PAST::Block.new( :name('!PREFIX__' ~ $name),
                    PAST::Op.new(
                        PAST::Var.new( :name('self'), :scope('register') ),
                        $name,
                        :name('!PREFIX__!protoregex'),
                        :pasttype('callmethod')
                    ),
                    :blocktype('method'),
                    :pirflags(':anon'),
                    :lexical(0),
                    :node($/)
                ),
                'Regex', 0);
    } else {
        # Clear modifiers stack entry for this regex.
        @MODIFIERS.shift;

        # Create the regex sub along with its signature.
        $past := Regex::P6Regex::Actions::buildsub($<p6regex>.ast, @BLOCK.shift);
        $past.unshift(PAST::Op.new(
            :pasttype('inline'),
            :inline("    .local pmc self\n    self = find_lex 'self'")
            ));
        my $sig := $<signature> ?? $<signature>[0].ast !! Perl6::Compiler::Signature.new();
        $sig.add_invocant();
        $sig.set_default_parameter_type('Any');
        $past[0].unshift(PAST::Var.new( :name('self'), :scope('lexical'), :isdecl(1), :viviself(sigiltype('$')) ));
        $past.symbol('self', :scope('lexical'));
        add_signature($past, $sig, 1);
        $past.name($name);
        $past.blocktype("declaration");
        
        # If the methods are not :anon they'll conflict at class composition time.
        $past.pirflags(':anon');

        # Create code object and install it provided it has a name.
        if ($name) {
            my $code := block_closure(blockref($past), 'Regex', 0);
            our @PACKAGE;
            unless +@PACKAGE {
                $/.CURSOR.panic("Can not declare named " ~ ~$<sym> ~ " outside of a package");
            }
            my %table;
            %table := @PACKAGE[0].methods();
            install_method($/, $code, $name, %table);
        }
        else {
            $past := block_closure($past, 'Regex', 0);
        }
    }
    make $past;
}

method type_declarator:sym<enum>($/) {
    my $value_ast := PAST::Op.new(
        :pasttype('call'),
        :name('!create_anon_enum'),
        $<circumfix>.ast
    );
    if $<name> {
        # Named; need to compile and run the AST right away.
        our $?RAKUDO_HLL;
        my $compiled := PAST::Compiler.compile(PAST::Block.new(
            :hll($?RAKUDO_HLL), $value_ast
        ));
        my $result := (pir::find_sub_not_null__ps('!YOU_ARE_HERE'))($compiled);
        
        # Only support our-scoped so far.
        unless $*SCOPE eq '' || $*SCOPE eq 'our' {
            $/.CURSOR.panic("Do not yet support $*SCOPE scoped enums");
        }
        
        # Install names.
        $/.CURSOR.add_name(~$<name>[0]);
        for $result {
            $/.CURSOR.add_name(~$_.key);
            $/.CURSOR.add_name(~$<name>[0] ~ '::' ~ ~$_.key);
        }
        
        # Emit code to set up named enum.
        @PACKAGE[0].block.loadinit.push(PAST::Op.new(
            :pasttype('call'),
            :name('!setup_named_enum'),
            ~$<name>[0],
            $value_ast
        ));
        my @name := Perl6::Grammar::parse_name(~$<name>[0]);
        make PAST::Var.new( :name(@name.pop), :namespace(@name), :scope('package') );
    }
    else {
        # Anonymous, so we're done.
        make $value_ast;
    }
}

method type_declarator:sym<subset>($/) {
    # Figure out our refinee.
    my $of_trait := has_compiler_trait($<trait>, '&trait_mod:<of>');
    my $refinee := $of_trait ??
        $of_trait[0] !!
        PAST::Var.new( :name('Any'), :namespace(Nil), :scope('package') );

    # Construct subset and install it in the right place.
    my $cons_past := PAST::Op.new(
        :name('&CREATE_SUBSET_TYPE'),
        $refinee,
        $<EXPR> ?? where_blockify($<EXPR>[0].ast) !!
                   PAST::Var.new( :name('True'), :namespace('Bool'), :scope('package') )
    );

    # Stick it somewhere appropriate.
    if $<longname> {
        my $name := $<longname>[0].Str;
        if $*SCOPE eq '' || $*SCOPE eq 'our' {
            # Goes in the package.
            @PACKAGE[0].block.loadinit.push(PAST::Op.new(
                :pasttype('bind'),
                PAST::Var.new( :name($name), :scope('package') ),
                $cons_past
            ));
            @BLOCK[0].symbol($name, :scope('package') );
        }
        elsif $*SCOPE eq 'my' {
            # Install in the lexpad.
            @BLOCK[0][0].push(PAST::Var.new(
                :name($name), :isdecl(1),
                :viviself($cons_past), :scope('lexical')
            ));
            @BLOCK[0].symbol($name, :scope('lexical') );
        }
        else {
            $/.CURSOR.panic("Can not declare a subset with scope declarator " ~ $*SCOPE);
        }
        make PAST::Var.new( :name($name) );
    }
    else {
        if $*SCOPE ne '' && $*SCOPE ne 'anon' {
            $/.CURSOR.panic('A ' ~ $*SCOPE ~ ' scoped subset must have a name.');
        }
        make $cons_past;
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

method fakesignature($/) {
    @BLOCK.shift;
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
                $/.CURSOR.panic("Cannot put ':' parameter separator after first parameter");
            }
        }
        if @*seps[$cur_param] eq ';;' {
            $is_multi_invocant := 0;
        }
        $signature.add_parameter($param);
        $cur_param := $cur_param + 1;
    }
    @BLOCK[0]<signature> := $signature;
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
        my $coerce := has_compiler_trait($<trait>, '&trait_mod:<as>');
        if $coerce {
            $*PARAMETER.coerce_to(PAST::Op.new( :pasttype('callmethod'), :name('perl'), $coerce[0]));
        }
    }

    make $*PARAMETER;
}

method param_var($/) {
    if $<signature> {
        if pir::defined__IP($*PARAMETER.sub_llsig) {
            $/.CURSOR.panic('Can not have more than one sub-signature for a parameter');
        }
        $*PARAMETER.sub_llsig( $<signature>.ast );
        if pir::substr(~$/, 0, 1) eq '[' {
            $*PARAMETER.var_name('@');
        }
    }
    else {
        my $twigil := $<twigil> ?? ~$<twigil>[0] !! '';
        $*PARAMETER.var_name(~$/);
        if $twigil eq '' {
            if $<name> {
                if @BLOCK[0].symbol(~$/) {
                    $/.CURSOR.panic("Redeclaration of symbol ", ~$/);
                }
                @BLOCK[0].symbol(~$/, :scope($*SCOPE eq 'my' ?? 'lexical' !! 'package'));
            }
        }
        elsif $twigil ne '!' && $twigil ne '.' && $twigil ne '*' {
            my $error := "In signature parameter, '" ~ ~$/ ~ "', it is illegal to use '" ~ $twigil ~ "' twigil";
            if $twigil eq ':' {
                $error := "In signature parameter, placeholder variables like " ~ ~$/ ~ " are illegal\n"
                           ~ "you probably meant a named parameter: ':" ~ $<sigil> ~ ~$<name>[0] ~ "'";
            }
            $/.CURSOR.panic($error);
        }
    }
}

method named_param($/) {
    if $<name>               { $*PARAMETER.names.push(~$<name>); }
    elsif $<param_var><name> { $*PARAMETER.names.push(~$<param_var><name>[0]); }
    else                     { $*PARAMETER.names.push(''); }
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
        $*PARAMETER.nom_type(PAST::Op.new(
            :pirop('deobjectref__PP'),
            PAST::Op.new( :pasttype('callmethod'), :name('WHAT'), $<value>.ast )
        ));
        $*PARAMETER.cons_types.push($<value>.ast);
    }
    else {
        $/.CURSOR.panic('Can not do non-typename cases of type_constraint yet');
    }
}

method post_constraint($/) {
    if $<signature> {
        if pir::defined__IP($*PARAMETER.sub_llsig) {
            $/.CURSOR.panic('Can not have more than one sub-signature for a parameter');
        }
        $*PARAMETER.sub_llsig( $<signature>.ast );
    }
    else {
        $*PARAMETER.cons_types.push(where_blockify($<EXPR>.ast));
    }
}

method trait($/) {
    my $past;
    if $<trait_mod> {
        $past := $<trait_mod>.ast;
    }
    elsif $<colonpair> {
        $/.CURSOR.panic('traits specified as colon pairs not yet understood');
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
            :scope(is_lexical(~$<longname>) ?? 'lexical' !! 'package'),
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
    unless $past.isa(PAST::Op) && $past.pasttype() eq 'callmethod' {
        $/.CURSOR.panic("Can not use " ~ $<sym>.Str ~ " on a non-identifier method call");
    }
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
    $past.pasttype('callmethod');
    if $<longname> {
        # May just be .foo, but could also be .Foo::bar
        my @parts := Perl6::Grammar::parse_name(~$<longname>);
        my $name := @parts.pop;
        if +@parts {
            my $scope := is_lexical(pir::join('::', @parts)) ?? 'lexical' !! 'package';
            $past.unshift(PAST::Var.new(
                :name(@parts.pop),
                :namespace(@parts),
                :scope($scope)
            ));
            $past.unshift($name);
            $past.name('!dispatch_::');
            $past.pasttype('call');
        }
        else {
            $past.name( $name );
        }
    }
    elsif $<quote> {
        $past.name( $<quote>.ast );
    }
    elsif $<variable> {
        $past.unshift($<variable>.ast);
        $past.name('!dispatch_variable');
        $past.pasttype('call');
    }
    make $past;
}


method term:sym<self>($/) {
    make PAST::Var.new( :name('self'), :node($/) );
}

method term:sym<Nil>($/) {
    make PAST::Op.new(:name('&Nil'), :node($/) );
}

method term:sym<rand>($/) {
    make PAST::Op.new(:name('&rand'), :node($/) );
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
    $past.unshift(PAST::Var.new( :name('$_'), :scope('lexical') ) );
    make $past;
}

method term:sym<identifier>($/) {
    my $past := capture_or_parcel($<args>.ast, ~$<identifier>);
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
        $var := PAST::Var.new(
            :name(~$name), :namespace($ns), :scope('package'),
            :viviself(PAST::Op.new(
                :pasttype('call'), :name('!FAIL'),
                "Can not find sub " ~ ~$<longname>
            ))
        );
    }
    my $past := $var;
    if $<args> {
        $past := capture_or_parcel($<args>.ast, ~$<longname>);
        if $ns {
            $past.unshift($var);
            unless pir::substr($var.name, 0, 1) eq '&' {
                $var.name('&' ~ $var.name);
            }
        }
        else { $past.name('&' ~ $name); }
    }
    elsif $<arglist> {
        $past := $<arglist>[0].ast;
        $past.pasttype('callmethod');
        $past.name('!select');
        $past.unshift($var);
    }
    $past.node($/);
    make $past;
}

method term:sym<pir::op>($/) {
    if $*FORBID_PIR {
        pir::die("pir::op forbidden in safe mode\n");
    }
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
        :pasttype('callmethod'), :name('new'), :node($/), :lvalue(1), :returns('Whatever'),
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
    # Build up argument list, hanlding nameds as we go.
    my $past := PAST::Op.new( );
    if $<EXPR> {
        my $expr := $<EXPR>.ast;
        if $expr.name eq '&infix:<,>' {
            for $expr.list { $past.push(handle_named_parameter($_)); }
        }
        else { $past.push(handle_named_parameter($expr)); }
    }

    # See if we have any uses of prefix:<|>; if we have, then we take it and
    # evaluate it once. We then stick it in a register, and pull out an RPA
    # and a Hash that Parrot knows what to do with.
    my $result := PAST::Op.new( :pasttype('call'), :node($/) );
    for @($past) {
        if $_.isa(PAST::Op) && $_.name() eq '&prefix:<|>' {
            my $reg_name := $past.unique('flatten_tmp_');
            my $steps := PAST::Stmts.new(
                PAST::Op.new( :pasttype('bind'),
                    PAST::Var.new( :name($reg_name), :scope('register'), :isdecl(1) ),
                    $_
                ),
                PAST::Op.new(
                    :pasttype('callmethod'), :name('!PARROT_POSITIONALS'),
                    PAST::Var.new( :name($reg_name), :scope('register') )
                )
            );
            $steps.flat(1);
            $result.push($steps);
            $result.push(PAST::Op.new(
                :flat(1), :named(1),
                :pasttype('callmethod'), :name('!PARROT_NAMEDS'),
                PAST::Var.new( :name($reg_name), :scope('register') )
            ));
        }
        else {
            $result.push($_);
        }
    }

    make $result;
}

sub handle_named_parameter($arg) {
    if $arg ~~ PAST::Node && $arg.returns() eq 'Pair' {
        my $result := $arg[2];
        $result.named(~$arg[1].value());
        $result<before_promotion> := $arg;
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
    my $stmts := +$<pblock><blockoid><statementlist><statement>;
    if $stmts == 0 {
        # empty block, so a hash
        $is_hash := 1;
    }
    elsif $stmts == 1 {
        my $elem := $past[1][0];
        if $elem ~~ PAST::Op && $elem.name eq '&infix:<,>' {
            # block contains a list, so test the first element
            $elem := $elem[0];
        }
        if $elem ~~ PAST::Op 
                && ($elem.returns eq 'Pair' || $elem.name eq '&infix:<=>>') {
            # first item is a pair
            $is_hash := 1;
        }
        elsif $elem ~~ PAST::Var
                && pir::substr($elem.name, 0, 1) eq '%' {
            # first item is a hash
            $is_hash := 1;
        }
    }
    if $is_hash && $past.arity < 1 {
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
        $past := block_closure($past, 'Block', 0);
        $past<bareblock> := 1;
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
    if $/<drop> { make PAST::Stmts.new(); return 0; }
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
        my $inv := $/[0].ast;
        $past.unshift(
            PAST::Op.ACCEPTS($past) && $past.pasttype eq 'callmethod'
            ?? PAST::Op.new( :pirop('deref_unless_object PP'), $inv, :returns($inv.returns) )
            !! $inv
        );
    }
    else {
        for $/.list { if $_.ast { $past.push($_.ast); } }
    }
    if $key eq 'PREFIX' || $key eq 'INFIX' || $key eq 'POSTFIX' {
        $past := whatever_curry($past, $key eq 'INFIX' ?? 2 !! 1);
    }
    make $past;
}

method prefixish($/) {
    if $<prefix_postfix_meta_operator> {
        my $opsub := '&prefix:<' ~ $<OPER>.Str ~ '<<>';
        unless %*METAOPGEN{$opsub} {
            my $base_op := '&prefix:<' ~ $<OPER>.Str ~ '>';
            $*UNITPAST.loadinit.push(PAST::Op.new(
                :pasttype('bind'),
                PAST::Var.new( :name($opsub), :scope('package') ),
                PAST::Op.new(
                    :pasttype('callmethod'), :name('assuming'),
                    PAST::Op.new( :pirop('find_sub_not_null__Ps'), '&hyper' ),
                    PAST::Op.new( :pirop('find_sub_not_null__Ps'), $base_op )
                )
            ));
            %*METAOPGEN{$opsub} := 1;
        }
        make PAST::Op.new( :name($opsub), :pasttype('call') );
    }
}

method infixish($/) {
    if $<infix_postfix_meta_operator> {
        my $sym := ~$<infix><sym>;
        my $opsub := "&infix:<$sym=>";
        unless %*METAOPGEN{$opsub} {
            $*UNITPAST.loadinit.push(
                PAST::Op.new( :name('!gen_assign_metaop'), $sym,
                              :pasttype('call') )
            );
            %*METAOPGEN{$opsub} := 1;
        }
        make PAST::Op.new( :name($opsub), :pasttype('call') );
    }

    if $<infix_prefix_meta_operator> {
        my $metaop := ~$<infix_prefix_meta_operator><sym>;
        my $sym := ~$<infix_prefix_meta_operator><infixish><OPER>;
        my $opsub := "&infix:<$/>";
        my $base_opsub := "&infix:<$sym>";
        if $opsub eq "&infix:<!=>" {
            $base_opsub := "&infix:<==>";
        }
        unless %*METAOPGEN{$opsub} {
            my $helper := "";
            if $metaop eq '!' {
                $helper := '&negate';
            } elsif $metaop eq 'R' {
                $helper := '&reverseargs';
            } elsif $metaop eq 'S' {
                $helper := '&sequentialargs';
            } elsif $metaop eq 'X' {
                $helper := '&crosswith';
            } elsif $metaop eq 'Z' {
                $helper := '&zipwith';
            }

            $*UNITPAST.loadinit.push(
                PAST::Op.new( :pasttype('bind'),
                              PAST::Var.new( :name($opsub), :scope('package') ),
                              PAST::Op.new( :pasttype('callmethod'),
                                            :name('assuming'),
                                            PAST::Op.new( :pirop('find_sub_not_null__Ps'),
                                                          $helper ),
                                            PAST::Op.new( :pirop('find_sub_not_null__Ps'),
                                                           $base_opsub ) ) ) );
            %*METAOPGEN{$opsub} := 1;
        }

        make PAST::Op.new( :name($opsub), :pasttype('call') );
    }

    if $<infixish> {
        make $<infixish>.ast;
    }
}

method prefix_circumfix_meta_operator:sym<reduce>($/) {
    my $opsub := '&prefix:<' ~ ~$/ ~ '>';
    unless %*METAOPGEN{$opsub} {
        my $base_op := '&infix:<' ~ $<op><OPER>.Str ~ '>';
        $*UNITPAST.loadinit.push(PAST::Op.new(
            :pasttype('bind'),
            PAST::Var.new( :name($opsub), :scope('package') ),
            PAST::Op.new(
                :pasttype('callmethod'), :name('assuming'),
                PAST::Op.new( :pirop('find_sub_not_null__Ps'), '&reducewith' ),
                PAST::Op.new( :pirop('find_sub_not_null__Ps'), $base_op ),
                PAST::Val.new( :named('triangle'), :value($<triangle> ?? 1 !! 0) ),
                PAST::Val.new( :named('chaining'), :value($<op><OPER><O><prec> eq 'm=') ),
                PAST::Val.new( :named('right-assoc'), :value($<op><OPER><O><assoc> eq 'right') )
            )
        ));
        %*METAOPGEN{$opsub} := 1;
    }
    make PAST::Op.new( :name($opsub), :pasttype('call') );
}

method infix_circumfix_meta_operator:sym«<< >>»($/) {
    make make_hyperop($/);
}

method infix_circumfix_meta_operator:sym<« »>($/) {
    make make_hyperop($/);
}

sub make_hyperop($/) {
    my $opsub := '&infix:<' ~ ~$/ ~ '>';
    unless %*METAOPGEN{$opsub} {
        my $base_op := '&infix:<' ~ $<infixish><OPER>.Str ~ '>';
        my $dwim_lhs := $<opening> eq '<<' || $<opening> eq '«';
        my $dwim_rhs := $<closing> eq '>>' || $<closing> eq '»';
        $*UNITPAST.loadinit.push(PAST::Op.new(
            :pasttype('bind'),
            PAST::Var.new( :name($opsub), :scope('package') ),
            PAST::Op.new(
                :pasttype('callmethod'), :name('assuming'),
                PAST::Op.new( :pirop('find_sub_not_null__Ps'), '&hyper' ),
                PAST::Op.new( :pirop('find_sub_not_null__Ps'), $base_op ),
                PAST::Val.new( :value($dwim_lhs), :named('dwim-left') ),
                PAST::Val.new( :value($dwim_rhs), :named('dwim-right') )
            )
        ));
        %*METAOPGEN{$opsub} := 1;
    }
    return PAST::Op.new( :name($opsub), :pasttype('call') );
}

method postfixish($/) {
    if $<postfix_prefix_meta_operator> {
        my $past := $<OPER>.ast;
        if $past && $past.isa(PAST::Op) && $past.pasttype() eq 'call' {
            $past.unshift($past.name());
            $past.name('!dispatch_dispatcher_parallel');
        }
        elsif $past && $past.isa(PAST::Op) && $past.pasttype() eq 'callmethod' {
            $past.unshift($past.name());
            $past.name('!dispatch_method_parallel');
            $past.pasttype('call');
        }
        else {
            # Hyper-op over a normal postfix.
            my $opsub := '&postfix:<>>' ~ $<OPER>.Str ~ '>';
            unless %*METAOPGEN{$opsub} {
                my $base_op := '&postfix:<' ~ $<OPER>.Str ~ '>';
                $*UNITPAST.loadinit.push(PAST::Op.new(
                    :pasttype('bind'),
                    PAST::Var.new( :name($opsub), :scope('package') ),
                    PAST::Op.new(
                        :pasttype('callmethod'), :name('assuming'),
                        PAST::Op.new( :pirop('find_sub_not_null__Ps'), '&hyper' ),
                        PAST::Op.new( :pirop('find_sub_not_null__Ps'), $base_op )
                    )
                ));
                %*METAOPGEN{$opsub} := 1;
            }
            $past := PAST::Op.new( :name($opsub), :pasttype('call') );
        }
        make $past;
    }
}

method postcircumfix:sym<[ ]>($/) {
    my $past := PAST::Op.new( :name('!postcircumfix:<[ ]>'), :pasttype('call'), :node($/) );
    if $<semilist><statement> { $past.push($<semilist>.ast); }
    make $past;
}

method postcircumfix:sym<{ }>($/) {
    my $past := PAST::Op.new( :name('!postcircumfix:<{ }>'), :pasttype('call'), :node($/) );
    if $<semilist><statement> {
        if +$<semilist><statement> > 1 {
            $/.CURSOR.panic("Sorry, multi-dimensional indexes are not yet supported");
        }
        $past.push($<semilist>.ast);
    }
    make $past;
}

method postcircumfix:sym<ang>($/) {
    my $past := PAST::Op.new( :name('!postcircumfix:<{ }>'), :pasttype('call'), :node($/) );
    $past.push( $<quote_EXPR>.ast ) 
        if +$<quote_EXPR><quote_delimited><quote_atom> > 0;
    make $past;
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
    if $<integer> { make PAST::Val.new( :value($<integer>.ast), :returns('Int') ); }
    elsif $<dec_number> { make $<dec_number>.ast; }
    elsif $<rad_number> { make $<rad_number>.ast; }
    else {
        make PAST::Var.new( :name(~$/), :namespace(''), :scope('package') );
    }
}

method dec_number($/) {
    my $int  := $<int> ?? ~$<int> !! "0";
    my $frac := $<frac> ?? ~$<frac> !! "0";
    if $<escale> {
        my $exp := ~$<escale>[0]<decint>;
        make PAST::Op.new(
            :pasttype('call'),
            PAST::Var.new(:scope('package'), :name('&str2num-num'), :namespace('Str')),
             0, $int, $frac, ($<escale>[0]<sign> eq '-'), $exp
        );
    } else {
        make PAST::Op.new(
            :pasttype('call'),
            PAST::Var.new(:scope('package'), :name('&str2num-rat'), :namespace('Str')),
             0, $int, $frac
        );
    }
}

method rad_number($/) {
    my $radix    := +($<radix>.Str);
    if $<circumfix> {
        make PAST::Op.new(:name('&radcalc'), :pasttype('call'),
            $radix, $<circumfix>.ast);
    } else {
        my $intpart  := $<intpart>.Str;
        my $fracpart := $<fracpart> ?? $<fracpart>.Str !! "0";
        my $intfrac  := $intpart ~ $fracpart; #the dot is a part of $fracpart, so no need for ~ "." ~
        my $base     := $<base> ?? +($<base>[0].Str) !! 0;
        my $exp      := $<exp> ?? +($<exp>[0].Str) !! 0;

        make PAST::Op.new( :name('&radcalc'), :pasttype('call'),
            $radix, $intfrac, $base, $exp
        );
    }
}

method typename($/) {
    my $past;

    if is_lexical($<longname>.Str) {
        # We need to build a thunk.
        my $sig := Perl6::Compiler::Signature.new(
            Perl6::Compiler::Parameter.new(:var_name('$_')));
        $past := make_block_from(
                    $sig,
                    PAST::Op.new( :pasttype('callmethod'), :name('ACCEPTS'),
                        PAST::Var.new(:name($<longname>.Str),:scope('lexical')),
                        PAST::Var.new(:name('$_'), :scope('lexical') ) ),
                    'Block'
                 );
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
    if $<arglist> {
        my $args := $<arglist>[0].ast;
        $args.pasttype('callmethod');
        $args.name('!select');
        $args.unshift($past);
        $past := $args;
    }
    if $<typename> {
        $past := PAST::Op.new(
            :pasttype('callmethod'), :name('!select'),
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
    if $*FORBID_PIR {
        pir::die("Q:PIR forbidden in safe mode\n");
    }
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
    make block_closure($past, 'Regex', 0);
}
method quote:sym<rx>($/) {
    my $past := Regex::P6Regex::Actions::buildsub($<p6regex>.ast);
    make block_closure($past, 'Regex', 0);
}
method quote:sym<m>($/) {
    my $past := Regex::P6Regex::Actions::buildsub($<p6regex>.ast);
    make block_closure($past, 'Regex', 0);
}

method quote:sym<s>($/) {
    # Build the regex.
    my $regex_ast := Regex::P6Regex::Actions::buildsub($<p6regex>.ast);
    my $regex := block_closure($regex_ast, 'Regex', 0);

    # Quote needs to be closure-i-fied.
    my $closure_ast := PAST::Block.new(
        PAST::Stmts.new(),
        PAST::Stmts.new(
            $<quote_EXPR> ?? $<quote_EXPR>.ast !! $<EXPR>.ast
        )
    );
    my $closure := block_closure($closure_ast, 'Block', 0);

    # Make a Substitution.
    $regex.named('matcher');
    $closure.named('replacer');
    make PAST::Op.new(
        :pasttype('callmethod'), :name('new'),
        PAST::Var.new( :name('Substitution'), :scope('package') ),
        $regex, $closure
    );
}

method quote_escape:sym<$>($/) {
    make steal_back_spaces($/, PAST::Op.new( $<EXPR>.ast, :pirop('set SP') ));
}

method quote_escape:sym<array>($/) {
    make steal_back_spaces($/, PAST::Op.new( $<EXPR>.ast, :pirop('set SP') ));
}

method quote_escape:sym<%>($/) {
    make steal_back_spaces($/, PAST::Op.new( $<EXPR>.ast, :pirop('set SP') ));
}

method quote_escape:sym<&>($/) {
    make steal_back_spaces($/, PAST::Op.new( $<EXPR>.ast, :pirop('set SP') ));
}

# Unfortunately, the operator precedence parser (probably correctly)
# steals spaces after a postfixish. Thus "$a $b" would get messed up.
# Here we take them back again. Hacky, better solutions welcome.
sub steal_back_spaces($/, $expr) {
    my $pos := pir::length__IS($/) - 1;
    while pir::is_cclass__IISI(32, $/, $pos) {
        $pos--;
    }
    my $nab_back := pir::substr__SSI($/, $pos + 1);
    if $nab_back {
        PAST::Op.new( :pasttype('call'), :name('&infix:<~>'), $expr, ~$nab_back )
    }
    else {
        $expr
    }
}

method quote_escape:sym<{ }>($/) {
    make PAST::Op.new(
        :pirop('set S*'), block_immediate($<block>.ast), :node($/)
    );
}

# overrides versions from HLL::Actions to handle Perl6Str
# and use &infix:<,> to build the parcel
method quote_EXPR($/) {
    my $past := $<quote_delimited>.ast;
    if $/.CURSOR.quotemod_check('w') {
        if !$past.isa(PAST::Val) {
            $/.CURSOR.panic("Can't form :w list from non-constant strings (yet)");
        }
        else {
            my @words := HLL::Grammar::split_words($/, $past.value);
            if +@words != 1 {
                $past := PAST::Op.new( :name('&infix:<,>'), :node($/) );
                for @words { $past.push($_); }
                $past := PAST::Stmts.new($past);
            }
            else {
                $past := PAST::Val.new( :value(~@words[0]), :returns('Str') );
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
                    PAST::Val.new( :value($lastlit), :returns('Str') )
                );
            }
            @parts.push($ast);
            $lastlit := '';
        }
    }
    if $lastlit gt '' || !@parts {
        @parts.push(
            PAST::Val.new( :value($lastlit), :returns('Str') )
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

    method metachar:sym<{ }>($/) { 
        make PAST::Regex.new( $<codeblock>.ast,
                              :pasttype<pastnode>, :node($/) );
    }

    method metachar:sym<rakvar>($/) {
        make PAST::Regex.new( '!INTERPOLATE', $<var>.ast,
                              :pasttype<subrule>, :subtype<method>, :node($/));
    }

    method assertion:sym<{ }>($/) { 
        make PAST::Regex.new( '!INTERPOLATE', 
                 PAST::Op.new( :name<!MAKE_REGEX>, $<codeblock>.ast ),
                 :pasttype<subrule>, :subtype<method>, :node($/));
    }

    method assertion:sym<?{ }>($/) {
        make PAST::Regex.new( $<codeblock>.ast,
                              :subtype<zerowidth>, :negate( $<zw> eq '!' ),
                              :pasttype<pastnode>, :node($/) );
    }

    method assertion:sym<var>($/) {
        make PAST::Regex.new( '!INTERPOLATE', 
                 PAST::Op.new( :name<!MAKE_REGEX>, $<var>.ast ),
                 :pasttype<subrule>, :subtype<method>, :node($/));
    }

    method codeblock($/) {
        my $block := Perl6::Actions::block_immediate($<block>.ast);
        my $past := 
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
            );
        make $past;
    }

    method p6arglist($/) {
        my $arglist := $<arglist>.ast;
        make $arglist;
    }

}

# Takes a block and adds a signature to it, as well as code to bind the call
# capture against the signature. Returns the name of the signature setup block.
sub add_signature($block, $sig_obj, $lazy) {
    # Set arity.
    $block.arity($sig_obj.arity);

    # Add call to signature binder as well as lexical declarations
    # to the start of the block.
    $block[0].push(PAST::Var.new( :name('call_sig'), :scope('parameter'), :call_sig(1) ));
    my $decls := $sig_obj.get_declarations();
    for @($decls) {
        if $_.isa(PAST::Var) {
            $_.isdecl(1);
            $block.symbol( $_.name, :scope('lexical') );
        }
    }
    $block[0].push($decls);
    $block[0].push(PAST::Op.new(
        :pirop('bind_llsig vP'),
        PAST::Var.new( :name('call_sig'), :scope('lexical') )
    ));

    # make signature setup block
    my $lazysig := PAST::Block.new(:blocktype<declaration>, $sig_obj.ast(1));
    $block[0].push($lazysig);
    $block<lazysig> := PAST::Val.new( :value($lazysig) );
}

# Makes a lazy signature building block.
sub make_lazy_sig_block($block) {
    my $sig_setup_block :=
            PAST::Block.new( :blocktype<declaration>, $block<signature_ast> );
    $block[0].push($sig_setup_block);
    PAST::Val.new(:value($sig_setup_block));
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

# Looks through the blocks for the first one with a signature and returns
# that signature.
sub get_nearest_signature() {
    for @BLOCK {
        if pir::defined__IP($_<signature>) {
            return $_<signature>;
        }
    }
    Perl6::Compiler::Signature.new()
}


sub blockref($block) {
    my $ref := PAST::Val.new( :value($block) );
    $ref<block_past> := $block;
    $ref<lazysig>    := $block<lazysig>;
    $ref;
}

# Returns the (static) code object for a block.
# Note that it never holds the block directly -- it's always
# obtained by reference.
sub block_code($block, $type = 'Block', $multiness?) {
    my @name := Perl6::Grammar::parse_name($type);
    my $past := PAST::Op.new(
        :pasttype('callmethod'),
        :name('!get_code'),
        PAST::Val.new( :value($block) ),
        PAST::Var.new( :name(@name.pop), :namespace(@name), :scope('package') )
    );
    $past.push($block<lazysig>) if pir::defined($block<lazysig>);
    $past.push($multiness) if $multiness;
    $past<block_past> := $block;
    $past<block_type> := $type;
    $past;
}

# Returns the (dynamic) closure for a block.  Unlike
# block_code above, this *does* hold the block directly.
sub block_closure($block, $type = 'Block', $multiness?) {
    my @name := Perl6::Grammar::parse_name($type);
    my $past := PAST::Op.new(
        :pasttype('callmethod'),
        :name('!get_closure'),
        $block,
        PAST::Var.new( :name(@name.pop), :namespace(@name), :scope('package') )
    );
    $past.push($block<lazysig>) if pir::defined($block<lazysig>);
    $past.push($multiness) if pir::defined($multiness);
    $past<block_past> := $block;
    $past<block_type> := $type;
    $past;
}

# Wraps a sub up in a block type.
sub create_code_object($block, $type, $multiness) {
    my @name := Perl6::Grammar::parse_name($type);
    my $past := PAST::Op.new(
        :pasttype('callmethod'),
        :name('new'),
        PAST::Var.new( :name(@name.pop), :namespace(@name), :scope('package') ),
        $block,
        $multiness
    );
    $past.push($block<lazysig>) if pir::defined($block<lazysig>);
    $past<past_block> := $block;
    $past<block_class> := $type;
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
    $routine.loadinit.push(
        PAST::Var.new( 
            :name('trait_subject'), :scope('register'), :isdecl(1),
            :viviself(block_code($routine, $type, $*MULTINESS eq 'multi') ) )
    );
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
sub push_block_handler($/, $block, $handler) {
    unless $block.handlers() {
        $block.handlers([]);
    }
    $handler.blocktype('declaration');
    $handler := PAST::Block.new(
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
            $handler,
        ),
    );
    $handler.symbol('$_', :scope('lexical'));
    $handler.symbol('$!', :scope('lexical'));
    $handler := PAST::Stmts.new(
        PAST::Op.new( :pasttype('call'),
            $handler,
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

    $block.handlers.unshift(
        PAST::Control.new(
            :node($/),
            $handler,
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
    $block.symbol('self', :scope('lexical'));
    my $sig := Perl6::Compiler::Signature.new(
        Perl6::Compiler::Parameter.new(:var_name('$_')));
    $sig.add_invocant();
    add_signature($block, $sig, 1);
    create_code_object($block, 'Method', 0);
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

# Takes something that may be a block already, and if not transforms it into
# one. Used by things doing where clause-ish things.
sub where_blockify($expr) {
    my $past;
    if $expr<past_block> && $expr<block_class> eq 'Block' {
        my $lazy_name := make_lazy_sig_block($expr<past_block>);
        $past := create_code_object($expr<past_block>, 'Block', 0);
    }
    else {
        my $sig := Perl6::Compiler::Signature.new(
            Perl6::Compiler::Parameter.new(:var_name('$_')));
        $past := make_block_from($sig, PAST::Op.new(
            :pasttype('call'), :name('&infix:<~~>'),
            PAST::Var.new( :name('$_'), :scope('lexical') ),
            $expr
        ));
    }
    $past
}

# This is the hook where, in the future, we'll use this as the hook to check
# if we have a proto or other declaration in scope that states that this sub
# has a signature of the form :(\|$parcel), in which case we don't promote
# the Parcel to a Capture when calling it. For now, we just worry about the
# special case, return.
sub capture_or_parcel($args, $name) {
    if $name eq 'return' {
        # Need to demote pairs again.
        my $parcel := PAST::Op.new();
        for @($args) {
            $parcel.push($_<before_promotion> ?? $_<before_promotion> !! $_);
        }
        $parcel
    }
    else {
        $args
    }
}

# This checks if we have something of the form * op *, * op <thing> or
# <thing> op * and if so, and if it's not one of the ops we do not
# auto-curry for, emits a closure instead. We hard-code the things not
# to curry for now; in the future, we will inspect the multi signatures
# of the op to decide, or likely store things in this hash from that
# introspection and keep it as a quick cache.
our %not_curried;
INIT {
    %not_curried{'&infix:<...>'}  := 1;
    %not_curried{'&infix:<..>'}   := 1;
    %not_curried{'&infix:<..^>'}  := 1;
    %not_curried{'&infix:<^..>'}  := 1;
    %not_curried{'&infix:<^..^>'} := 1;
    %not_curried{'&prefix:<^>'}   := 1;
    %not_curried{'&infix:<xx>'}   := 1;
    %not_curried{'&infix:<~~>'}   := 1;
    %not_curried{'&infix:<=>'}    := 1;
    %not_curried{'&infix:<:=>'}   := 1;
}
sub whatever_curry($past, $upto_arity) {
    if $past.isa(PAST::Op) && !%not_curried{$past.name} {
        if $upto_arity == 2 && $past[0] ~~ PAST::Op && $past[0].returns eq 'Whatever'
                            && $past[1] ~~ PAST::Op && $past[1].returns eq 'Whatever' {
            # Curry left and right, two args.
            $past.shift; $past.shift;
            $past.push(PAST::Var.new( :name('$x'), :scope('lexical') ));
            $past.push(PAST::Var.new( :name('$y'), :scope('lexical') ));
            my $sig := Perl6::Compiler::Signature.new(
                Perl6::Compiler::Parameter.new(:var_name('$x')),
                Perl6::Compiler::Parameter.new(:var_name('$y')));
            $past := make_block_from($sig, $past, 'WhateverCode');
        }
        elsif $upto_arity == 2 && $past[1] ~~ PAST::Op && $past[1].returns eq 'Whatever' {
            # Curry right arg.
            $past.pop;
            $past.push(PAST::Var.new( :name('$y'), :scope('lexical') ));
            my $sig := Perl6::Compiler::Signature.new(
                Perl6::Compiler::Parameter.new(:var_name('$y')));
            $past := make_block_from($sig, $past, 'WhateverCode');
        }
        elsif $upto_arity >= 1 && $past[0] ~~ PAST::Op && $past[0].returns eq 'Whatever' {
            # Curry left (or for unary, only) arg.
            $past.shift;
            $past.unshift(PAST::Var.new( :name('$x'), :scope('lexical') ));
            my $sig := Perl6::Compiler::Signature.new(
                Perl6::Compiler::Parameter.new(:var_name('$x')));
            $past := make_block_from($sig, $past, 'WhateverCode');
        }
    }
    $past
}

# Helper for constructing a simple Perl 6 Block with the given signature
# and body.
sub make_block_from($sig, $body, $type = 'Block') {
    my $past := PAST::Block.new( :blocktype('declaration'),
        PAST::Stmts.new( ),
        PAST::Stmts.new(
            $body
        )
    );
    add_signature($past, $sig, 1);
    create_code_object($past, $type, 0);
}

# vim: ft=perl6
