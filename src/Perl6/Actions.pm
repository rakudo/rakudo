use NQPP6Regex;

INIT {
    # Add our custom nqp:: opcodes.
    PAST::Node.map_add('nqp', 
        p6box_i      => 'perl6_box_int__Pi',
        p6box_n      => 'perl6_box_num__Pn',
        p6box_s      => 'perl6_box_str__Ps',
        p6bool       => 'perl6_booleanize__Pi',
        p6listiter   => 'perl6_iter_from_rpa__PPP',
        p6list       => 'perl6_list_from_rpa__PPPP',

        istype       => 'type_check__IPP',
        lcm_i        => 'lcm__Iii',
        gcd_i        => 'gcd__Iii',
    );
}


class Perl6::Actions is HLL::Actions {
    our @PACKAGE;
    our $TRUE;
    our @MAX_PERL_VERSION;

    our $FORBID_PIR;
    our $STATEMENT_PRINT;

    INIT {
        # initialize @PACKAGE
        our @PACKAGE := Q:PIR { %r = root_new ['parrot';'ResizablePMCArray'] };
        our $TRUE := PAST::Var.new( :name('true'), :scope('register') );

        # Tell PAST::Var how to encode Perl6Str and Str values
        my %valflags :=
            Q:PIR { %r = get_hll_global ['PAST';'Compiler'], '%valflags' };
        %valflags<Perl6Str> := 'e';
        %valflags<Str>      := 'e';

        # If, e.g., we support Perl up to v6.1.2, set
        # @MAX_PERL_VERSION to [6, 1, 2].
        @MAX_PERL_VERSION[0] := 6;

        $FORBID_PIR := 0;
        $STATEMENT_PRINT := 0;
    }

    sub xblock_immediate($xblock) {
        $xblock[1] := pblock_immediate($xblock[1]);
        $xblock;
    }

    sub pblock_immediate($pblock) {
        block_immediate($pblock<uninstall_if_immediately_used>.shift);
    }

    sub block_immediate($block) {
        $block.blocktype('immediate');
        $block;
    }

    sub sigiltype($sigil) {
        $sigil eq '%' ?? 'Hash'   !!
        $sigil eq '@' ?? 'Array'  !!
                         'Scalar'
    }

    method deflongname($/) {
        make $<colonpair>
             ?? ~$<name> ~ ':<' ~ ~$<colonpair>[0]<circumfix><quote_EXPR><quote_delimited><quote_atom>[0] ~ '>'
             !! ~$<name>;
    }

    # Turn $code into "for lines() { $code }"
    sub wrap_option_n_code($code) {
        return PAST::Op.new(:name<&eager>,
            PAST::Op.new(:pasttype<callmethod>, :name<map>,
                PAST::Op.new( :name<&flat>,
                    PAST::Op.new(:name<&flat>,
                        PAST::Op.new(
                            :name<&lines>,
                            :pasttype<call>
                        )
                    )
                ),
                make_block_from(
                    Perl6::Compiler::Signature.new(
                        Perl6::Compiler::Parameter.new(
                            :var_name('$_'), :is_copy(1)
                        )
                    ),
                    $code
                )
            )
        );
    }

    # Turn $code into "for lines() { $code; say $_ }"
    # &wrap_option_n_code already does the C<for> loop, so we just add the
    # C<say> call here
    sub wrap_option_p_code($code) {
        return wrap_option_n_code(
            PAST::Stmts.new(
                $code,
                PAST::Op.new(:name<&say>, :pasttype<call>,
                    PAST::Var.new(:name<$_>)
                )
            )
        );
    }

    method comp_unit($/, $key?) {
        our $?RAKUDO_HLL;
        
        # Checks.
        $*ST.assert_stubs_defined();
        
        # Get the block for the unit mainline code.
        my $unit := $*UNIT;
        my $mainline := $<statementlist>.ast;

        if %*COMPILING<%?OPTIONS><p> { # also covers the -np case, like Perl 5
            $mainline := wrap_option_p_code($mainline);
        }
        elsif %*COMPILING<%?OPTIONS><n> {
            $mainline := wrap_option_n_code($mainline);
        }
        
        # Unit needs to have a load-init holding the deserialization or
        # fixup code for this compilation unit.
        $unit.loadinit().push($*ST.to_past());
        
        # We'll install our view of GLOBAL as the main one; any other
        # compilation unit that is using this one will then replace it
        # with its view later (or be in a position to restore it).
        $unit.loadinit().push(PAST::Op.new(
            :pasttype('bind_6model'),
            PAST::Var.new( :name('GLOBAL'), :namespace([]), :scope('package') ),
            $*ST.get_slot_past_for_object($*GLOBALish)
        ));

        # Get the block for the entire compilation unit.
        my $outer := $*UNIT_OUTER;
        $outer.node($/);
        
        # Set HLL and load the needed libraries.
        $outer.hll('perl6');
        $unit.loadlibs('nqp_group', 'nqp_ops', 'perl6_group', 'perl6_ops',
                       'bit_ops', 'math_ops', 'trans_ops', 'io_ops',
                       'obscure_ops');

        # If the unit defines &MAIN, add a &MAIN_HELPER.
        my $mainparam := PAST::Var.new(:name('$MAIN'), :scope('parameter'),
                             :viviself( PAST::Val.new( :value(0) ) ) );
        $unit.symbol('$MAIN', :scope<lexical>);
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
        
        # If our caller wants to know the mainline ctx, provide it here.
        # (CTXSAVE is inherited from HLL::Actions.) Don't do this when
        # there was an explicit {YOU_ARE_HERE}.
        unless $*HAS_YOU_ARE_HERE {
            $unit.push( self.CTXSAVE() );
        }
        
        # Add the mainline code to the unit.
        $unit.push($mainline);

        # Executing the compilation unit causes the mainline to be executed.
        # We force a return here, because we have other :load/:init blocks
        # that have to be done at the end of the unit, and we don't want them
        # executed by the mainline.
        $outer.push(
            PAST::Op.new(
                :pirop('return'),
                PAST::Op.new( :pasttype<call>, $unit )
            )
        );

        # If this unit is loaded via load_bytecode, we want it to automatically
        # execute the mainline code above after all other initializations have
        # occurred.
        $outer.push(
            PAST::Block.new(
                :pirflags(':load'), :lexical(0), :namespace(''),
                PAST::Op.new(
                    :pasttype('call'),
                    PAST::Val.new( :value($outer) ),
                )
            )
        );

        # Add file annotation.
        my $file := pir::find_caller_lex__ps('$?FILES');
        unless pir::isnull($file) {
            $outer.unshift(PAST::Op.new(:inline(".annotate 'file', '" ~ $file ~ "'")));
        }

        make $outer;
    }

    method unitstart($/) {
        # Use SET_BLOCK_OUTER_CTX (inherited from HLL::Actions)
        # to set dynamic outer lexical context and namespace details
        # for the compilation unit.
        self.SET_BLOCK_OUTER_CTX($*UNIT_OUTER);
    }

    method statementlist($/) {
        my $past := PAST::Stmts.new( :node($/) );
        if $<statement> {
            for $<statement> {
                my $ast := $_.ast;
                if $ast {
                    if $ast<sink_past> {
                        $ast := $ast<sink_past>;
                    }
                    $ast := PAST::Stmt.new($ast) if $ast ~~ PAST::Node;
                    $past.push( $ast );
                }
            }
        }
        $past.push(PAST::Var.new(:name('Nil'), :scope('lexical'))) if +$past.list < 1;
        make $past;
    }

    method semilist($/) {
        my $past := PAST::Stmts.new( :node($/) );
        if $<statement> {
            for $<statement> { $past.push($_.ast); }
        }
        else { 
            $past.push( PAST::Op.new( :name('&infix:<,>') ) );
        }
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
                $mc.ast.push(PAST::Var.new(:name('Nil'), :scope('lexical')));
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
                    unless $past<block_past> {
                        my $sig := Perl6::Compiler::Signature.new(
                                       Perl6::Compiler::Parameter.new(:var_name('$_')));
                        $past := block_closure(blockify($past, $sig), 'Block', 0);
                    }
                    $past := PAST::Op.new( 
                                 :pasttype<callmethod>, :name<map>, :node($/),
                                 $cond,
                                 $past
                             );
                    $past := PAST::Op.new( :name<&eager>, $past, :node($/) );
                }
                else {
                    $past := PAST::Op.new($cond, $past, :pasttype(~$ml<sym>), :node($/) );
                }
            }
        }
        elsif $<statement_control> { $past := $<statement_control>.ast; }
        else { $past := 0; }
        if $STATEMENT_PRINT && $past {
            $past := PAST::Stmts.new(:node($/),
                PAST::Op.new(
                    :pirop<say__vs>,
                    PAST::Val.new(:value(~$/))
                ),
                $past
            );
        }
        make $past;
    }

    method xblock($/) {
        make PAST::Op.new( $<EXPR>.ast, $<pblock>.ast, :pasttype('if'), :node($/) );
    }

    method pblock($/) {
        if $<blockoid><you_are_here> {
            make $<blockoid>.ast;
        }
        else {
            # Locate or build a set of parameters.
            my @params;
            my $block := $<blockoid>.ast;
            if $block<placeholder_sig> && $<signature> {
                $/.CURSOR.panic('Placeholder variable cannot override existing signature');
            }
            elsif $block<placeholder_sig> {
                @params := $block<placeholder_sig>;
            }
            elsif $<signature> {
                @params := $<signature>.ast;
            }
            else {
                unless $block.symbol('$_') {
                    if $*IMPLICIT {
                        @params.push(hash(
                            :variable_name('$_'), :optional(1),
                            :nominal_type($*ST.find_symbol(['Mu'])),
                            :default_from_outer(1), :is_parcel(1),
                        ));
                    }
                    add_implicit_var($block, '$_');
                }
            }
            
            # Create signature object and set up binding.
            if $<lambda> eq '<->' {
                for @params { $_<is_rw> := 1 }
            }
            set_default_parameter_type(@params, 'Mu');
            my $signature := create_signature_object(@params, $block);
            add_signature_binding_code($block, $signature);
            
            # We'll install PAST in current block so it gets capture_lex'd.
            # Then evaluate to a reference to the block (non-closure - higher
            # up stuff does that if it wants to).
            ($*ST.cur_lexpad())[0].push(my $uninst := PAST::Stmts.new($block));
            my $code := $*ST.create_code_object($block, 'Block', $signature);
            my $ref := reference_to_code_object($code, $block);
            $ref<uninstall_if_immediately_used> := $uninst;
            make $ref;
        }
    }

    method block($/) {
        my $block := $<blockoid>.ast;
        if $block<placeholder_sig> {
            $/.CURSOR.panic("Cannot use placeholder parameters in this kind of block");
        }
        make reference_to_code_object(
            make_simple_code_object($block, 'Block'),
            $block);
    }

    method blockoid($/) {
        if $<statementlist> {
            my $past := $<statementlist>.ast;
            my $BLOCK := $*CURPAD;
            $BLOCK.push($past);
            $BLOCK.node($/);
            make $BLOCK;
        }
        else {
            if $*HAS_YOU_ARE_HERE {
                $/.CURSOR.panic('{YOU_ARE_HERE} may only appear once in a setting');
            }
            $*HAS_YOU_ARE_HERE := 1;
            make $<you_are_here>.ast;
        }
    }
    
    method you_are_here($/) {
        make self.CTXSAVE();
    }

    method newpad($/) {
        my $new_block := $*ST.cur_lexpad();
        $new_block<IN_DECL> := $*IN_DECL;
    }

    method finishpad($/) {
        # Generate the $_, $/, and $! lexicals if they aren't already
        # declared. We don't actually give them a value, but rather the
        # Perl6LexPad will generate containers (and maybe fill them with
        # the outer's value) on demand.
        my $BLOCK := $*ST.cur_lexpad();
        for <$_ $/ $!> {
            # Generate the lexical variable except if...
            #   (1) the block already has one, or
            #   (2) the variable is '$_' and $*IMPLICIT is set
            #       (this case gets handled by getsig)
            unless $BLOCK.symbol($_) || ($_ eq '$_' && $*IMPLICIT) {
                add_implicit_var($BLOCK, $_);
            }
        }
    }


    ## Statement control

    method statement_control:sym<if>($/) {
        my $count := +$<xblock> - 1;
        my $past := xblock_immediate( $<xblock>[$count].ast );
        # push the else block if any, otherwise 'if' returns C<Nil> (per S04)
        $past.push( $<else> 
                    ?? pblock_immediate( $<else>[0].ast )
                    !!  PAST::Var.new(:name('Nil'), :scope('lexical')) 
        );
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
        my $xblock := $<xblock>.ast;
        my $past := PAST::Op.new( 
                        :pasttype<callmethod>, :name<map>, :node($/),
                        $xblock[0],
                        block_closure($xblock[1])
        );
        $past := PAST::Op.new( :name<&eager>, $past, :node($/) );
        make $past;
    }

    method statement_control:sym<loop>($/) {
        my $block := PAST::Op.new($<block>.ast);
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

    method statement_control:sym<import>($/) {
        my $past := PAST::Stmts.new( :node($/) );
        import($/);
        make $past;
    }

    method statement_control:sym<use>($/) {
        my $past := PAST::Stmts.new( :node($/) );
        if $<version> {
            # TODO: replace this by code that doesn't always die with
            # a useless error message
#            my $i := -1;
#            for $<version><vnum> {
#                ++$i;
#                if $_ ne '*' && $_ < @MAX_PERL_VERSION[$i] {
#                    last;
#                } elsif $_ > @MAX_PERL_VERSION[$i] {
#                    my $mpv := pir::join('.', @MAX_PERL_VERSION);
#                    $/.CURSOR.panic("Perl $<version> required--this is only v$mpv")
#                }
#            }
        } elsif $<module_name> {
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
            elsif ~$<module_name> eq 'FORBID_PIR' {
                $FORBID_PIR := 1;
            }
            elsif ~$<module_name> eq 'Devel::Trace' {
                $STATEMENT_PRINT := 1;
            }
        }
        make $past;
    }

    method statement_control:sym<require>($/) {
        if $<module_name> && $<EXPR> {
            $/.CURSOR.panic("require with argument list not yet implemented");
        }
        my $name_past := $<module_name>
                        ?? PAST::Val.new(:value($<module_name><longname><name>.Str))
                        !! $<EXPR>[0].ast;
        my @module_loader := Perl6::Grammar::parse_name('Perl6::Module::Loader');
        my $past := PAST::Op.new(
            :node($/),
            :pasttype('callmethod'),
            :name('need'),
            PAST::Var.new( :name(@module_loader.pop),
                           :namespace(@module_loader), :scope('package') ),
            $name_past
        );
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
        push_block_handler($/, $*ST.cur_lexpad(), $block);
        $*ST.cur_lexpad().handlers()[0].handle_types_except('CONTROL');
        make PAST::Stmts.new(:node($/));
    }

    method statement_control:sym<CONTROL>($/) {
        my $block := $<block>.ast;
        push_block_handler($/, $*ST.cur_lexpad(), $block);
        $*ST.cur_lexpad().handlers()[0].handle_types('CONTROL');
        make PAST::Stmts.new(:node($/));
    }

    method statement_prefix:sym<BEGIN>($/) { self.add_phaser($/, $<blorst>.ast, 'BEGIN'); }
    method statement_prefix:sym<CHECK>($/) { self.add_phaser($/, $<blorst>.ast, 'CHECK'); }
    method statement_prefix:sym<INIT>($/)  { self.add_phaser($/, $<blorst>.ast, 'INIT'); }
    method statement_prefix:sym<END>($/)   { self.add_phaser($/, $<blorst>.ast, 'END'); }

    method statement_prefix:sym<do>($/) {
        my $past := $<blorst>.ast;
        $past.blocktype('immediate');
        make $past;
    }

    method statement_prefix:sym<gather>($/) {
        my $past := block_closure($<blorst>.ast);
        make PAST::Op.new( :pasttype('call'), :name('&GATHER'), $past );
    }

    method statement_prefix:sym<sink>($/) {
        my $blast := $<blorst>.ast;
        $blast.blocktype('immediate');
        make PAST::Stmts.new(
            PAST::Op.new( :name('&eager'), $blast ),
            PAST::Var.new( :name('Nil'), :namespace([]), :scope('package')),
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
        $past.push(PAST::Op.new( :pasttype('bind_6model'),
            PAST::Var.new( :name('$!'), :scope('lexical') ),
            PAST::Op.new( :pasttype('call'), :name('!FAIL') )
        ));

        make $past;
    }

    method blorst($/) {
        make $<block> ?? $<block>.ast !! make_thunk_ref($<statement>.ast);
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
    method term:sym<circumfix>($/)          { make $<circumfix>.ast; }
    method term:sym<statement_prefix>($/)   { make $<statement_prefix>.ast; }
    method term:sym<lambda>($/)             { make block_closure($<pblock>.ast); }
    method term:sym<sigterm>($/)            { make $<sigterm>.ast; }

    method name($/) { }

    method module_name($/) {
        # XXX Needs re-doing.
        my @name := Perl6::Grammar::parse_name(~$<longname>);
        my $var := PAST::Var.new(
            :name(@name.pop),
            :namespace(@name),
            :scope('package')
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
            make $<fakesignature>.ast;
        }
        else {
            make $*value.ast;
        }
    }

    sub make_pair($key_str, $value) {
        my $key := $*ST.add_constant('Str', 'str', $key_str);
        $key.named('key');
        $value.named('value');
        PAST::Op.new(
            :pasttype('callmethod'), :name('new'), :returns('Pair'),
            PAST::Var.new( :name('Pair'), :scope('lexical') ),
            $key, $value
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
        my $past := PAST::Var.new( :name(@name[+@name - 1]), :node($/));
        if $<twigil>[0] eq '*' {
            $past := PAST::Op.new( $past.name(), :pasttype('call'), :name('!find_contextual'), :lvalue(0) );
        }
        elsif $<twigil>[0] eq '!' {
            # In a declaration, don't produce anything here.
            if $*IN_DECL ne 'variable' {
                # Ensure attribute actaully exists before emitting lookup.
                unless pir::can($*PACKAGE.HOW, 'get_attribute_for_usage') {
                    $/.CURSOR.panic("Cannot understand $name in this context");
                }
                my $attr := $*PACKAGE.HOW.get_attribute_for_usage($*PACKAGE, $name);
                if $attr {
                    $past.scope('attribute_6model');
                    $past.type($attr.type);
                    $past.unshift($*ST.get_object_sc_ref_past($*ST.find_symbol(['$?CLASS'])));
                    $past.unshift(PAST::Var.new( :name('self'), :scope('lexical') ));
                    $past := box_native_if_needed($past, $attr.type);
                }
                else {
                    $/.CURSOR.panic("Attribute $name not declared in $*PKGDECL " ~
                        $*PACKAGE.HOW.name($*PACKAGE));
                }
            }
        }
        elsif $<twigil>[0] eq '.' && $*IN_DECL ne 'variable' {
            # Need to transform this to a method call.
            $past := $<arglist> ?? $<arglist>[0].ast !! PAST::Op.new();
            $past.pasttype('callmethod');
            $past.name(~$<desigilname>);
            $past.unshift(PAST::Var.new( :name('self'), :scope('lexical') ));
        }
        elsif $<twigil>[0] eq '^' || $<twigil>[0] eq ':' {
            $past := add_placeholder_parameter($/, $<sigil>.Str, $<desigilname>.Str, :named($<twigil>[0] eq ':'));
        }
        elsif ~$/ eq '@_' {
            unless get_nearest_signature().declares_symbol('@_') {
                $past := add_placeholder_parameter($/, '@', '_', :slurpy_pos(1));
            }
        }
        elsif ~$/ eq '%_' {
            unless get_nearest_signature().declares_symbol('%_') {
                $past := add_placeholder_parameter($/, '%', '_', :slurpy_named(1));
            }
        }
        elsif +@name > 1 {
            $past := $*ST.symbol_lookup(@name, $/);
        }
        else {
            my $attr_alias := $*ST.is_attr_alias($past.name);
            if $attr_alias {
                $past.name($attr_alias);
                $past.scope('attribute');
                $past.unshift($*ST.get_object_sc_ref_past($*ST.find_symbol(['$?CLASS'])));
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
    method package_declarator:sym<knowhow>($/) { make $<package_def>.ast; }
    method package_declarator:sym<native>($/)  { make $<package_def>.ast; }
    
    method package_declarator:sym<trusts>($/) {
        $/.CURSOR.panic("trusts not yet implemented");
    }
    
    method package_declarator:sym<also>($/) {
        $/.CURSOR.panic("also not yet implemented");
    }

    method package_def($/) {
        # Get the body block PAST.
        my $block;
        if $<blockoid> {
            $block := $<blockoid>.ast;
        }
        else {
            $block := $*CURPAD;
            $block.push($<statementlist>.ast);
            $block.node($/);
        }
        $block.blocktype('immediate');
        
        # If it's a stub, add it to the "must compose at some point" list,
        # then just evaluate to the type object. Don't need to do any more
        # just yet.
        if pir::substr__Ssii($<blockoid><statementlist><statement>[0], 0, 3) eq '...' {
            $*ST.add_stub_to_check($*PACKAGE);
            $block.blocktype('declaration');
            make PAST::Stmts.new( $block, $*ST.get_slot_past_for_object($*PACKAGE) );
            return 1;
        }
    
        # Handle parametricism for roles.
        if $*PKGDECL eq 'role' {
            # Set up signature. Needs to have $?CLASS as an implicit
            # parameter, since any mention of it is generic.
            my @params := $<signature> ?? $<signature>[0].ast !! [];
            @params.unshift(hash(
                is_multi_invocant => 1,
                type_captures     => ['$?CLASS']
            ));
            set_default_parameter_type(@params, 'Mu');
            my $sig := create_signature_object(@params, $block);
            add_signature_binding_code($block, $sig);
            $block.blocktype('declaration');

            # As its last act, it should grab the current lexpad so that
            # we have the type environment.
            $block.push(PAST::Op.new(
                :pirop('set PQPS'),
                PAST::Op.new( :pirop('getinterp P') ),
                'lexpad'));
            
            # Create code object and add it as the role's body block.
            my $code := $*ST.create_code_object($block, 'Block', $sig);
            $*ST.pkg_set_role_body_block($*PACKAGE, $sig, $code);
        }
        
        # Compose.
        $*ST.pkg_compose($*PACKAGE);
        
        make PAST::Stmts.new(
            $block, $*ST.get_object_sc_ref_past($*PACKAGE)
        );
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
        if $*ST.cur_lexpad().symbol($name) {
            $/.CURSOR.panic("Redeclaration of symbol ", $name);
        }
        make declare_variable($/, $past, ~$sigil, ~$twigil, ~$<variable><desigilname>, $<trait>);
    }

    sub declare_variable($/, $past, $sigil, $twigil, $desigilname, $trait_list) {
        my $name  := $sigil ~ $twigil ~ $desigilname;
        my $BLOCK := $*ST.cur_lexpad();

        if $*SCOPE eq 'has' {
            # Ensure current package can take attributes.
            unless pir::can($*PACKAGE.HOW, 'add_attribute') {
                $/.CURSOR.panic("A $*PKGDECL cannot have attributes");
            }
            
            # Create container descriptor and decide on any default value..
            my $attrname   := ~$sigil ~ '!' ~ $desigilname;
            my $type       := $*TYPENAME ?? $*TYPENAME.ast !! $*ST.find_symbol(['Mu']);
            my $descriptor := $*ST.create_container_descriptor($type, 1, $attrname);
            my @default    := $sigil eq '$' ?? [$type] !! [];
            
            # Create meta-attribute and add it.
            my $metaattr := %*HOW{$*PKGDECL ~ '-attr'};
            my $attr := $*ST.pkg_add_attribute($*PACKAGE, $metaattr,
                hash(
                    name => $attrname,
                    has_accessor => $twigil eq '.'
                ),
                hash( 
                    container_descriptor => $descriptor,
                    type => $type),
                sigiltype($sigil), $descriptor, |@default);
            
            # If no twigil, note $foo is an alias to $!foo.
            if $twigil eq '' {
                $BLOCK.symbol($name, :attr_alias($attrname));
            }
            
            # Apply any traits.
            for $trait_list {
                my $applier := $_.ast;
                if $applier { $applier($attr); }
            }

            # Nothing to emit here; just hand back an empty node but
            # annotated with the attribute object in case we get a
            # default vlaue "assigned".
            $past := PAST::Op.new( :pasttype('null') );
            $past<attribute_declarand> := $attr;
        }
        elsif $*SCOPE eq 'my' {
            # Create a container descriptor. Default to rw and set a
            # type if we have one; a trait may twiddle with that later.
            my $descriptor := $*ST.create_container_descriptor(
                $*TYPENAME ?? $*TYPENAME.ast !! $*ST.find_symbol(['Mu']),
                1, $name);
                
            # Install the container. Scalars default to Any if untyped.
            if $sigil eq '$' || $sigil eq '&' {
                $*ST.install_lexical_container($BLOCK, $name, sigiltype($sigil), $descriptor,
                    $*TYPENAME ?? $*TYPENAME.ast !! $*ST.find_symbol(['Any']));
            }
            else {
                $*ST.install_lexical_container($BLOCK, $name, sigiltype($sigil), $descriptor);
            }
        }
        else {
            $/.CURSOR.panic("$*SCOPE scoped variables not yet implemented");
        }

        return $past;
    }

    method routine_declarator:sym<sub>($/) { make $<routine_def>.ast; }
    method routine_declarator:sym<method>($/) { make $<method_def>.ast; }
    method routine_declarator:sym<submethod>($/) { make $<method_def>.ast; }

    method routine_def($/) {
        my $block;
        
        if $<onlystar> {
            $block := $<onlystar>.ast;
        }
        else {
            $block := $<blockoid>.ast;
            $block.blocktype('declaration');
            unless is_clearly_returnless($block) {
                $block.control('return_pir');
            }
        }
        
        # Obtain parameters, create signature object and generate code to
        # call binder.
        if $block<placeholder_sig> && $<multisig> {
            $/.CURSOR.panic('Placeholder variable cannot override existing signature');
        }
        my @params := 
                $<multisig>             ?? $<multisig>[0].ast      !!
                $block<placeholder_sig> ?? $block<placeholder_sig> !!
                [];
        set_default_parameter_type(@params, 'Any');
        my $signature := create_signature_object(@params, $block);
        add_signature_binding_code($block, $signature);

        # Create code object.
        my $code := $*ST.create_code_object($block, 'Sub', $signature,
            $*MULTINESS eq 'proto');

        # Install PAST block so that it gets capture_lex'd correctly and also
        # install it in the lexpad.
        my $outer := $*ST.cur_lexpad();
        $outer[0].push(PAST::Stmt.new($block));

        my $past;
        if $<deflongname> {
            # Set name.
            my $name := '&' ~ ~$<deflongname>[0].ast;
            $block.name(~$<deflongname>[0].ast);
            $block.nsentry('');

            # If it's a multi, need to associate it with the surrounding
            # proto.
            # XXX Also need to auto-multi things with a proto in scope.
            if $*MULTINESS eq 'multi' {
                # Locate the proto - or what we hope will be it.
                my %proto_sym := $outer.symbol($name);
                unless %proto_sym {
                    $/.CURSOR.panic("proto and dispatch auto-generation for multis not yet implemented");
                }
                my $proto := %proto_sym<value>;
                # XXX ensure it's actuall a proto or dispatch...
                
                # Install the candidate.
                $*ST.add_dispatchee_to_proto($proto, $code);
            }
            else {
                # Install.
                if $*SCOPE eq '' || $*SCOPE eq 'my' {
                    $*ST.install_lexical_symbol($outer, $name, $code);
                }
                elsif $*SCOPE eq 'our' {
                    # Install in lexpad and in package, and set up code to
                    # re-bind it per invocation of its outer.
                    $*ST.install_lexical_symbol($outer, $name, $code);
                    $*ST.install_package_symbol($*PACKAGE, $name, $code);
                    $outer[0].push(PAST::Op.new(
                        :pasttype('bind_6model'),
                        $*ST.symbol_lookup([$name], $/, :package_only(1)),
                        PAST::Var.new( :name($name), :scope('lexical') )
                    ));
                }
                else {
                    $/.CURSOR.panic("Cannot use '$*SCOPE' scope with a sub");
                }
            }
        }
        elsif $*MULTINESS {
            $/.CURSOR.panic('Cannot put ' ~ $*MULTINESS ~ ' on anonymous routine');
        }
        
        my $closure := block_closure(reference_to_code_object($code, $past));
        $closure<sink_past> := PAST::Op.new( :pasttype('null') );
        make $closure;
    }

    method method_def($/) {
        my $past;
        if $<onlystar> {
            $past := $<onlystar>.ast;
        }
        else {
            $past := $<blockoid>.ast;
            $past.blocktype('declaration');
            unless is_clearly_returnless($past) {
                $past.control('return_pir');
            }
        }
        $past.name(~$<longname>);
        
        # Get signature and ensure it has an invocant.
        if $past<placeholder_sig> {
            $/.CURSOR.panic('Placeholder variables cannot be used in a method');
        }
        my @params := $<multisig> ?? $<multisig>[0].ast !! [];
        unless @params[0]<is_invocant> {
            @params.unshift(hash(
                nominal_type => $*ST.find_symbol([$<longname> ?? '$?CLASS' !! 'Mu']),
                is_invocant => 1,
                is_multi_invocant => 1
            ));
        }
        set_default_parameter_type(@params, 'Any');
        my $signature := create_signature_object(@params, $past);
        add_signature_binding_code($past, $signature);
        
        # Place to store invocant.
        $past[0].unshift(PAST::Var.new( :name('self'), :scope('lexical'), :isdecl(1) ));
        $past.symbol('self', :scope('lexical'));

        # Create code object.
        my $type := $*METHODTYPE eq 'submethod' ?? 'Submethod' !! 'Method';
        my $code := $*ST.create_code_object($past, $type, $signature,
            $*MULTINESS eq 'proto');
        
        # Install PAST block so that it gets capture_lex'd correctly.
        my $outer := $*ST.cur_lexpad();
        $outer[0].push($past);

        # Install method.
        if $<longname> {
            # Ensure that current package supports methods.
            my $meta_meth := $*MULTINESS eq 'multi' ?? 'add_multi_method' !! 'add_method';
            unless pir::can($*PACKAGE.HOW, $meta_meth) {
                my $nocando := $*MULTINESS eq 'multi' ?? 'multi-method' !! 'method';
                $/.CURSOR.panic("Cannot add a $nocando to a $*PKGDECL");
            }
        
            # Add to methods table.
            my $name := $<longname>.Str;
            $*ST.pkg_add_method($*PACKAGE, $meta_meth, $name, $code);
        }
        elsif $*MULTINESS {
            $/.CURSOR.panic('Cannot put ' ~ $*MULTINESS ~ ' on anonymous method');
        }

        my $closure := block_closure(reference_to_code_object($code, $past));
        $closure<sink_past> := PAST::Op.new( :pasttype('null') );
        make $closure;
    }
    
    sub is_clearly_returnless($block) {
        # If the only thing is a pirop, can assume no return
        +$block[1].list == 1 && $block[1][0].isa(PAST::Op) && $block[1][0].pirop()
    }

    method onlystar($/) {
        my $BLOCK := $*CURPAD;
        $BLOCK.push(PAST::Op.new( :pirop('perl6_enter_multi_dispatch_from_onlystar_block P') ));
        $BLOCK.node($/);
        make $BLOCK;
    }

    sub install_method($/, $code, $name, %table) {
        my $installed;
        
        # Create method table entry if we need one.
        unless %table{$name} { my %tmp; %table{$name} := %tmp; }

        # If it's an only and there's already a symbol, problem.
        if $*MULTINESS eq 'only' && %table{$name} {
            $/.CURSOR.panic('Cannot declare only method ' ~ $name ~
                ' when another method with this name was already declared');
        }
        elsif $*MULTINESS || %table{$name}<multis> {
            # If no multi declarator and no proto, error.
            if !$*MULTINESS && !%table{$name}<proto> {
                $/.CURSOR.panic('Cannot re-declare method ' ~ $name ~ ' without declaring it multi');
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
                $*ST.cur_lexpad()[0].push(PAST::Var.new( :name('&' ~ $name), :isdecl(1),
                        :viviself($installed), :scope('lexical') ));
                $*ST.cur_lexpad().symbol($name, :scope('lexical') );
            }
            elsif $*SCOPE eq 'our' {
                @PACKAGE[0].block.loadinit.push(PAST::Op.new(
                    :pasttype('bind_6model'),
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
            unless ($name) {
                $/.CURSOR.panic('proto ' ~ ~$<sym> ~ 's cannot be anonymous');
            }
            our @PACKAGE;
            unless +@PACKAGE {
                $/.CURSOR.panic("Cannot declare named " ~ ~$<sym> ~ " outside of a package");
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
            $past := Regex::P6Regex::Actions::buildsub($<p6regex>.ast, $*CURPAD);
            $past.unshift(PAST::Op.new(
                :pasttype('inline'),
                :inline("    .local pmc self\n    self = find_lex 'self'")
                ));
            my $sig := $<signature> ?? $<signature>[0].ast !! Perl6::Compiler::Signature.new();
            $sig.add_invocant();
            $sig.set_default_parameter_type('Any');
            $past[0].unshift(PAST::Var.new( :name('self'), :scope('lexical'), :isdecl(1), :viviself(sigiltype('$')) ));
            $past.symbol('self', :scope('lexical'));
            add_signature($past, $sig);
            $past.name($name);
            $past.blocktype("declaration");
            
            # If the methods are not :anon they'll conflict at class composition time.
            $past.pirflags(':anon');

            # Create code object and install it provided it has a name.
            if ($name) {
                my $code := block_closure(blockref($past), 'Regex', 0);
                our @PACKAGE;
                unless +@PACKAGE {
                    $/.CURSOR.panic("Cannot declare named " ~ ~$<sym> ~ " outside of a package");
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
            my $result := (pir::find_sub_not_null__ps('!YOU_ARE_HERE'))($compiled)();
            
            # Only support our-scoped so far.
            unless $*SCOPE eq '' || $*SCOPE eq 'our' {
                $/.CURSOR.panic("Do not yet support $*SCOPE scoped enums");
            }

            if $/.CURSOR.is_name(~$<name>[0]) {
                $/.CURSOR.panic("Illegal redeclaration of symbol '"
                                 ~ $<name>[0] ~ "'");
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
                :name('&SETUP_NAMED_ENUM'),
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
        # We refine Any by default; "of" may override.
        my $refinee := $*ST.find_symbol(['Any']);
        
        # If we have a refinement, make sure it's thunked if needed. If none,
        # just always true.
        my $refinement := make_where_block($<EXPR> ?? $<EXPR>[0].ast !!
            PAST::Op.new( :pirop('perl6_booleanize__PI'), 1 ));
        
        # Create the meta-object.
        my $subset := $<longname> ??
            $*ST.create_subset(%*HOW<subset>, $refinee, $refinement, :name($<longname>[0].Str)) !!
            $*ST.create_subset(%*HOW<subset>, $refinee, $refinement);
        
        # Apply traits.
        for $<trait> {
            ($_.ast)($subset) if $_.ast;
        }
        
        # Install it as needed.
        if $<longname> {
            $*ST.install_package($/, $<longname>[0], ($*SCOPE || 'our'),
                'subset', $*PACKAGE, $*ST.cur_lexpad(), $subset);
        }
        
        # We evaluate to the refinement type object.
        make $*ST.get_object_sc_ref_past($subset);
    }

    method type_declarator:sym<constant>($/) {
        $/.CURSOR.panic('Constant type declarator not yet implemented');
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
        make $*ST.get_slot_past_for_object($*ST.make_signature($<signature>.ast));
    }

    method signature($/) {
        # Fix up parameters with flags according to the separators.
        my @parameter_infos;
        my $param_idx := 0;
        my $multi_invocant := 1;
        for $<parameter> {
            my %info := $_.ast;
            %info<is_multi_invocant> := $multi_invocant;
            my $sep := @*seps[$param_idx];
            if ~$sep eq ':' {
                if $param_idx != 0 {
                    $/.CURSOR.panic("Can only use : in a signature after the first parameter");
                }
                %info<is_invocant> := 1;
            }
            elsif ~$sep eq ';;' {
                $multi_invocant := 0;
            }
            @parameter_infos.push(%info);
            $param_idx := $param_idx + 1;
        }
        
        # Result is set of parameter descriptors.
        make @parameter_infos;
    }

    method parameter($/) {
        # Sanity checks.
        my $quant := $<quant>;
        if $<default_value> {
            if $quant eq '*' {
                $/.CURSOR.panic("Cannot put default on slurpy parameter");
            }
            if $quant eq '!' {
                $/.CURSOR.panic("Cannot put default on required parameter");
            }
            %*PARAM_INFO<default_closure> := $<default_value>[0].ast;
        }

        # Set up various flags.
        %*PARAM_INFO<pos_slurpy>   := $quant eq '*' && %*PARAM_INFO<sigil> eq '@';
        %*PARAM_INFO<pos_lol>      := $quant eq '**' && %*PARAM_INFO<sigil> eq '@';
        %*PARAM_INFO<named_slurpy> := $quant eq '*' && %*PARAM_INFO<sigil> eq '%';
        %*PARAM_INFO<optional>     := $quant eq '?' || $<default_value> || ($<named_param> && $quant ne '!');
        %*PARAM_INFO<is_parcel>    := $quant eq '\\';
        %*PARAM_INFO<is_capture>   := $quant eq '|';
        
        # Stash any traits.
        %*PARAM_INFO<traits> := $<trait>;
        
        # Result is the parameter info hash.
        make %*PARAM_INFO;
    }

    method param_var($/) {
        if $<signature> {
            if pir::exists(%*PARAM_INFO, 'sub_signature') {
                $/.CURSOR.panic('Cannot have more than one sub-signature for a parameter');
            }
            my @params := $<signature>.ast;
            set_default_parameter_type(@params, 'Mu');
            %*PARAM_INFO<sub_signature> := create_signature_object(@params, $*ST.cur_lexpad());
            if pir::substr(~$/, 0, 1) eq '[' {
                %*PARAM_INFO<sigil> := '@';
            }
        }
        else {
            # Set name, if there is one.
            if $<name> {
                %*PARAM_INFO<variable_name> := ~$/;
            }
            %*PARAM_INFO<sigil> := ~$<sigil>;
            
            # Handle twigil.
            my $twigil := $<twigil> ?? ~$<twigil>[0] !! '';
            if $twigil eq '' || $twigil eq '*' {
                # Need to add the name.
                if $<name> {
                    my $cur_pad := $*ST.cur_lexpad();
                    if $cur_pad.symbol(~$/) {
                        $/.CURSOR.panic("Redeclaration of symbol ", ~$/);
                    }
                    $cur_pad[0].push(PAST::Var.new( :name(~$/), :scope('lexical'), :isdecl(1) ));
                    $cur_pad.symbol(~$/, :scope($*SCOPE eq 'my' ?? 'lexical' !! 'package'));
                }
            }
            elsif $twigil eq '!' {
                # XXX Can compile-time check name when we have attributes. :-)
                %*PARAM_INFO<bind_attr> := 1;
            }
            elsif $twigil eq '.' {
                %*PARAM_INFO<bind_accessor> := 1;
                if $<name> {
                    %*PARAM_INFO<variable_name> := ~$<name>[0];
                }
                else {
                    $/.CURSOR.panic("Cannot declare $. parameter in signature without an accessor name");
                }
            }
            else {
                if $twigil eq ':' {
                    $/.CURSOR.panic("In signature parameter, placeholder variables like " ~ 
                        ~$/ ~ " are illegal\n" ~ 
                        "you probably meant a named parameter: ':" ~ $<sigil> ~ ~$<name>[0] ~ "'");
                }
                else {
                    $/.CURSOR.panic("In signature parameter, '" ~ ~$/ ~ 
                        "', it is illegal to use '" ~ $twigil ~ "' twigil");
                }
            }
        }
    }

    method named_param($/) {
        %*PARAM_INFO<named_names> := %*PARAM_INFO<named_names> || [];
        if $<name>               { %*PARAM_INFO<named_names>.push(~$<name>); }
        elsif $<param_var><name> { %*PARAM_INFO<named_names>.push(~$<param_var><name>[0]); }
        else                     { %*PARAM_INFO<named_names>.push(''); }
    }
    
    method default_value($/) {
        # Turn into a thunk.
        make make_thunk($<EXPR>.ast);
    }

    method type_constraint($/) {
        if $<typename> {
            if pir::substr(~$<typename>, 0, 2) eq '::' {
                # Set up signature so it will find the typename.
                my $desigilname := pir::substr(~$<typename>, 2);
                unless %*PARAM_INFO<type_captures> {
                    %*PARAM_INFO<type_captures> := []
                }
                %*PARAM_INFO<type_captures>.push($desigilname);
                
                # Install type variable in the static lexpad. Of course,
                # we'll find the real thing at runtime, but in the static
                # view it's a type variable to be reified.
                $*ST.install_lexical_symbol($*ST.cur_lexpad(), $desigilname,
                    $<typename>.ast);
            }
            else {
                if pir::exists(%*PARAM_INFO, 'nominal_type') {
                    $/.CURSOR.panic('Parameter may only have one prefix type constraint');
                }
                %*PARAM_INFO<nominal_type> := $<typename>.ast;
                for $<typename><longname><colonpair> {
                    if $_<identifier> {
                        if $_<identifier>.Str eq 'D' {
                            %*PARAM_INFO<defined_only> := 1;
                        }
                        elsif $_<identifier>.Str eq 'U' {
                            %*PARAM_INFO<undefined_only> := 1;
                        }
                    }
                }
            }
        }
        elsif $<value> {
            if pir::exists(%*PARAM_INFO, 'nominal_type') {
                $/.CURSOR.panic('Parameter may only have one prefix type constraint');
            }
            # XXX Bit tricky - need constants refactor first.
            $/.CURSOR.panic('Value type constraints not yet implemented');
        }
        else {
            $/.CURSOR.panic('Cannot do non-typename cases of type_constraint yet');
        }
    }

    method post_constraint($/) {
        if $<signature> {
            if pir::exists(%*PARAM_INFO, 'sub_signature') {
                $/.CURSOR.panic('Cannot have more than one sub-signature for a parameter');
            }
            my @params := $<signature>.ast;
            set_default_parameter_type(@params, 'Mu');
            %*PARAM_INFO<sub_signature> := create_signature_object(@params, $*ST.cur_lexpad());
            if pir::substr(~$/, 0, 1) eq '[' {
                %*PARAM_INFO<sigil> := '@';
            }
        }
        else {
            unless %*PARAM_INFO<post_constraints> {
                %*PARAM_INFO<post_constraints> := [];
            }
            %*PARAM_INFO<post_constraints>.push(make_where_block($<EXPR>.ast));
        }
    }
    
    # Sets the default parameter type for a signature.
    sub set_default_parameter_type(@parameter_infos, $type_name) {
        my $type := $*ST.find_symbol([$type_name]);
        for @parameter_infos {
            unless pir::exists($_, 'nominal_type') {
                $_<nominal_type> := $type;
            }
        }
    }
    
    # Create Parameter objects, along with container descriptors
    # if needed. Parameters will be bound into the specified
    # lexpad.
    sub create_signature_object(@parameter_infos, $lexpad) {
        my @parameters;
        for @parameter_infos {
            # Add variable as needed.
            if $_<variable_name> {
                my %sym := $lexpad.symbol($_<variable_name>);
                if +%sym {
                    $_<container_descriptor> := $*ST.create_container_descriptor(
                        $_<nominal_type>, $_<is_rw> ?? 1 !! 0, $_<variable_name>);
                    $lexpad.symbol($_<variable_name>, :descriptor($_<container_descriptor>));
                }
            }
            
            # Create parameter object and apply any traits.
            my $param_obj := $*ST.create_parameter($_);
            for $_<traits> {
                ($_.ast)($param_obj) if $_.ast;
            }
            
            # Add it to the signature.
            @parameters.push($param_obj);
        }
        $*ST.create_signature(@parameters)
    }
    
    method trait($/) {
        make $<trait_mod> ?? $<trait_mod>.ast !! $<colonpair>.ast;
    }

    method trait_mod:sym<is>($/) {
        # Handle is repr specially.
        if ~$<longname> eq 'repr' {
            if $<circumfix> {
                $*REPR := compile_time_value_str($<circumfix>[0].ast[0], "is repr(...) trait", $/);
            }
            else {
                $/.cursor.panic("is repr(...) trait needs a parameter");
            }
        }
        else
        {
            # If we have a type name then we need to dispatch with that type; otherwise
            # we need to dispatch with it as a named argument.
            my @name := Perl6::Grammar::parse_name(~$<longname>);
            if $*ST.is_type(@name) {
                my $trait := $*ST.find_symbol(@name);
                make -> $declarand {
                    $*ST.apply_trait('&trait_mod:<is>', $declarand, $trait);
                };
            }
            else {
                my %arg;
                %arg{~$<longname>} := ($*ST.add_constant('Int', 'int', 1))<compile_time_value>;
                make -> $declarand {
                    $*ST.apply_trait('&trait_mod:<is>', $declarand, |%arg);
                };
            }
        }
    }

    method trait_mod:sym<hides>($/) {
        make -> $declarand {
            $*ST.apply_trait('&trait_mod:<hides>', $declarand, $<typename>.ast);
        };
    }

    method trait_mod:sym<does>($/) {
        make -> $declarand {
            $*ST.apply_trait('&trait_mod:<does>', $declarand, $<typename>.ast);
        };
    }

    method trait_mod:sym<will>($/) {

    }

    method trait_mod:sym<of>($/) {
        make -> $declarand {
            $*ST.apply_trait('&trait_mod:<of>', $declarand, $<typename>.ast);
        };
    }

    method trait_mod:sym<as>($/) {
        make -> $declarand {
            $*ST.apply_trait('&trait_mod:<as>', $declarand, $<typename>.ast);
        };
    }

    method trait_mod:sym<returns>($/) {
        make -> $declarand {
            $*ST.apply_trait('&trait_mod:<returns>', $declarand, $<typename>.ast);
        };
    }

    method trait_mod:sym<handles>($/) {

    }

    method postop($/) {
        make $<postfix> ?? $<postfix>.ast !! $<postcircumfix>.ast;
    }

    method dotty:sym<.>($/) { make $<dottyop>.ast; }

    method dotty:sym<.*>($/) {
        my $past := $<dottyop>.ast;
        unless $past.isa(PAST::Op) && $past.pasttype() eq 'callmethod' {
            $/.CURSOR.panic("Cannot use " ~ $<sym>.Str ~ " on a non-identifier method call");
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
            # May just be .foo, but could also be .Foo::bar. Also handle the
            # macro-ish cases.
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
            elsif $name eq 'WHAT' {
                $past.pasttype('pirop');
                $past.pirop('get_what PP');
            }
            elsif $name eq 'HOW' {
                $past.pasttype('pirop');
                $past.pirop('get_how PP');
            }
            elsif $name eq 'WHO' {
                $past.pasttype('pirop');
                $past.pirop('get_who PP');
            }
            elsif $name eq 'VAR' {
                $past.pasttype('pirop');
                $past.pirop('perl6_var PP');
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

    method term:sym<now>($/) {
        make PAST::Op.new( :name('&term:<now>'), :node($/) );
    }

    method term:sym<time>($/) {
        make PAST::Op.new( :name('&term:<time>'), :node($/) );
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
        my @name := Perl6::Grammar::parse_name(~$<longname>);
        my $past;
        
        # If we have args, it's a call. Look it up dynamically
        # and make the call.
        if $<args> {
            # Add & to name.
            my $final := @name[+@name - 1];
            if pir::substr($final, 0, 1) ne '&' {
                @name[+@name - 1] := '&' ~ $final;
            }
            $past := capture_or_parcel($<args>.ast, ~$<longname>);
            $past.unshift($*ST.symbol_lookup(@name, $/));
        }
        
        # Otherwise, it's a type name; build a reference to that
        # type, since we can statically resolve them.
        else {
            my $sym := $*ST.find_symbol(@name);
            if $<arglist> {
                $/.CURSOR.panic("Parametric roles not yet implemented");
            }
            $past := $*ST.get_object_sc_ref_past($sym);
            $past<has_compile_time_value> := 1;
            $past<compile_time_value> := $sym;
        }
        
        $past.node($/);
        make $past;
    }

    method term:sym<pir::op>($/) {
        if $FORBID_PIR {
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

    method term:sym<pir::const>($/) {
        make PAST::Val.new(:value(~$<const>), :returns<!macro_const>, :node($/));
    }

    method term:sym<nqp::op>($/) {
        my $op    := ~$<op>;
        my $args  := $<args> ?? $<args>[0].ast.list !! [];
        my $past  := PAST::Node.'map_node'(|$args, :map<nqp>, :op($op),
                                           :node($/));

        pir::defined($past) ||
            $/.CURSOR.panic("Unrecognized nqp:: opcode 'nqp::$op'");
        make $past;
    }

    method term:sym<*>($/) {
        make PAST::Op.new(
            :pasttype('callmethod'), :name('new'), :node($/), :lvalue(1), :returns('Whatever'),
            PAST::Var.new( :name('Whatever'), :scope('lexical') )
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
        my $past := PAST::Op.new( :pasttype('call'), :node($/) );
        if $<EXPR> {
            my $expr := $<EXPR>.ast;
            if $expr.name eq '&infix:<,>' {
                for $expr.list { $past.push(handle_named_parameter($_, $/)); }
            }
            else { $past.push(handle_named_parameter($expr, $/)); }
        }

        make $past;
    }

    sub handle_named_parameter($arg, $/) {
        if $arg ~~ PAST::Op && $arg.returns() eq 'Pair' {
            my $result := $arg[2];
            $result.named(compile_time_value_str($arg[1], 'LHS of pair', $/));
            $result<before_promotion> := $arg;
            $result;
        }
        else {
            $arg;
        }
    }

    method term:sym<value>($/) { make $<value>.ast; }

    method circumfix:sym<( )>($/) {
        my $past := $<semilist>.ast;
        my $size := +$past.list;
        if $size == 0 {
            $past := PAST::Op.new( :name('&infix:<,>') );
        }
        else {
            my $last := $past[ $size - 1 ];
            if pir::defined($last.returns) {
                $past.returns($last.returns);
            }
            if pir::defined($last.arity) {
                $past.arity($last.arity);
            }
        }
        make $past;
    }

    method circumfix:sym<ang>($/) { make $<quote_EXPR>.ast; }

    method circumfix:sym<« »>($/) { make $<quote_EXPR>.ast; }

    method circumfix:sym<{ }>($/) {
        # If it was {YOU_ARE_HERE}, nothing to do here.
        my $past := $<pblock>.ast;
        if ~$/ eq '{YOU_ARE_HERE}' {
            make $past;
            return 1;
        }

        # If it is completely empty or consists of a single list, the first
        # element of which is either a hash or a pair, it's a hash constructor.
        my $is_hash := 0;
        my $stmts := +$<pblock><blockoid><statementlist><statement>;
        if $stmts == 0 {
            # empty block, so a hash
            $is_hash := 1;
        }
        elsif $stmts == 1 {
            my $elem := $past<past_block>[1][0][0];
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
        if $is_hash { # XXX && $past.arity < 1 {
            my @children := @($past<past_block>[1]);
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
            $past := block_closure($past);
            $past<sink_past> := $past<past_block>;
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
        my $sym := ~$<infix><sym>;
        if !$past && $sym eq '.=' {
            make make_dot_equals($/[0].ast, $/[1].ast);
            return 1;
        }
        elsif $sym eq '==>' || $sym eq '<==' || $sym eq '==>>' || $sym eq '<<==' {
            make make_feed($/);
            return 1;
        }
        elsif $sym eq '~~' {
            make make_smartmatch($/, 0);
            return 1;
        }
        elsif $sym eq '!~~' {
            make make_smartmatch($/, 1);
            return 1;
        }
        elsif $sym eq ':=' {
            make bind_op($/, 0);
            return 1;
        }
        elsif $sym eq '::=' {
            make bind_op($/, 1);
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
            # Method calls may be to a foreign language, and thus return
            # values may need type mapping into Perl 6 land.
            $past.unshift($/[0].ast);
            if $past.isa(PAST::Op) && $past.pasttype eq 'callmethod' {
                $past := PAST::Op.new( :pirop('perl6ize_type PP'), $past );
            }
        }
        else {
            for $/.list { if $_.ast { $past.push($_.ast); } }
        }
        if $sym eq '^^' || $sym eq 'xor' {
            # XXX Put something equivalent in place...or fix.
            #$past := PAST::Op.new(
            #    :pasttype<call>, :name('!Undef_to_False'), $past
            #);
        }
        if $key eq 'PREFIX' || $key eq 'INFIX' || $key eq 'POSTFIX' {
            $past := whatever_curry($/, $past, $key eq 'INFIX' ?? 2 !! 1);
        }
        make $past;
    }

    sub make_feed($/) {
        # Assemble into list of AST of each step in the pipeline.
        my @stages;
        if $/<infix><sym> eq '==>' {
            for @($/) { @stages.push($_.ast); }
        }
        elsif $/<infix><sym> eq '<==' {
            for @($/) { @stages.unshift($_.ast); }
        }
        else {
            $/.CURSOR.panic('Sorry, the ' ~ $/<infix> ~ ' feed operator is not yet implemented');
        }
        
        # Check what's in each stage and make a chain of blocks
        # that call each other. They'll return lazy things, which
        # will be passed in as var-arg parts to other things. The
        # first thing is just considered the result.
        my $result := @stages.shift;
        for @stages {
            # Wrap current result in a block, so it's thunked and can be
            # called at the right point.
            $result := PAST::Block.new( $result );

            # Check what we have. XXX Real first step should be looking
            # for @(*) since if we find that it overrides all other things.
            # But that's todo...soon. :-)
            if $_ ~~ PAST::Op && $_.pasttype eq 'call' {
                # It's a call. Stick a call to the current supplier in
                # as its last argument.
                $_.push(PAST::Op.new( :pasttype('call'), $result ));
            }
            elsif $_ ~~ PAST::Var {
                # It's a variable. We need code that gets the results, pushes
                # them onto the variable and then returns them (since this
                # could well be a tap.
                $_ := PAST::Stmts.new(
                    PAST::Op.new(
                        :pasttype('bind_6model'),
                        PAST::Var.new( :scope('register'), :name('tmp'), :isdecl(1) ),
                        PAST::Op.new( :pasttype('call'), $result )
                    ),
                    PAST::Op.new(
                        :pasttype('callmethod'), :name('push'),
                        $_,
                        PAST::Var.new( :scope('register'), :name('tmp') )
                    ),
                    PAST::Var.new( :scope('register'), :name('tmp') )
                );
            }
            else {
                $/.CURSOR.panic('Sorry, do not know how to handle this case of a feed operator yet.');
            }
            $result := $_;
        }

        return $result;
    }

    sub make_smartmatch($/, $negated) {
        my $lhs := $/[0].ast;
        my $rhs := $/[1].ast;
        my $old_topic_var := $lhs.unique('old_topic');
        my $result_var := $lhs.unique('sm_result');
        my $sm_call := PAST::Op.new(
            :pasttype('callmethod'), :name('ACCEPTS'),
            $rhs,
            PAST::Var.new( :name('$_'), :scope('lexical') )
        );
        if $negated {
            $sm_call := PAST::Op.new( :name('&prefix:<!>'), $sm_call );
        }
        PAST::Stmt.new(PAST::Op.new(
            :pasttype('stmts'),

            # Stash original $_.
            PAST::Op.new( :pasttype('bind_6model'),
                PAST::Var.new( :name($old_topic_var), :scope('register'), :isdecl(1) ),
                PAST::Var.new( :name('$_'), :scope('lexical') )
            ),

            # Evaluate LHS and bind it to $_.
            PAST::Op.new( :pasttype('bind_6model'),
                PAST::Var.new( :name('$_'), :scope('lexical') ),
                $lhs
            ),

            # Evaluate RHS and call ACCEPTS on it, passing in $_. Bind the
            # return value to a result variable.
            PAST::Op.new( :pasttype('bind_6model'),
                PAST::Var.new( :name($result_var), :scope('lexical'), :isdecl(1) ),
                $sm_call
            ),

            # Re-instate original $_.
            PAST::Op.new( :pasttype('bind_6model'),
                PAST::Var.new( :name('$_'), :scope('lexical') ),
                PAST::Var.new( :name($old_topic_var), :scope('register') )
            ),

            # And finally evaluate to the smart-match result.
            PAST::Var.new( :name($result_var), :scope('lexical') )
        ));
    }
    
    sub bind_op($/, $sigish) {
        my $target := $/[0].ast;
        my $source := $/[1].ast;
        
        # Check we know how to bind to the thing on the LHS.
        if $target.isa(PAST::Var) {
            # We may need to decontainerize the right, depending on sigil.
            my $sigil := pir::substr($target.name(), 0, 1);
            if $sigil eq '@' || $sigil eq '%' {
                $source := PAST::Op.new( :pirop('perl6_decontainerize PP'), $source );
            }
            
            # Now go by scope.
            if $target.scope eq 'attribute_6model' {
                # Ensure we're got going to try and bind to a native attribute;
                # that's not allowed.
                if $target.type.HOW.WHAT =:= %*HOW<native> {
                    $/.CURSOR.panic("Cannot bind to a native attribute; use assignment instead");
                }
                
                # Source needs type check.
                my $meta_attr := $*PACKAGE.HOW.get_attribute_for_usage($*PACKAGE, $target.name);
                $source := PAST::Op.new(
                    :pirop('perl6_assert_bind_ok 0PP'),
                    $source, $*ST.get_object_sc_ref_past($meta_attr.container_descriptor))
            }
            else {
                # Probably a lexical.
                my $was_lexical := 0;
                try {
                    my $descriptor := $*ST.find_lexical_container_descriptor($target.name);
                    $source := PAST::Op.new(
                        :pirop('perl6_assert_bind_ok 0PP'),
                        $source, $*ST.get_object_sc_ref_past($descriptor));
                    $was_lexical := 1;
                }
                unless $was_lexical {
                    $/.CURSOR.panic("Cannot use bind operator with this LHS");
                }
            }
            
            # Finally, just need to make a bind.
            make PAST::Op.new( :pasttype('bind_6model'), $target, $source );
        }
        # XXX Several more cases to do...
        else {
            $/.CURSOR.panic("Cannot use bind operator with this LHS");
        }
    }

    method prefixish($/) {
        if $<prefix_postfix_meta_operator> {
            my $opsub := '&prefix:<' ~ $<OPER>.Str ~ '<<>';
            unless %*METAOPGEN{$opsub} {
                my $base_op := '&prefix:<' ~ $<OPER>.Str ~ '>';
                $*UNITPAST.loadinit.push(PAST::Op.new(
                    :pasttype('bind_6model'),
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
                    PAST::Op.new( :pasttype('bind_6model'),
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
                :pasttype('bind_6model'),
                PAST::Var.new( :name($opsub), :scope('package') ),
                PAST::Op.new(
                    :pasttype('callmethod'), :name('assuming'),
                    PAST::Op.new( :pirop('find_sub_not_null__Ps'), '&reducewith' ),
                    PAST::Op.new( :pirop('find_sub_not_null__Ps'), $base_op ),
                    PAST::Val.new( :named('triangle'), :value($<triangle> ?? 1 !! 0) ),
                    PAST::Val.new( :named('chaining'), :value($<op><OPER><O><prec> eq 'm=') ),
                    PAST::Val.new( :named('right-assoc'), :value($<op><OPER><O><assoc> eq 'right') ),
                    PAST::Val.new( :named('xor'), :value($<op><OPER><O><pasttype> eq 'xor') )
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
            my $base_op := '&infix:<' ~ $<infixish>.Str ~ '>';
            my $dwim_lhs := $<opening> eq '<<' || $<opening> eq '«';
            my $dwim_rhs := $<closing> eq '>>' || $<closing> eq '»';
            $*UNITPAST.loadinit.push(PAST::Op.new(
                :pasttype('bind_6model'),
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
                if ($past.name() eq '') {
                    $past.name('!dispatch_invocation_parallel');
                }
                else {
                    $past.unshift($past.name());
                    $past.name('!dispatch_dispatcher_parallel');
                }
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
                        :pasttype('bind_6model'),
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
        my $past := PAST::Op.new( :name('postcircumfix:<[ ]>'), :pasttype('callmethod'), :node($/) );
        if $<semilist><statement> { $past.push($<semilist>.ast); }
        make $past;
    }

    method postcircumfix:sym<{ }>($/) {
        my $past := PAST::Op.new( :name('postcircumfix:<{ }>'), :pasttype('callmethod'), :node($/) );
        if $<semilist><statement> {
            if +$<semilist><statement> > 1 {
                $/.CURSOR.panic("Sorry, multi-dimensional indexes are not yet supported");
            }
            $past.push($<semilist>.ast);
        }
        make $past;
    }

    method postcircumfix:sym<ang>($/) {
        my $past := PAST::Op.new( :name('postcircumfix:<{ }>'), :pasttype('callmethod'), :node($/) );
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
        # XXX Work out at compile time, then...
        # make $*ST.add_constant('Complex', 'complex', [$re, $im]);
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
        if $<integer> {
            make PAST::Want.new(
                     $*ST.add_constant('Int', 'int', $<integer>.ast),
                     'IiNn', $<integer>.ast);
        }
        elsif $<dec_number> { make $<dec_number>.ast; }
        elsif $<rad_number> { make $<rad_number>.ast; }
        else {
            make PAST::Want.new(
                      $*ST.add_constant('Num', 'num', +(~$/)),
                      'IiNn', +(~$/));
        }
    }

    method dec_number($/) {
        my $int  := $<int> ?? ~$<int> !! "0";
        my $frac := $<frac> ?? ~$<frac> !! "0";
        if $<escale> {
            my $exp := ~$<escale>[0]<decint>;
            make $*ST.add_constant('Num', 'num',
                str2num(0, $int, $frac, ($<escale>[0]<sign> eq '-'), $exp));
        } else {
            # XXX Work out at compile time, then...
            # make $*ST.add_constant('Rat', 'rational', [$nu, $de]);
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
        # Locate the type object and make that. Anything that wants a PAST
        # reference to it can obtain one, but many things really want the
        # actual type object to build up some data structure or make a trait
        # dispatch with. Note that for '::T' style things we need to make a
        # GenericHOW, though whether/how it's used depends on context.
        if $<longname> {
            if pir::substr(~$<longname>, 0, 2) ne '::' {
                my $type := $*ST.find_symbol(Perl6::Grammar::parse_name(
                    Perl6::Grammar::canonical_type_longname($<longname>)));
                if $<arglist> {
                    $type := $*ST.curry_role(%*HOW<role-curried>, $type, $<arglist>, $/);
                }
                make $type;
            }
            else {
                if $<arglist> || $<typename> {
                    $/.CURSOR.panic("Cannot put type parameters on a type capture");
                }
                make $*ST.pkg_create_mo(%*HOW<generic>, :name(pir::substr(~$<longname>, 2)));
            }
        }
        else {
            make $*ST.find_symbol(Perl6::Grammar::parse_name('::?' ~ ~$<identifier>));
        }   
    }

    our %SUBST_ALLOWED_ADVERBS;
    our %SHARED_ALLOWED_ADVERBS;
    our %MATCH_ALLOWED_ADVERBS;
    INIT {
        my $mods := 'i ignorecase s sigspace r ratchet';
        for pir::split__PSS(' ', $mods) {
            %SHARED_ALLOWED_ADVERBS{$_} := 1;
        }

        $mods := 'g global ii samecase x c continue p pos nth th st nd rd';
        for pir::split__PSS(' ', $mods) {
            %SUBST_ALLOWED_ADVERBS{$_} := 1;
        }

        # TODO: add g global ov overlap  once they actually work
        $mods := 'x c continue p pos nth th st nd rd';
        for pir::split__PSS(' ', $mods) {
            %MATCH_ALLOWED_ADVERBS{$_} := 1;
        }
    }


    method quotepair($/) {
        unless $*value ~~ PAST::Node {
            if ($*key eq 'c' || $*key eq 'continue'
            || $*key eq 'p' || $*key eq 'pos') && $*value == 1 {
                $*value := PAST::Op.new(
                    :node($/),
                    :pasttype<if>,
                    PAST::Var.new(:name('$/'), :scope('lexical')),
                    PAST::Op.new(:pasttype('callmethod'),
                        PAST::Var.new(:name('$/'), :scope<lexical>),
                        :name<to>
                    ),
                    PAST::Val.new(:value(0)),
                );
            } else {
                $*value := PAST::Val.new( :value($*value) );
            }
        }
        $*value.named(~$*key);
        make $*value;
    }

    method setup_quotepairs($/) {
        my %h;
        for @*REGEX_ADVERBS {
            my $key := $_.ast.named;
            my $value := $_.ast;
            if $value ~~ PAST::Val {
                $value := $value.value;
            } else {
                if %SHARED_ALLOWED_ADVERBS{$key} {
                    $/.CURSOR.panic('Value of adverb :' ~ $key ~ ' must be known at compile time');
                }
            }
            if $key eq 'samecase' || $key eq 'ii' {
                %h{'i'} := 1;
            }
            %h{$key} := $value;
        }

        my @MODIFIERS := Q:PIR {
            %r = get_hll_global ['Regex';'P6Regex';'Actions'], '@MODIFIERS'
        };
        @MODIFIERS.unshift(%h);
    }

    method cleanup_modifiers($/) {
        my @MODIFIERS := Q:PIR {
            %r = get_hll_global ['Regex';'P6Regex';'Actions'], '@MODIFIERS'
        };
        @MODIFIERS.shift();

    }

    method quote:sym<apos>($/) { make $<quote_EXPR>.ast; }
    method quote:sym<dblq>($/) { make $<quote_EXPR>.ast; }
    method quote:sym<qq>($/)   { make $<quote_EXPR>.ast; }
    method quote:sym<qw>($/)   { make $<quote_EXPR>.ast; }
    method quote:sym<q>($/)    { make $<quote_EXPR>.ast; }
    method quote:sym<Q>($/)    { make $<quote_EXPR>.ast; }
    method quote:sym<Q:PIR>($/) {
        if $FORBID_PIR {
            pir::die("Q:PIR forbidden in safe mode\n");
        }
        my $pir := compile_time_value_str($<quote_EXPR>.ast, "Q:PIR", $/);
        make PAST::Op.new( :inline( $pir ), :pasttype('inline'), :node($/) );
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

        self.handle_and_check_adverbs($/, %SHARED_ALLOWED_ADVERBS, 'rx');
        my $past := Regex::P6Regex::Actions::buildsub($<p6regex>.ast);
        make block_closure($past, 'Regex', 0);
    }
    method quote:sym<m>($/) {
        $regex := Regex::P6Regex::Actions::buildsub($<p6regex>.ast);
        my $regex := block_closure($regex, 'Regex', 0);

        my $past := PAST::Op.new(
            :node($/),
            :pasttype('callmethod'), :name('match'),
            PAST::Var.new( :name('$_'), :scope('lexical') ),
            $regex
        );
        self.handle_and_check_adverbs($/, %MATCH_ALLOWED_ADVERBS, 'm', $past);
        $past := PAST::Op.new(
            :node($/),
            :pasttype('call'), :name('&infix:<:=>'),
            PAST::Var.new(:name('$/'), :scope('lexical')),
            $past
        );

        make $past;
    }

    method handle_and_check_adverbs($/, %adverbs, $what, $past?) {
        for $<quotepair> {
            unless %SHARED_ALLOWED_ADVERBS{$_.ast.named} || %adverbs{$_.ast.named} {
                $/.CURSOR.panic("Adverb '" ~ $_.ast.named ~ "' not allowed on " ~ $what);
            }
            if $past {
                $past.push($_.ast);
            }
        }
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

        # make $_ = $_.subst(...)
        my $past := PAST::Op.new(
            :node($/),
            :pasttype('callmethod'), :name('subst'),
            PAST::Var.new( :name('$_'), :scope('lexical') ),
            $regex, $closure
        );
        self.handle_and_check_adverbs($/, %SUBST_ALLOWED_ADVERBS, 'substitution', $past);
        if $/[0] {
            pir::push__vPP($past, PAST::Val.new(:named('samespace'), :value(1)));
        }

        $past := PAST::Op.new(
            :node($/),
            :pasttype('call'),
            :name('&infix:<=>'),
            PAST::Var.new(:name('$_'), :scope('lexical')),
            $past
        );

        make $past;
    }

    method quote_escape:sym<$>($/) {
        make steal_back_spaces($/, $<EXPR>.ast);
    }

    method quote_escape:sym<array>($/) {
        make steal_back_spaces($/, $<EXPR>.ast);
    }

    method quote_escape:sym<%>($/) {
        make steal_back_spaces($/, $<EXPR>.ast);
    }

    method quote_escape:sym<&>($/) {
        make steal_back_spaces($/, $<EXPR>.ast);
    }

    # Unfortunately, the operator precedence parser (probably correctly)
    # steals spaces after a postfixish. Thus "$a $b" would get messed up.
    # Here we take them back again. Hacky, better solutions welcome.
    sub steal_back_spaces($/, $expr) {
        my $pos := nqp::chars($/) - 1;
        while pir::is_cclass__IISI(32, $/, $pos) {
            $pos--;
        }
        my $nab_back := pir::substr__SSI($/, $pos + 1);
        if $nab_back {
            PAST::Op.new( :pasttype('call'), :name('&infix:<~>'), $expr, $*ST.add_constant('Str', 'str', ~$nab_back) )
        }
        else {
            $expr
        }
    }

    method quote_escape:sym<{ }>($/) {
        make PAST::Op.new(
            :pasttype('callmethod'), :name('Stringy'),
            PAST::Op.new( $<block>.ast ), :node($/)
        );
    }

    # overrides versions from HLL::Actions to handle Perl6Str
    # and use &infix:<,> to build the parcel
    method quote_EXPR($/) {
        my $past := $<quote_delimited>.ast;
        if $/.CURSOR.quotemod_check('w') {
            my @words := HLL::Grammar::split_words($/,
                compile_time_value_str($past, ":w list", $/));
            if +@words != 1 {
                $past := PAST::Op.new( :name('&infix:<,>'), :node($/) );
                for @words { $past.push($*ST.add_constant('Str', 'str', ~$_)); }
                $past := PAST::Stmts.new($past);
            }
            else {
                $past := $*ST.add_constant('Str', 'str', ~@words[0]);
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
                    @parts.push($*ST.add_constant('Str', 'str', $lastlit));
                }
                @parts.push($ast);
                $lastlit := '';
            }
        }
        if $lastlit gt '' || !@parts {
            @parts.push($*ST.add_constant('Str', 'str', $lastlit));
        }
        my $past := @parts ?? @parts.shift !! $*ST.add_constant('Str', 'str', '');
        while @parts {
            $past := PAST::Op.new(
                :pasttype('call'), :name('&infix:<~>'),
                $past, @parts.shift
            );
        }
        make $past;
    }

    # Adds code to do the signature binding.
    sub add_signature_binding_code($block, $sig_obj) {
        # Set arity.
        # XXX TODO
        #$block.arity($sig_obj.arity);

        # We tell Parrot that we'll have all args in the call_sig so it won't
        # do its own arg processing. We also add a call to bind the signature.
        $block[0].push(PAST::Var.new( :name('call_sig'), :scope('parameter'), :call_sig(1) ));
        $block[0].push(PAST::Op.new( :pirop('bind_signature vP') ));

        $block;
    }

    # Adds a placeholder parameter to this block's signature.
    sub add_placeholder_parameter($/, $sigil, $ident, :$named, :$slurpy_pos, :$slurpy_named) {
        # Obtain/create placeholder parameter list.
        my $block := $*ST.cur_lexpad();
        my @params := $block<placeholder_sig> || ($block<placeholder_sig> := []);
        
        # Make descriptor.
        my $name := ~$sigil ~ ~$ident;
        my %param_info := hash(
            variable_name => $name,
            slurpy_pos    => $slurpy_pos,
            slurpy_named  => $slurpy_named);
        
        # If it's slurpy, just goes on the end.
        if $slurpy_pos || $slurpy_named {
            @params.push(%param_info);
        }
        
        # If it's named, just shove it on the end, but before any slurpies.
        elsif $named {
            %param_info<named_names> := [$ident];
            my @popped;
            while @params
                    && (@params[+@params - 1]<slurpy_pos> || @params[+@params - 1]<slurpy_named>) {
                @popped.push(@params.pop);
            }
            @params.push(%param_info);
            while @popped { @params.push(@popped.pop) }
        }
        
        # Otherwise, put it in correct lexicographic position.
        else {
            my @shifted;
            for @params {
                last if $_<slurpy_pos> || $_<slurpy_named> ||
                        $_<named_names> ||
                        pir::substr__SSi($_<variable_name>, 1) gt $ident;
                @shifted.push(@params.shift);
            }
            @params.unshift(%param_info);
            while @shifted { @params.unshift(@shifted.pop) }
        }

        # Add variable declaration, and evaluate to a lookup of it.
        my %existing := $block.symbol($name);
        if +%existing && !%existing<placeholder_parameter> {
            $/.CURSOR.panic("Redeclaration of symbol $name as a placeholder parameter");
        }
        $block[0].push(PAST::Var.new( :name($name), :scope('lexical'), :isdecl(1) ));
        $block.symbol($name, :scope('lexical'), :placeholder_parameter(1));
        return PAST::Var.new( :name($name), :scope('lexical') );
    }

    sub reference_to_code_object($code_obj, $past_block) {
        my $ref := $*ST.get_object_sc_ref_past($code_obj);
        $ref<past_block> := $past_block;
        $ref<code_object> := $code_obj;
        return $ref;
    }
    
    sub block_closure($code) {
        my $closure := PAST::Op.new(
            :pasttype('callmethod'), :name('clone'),
            $code
        );
        $closure<block_past> := $code<block_past>;
        $closure<code_object> := $code<code_object>;
        return $closure;
    }
    
    sub make_thunk($to_thunk) {
        make_simple_code_object(PAST::Block.new( $to_thunk ), 'Code');
    }
    
    sub make_thunk_ref($to_thunk) {
        my $block := PAST::Block.new( $to_thunk );
        reference_to_code_object(
            make_simple_code_object($block, 'Code'),
            $block);
    }
    
    sub make_simple_code_object($block, $type) {
        ($*ST.cur_lexpad())[0].push($block);
        my $sig  := $*ST.create_signature([]);
        return $*ST.create_code_object($block, $type, $sig);
    }
    
    sub make_where_block($expr) {
        # If it's already a block, nothing to do at all.
        if $expr<past_block> {
            return $expr<code_object>;
        }
        
        # Build a block that'll smartmatch the topic against the
        # expression.
        my $past := PAST::Block.new(
            PAST::Stmts.new(
                PAST::Var.new( :name('$_'), :scope('lexical'), :isdecl(1) )
            ),
            PAST::Stmts.new(
                PAST::Op.new(
                    :pasttype('callmethod'), :name('ACCEPTS'),
                    $expr,
                    PAST::Var.new( :name('$_'), :scope('lexical') )
                )));
        ($*ST.cur_lexpad())[0].push($past);
        
        # Give it a signature and create code object.
        my $sig := $*ST.create_signature([
            $*ST.create_parameter(hash(
                variable_name => '$_',
                nominal_type => $*ST.find_symbol(['Mu'])
            ))]);
        add_signature_binding_code($past, $sig);
        return $*ST.create_code_object($past, 'Block', $sig);
    }

    sub add_implicit_var($block, $name) {
        $block[0].push(PAST::Var.new( :name($name), :scope('lexical'), :isdecl(1) ));
        $block.symbol($name, :scope('lexical') );
    }

    sub when_handler_helper($block) {
        my $BLOCK := $*ST.cur_lexpad();
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
            PAST::Op.new( :pasttype('bind_6model'),
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
            PAST::Op.new( :pasttype('bind_6model'),
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
            PAST::Op.new( :pasttype('bind_6model'),
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

    # Handles the case where we have a default value closure for an
    # attribute.
    method install_attr_init($/) {
        # Locate attribute.
        my $attr := ($/[0].ast)<attribute_declarand>;
        
        # Construct signature and anonymous method.
        my $sig := $*ST.create_signature([
            $*ST.create_parameter(hash(is_invocant => 1, nominal_type => $*PACKAGE)),
            $*ST.create_parameter(hash( variable_name => '$_', nominal_type => $*ST.find_symbol(['Mu'])))
            ]);
        my $block := PAST::Block.new(
            PAST::Stmts.new(
                PAST::Var.new( :name('self'), :scope('lexical'), :isdecl(1) ),
                PAST::Var.new( :name('$_'), :scope('lexical'), :isdecl(1) )
            ),
            PAST::Stmts.new( $/[1].ast ));
        my $code := $*ST.create_code_object($block, 'Method', $sig);
        
        # Block should go in current lexpad, in correct lexical context.
        ($*ST.cur_lexpad())[0].push($block);
        
        # Dispatch trait. XXX Should really be Bool::True, not Int here...
        my $true := ($*ST.add_constant('Int', 'int', 1))<compile_time_value>;
        $*ST.apply_trait('&trait_mod:<will>', $attr, $code, :build($true));
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

    # not_curried = 1 means do not curry Whatever, but do curry WhateverCode
    # not_curried = 2 means do not curry either.

    our %not_curried;
    INIT {
        %not_curried{'&infix:<...>'}  := 2;
        %not_curried{'&infix:<...^>'} := 2;
        %not_curried{'&infix:<..>'}   := 1;
        %not_curried{'&infix:<..^>'}  := 1;
        %not_curried{'&infix:<^..>'}  := 1;
        %not_curried{'&infix:<^..^>'} := 1;
        %not_curried{'&prefix:<^>'}   := 2;
        %not_curried{'&infix:<xx>'}   := 1;
        %not_curried{'&infix:<~~>'}   := 2;
        %not_curried{'&infix:<=>'}    := 2;
        %not_curried{'&infix:<:=>'}   := 2;
        %not_curried{'WHAT'}          := 2;
        %not_curried{'HOW'}           := 2;
        %not_curried{'WHO'}           := 2;
        %not_curried{'WHERE'}         := 2;
    }
    sub whatever_curry($/, $past, $upto_arity) {
        if $past.isa(PAST::Op) && %not_curried{$past.name} != 2
                               && ($past<pasttype> ne 'call' || pir::index($past.name, '&infix:') == 0) {
            if ($upto_arity >= 1 && (($past[0].returns eq 'Whatever' && !%not_curried{$past.name})
                                     || $past[0].returns eq 'WhateverCode'))
            || ($upto_arity == 2 && (($past[1].returns eq 'Whatever' && !%not_curried{$past.name})
                                     || $past[1].returns eq 'WhateverCode')) {

                my $counter := 0;
                my $sig := Perl6::Compiler::Signature.new();
                my $left := $past.shift;
                my $left_new;
                my $right_new;

                if $left.returns eq 'WhateverCode' {
                    $left_new := PAST::Op.new( :pasttype('call'), :node($/), $left);
                    my $left_arity := $left.arity;
                    while $counter < $left_arity {
                        $counter++;
                        $left_new.push(PAST::Var.new( :name('$x' ~ $counter), :scope('lexical') ));
                        $sig.add_parameter(Perl6::Compiler::Parameter.new(:var_name('$x' ~ $counter)));
                    }
                }
                elsif $left.returns eq 'Whatever' {
                    $counter++;
                    $left_new := PAST::Var.new( :name('$x' ~ $counter), :scope('lexical') );
                    $sig.add_parameter(Perl6::Compiler::Parameter.new(:var_name('$x' ~ $counter)));
                }
                else {
                    $left_new := $left;
                }

                if $upto_arity == 2 {
                    my $right := $past.shift;

                    if $right.returns eq 'WhateverCode' {
                        $right_new := PAST::Op.new( :pasttype('call'), :node($/), $right);
                        # Next block is a bit weird, because $counter + $right.arity was
                        # consistently failing.  So we create a new variable as a temporary
                        # counter.
                        my $right_arity := $right.arity;
                        my $right_counter := 0;
                        while $right_counter < $right_arity {
                            $counter++;
                            $right_counter++;
                            $right_new.push(PAST::Var.new( :name('$x' ~ $counter), :scope('lexical') ));
                            $sig.add_parameter(Perl6::Compiler::Parameter.new(:var_name('$x' ~ $counter)));
                        }
                    }
                    elsif $right.returns eq 'Whatever' {
                        $counter++;
                        $right_new := PAST::Var.new( :name('$x' ~ $counter), :scope('lexical') );
                        $sig.add_parameter(Perl6::Compiler::Parameter.new(:var_name('$x' ~ $counter)));
                    }
                    else {
                        $right_new := $right;
                    }
                }

                if $upto_arity == 2 {
                    $past.unshift($right_new);
                }
                $past.unshift($left_new);
                $past := block_closure(blockify($past, $sig), 'WhateverCode', 0);
                $past.returns('WhateverCode');
                $past.arity($sig.arity);
            }
        }
        $past
    }
    
    # Ensures that the given PAST node has a value known at compile
    # time and if so obtains it. Otherwise reports an error, involving
    # the $usage parameter to make it more helpful.
    sub compile_time_value_str($past, $usage, $/) {
        if $past<has_compile_time_value> {
            nqp::unbox_s($past<compile_time_value>);
        }
        else {
            $/.CURSOR.panic("$usage must have a value known at compile time");
        }
    }
    
    my @prim_spec_map := ['', 'perl6_box_int__PI', 'perl6_box_num__PN', 'perl6_box_str__PS'];
    sub box_native_if_needed($past, $type) {
        my $primspec := pir::repr_get_primitive_type_spec__IP($type);
        if $primspec {
            PAST::Op.new( :pirop(@prim_spec_map[$primspec]), $past )
        }
        else {
            $past
        }
    }
    
    # XXX This probably dupes something in HLL::Actions...
    # XXX Either way, shouldn't be in PIR.
    our sub str2num-int($src) {
        Q:PIR {
            .local pmc src
            .local string src_s
            src = find_lex '$src'
            src_s = src
            .local int pos, eos
            .local num result
            pos = 0
            eos = length src_s
            result = 0
          str_loop:
            unless pos < eos goto str_done
            .local string char
            char = substr src_s, pos, 1
            if char == '_' goto str_next
            .local int digitval
            digitval = index "0123456789", char
            if digitval < 0 goto err_base
            if digitval >= 10 goto err_base
            result *= 10
            result += digitval
          str_next:
            inc pos
            goto str_loop
          err_base:
        src.'panic'('Invalid radix conversion of "', char, '"')
          str_done:
            %r = box result
        };
    }

    # XXX Translate to NQP.
    our sub str2num-base($src) {
        Q:PIR {
            .local pmc src
            .local string src_s
            src = find_lex '$src'
            src_s = src
            .local int pos, eos
            .local num result
            pos = 0
            eos = length src_s
            result = 1
          str_loop:
            unless pos < eos goto str_done
            .local string char
            char = substr src_s, pos, 1
            if char == '_' goto str_next
            result *= 10
          str_next:
            inc pos
            goto str_loop
          str_done:
            %r = box result
        };
    }

    sub str2num($negate, $int_part, $frac_part, $exp_part_negate, $exp_part) {
        my $exp := str2num-int($exp_part);
        $exp := -$exp if $exp_part_negate;
        my $result := (str2num-int($int_part) + str2num-int($frac_part) / str2num-base($frac_part))
                     * 10 ** $exp;
        $result := -$result if $negate;
        $result;
    }
}

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
                    :pasttype('bind_6model')
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

# vim: ft=perl6
