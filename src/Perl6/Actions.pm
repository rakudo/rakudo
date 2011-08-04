use NQPP6Regex;
use Perl6::Pod;
use QRegex;

INIT {
    # Add our custom nqp:: opcodes.
    PAST::Node.map_add('nqp',
        p6box_i      => 'perl6_box_int__Pi',
        p6box_n      => 'perl6_box_num__Pn',
        p6box_s      => 'perl6_box_str__Ps',
        p6bool       => 'perl6_booleanize__Pi',
        p6bigint     => 'perl6_box_bigint__Pn',
        p6parcel     => 'perl6_parcel_from_rpa__PPP',
        p6listiter   => 'perl6_iter_from_rpa__PPP',
        p6list       => 'perl6_list_from_rpa__PPPP',
        attrinited   => 'repr_is_attr_initialized__IPPs',

        istype       => 'type_check__IPP',
        lcm_i        => 'lcm__Iii',
        gcd_i        => 'gcd__Iii',
        find_method  => 'find_method__PPs',
        create       => 'repr_instance_of__PP',
        exit         => 'exit__vi',
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
        my $mainline := PAST::Stmts.new(
            $*POD_PAST,
            $<statementlist>.ast,
        );

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

        # Mainline should have fresh lexicals.
        $unit.loadinit().push(PAST::Op.new(
            :pasttype('callmethod'), :name('set_fresh_magicals'),
            PAST::Op.new(
                :pasttype('callmethod'), :name('get_lexinfo'),
                PAST::Val.new( :value($unit) ))));

        # Get the block for the entire compilation unit.
        my $outer := $*UNIT_OUTER;
        $outer.node($/);

        # Set HLL and load the needed libraries.
        $outer.hll('perl6');
        $unit.loadlibs('nqp_group', 'nqp_ops', 'perl6_group', 'perl6_ops',
                       'bit_ops', 'math_ops', 'trans_ops', 'io_ops',
                       'obscure_ops', 'os', 'file');

        # If the unit defines &MAIN, add a &MAIN_HELPER.
        my $mainparam := PAST::Var.new(:name('$MAIN'), :scope('parameter'),
                             :viviself( PAST::Val.new( :value(0) ) ) );
        $unit.symbol('$MAIN', :scope<lexical_6model>);
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

    method pod_content_toplevel($/) {
        my $child := $<pod_block>.ast;
        # make sure we don't push the same thing twice
        if $child {
            my $id := $/.from ~ "," ~ ~$/.to;
            if !$*POD_BLOCKS_SEEN{$id} {
                $*POD_BLOCKS.push($child);
                $*POD_BLOCKS_SEEN{$id} := 1;
            }
        }
        make $child;
    }

    method pod_content:sym<block>($/) {
        make $<pod_block>.ast;
    }

    method pod_block:sym<delimited>($/) {
        make Perl6::Pod::any_block($/);
    }

    method pod_block:sym<delimited_raw>($/) {
        make Perl6::Pod::raw_block($/);
    }

    method pod_block:sym<delimited_table>($/) {
        make Perl6::Pod::table($/);
    }

    method pod_block:sym<paragraph>($/) {
        make Perl6::Pod::any_block($/);
    }

    method pod_block:sym<paragraph_raw>($/) {
        make Perl6::Pod::raw_block($/);
    }

    method pod_block:sym<paragraph_table>($/) {
        make Perl6::Pod::table($/);
    }

    method pod_block:sym<abbreviated>($/) {
        make Perl6::Pod::any_block($/);
    }

    method pod_block:sym<abbreviated_raw>($/) {
        make Perl6::Pod::raw_block($/);
    }

    method pod_block:sym<abbreviated_table>($/) {
        make Perl6::Pod::table($/);
    }

    method pod_block:sym<end>($/) {
    }

    method raw_block($/) {
    }

    method pod_text_para($/) {
        my $t    := Perl6::Pod::formatted_text($<text>.Str);
        my $past := Perl6::Pod::serialize_object(
            'Pod::Block::Para',
            :content(Perl6::Pod::serialize_aos([$t])<compile_time_value>),
        );
        make $past<compile_time_value>;
    }

    method pod_content:sym<text>($/) {
        my @ret := [];
        for $<pod_textcontent> {
            @ret.push($_.ast);
        }
        my $past := Perl6::Pod::serialize_array(@ret);
        make $past<compile_time_value>;
    }

    method pod_textcontent:sym<regular>($/) {
        my $t    := Perl6::Pod::formatted_text($<text>.Str);
        # is only one string now, will be a twine
        my $twine := Perl6::Pod::serialize_aos([$t])<compile_time_value>;
        make Perl6::Pod::serialize_object(
            'Pod::Block::Para', :content($twine)
        )<compile_time_value>
    }

    method pod_textcontent:sym<code>($/) {
        my $s := $<spaces>.Str;
        my $t := subst($<text>.Str, /\n$s/, "\n", :global);
        $t    := subst($t, /\n$/, ''); # chomp!
        my $past := Perl6::Pod::serialize_object(
            'Pod::Block::Code',
            :content(Perl6::Pod::serialize_aos([$t])<compile_time_value>),
        );
        make $past<compile_time_value>;
    }

    method table_row($/) {
        make ~$/
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
        $past.push(PAST::Var.new(:name('Nil'), :scope('lexical_6model'))) if +$past.list < 1;
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
                $mc.ast.push(PAST::Var.new(:name('Nil'), :scope('lexical_6model')));
                $past := $mc.ast;
            }
            if $ml {
                my $cond := $ml<smexpr>.ast;
                if ~$ml<sym> eq 'given' {
                    $past := PAST::Op.new(
                        :pasttype('call'),
                        make_topic_block_ref($past),
                        $cond
                    );
                }
                elsif ~$ml<sym> eq 'for' {
                    unless $past<past_block> {
                        $past := make_topic_block_ref($past);
                    }
                    $past := PAST::Op.new(
                        :name<&eager>, :node($/),
                        PAST::Op.new(
                            :pasttype<callmethod>, :name<map>, :node($/),
                            PAST::Op.new(:name('&infix:<,>'), $cond),
                            $past
                        ));
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
            add_signature_binding_code($block, $signature, @params);

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
                    !!  PAST::Var.new(:name('Nil'), :scope('lexical_6model'))
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
                        PAST::Op.new(:name('&infix:<,>'), $xblock[0]),
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
                        :name('&DYNAMIC'),
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
            PAST::Var.new( :name('$_'), :scope('lexical_6model') ),
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
        my $block := $<block>.ast;
        when_handler_helper($block<past_block>);
        make PAST::Op.new($block);
    }

    method statement_control:sym<CATCH>($/) {
        my $block := $<block>.ast;
        push_block_handler($/, $*ST.cur_lexpad(), $block);
        $*ST.cur_lexpad().handlers()[0].handle_types_except('CONTROL');
        make PAST::Var.new( :name('Nil'), :scope('lexical') );
    }

    method statement_control:sym<CONTROL>($/) {
        my $block := $<block>.ast;
        push_block_handler($/, $*ST.cur_lexpad(), $block);
        $*ST.cur_lexpad().handlers()[0].handle_types('CONTROL');
        make PAST::Var.new( :name('Nil'), :scope('lexical') );
    }

    method statement_prefix:sym<BEGIN>($/) { $*ST.add_phaser($/, ($<blorst>.ast)<code_object>, 'BEGIN'); }
    method statement_prefix:sym<CHECK>($/) { $*ST.add_phaser($/, ($<blorst>.ast)<code_object>, 'CHECK'); }
    method statement_prefix:sym<INIT>($/)  { $*ST.add_phaser($/, ($<blorst>.ast)<code_object>, 'INIT'); }
    method statement_prefix:sym<END>($/)   { $*ST.add_phaser($/, ($<blorst>.ast)<code_object>, 'END'); }

    method statement_prefix:sym<DOC>($/)   {
        $*ST.add_phaser($/, ($<blorst>.ast)<code_object>, ~$<phase>)
            if %*COMPILING<%?OPTIONS><doc>;
    }

    method statement_prefix:sym<do>($/) {
        make PAST::Op.new( :pasttype('call'), $<blorst>.ast );
    }

    method statement_prefix:sym<gather>($/) {
        my $past := block_closure($<blorst>.ast);
        make PAST::Op.new( :pasttype('call'), :name('&GATHER'), $past );
    }

    method statement_prefix:sym<sink>($/) {
        my $blast := PAST::Op.new( $<blorst>.ast );
        make PAST::Stmts.new(
            PAST::Op.new( :name('&eager'), $blast ),
            PAST::Var.new( :name('Nil'), :scope('lexical')),
            :node($/)
        );
    }

    method statement_prefix:sym<try>($/) {
        my $block := PAST::Op.new(:pasttype<call>, block_closure($<blorst>.ast)); # XXX should be immediate
        my $past := PAST::Op.new( :pasttype('try'), $block );

        # On failure, capture the exception object into $!.
        $past.push(
            PAST::Op.new(:pasttype<bind_6model>,
                PAST::Var.new(:name<$!>, :scope<lexical_6model>),
                PAST::Op.new(:name<&EXCEPTION>, :pasttype<call>,
                    PAST::Op.new(:inline("    .get_results (%r)\n    finalize %r")))));

        # Otherwise, put Mu into $!.
        $past.push(
            PAST::Op.new(:pasttype<bind_6model>,
                PAST::Var.new( :name<$!>, :scope<lexical_6model> ),
                PAST::Var.new( :name<Mu>, :scope<lexical_6model> )));

        make $past;
    }

    method blorst($/) {
        make $<block> ?? $<block>.ast !! make_thunk_ref($<statement>.ast, $/);
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
                my $val_ast := $*value.ast;
                if $val_ast.isa(PAST::Stmts) && +@($val_ast) == 1 {
                    $val_ast := $val_ast[0];
                }
                make make_pair($*key, $val_ast);
            }
            elsif $*value == 0 {
                make make_pair($*key, PAST::Op.new( :pirop('perl6_booleanize PI'), 0 ));
            }
            else {
                make make_pair($*key, PAST::Op.new( :pirop('perl6_booleanize PI'), 1 ));
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
            PAST::Var.new( :name('Pair'), :scope('lexical_6model') ),
            $key, $value
        )
    }

    method variable($/) {
        my $past;
        if $<index> {
            $past := PAST::Op.new(
                :pasttype('callmethod'),
                :name('postcircumfix:<[ ]>'),
                PAST::Var.new(:name('$/'), :scope('lexical_6model')),
                $*ST.add_constant('Int', 'int', +$<index>),
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
            if $<desigilname> && $<desigilname><longname> && self.is_indirect_lookup($<desigilname><longname>) {
                if $*IN_DECL {
                    $/.CURSOR.panic("Variable variable names not allowed in declarations");
                }
                $past := self.make_indirect_lookup($<desigilname><longname>, ~$<sigil>);
            } else {
                $past := make_variable($/, ~$/);
            }
        }
        make $past;
    }

    sub make_variable($/, $name) {
        make_variable_from_parts($/, $name, $<sigil>.Str, $<twigil>[0], ~$<desigilname>);
    }

    sub make_variable_from_parts($/, $name, $sigil, $twigil, $desigilname) {
        my @name := Perl6::Grammar::parse_name($name);
        my $past := PAST::Var.new( :name(@name[+@name - 1]), :node($/));
        if $twigil eq '*' {
            $past := PAST::Op.new(
                $*ST.add_constant('Str', 'str', ~$past.name()),
                :pasttype('call'), :name('&DYNAMIC'), :lvalue(0) );
        }
        elsif $twigil eq '!' {
            # In a declaration, don't produce anything here.
            if $*IN_DECL ne 'variable' {
                # Ensure attribute actaully exists before emitting lookup.
                unless pir::can($*PACKAGE.HOW, 'get_attribute_for_usage') {
                    $/.CURSOR.panic("Cannot understand $name in this context");
                }
                my $attr;
                my $found := 0;
                try {
                    $attr := $*PACKAGE.HOW.get_attribute_for_usage($*PACKAGE, $name);
                    $found := 1;
                }
                if $found {
                    $past.scope('attribute_6model');
                    $past.type($attr.type);
                    $past.unshift(instantiated_type(['$?CLASS'], $/));
                    $past.unshift(PAST::Var.new( :name('self'), :scope('lexical_6model') ));
                    $past := box_native_if_needed($past, $attr.type);
                }
                else {
                    $/.CURSOR.panic("Attribute $name not declared in $*PKGDECL " ~
                        $*PACKAGE.HOW.name($*PACKAGE));
                }
            }
        }
        elsif $twigil eq '.' && $*IN_DECL ne 'variable' {
            # Need to transform this to a method call.
            $past := $<arglist> ?? $<arglist>[0].ast !! PAST::Op.new();
            $past.pasttype('callmethod');
            $past.name($desigilname);
            $past.unshift(PAST::Var.new( :name('self'), :scope('lexical_6model') ));
        }
        elsif $twigil eq '^' || $twigil eq ':' {
            $past := add_placeholder_parameter($/, $sigil, $desigilname, :named($twigil eq ':'));
        }
        elsif $name eq '@_' {
            unless $*ST.nearest_signatured_block_declares('@_') {
                $past := add_placeholder_parameter($/, '@', '_', :pos_slurpy(1));
            }
        }
        elsif $name eq '%_' {
            unless $*ST.nearest_signatured_block_declares('%_') || $*METHODTYPE {
                $past := add_placeholder_parameter($/, '%', '_', :named_slurpy(1));
            }
        }
        elsif +@name > 1 {
            $past := $*ST.symbol_lookup(@name, $/, :lvalue(1));
        }
        elsif (my $attr_alias := $*ST.is_attr_alias($past.name)) {
            $past.name($attr_alias);
            $past.scope('attribute_6model');
            $past.unshift(instantiated_type(['$?CLASS'], $/));
            $past.unshift(PAST::Var.new( :name('self'), :scope('lexical_6model') ));
        }
        elsif $*IN_DECL ne 'variable' {
            # Expect variable to have been declared somewhere.
            # Locate descriptor and thus type.
            try {
                my $cd := $*ST.find_lexical_container_descriptor($past.name);
                $past.scope('lexical_6model');
                $past.type($cd.of);
                $past := box_native_if_needed($past, $cd.of);
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
        $*ST.apply_trait('&trait_mod:<trusts>', $*PACKAGE, $<typename>.ast);
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
            make PAST::Stmts.new( $block, $*ST.get_object_sc_ref_past($*PACKAGE) );
            return 1;
        }

        # Handle parametricism for roles.
        if $*PKGDECL eq 'role' {
            # Set up signature. Needs to have $?CLASS as an implicit
            # parameter, since any mention of it is generic.
            my @params := $<signature> ?? $<signature>[0].ast !! [];
            @params.unshift(hash(
                is_multi_invocant => 1,
                type_captures     => ['$?CLASS', '::?CLASS']
            ));
            set_default_parameter_type(@params, 'Mu');
            my $sig := create_signature_object(@params, $block);
            add_signature_binding_code($block, $sig, @params);
            $block.blocktype('declaration');

            # Need to ensure we get lexical outers fixed up
            # properly.
            $block.push($*ST.create_lexical_capture_fixup());

            # As its last act, it should grab the current lexpad so that
            # we have the type environment.
            $block.push(PAST::Op.new(
                :pirop('set PQPS'),
                PAST::Op.new( :pirop('getinterp P') ),
                'lexpad'));

            # Create code object and add it as the role's body block.
            my $code := $*ST.create_code_object($block, 'Block', $sig);
            $*ST.pkg_set_role_body_block($*PACKAGE, $code, $block);
        }

        # Compose.
        $*ST.pkg_compose($*PACKAGE);

        # Document
        document($*PACKAGE, $*DOC);

        make PAST::Stmts.new(
            $block, $*ST.get_object_sc_ref_past($*PACKAGE)
        );
    }

    method scope_declarator:sym<my>($/)      { make $<scoped>.ast; }
    method scope_declarator:sym<our>($/)     { make $<scoped>.ast; }
    method scope_declarator:sym<has>($/)     { make $<scoped>.ast; }
    method scope_declarator:sym<anon>($/)    { make $<scoped>.ast; }
    method scope_declarator:sym<augment>($/) { make $<scoped>.ast; }
    method scope_declarator:sym<state>($/)   { make $<scoped>.ast; }

    method declarator($/) {
        if    $<variable_declarator> { make $<variable_declarator>.ast }
        elsif $<routine_declarator>  { make $<routine_declarator>.ast  }
        elsif $<regex_declarator>    { make $<regex_declarator>.ast    }
        elsif $<signature> {
            # Go over the params and declare the variable defined
            # in them.
            my $list   := PAST::Op.new( :pasttype('call'), :name('&infix:<,>') );
            my @params := $<signature>.ast;
            for @params {
                if $_<variable_name> {
                    my $past := PAST::Var.new( :name($_<variable_name>) );
                    $past := declare_variable($/, $past, $_<sigil>, $_<twigil>,
                        $_<desigilname>, []);
                    unless $past.isa(PAST::Op) && $past.pasttype eq 'null' {
                        $list.push($past);
                    }
                }
                else {
                    $list.push($*ST.build_container_past(
                        sigiltype($_<sigil> || '$'),
                        $*ST.create_container_descriptor(
                            $*ST.find_symbol(['Mu']), 1, 'anon')));
                }
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
                    type => $type,
                    package => $*ST.find_symbol(['$?CLASS'])),
                sigiltype($sigil), $descriptor, |@default);

            # Document it
            document($attr, $*DOC);

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
        elsif $*SCOPE eq 'my' || $*SCOPE eq 'state' {
            # Create a container descriptor. Default to rw and set a
            # type if we have one; a trait may twiddle with that later.
            my $descriptor := $*ST.create_container_descriptor(
                $*TYPENAME ?? $*TYPENAME.ast !! $*ST.find_symbol(['Mu']),
                1, $name);

            # Install the container. Scalars default to Any if untyped.
            if $sigil eq '$' || $sigil eq '&' {
                $*ST.install_lexical_container($BLOCK, $name, sigiltype($sigil), $descriptor,
                    $*TYPENAME ?? $*TYPENAME.ast !! $*ST.find_symbol(['Any']),
                    :state($*SCOPE eq 'state'));
            }
            else {
                $*ST.install_lexical_container($BLOCK, $name, sigiltype($sigil), $descriptor,
                    :state($*SCOPE eq 'state'));
            }

            # Set scope and type on container.
            if $past.isa(PAST::Var) {
                $past.scope('lexical_6model');
                $past.type($descriptor.of);
                $past := box_native_if_needed($past, $descriptor.of);
            }
        }
        elsif $*SCOPE eq 'our' {
            if $*TYPENAME {
                $/.CURSOR.panic("Cannot put a type constraint on an 'our'-scoped variable");
            }
            $BLOCK[0].push(PAST::Var.new(
                :name($name), :scope('lexical'), :isdecl(1),
                :viviself($*ST.symbol_lookup([$name], $/, :package_only(1), :lvalue(1)))));
            $BLOCK.symbol($name, :scope('lexical'));
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
                $block[1] := wrap_return_handler($block[1]);
                # $block.control('return_pir');
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
        add_signature_binding_code($block, $signature, @params);

        # If it's a multi, needs a slot that can hold an (unvivified)
        # dispatcher.
        if $*MULTINESS eq 'multi' {
            $*ST.install_lexical_symbol($block, '$*DISPATCHER', $*ST.find_symbol(['MultiDispatcher']));
            $block[0].unshift(PAST::Op.new(:pirop('perl6_take_dispatcher v')));
        }

        # Create code object.
        if $<deflongname> {
            $block.name(~$<deflongname>[0].ast);
            $block.nsentry('');
        }
        my $code := $*ST.create_code_object($block, 'Sub', $signature,
            $*MULTINESS eq 'proto');

        # Document it
        document($code, $*DOC);

        # Install PAST block so that it gets capture_lex'd correctly and also
        # install it in the lexpad.
        my $outer := $*ST.cur_lexpad();
        $outer[0].push(PAST::Stmt.new($block));

        # Install &?ROUTINE.
        $*ST.install_lexical_symbol($block, '&?ROUTINE', $code);

        my $past;
        if $<deflongname> {
            # If it's a multi, need to associate it with the surrounding
            # proto.
            # XXX Also need to auto-multi things with a proto in scope.
            my $name := '&' ~ ~$<deflongname>[0].ast;
            if $*MULTINESS eq 'multi' {
                # Do we have a proto in the current scope?
                my $proto;
                if $outer.symbol($name) {
                    $proto := $outer.symbol($name)<value>;
                }
                else {
                    # None; search outer scopes.
                    my $new_proto;
                    try {
                        $proto := $*ST.find_symbol([$name]);
                    }
                    if $proto {
                        # Found in outer scope. Need to derive.
                        $new_proto := $*ST.derive_dispatcher($proto);
                    }
                    else {
                        # Generate a proto foo(|$) { * }
                        my $p_past := PAST::Block.new(
                            :name($block.name), :nsentry(''),
                            PAST::Stmts.new(),
                            PAST::Op.new( :pirop('perl6_enter_multi_dispatch_from_onlystar_block P') ));
                        $outer[0].push(PAST::Stmt.new($p_past));
                        my @p_params := [hash(is_capture => 1, nominal_type => $*ST.find_symbol(['Mu']) )];
                        my $p_sig := $*ST.create_signature([$*ST.create_parameter(@p_params[0])]);
                        add_signature_binding_code($p_past, $p_sig, @p_params);
                        $new_proto := $*ST.create_code_object($p_past, 'Sub', $p_sig, 1);
                    }

                    # Install in current scope.
                    $*ST.install_lexical_symbol($outer, $name, $new_proto);
                    $proto := $new_proto;
                }

                # Ensure it's actually a dispatcher.
                unless $proto.is_dispatcher {
                    $/.CURSOR.panic("Cannot declare a multi when an only is in scope");
                }

                # Install the candidate.
                $*ST.add_dispatchee_to_proto($proto, $code);
            }
            else {
                # Install.
                if $outer.symbol($name) {
                    $/.CURSOR.panic("Illegal redeclaration of routine '" ~
                        ~$<deflongname>[0].ast ~ "'");
                }
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
                        PAST::Var.new( :name($name), :scope('lexical_6model') )
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

        # Apply traits.
        for $<trait> {
            if $_.ast { ($_.ast)($code) }
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
                $past[1] := wrap_return_handler($past[1]);
                # $past.control('return_pir');
            }
        }
        $past.name($<longname> ?? $<longname>.Str !! '<anon>');
        $past.nsentry('');

        # Do the various tasks to trun the block into a method code object.
        my @params    := $<multisig> ?? $<multisig>[0].ast !! [];
        my $inv_type  := $*ST.find_symbol([
            $<longname> && $*ST.is_lexical('$?CLASS') ?? '$?CLASS' !! 'Mu']);
        my $code_type := $*METHODTYPE eq 'submethod' ?? 'Submethod' !! 'Method';
        my $code := methodize_block($/, $past, @params, $inv_type, $code_type);

        # Document it
        document($code, $*DOC);

        # Install &?ROUTINE.
        $*ST.install_lexical_symbol($past, '&?ROUTINE', $code);

        # Install PAST block so that it gets capture_lex'd correctly.
        my $outer := $*ST.cur_lexpad();
        $outer[0].push($past);

        # Install method.
        if $<longname> {
            install_method($/, $<longname>.Str, $*SCOPE, $code, $outer,
                :private($<specials> && ~$<specials> eq '!'));
        }
        elsif $*MULTINESS {
            $/.CURSOR.panic('Cannot put ' ~ $*MULTINESS ~ ' on anonymous method');
        }

        # Apply traits.
        for $<trait> {
            if $_.ast { ($_.ast)($code) }
        }

        my $closure := block_closure(reference_to_code_object($code, $past));
        $closure<sink_past> := PAST::Op.new( :pasttype('null') );
        make $closure;
    }

    sub methodize_block($/, $past, @params, $invocant_type, $code_type) {
        # Get signature and ensure it has an invocant and *%_.
        if $past<placeholder_sig> {
            $/.CURSOR.panic('Placeholder variables cannot be used in a method');
        }
        unless @params[0]<is_invocant> {
            @params.unshift(hash(
                nominal_type => $invocant_type,
                is_invocant => 1,
                is_multi_invocant => 1
            ));
        }
        unless @params[+@params - 1]<named_slurpy> {
            @params.push(hash(
                variable_name => '%_',
                nominal_type => $*ST.find_symbol(['Mu']),
                named_slurpy => 1,
                is_multi_invocant => 1,
                is_method_named_slurpy => 1
            ));
            $past[0].unshift(PAST::Var.new( :name('%_'), :scope('lexical_6model'), :isdecl(1) ));
            $past.symbol('%_', :scope('lexical_6model'));
        }
        set_default_parameter_type(@params, 'Any');
        my $signature := create_signature_object(@params, $past);
        add_signature_binding_code($past, $signature, @params);

        # Place to store invocant.
        $past[0].unshift(PAST::Var.new( :name('self'), :scope('lexical_6model'), :isdecl(1) ));
        $past.symbol('self', :scope('lexical_6model'));

        # Needs a slot to hold a multi or method dispatcher.
        $*ST.install_lexical_symbol($past, '$*DISPATCHER',
            $*ST.find_symbol([$*MULTINESS eq 'multi' ?? 'MultiDispatcher' !! 'MethodDispatcher']));
        $past[0].unshift(PAST::Op.new(:pirop('perl6_take_dispatcher v')));

        # Create code object.
        return $*ST.create_code_object($past, $code_type, $signature,
            $*MULTINESS eq 'proto');
    }

    # Installs a method into the various places it needs to go.
    sub install_method($/, $name, $scope, $code, $outer, :$private) {
        # Ensure that current package supports methods, and if so
        # add the method.
        my $meta_meth;
        if $private {
            if $*MULTINESS { $/.CURSOR.panic("Private multi-methods are not supported"); }
            $meta_meth := 'add_private_method';
        }
        else {
            $meta_meth := $*MULTINESS eq 'multi' ?? 'add_multi_method' !! 'add_method';
        }
        if $scope ne 'anon' && pir::can($*PACKAGE.HOW, $meta_meth) {
            $*ST.pkg_add_method($*PACKAGE, $meta_meth, $name, $code);
        }
        elsif $scope eq '' || $scope eq 'has' {
            my $nocando := $*MULTINESS eq 'multi' ?? 'multi-method' !! 'method';
            pir::printerr__vS("Useless declaration of a has-scoped $nocando in " ~
                ($*PKGDECL || "mainline") ~ "\n");
        }

        # May also need it in lexpad and/or package.
        if $*SCOPE eq 'my' {
            $*ST.install_lexical_symbol($outer, '&' ~ $name, $code);
        }
        elsif $*SCOPE eq 'our' {
            $*ST.install_lexical_symbol($outer, '&' ~ $name, $code);
            $*ST.install_package_symbol($*PACKAGE, '&' ~ $name, $code);
        }
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

    method regex_declarator:sym<regex>($/, $key?) {
        make $<regex_def>.ast;
    }

    method regex_declarator:sym<token>($/, $key?) {
        make $<regex_def>.ast;
    }

    method regex_declarator:sym<rule>($/, $key?) {
        make $<regex_def>.ast;
    }

    method regex_def($/) {
        my $coderef;
        my $name := ~$<deflongname>[0];

        if $*MULTINESS eq 'proto' {
            $/.CURSOR.panic('protoregexes not yet implemented');
        } else {
            my @params := $<signature> ?? $<signature>.ast !! [];
            $coderef := regex_coderef($/, $<p6regex>.ast, $*SCOPE, $name, @params, $*CURPAD);
        }

        # Apply traits.
        my $code := $coderef<code_object>;
        for $<trait> {
            if $_.ast { ($_.ast)($code) }
        }

        # Return closure if not in sink context.
        my $closure := block_closure($coderef);
        $closure<sink_past> := PAST::Op.new( :pasttype('null') );
        make $closure;
    }

    sub regex_coderef($/, $qast, $scope, $name, @params, $block) {
        # create a code reference from a regex qast tree
        my $past := QRegex::P6Regex::Actions::buildsub($qast, $block);
        $past.name($name);
        $past.blocktype("declaration");

        # Do the various tasks to turn the block into a method code object.
        my $inv_type  := $*ST.find_symbol([ # XXX Maybe Cursor below, not Mu...
            $name && $*ST.is_lexical('$?CLASS') ?? '$?CLASS' !! 'Mu']);
        my $code := methodize_block($/, $past, @params, $inv_type, 'Regex');

        # Need to put self into a register for the regex engine.
        $past[0].push(PAST::Var.new(
            :name('self'), :scope('register'), :isdecl(1),
            :viviself(PAST::Var.new( :name('self'), :scope('lexical_6model') ))));

        # Install PAST block so that it gets capture_lex'd correctly.
        my $outer := $*ST.cur_lexpad();
        $outer[0].push($past);

        # Install in needed scopes.
        install_method($/, $name, $scope, $code, $outer);

        # Return a reference to the code object
        reference_to_code_object($code, $past);
    }

    method type_declarator:sym<enum>($/) {
        # If it's an anonymous enum, just call anonymous enum former
        # and we're done.
        unless $<longname> || $<variable> {
            make PAST::Op.new( :name('&ANON_ENUM'), $<term>.ast );
            return 1;
        }

        # Get, or find, enumeration base type and create type object with
        # correct base type.
        my $base_type := $*TYPENAME ?? $*TYPENAME.ast !! $*ST.find_symbol(['Int']);
        my $name      := $<longname> ?? ~$<longname> !! $<variable><desigilname>;
        my $type_obj  := $*ST.pkg_create_mo(%*HOW<enum>, :name($name), :base_type($base_type));

        # Add roles (which will provide the enum-related methods).
        $*ST.apply_trait('&trait_mod:<does>', $type_obj, $*ST.find_symbol(['Enumeration']));
        if pir::type_check__IPP($type_obj, $*ST.find_symbol(['Numeric'])) {
            $*ST.apply_trait('&trait_mod:<does>', $type_obj, $*ST.find_symbol(['NumericEnumeration']));
        }

        # Apply traits, compose and install package.
        for $<trait> {
            ($_.ast)($type_obj) if $_.ast;
        }
        $*ST.pkg_compose($type_obj);
        if $<variable> { $/.CURSOR.panic("Variable case of enums not yet implemented"); }
        $*ST.install_package($/, $<longname>, ($*SCOPE || 'our'),
            'enum', $*PACKAGE, $*ST.cur_lexpad(), $type_obj);

        # Get list of either values or pairs; fail if we can't.
        my @values;
        my $term_ast := $<term>.ast;
        if $term_ast.isa(PAST::Stmts) && +@($term_ast) == 1 {
            $term_ast := $term_ast[0];
        }
        if $term_ast.isa(PAST::Op) && $term_ast.name eq '&infix:<,>' {
            for @($term_ast) {
                if $_.returns() eq 'Pair' && $_[1]<has_compile_time_value> && $_[2]<has_compile_time_value> {
                    @values.push($_);
                }
                elsif $_<has_compile_time_value> {
                    @values.push($_);
                }
                else {
                    $<term>.CURSOR.panic("Enumeration values must be known at compile time");
                }
            }
        }
        elsif $term_ast<has_compile_time_value> {
            @values.push($term_ast);
        }
        elsif $term_ast.returns() eq 'Pair' && $term_ast[1]<has_compile_time_value> && $term_ast[2]<has_compile_time_value> {
            @values.push($term_ast);
        }
        else {
            $<term>.CURSOR.panic("Enumeration values must be known at compile time");
        }

        # Now we have them, we can go about computing the value
        # for each of the keys, unless they have them supplied.
        # XXX Should not assume integers, and should use lexically
        # scoped &postfix:<++> or so.
        my $cur_value := 0;
        for @values {
            # If it's a pair, take that as the value; also find
            # key.
            my $cur_key;
            if $_.returns() eq 'Pair' {
                $cur_key   := $_[1]<compile_time_value>;
                $cur_value := nqp::unbox_i($_[2]<compile_time_value>);
            }
            else {
                $cur_key := $_<compile_time_value>;
            }

            # Create and install value.
            my $val_obj := $*ST.create_enum_value($type_obj, $cur_key, $cur_value);
            $*ST.install_package_symbol($type_obj, ~$cur_key, $val_obj);
            if $*SCOPE ne 'anon' {
                $*ST.install_lexical_symbol($*ST.cur_lexpad(), ~$cur_key, $val_obj);
            }
            if $*SCOPE eq '' || $*SCOPE eq 'our' {
                $*ST.install_package_symbol($*PACKAGE, ~$cur_key, $val_obj);
            }

            # Increment for next value.
            $cur_value := $cur_value + 1;
        }

        # We evaluate to the enum type object.
        make $*ST.get_object_sc_ref_past($type_obj);
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
        # We make an empty PAST node, but attach a callback to it
        # that we'll have made when we see the =.
        my $past := PAST::Op.new( :pasttype('null') );
        $past<constant_declarator> := -> $rhs_ast {
            # Get constant value.
            my $value;
            if $rhs_ast<has_compile_time_value> {
                $value := $rhs_ast<compile_time_value>;
            }
            else {
                $/.CURSOR.panic("Cannot handle constant with non-literal value yet");
            }

            # Get name to install it as.
            my $name;
            if $<identifier> {
                $name := ~$<identifier>;
            }
            elsif $<variable> {
                # Don't handle twigil'd case yet.
                if $<variable><twigil> {
                    $/.CURSOR.panic("Twigil-variable constants not yet implemented");
                }
                $name := ~$<variable>;
            }
            else {
                # Nothing to install, just return a PAST node to
                # get hold of the constant.
                return $*ST.get_object_sc_ref_past($value);
            }

            # Install.
            if $*SCOPE eq '' || $*SCOPE eq 'our' {
                $*ST.install_lexical_symbol($*ST.cur_lexpad(), $name, $value);
                $*ST.install_package_symbol($*PACKAGE, $name, $value);
            }
            elsif $*SCOPE eq 'my' {
                $*ST.install_lexical_symbol($*ST.cur_lexpad(), $name, $value);
            }
            else {
                $/.CURSOR.panic("$*SCOPE scoped constants are not yet implemented");
            }

            # Evaluate to the constant.
            return $*ST.get_object_sc_ref_past($value);
        };
        make $past;
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

        # Mark current block as having a signature.
        $*ST.mark_cur_lexpad_signatured();

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
                %*PARAM_INFO<desigilname> := ~$<name>[0];
            }
            %*PARAM_INFO<sigil> := my $sigil := ~$<sigil>;

            # Depending on sigil, use appropriate role.
            my $need_role;
            my $role_type;
            if $sigil eq '@' {
                $role_type := $*ST.find_symbol(['Positional']);
                $need_role := 1;
            }
            elsif $sigil eq '%' {
                $role_type := $*ST.find_symbol(['Associative']);
                $need_role := 1;
            }
            elsif $sigil eq '&' {
                $role_type := $*ST.find_symbol(['Callable']);
                $need_role := 1;
            }
            if $need_role {
                if pir::exists(%*PARAM_INFO, 'nominal_type') {
                    $/.CURSOR.panic("Typed arrays/hashes/callables not yet implemented");
                }
                else {
                    %*PARAM_INFO<nominal_type> := $role_type;
                }
            }

            # Handle twigil.
            my $twigil := $<twigil> ?? ~$<twigil>[0] !! '';
            %*PARAM_INFO<twigil> := $twigil;
            if $twigil eq '' || $twigil eq '*' {
                # Need to add the name.
                if $<name> {
                    my $cur_pad := $*ST.cur_lexpad();
                    if $cur_pad.symbol(~$/) {
                        $/.CURSOR.panic("Redeclaration of symbol ", ~$/);
                    }
                    $cur_pad[0].push(PAST::Var.new( :name(~$/), :scope('lexical_6model'), :isdecl(1) ));
                    $cur_pad.symbol(~$/, :scope('lexical_6model'));
                }
            }
            elsif $twigil eq '!' {
                # XXX Can compile-time check name when we have attributes. :-)
                %*PARAM_INFO<bind_attr> := 1;
                %*PARAM_INFO<attr_package> := $*PACKAGE;
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
        make make_thunk($<EXPR>.ast, $/);
    }

    method type_constraint($/) {
        if $<typename> {
            if pir::substr(~$<typename>, 0, 2) eq '::' && pir::substr(~$<typename>, 2, 1) ne '?' {
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
                for ($<typename><longname> ?? $<typename><longname><colonpair> !! $<typename><colonpair>) {
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
            my $ast := $<value>.ast;
            unless $ast<has_compile_time_value> {
                $/.CURSOR.panic('Cannot use a value type constraints whose value is unknown at compile time');
            }
            my $val := $ast<compile_time_value>;
            %*PARAM_INFO<nominal_type> := $val.WHAT;
            unless %*PARAM_INFO<post_constraints> {
                %*PARAM_INFO<post_constraints> := [];
            }
            %*PARAM_INFO<post_constraints>.push(make_where_block(
                $*ST.get_object_sc_ref_past($val)));
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
            if $*ST.is_name(@name) {
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
        $past.unshift($*ST.add_constant('Str', 'str', $past.name));
        $past.name('dispatch:<' ~ ~$<sym> ~ '>');
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
        # Compiling private method calls is somewhat interesting. If it's
        # in any way qualified, we need to ensure that the current package
        # is trusted by the target class. Otherwise we assume that the call
        # is to a private method in the current (non-virtual) package.
        # XXX Optimize the case where the method is declared up front - but
        # maybe this is for the optimizer, not for here.
        # XXX Attribute accesses? Again, maybe for the optimizer, since it
        # runs after CHECK time.
        my $past := $<methodop>.ast;
        if $<methodop><longname> {
            my @parts   := Perl6::Grammar::parse_name(~$<methodop><longname>);
            my $name    := @parts.pop;
            if @parts {
                my $methpkg := $*ST.find_symbol(@parts);
                unless $methpkg.HOW.is_trusted($methpkg, $*PACKAGE) {
                    $/.CURSOR.panic("Cannot call private method '$name' on package " ~
                        $methpkg.HOW.name($methpkg) ~ " because it does not trust " ~
                        $*PACKAGE.HOW.name($*PACKAGE));
                }
            }
            else {
                $past.unshift($*ST.get_object_sc_ref_past($*PACKAGE));
                $past.unshift($*ST.add_constant('Str', 'str', $name));
            }
            $past.name('dispatch:<!>');
        }
        elsif $<methodop><quote> {
            $past.unshift($*ST.get_object_sc_ref_past($*PACKAGE));
            $past.unshift($<methodop><quote>.ast);
            $past.name('dispatch:<!>');
        }
        else {
            $/.CURSOR.panic("Cannot use this form of method call with a private method");
        }
        make $past;
    }

    method methodop($/) {
        my $past := $<args> ?? $<args>.ast !! PAST::Op.new( :node($/) );
        $past.pasttype('callmethod');
        if $<longname> {
            # May just be .foo, but could also be .Foo::bar. Also handle the
            # macro-ish cases.
            my @parts := Perl6::Grammar::parse_name(~$<longname>);
            my $name := @parts.pop;
            if +@parts {
                $past.unshift($*ST.symbol_lookup(@parts, $/));
                $past.unshift($*ST.add_constant('Str', 'str', $name));
                $past.name('dispatch:<::>');
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
            $past.name('dispatch:<var>');
        }
        make $past;
    }

    ## temporary Bool::True/False generation
    method term:sym<boolean>($/) {
        make PAST::Op.new(:pirop<perl6_booleanize__Pi>, $<value> eq 'True');
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
        $past.unshift(PAST::Var.new( :name('$_'), :scope('lexical_6model') ) );
        make $past;
    }

    method term:sym<identifier>($/) {
        my $past := capture_or_parcel($<args>.ast, ~$<identifier>);
        $past.name('&' ~ $<identifier>);
        make $past;
    }

    method is_indirect_lookup($longname) {
        for $longname<name><morename> {
            if $_<EXPR> {
                return 1;
            }
        }
        0;
    }

    method make_indirect_lookup($longname, $sigil?) {
        my $past := PAST::Op.new(
            :pasttype<call>,
            :name<&INDIRECT_NAME_LOOKUP>,
        );
        $past.push($*ST.add_constant('Str', 'str', $sigil)) if $sigil;
        $past.push($*ST.add_constant('Str', 'str', ~$longname<name><identifier>))
            if $longname<name><identifier>;

        for $longname<name><morename> {
            if $_<EXPR> {
                $past.push($_<EXPR>[0].ast);
            } else {
                $past.push($*ST.add_constant('Str', 'str', ~$_<identifier>));
            }
        }
        $past;
    }

    method term:sym<name>($/) {
        my $past;

        if self.is_indirect_lookup($<longname>) {
            if $<args> {
                $/.CURSOR.panic("Combination of indirect name lookup and call not (yet?) allowed");
            }
            $past := self.make_indirect_lookup($<longname>)

        } elsif $<args> {
            # If we have args, it's a call. Look it up dynamically
            # and make the call.
            # Add & to name.
            my @name := Perl6::Grammar::parse_name(~$<longname>);
            my $final := @name[+@name - 1];
            if pir::substr($final, 0, 1) ne '&' {
                @name[+@name - 1] := '&' ~ $final;
            }
            $past := capture_or_parcel($<args>.ast, ~$<longname>);
            if +@name == 1 {
                $past.name(@name[0]);
            }
            else {
                $past.unshift($*ST.symbol_lookup(@name, $/));
            }
        }
        else {
            # Otherwise, it's a type name; build a reference to that
            # type, since we can statically resolve them.
            my @name := Perl6::Grammar::parse_name(~$<longname>);
            if $<arglist> {
                $/.CURSOR.panic("Parametric roles not yet implemented");
            }
            if ~$<longname> eq 'GLOBAL' {
                $past := $*ST.symbol_lookup(@name, $/);
            }
            else {
                $past := instantiated_type(@name, $/);
            }
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
            PAST::Var.new( :name('Whatever'), :scope('lexical_6model') )
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

    method semiarglist($/) {
        if +$<arglist> == 1 {
            make $<arglist>[0].ast;
        }
        else {
            my $past := PAST::Op.new( :pasttype('call'), :node($/) );
            for $<arglist> {
                my $ast := $_.ast;
                $ast.name('&infix:<,>');
                $past.push($ast);
            }
            make $past;
        }
    }

    method arglist($/) {
        # Build up argument list, hanlding nameds and flattens
        # as we go.
        my $past := PAST::Op.new( :pasttype('call'), :node($/) );
        if $<EXPR> {
            my $expr := $<EXPR>.ast;
            if $expr.name eq '&infix:<,>' {
                for $expr.list { $past.push(handle_parameter($_, $/)); }
            }
            else { $past.push(handle_parameter($expr, $/)); }
        }

        make $past;
    }

    sub handle_parameter($arg, $/) {
        if $arg ~~ PAST::Op && $arg.returns eq 'Pair' {
            my $result := $arg[2];
            $result.named(compile_time_value_str($arg[1], 'LHS of pair', $/));
            $result<before_promotion> := $arg;
            $result;
        }
        elsif $arg ~~ PAST::Op && $arg.name eq '&prefix:<|>' {
            PAST::Op.new(
                :pasttype('callmethod'), :name('ARGLIST_FLATTENABLE'),
                :flat(1), $arg[0]);
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
        if $is_hash && $past<past_block>.arity == 0 {
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
            $past<sink_past> := PAST::Op.new(
                :pasttype('call'),
                PAST::Val.new( :value($past<past_block>) ));
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
        elsif $sym eq '=' {
            make assign_op($/);
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
        if $past.pasttype eq 'xor_nqp' {
            $past.push(PAST::Var.new(:named<false>, :scope<lexical_6model>, :name<Nil>));
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
            PAST::Var.new( :name('$_'), :scope('lexical_6model') )
        );
        if $negated {
            $sm_call := PAST::Op.new( :name('&prefix:<!>'), $sm_call );
        }
        PAST::Stmt.new(PAST::Op.new(
            :pasttype('stmts'),

            # Stash original $_.
            PAST::Op.new( :pasttype('bind_6model'),
                PAST::Var.new( :name($old_topic_var), :scope('register'), :isdecl(1) ),
                PAST::Var.new( :name('$_'), :scope('lexical_6model') )
            ),

            # Evaluate LHS and bind it to $_.
            PAST::Op.new( :pasttype('bind_6model'),
                PAST::Var.new( :name('$_'), :scope('lexical_6model') ),
                $lhs
            ),

            # Evaluate RHS and call ACCEPTS on it, passing in $_. Bind the
            # return value to a result variable.
            PAST::Op.new( :pasttype('bind_6model'),
                PAST::Var.new( :name($result_var), :scope('lexical_6model'), :isdecl(1) ),
                $sm_call
            ),

            # Re-instate original $_.
            PAST::Op.new( :pasttype('bind_6model'),
                PAST::Var.new( :name('$_'), :scope('lexical_6model') ),
                PAST::Var.new( :name($old_topic_var), :scope('register') )
            ),

            # And finally evaluate to the smart-match result.
            PAST::Var.new( :name($result_var), :scope('lexical_6model') )
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
                # Source needs type check.
                my $meta_attr;
                try {
                    $meta_attr := $*PACKAGE.HOW.get_attribute_for_usage(
                        $*PACKAGE, $target.name
                    );
                }
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
        elsif $target<boxable_native> {
            $/.CURSOR.panic("Cannot bind to a natively typed variable; use assignment instead");
        }
        else {
            $/.CURSOR.panic("Cannot use bind operator with this LHS");
        }
    }

    sub assign_op($/) {
        my $past;
        my $lhs_ast := $/[0].ast;
        my $rhs_ast := $/[1].ast;
        if $lhs_ast && $lhs_ast<attribute_declarand> {
            Perl6::Actions.install_attr_init($/);
            $past := PAST::Stmts.new();
        }
        elsif $lhs_ast<constant_declarator> {
            $past := $lhs_ast<constant_declarator>($rhs_ast);
        }
        elsif $lhs_ast && $lhs_ast<boxable_native> {
            # Native assignment is actually really a bind at low
            # level. We grab the thing we want out of the PAST::Want
            # node.
            $past := PAST::Op.new(:pasttype('bind_6model'), $lhs_ast[2], $rhs_ast);
        }
        else {
            $past := PAST::Op.new(:pirop('perl6_container_store__0PP'),
                $lhs_ast, $rhs_ast);
        }
        return $past;
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
            my $base     := $<infix>;
            my $basesym  := ~$base<sym>;
            my $basepast := $base.ast
                              ?? $base.ast[0]
                              !! PAST::Var.new(:name("&infix:<$basesym>"),
                                               :scope<lexical_6model>);
            make PAST::Op.new( :node($/),
                     PAST::Op.new( :pasttype<call>,
                         :name<&METAOP_ASSIGN>, $basepast ));
        }

        if $<infix_prefix_meta_operator> {
            my $metasym  := ~$<infix_prefix_meta_operator><sym>;
            my $base     := $<infix_prefix_meta_operator><infixish>;
            my $basesym  := ~$base<OPER>;
            my $basepast := $base.ast
                              ?? $base.ast[0]
                              !! PAST::Var.new(:name("&infix:<$basesym>"),
                                               :scope<lexical_6model>);
            my $helper   := '';
            if    $metasym eq '!' { $helper := '&METAOP_NEGATE'; }
            if    $metasym eq 'R' { $helper := '&METAOP_REVERSE'; }
            elsif $metasym eq 'X' { $helper := '&METAOP_CROSS'; }
            elsif $metasym eq 'Z' { $helper := '&METAOP_ZIP'; }

            make PAST::Op.new( :node($/),
                     PAST::Op.new( :pasttype<call>,
                         :name($helper), $basepast ));
        }

        if $<infixish> {
            make $<infixish>.ast;
        }
    }

    method prefix_circumfix_meta_operator:sym<reduce>($/) {
        my $base     := $<op>;
        my $basepast := $base.ast
                          ?? $base.ast[0]
                          !! PAST::Var.new(:name("&infix:<" ~ $base<OPER><sym> ~ ">"),
                                           :scope<lexical_6model>);
        my $metaop   := '&METAOP_REDUCE';
        if $base<OPER><O><assoc> eq 'right'     { $metaop := '&METAOP_REDUCE_RIGHT' }
        elsif $base<OPER><O><prec> eq 'm='      { $metaop := '&METAOP_REDUCE_CHAIN' }
        elsif $base<OPER><O><pasttype> eq 'xor' { $metaop := '&METAOP_REDUCE_XOR' }
        my $metapast := PAST::Op.new( :pasttype<call>, :name($metaop), $basepast);
        if $<triangle> {
            my $tri := $*ST.add_constant('Int', 'int', 1);
            $tri.named('triangle');
            $metapast.push($tri);
        }
        make PAST::Op.new(:node($/), :pasttype<call>, $metapast);
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
        my $re := $*ST.add_constant('Num', 'num', 0e0);
        my $im := $*ST.add_constant('Num', 'num', +~$<im>);
        make $*ST.add_constant('Complex', 'type_new', $re<compile_time_value>, $im<compile_time_value>);
    }

    method number:sym<numish>($/) {
        make $<numish>.ast;
    }

    method numish($/) {
        if $<integer> {
            make $*ST.add_numeric_constant('Int', $<integer>.ast);
        }
        elsif $<dec_number> { make $<dec_number>.ast; }
        elsif $<rad_number> { make $<rad_number>.ast; }
        else {
            make $*ST.add_numeric_constant('Num', +$/);
        }
    }

    # filter out underscores and similar stuff
    sub filter_number($n) {
        my $i := 0;
        my $allowed := '0123456789';
        my $result := '';
        while $i < nqp::chars($n) {
            my $char := nqp::substr($n, $i, 1);
            $result := $result ~ $char if pir::index($allowed, $char) >= 0;
            $i++;
        }
        $result;
    }

    method escale($/) {
        make $<sign> eq '-' ?? -$<decint>.ast !! $<decint>.ast;
    }

    method dec_number($/) {
#        pir::say("dec_number: $/");
        my $int  := $<int> ?? filter_number(~$<int>) !! "0";
        my $frac := $<frac> ?? filter_number(~$<frac>) !! "0";
        if $<escale> {
            my $e := pir::isa($<escale>, 'ResizablePMCArray') ?? $<escale>[0] !! $<escale>;
#            pir::say('dec_number exponent: ' ~ ~$e.ast);
            make radcalc(10, $<coeff>, 10, $e.ast);
        } else {
            make radcalc(10, $<coeff>);
        }
    }

    method rad_number($/) {
        my $radix    := +($<radix>.Str);
        if $<circumfix> {
            pir::die('NYI form of number litereal encountered');
            make PAST::Op.new(:name('&radcalc'), :pasttype('call'),
                $radix, $<circumfix>.ast);
        } else {
            my $intpart  := $<intpart>.Str;
            my $fracpart := $<fracpart> ?? $<fracpart>.Str !! "0";
            my $intfrac  := $intpart ~ $fracpart; #the dot is a part of $fracpart, so no need for ~ "." ~
            my $base     := $<base> ?? +($<base>[0].Str) !! 0;
            my $exp      := $<exp> ?? +($<exp>[0].Str) !! 0;

            make radcalc($radix, $intfrac, $base, $exp);
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
            make $*ST.find_symbol(['::?' ~ ~$<identifier>]);
        }
    }

    our %SUBST_ALLOWED_ADVERBS;
    our %SHARED_ALLOWED_ADVERBS;
    our %MATCH_ALLOWED_ADVERBS;
    INIT {
        my $mods := 'i ignorecase s sigspace r ratchet';
        for nqp::split(' ', $mods) {
            %SHARED_ALLOWED_ADVERBS{$_} := 1;
        }

        $mods := 'g global ii samecase x c continue p pos nth th st nd rd';
        for nqp::split(' ', $mods) {
            %SUBST_ALLOWED_ADVERBS{$_} := 1;
        }

        # TODO: add g global ov overlap  once they actually work
        $mods := 'x c continue p pos nth th st nd rd';
        for nqp::split(' ', $mods) {
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
                    PAST::Var.new(:name('$/'), :scope('lexical_6model')),
                    PAST::Op.new(:pasttype('callmethod'),
                        PAST::Var.new(:name('$/'), :scope<lexical_6model>),
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

    method rx_adverbs($/) {
        my @pairs;
        for $<quotepair> {
            nqp::push(@pairs, $_.ast);
        }
        make @pairs;
    }

    method setup_quotepair($/) {
        my %h;
        my $key := $*ADVERB.ast.named;
        my $value := $*ADVERB.ast;
        if $value ~~ PAST::Val {
            $value := $value.value;
        }
        elsif $value<has_compile_time_value> {
            $value := $value<compile_time_value>;
        }
        else {
            if %SHARED_ALLOWED_ADVERBS{$key} {
                $/.CURSOR.panic('Value of adverb :' ~ $key ~ ' must be known at compile time');
            }
        }
        %*RX{$key} := $value;
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
        make PAST::Op.new( :name('&QX'), :pasttype('call'),
            $<quote_EXPR>.ast
        );
    }
    method quote:sym<qqx>($/)  {
        make PAST::Op.new( :name('&QX'), :pasttype('call'),
            $<quote_EXPR>.ast
        );
    }
    method quote:sym</ />($/) {
        my $block := PAST::Block.new(PAST::Stmts.new, PAST::Stmts.new, :node($/));
        my $coderef := regex_coderef($/, $<p6regex>.ast, 'anon', '', [], $block);
        # Return closure if not in sink context.
        my $closure := block_closure($coderef);
        $closure<sink_past> := PAST::Op.new( :pasttype('null') );
        make $closure;
    }

    method quote:sym<rx>($/) {
        my $block := PAST::Block.new(PAST::Stmts.new, PAST::Stmts.new, :node($/));
        self.handle_and_check_adverbs($/, %SHARED_ALLOWED_ADVERBS, 'm', $block);
        my $coderef := regex_coderef($/, $<p6regex>.ast, 'anon', '', [], $block);
        make block_closure($coderef);
    }
    method quote:sym<m>($/) {
        my $block := PAST::Block.new(PAST::Stmts.new, PAST::Stmts.new, :node($/));
        my $coderef := regex_coderef($/, $<p6regex>.ast, 'anon', '', [], $block);

        my $past := PAST::Op.new(
            :node($/),
            :pasttype('callmethod'), :name('match'),
            PAST::Var.new( :name('$_'), :scope('lexical_6model') ),
            block_closure($coderef)
        );
        self.handle_and_check_adverbs($/, %MATCH_ALLOWED_ADVERBS, 'm', $past);
        make PAST::Op.new( :pasttype('bind_6model'),
            PAST::Var.new(:name('$/'), :scope('lexical_6model')),
            $past
        );
    }

    method handle_and_check_adverbs($/, %adverbs, $what, $past?) {
        for $<rx_adverbs>.ast {
            unless %SHARED_ALLOWED_ADVERBS{$_.named} || %adverbs{$_.named} {
                $/.CURSOR.panic("Adverb '" ~ $_.named ~ "' not allowed on " ~ $what);
            }
            if $past {
                $past.push($_);
            }
        }
    }

    method quote:sym<s>($/) {
        # Build the regex.

        my $rx_block := PAST::Block.new(PAST::Stmts.new, PAST::Stmts.new, :node($/));
        my $rx_coderef := regex_coderef($/, $<p6regex>.ast, 'anon', '', [], $rx_block);
#        my $regex :=  block_closure($rx_coderef);

        # Quote needs to be closure-i-fied.
        my $closure_ast := PAST::Block.new(
            PAST::Stmts.new(),
            PAST::Stmts.new(
                $<quote_EXPR> ?? $<quote_EXPR>.ast !! $<EXPR>.ast
            )
        );
        my $closure := block_closure($closure_ast);

        # make $_ = $_.subst(...)
        my $past := PAST::Op.new(
            :node($/),
            :pasttype('callmethod'), :name('subst'),
            PAST::Var.new( :name('$_'), :scope('lexical_6model') ),
            $rx_coderef, $closure
        );
        self.handle_and_check_adverbs($/, %SUBST_ALLOWED_ADVERBS, 'substitution', $past);
#        if $/[0] {
#            pir::push__vPP($past, PAST::Val.new(:named('samespace'), :value(1)));
#        }

        $past := PAST::Op.new(
            :node($/),
            :pasttype('call'),
            :name('&infix:<=>'),
            PAST::Var.new(:name('$_'), :scope('lexical_6model')),
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
    sub add_signature_binding_code($block, $sig_obj, @params) {
        # Set arity.
        my $arity := 0;
        for @params {
            last if $_<optional> || $_<named_names> ||
               $_<pos_slurpy> || $_<named_slurpy>;
            $arity := $arity + 1;
        }
        $block.arity($arity);

        # We tell Parrot that we'll have all args in the call_sig so it won't
        # do its own arg processing. We also add a call to bind the signature.
        $block[0].push(PAST::Var.new( :name('call_sig'), :scope('parameter'), :call_sig(1) ));
        $block[0].push(PAST::Op.new( :pirop('bind_signature vP') ));

        $block;
    }

    # Adds a placeholder parameter to this block's signature.
    sub add_placeholder_parameter($/, $sigil, $ident, :$named, :$pos_slurpy, :$named_slurpy) {
        # Obtain/create placeholder parameter list.
        my $block := $*ST.cur_lexpad();
        my @params := $block<placeholder_sig> || ($block<placeholder_sig> := []);

        # If we already declared this as a placeholder, we're done.
        my $name := ~$sigil ~ ~$ident;
        for @params {
            if $_<variable_name> eq $name {
                return PAST::Var.new( :name($name), :scope('lexical_6model') );
            }
        }

        # Make descriptor.
        my %param_info := hash(
            variable_name => $name,
            pos_slurpy    => $pos_slurpy,
            named_slurpy  => $named_slurpy);

        # If it's slurpy, just goes on the end.
        if $pos_slurpy || $named_slurpy {
            @params.push(%param_info);
        }

        # If it's named, just shove it on the end, but before any slurpies.
        elsif $named {
            %param_info<named_names> := [$ident];
            my @popped;
            while @params
                    && (@params[+@params - 1]<pos_slurpy> || @params[+@params - 1]<named_slurpy>) {
                @popped.push(@params.pop);
            }
            @params.push(%param_info);
            while @popped { @params.push(@popped.pop) }
        }

        # Otherwise, put it in correct lexicographic position.
        else {
            my @shifted;
            for @params {
                last if $_<pos_slurpy> || $_<named_slurpy> ||
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
        $block[0].push(PAST::Var.new( :name($name), :scope('lexical_6model'), :isdecl(1) ));
        $block.symbol($name, :scope('lexical_6model'), :placeholder_parameter(1));
        return PAST::Var.new( :name($name), :scope('lexical_6model') );
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
        $closure<past_block> := $code<past_block>;
        $closure<code_object> := $code<code_object>;
        return $closure;
    }

    sub make_thunk($to_thunk, $/) {
        my $block := $*ST.push_lexpad($/);
        $block.push($to_thunk);
        $*ST.pop_lexpad();
        make_simple_code_object($block, 'Code');
    }

    sub make_thunk_ref($to_thunk, $/) {
        my $block := $*ST.push_lexpad($/);
        $block.push($to_thunk);
        $*ST.pop_lexpad();
        reference_to_code_object(
            make_simple_code_object($block, 'Code'),
            $block);
    }

    sub make_simple_code_object($block, $type) {
        ($*ST.cur_lexpad())[0].push($block);
        my $sig := $*ST.create_signature([]);
        return $*ST.create_code_object($block, $type, $sig);
    }

    sub make_topic_block_ref($past) {
        my $block := PAST::Block.new(
            PAST::Stmts.new(
                PAST::Var.new( :name('$_'), :scope('lexical_6model'), :isdecl(1) )
            ),
            $past);
        ($*ST.cur_lexpad())[0].push($block);
        my $param := hash(:variable_name('$_'), :nominal_type($*ST.find_symbol(['Mu'])));
        my $sig := $*ST.create_signature([$*ST.create_parameter($param)]);
        add_signature_binding_code($block, $sig, [$param]);
        return reference_to_code_object(
            $*ST.create_code_object($block, 'Block', $sig),
            $block);
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
                PAST::Var.new( :name('$_'), :scope('lexical_6model'), :isdecl(1) )
            ),
            PAST::Stmts.new(
                PAST::Op.new(
                    :pasttype('callmethod'), :name('ACCEPTS'),
                    $expr,
                    PAST::Var.new( :name('$_'), :scope('lexical_6model') )
                )));
        ($*ST.cur_lexpad())[0].push($past);

        # Give it a signature and create code object.
        my $param := hash(
            variable_name => '$_',
            nominal_type => $*ST.find_symbol(['Mu']));
        my $sig := $*ST.create_signature([
            $*ST.create_parameter($param)]);
        add_signature_binding_code($past, $sig, [$param]);
        return $*ST.create_code_object($past, 'Block', $sig);
    }

    sub add_implicit_var($block, $name) {
        $block[0].push(PAST::Var.new( :name($name), :scope('lexical_6model'), :isdecl(1) ));
        $block.symbol($name, :scope('lexical_6model') );
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

    sub make_dot_equals($target, $call) {
        $call.unshift($*ST.add_constant('Str', 'str', $call.name));
        $call.unshift($target);
        $call.name('dispatch:<.=>');
        $call.pasttype('callmethod');
        $call;
    }

    # XXX This isn't quite right yet... need to evaluate these semantics
    sub push_block_handler($/, $block, $handler) {
        unless $block.handlers() {
            $block.handlers([]);
        }
        $handler := PAST::Block.new(
            :blocktype('declaration'),
            PAST::Var.new( :scope('parameter'), :name('$_') ),
            PAST::Op.new( :pasttype('bind_6model'),
                PAST::Var.new( :scope('lexical_6model'), :name('$_') ),
                PAST::Op.new(
                    :name('&EXCEPTION'),
                    PAST::Var.new( :scope('lexical_6model'), :name('$_') ),
                ),
            ),
            PAST::Var.new( :scope('lexical_6model'), :name('$/'), :isdecl(1) ),
            PAST::Op.new( :pasttype('bind_6model'),
                PAST::Var.new( :scope('lexical_6model'), :name('$!'), :isdecl(1) ),
                PAST::Var.new( :scope('lexical_6model'), :name('$_') ),
            ),
            PAST::Op.new( :pasttype('call'),
                $handler,
            ));
        $handler.symbol('$_', :scope('lexical_6model'));
        $handler.symbol('$!', :scope('lexical_6model'));
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
            ),
            PAST::Op.new( :pirop('finalize vP'),
                PAST::Var.new( :scope('register'), :name('exception')))
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
        my @params := [
            hash( is_invocant => 1, nominal_type => $*PACKAGE),
            hash( variable_name => '$_', nominal_type => $*ST.find_symbol(['Mu']))
        ];
        my $sig := $*ST.create_signature([
            $*ST.create_parameter(@params[0]),
            $*ST.create_parameter(@params[1])
        ]);
        my $block := PAST::Block.new(
            PAST::Stmts.new(
                PAST::Var.new( :name('self'), :scope('lexical_6model'), :isdecl(1) ),
                PAST::Var.new( :name('$_'), :scope('lexical_6model'), :isdecl(1) )
            ),
            PAST::Stmts.new( $/[1].ast ));
        $block.symbol('self', :scope('lexical_6model'));
        add_signature_binding_code($block, $sig, @params);
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

    # %curried == 0 means do not curry
    # %curried == 1 means curry WhateverCode only
    # %curried == 2 means curry both WhateverCode and Whatever (default)

    our %curried;
    INIT {
        %curried{'&infix:<...>'}  := 0;
        %curried{'&infix:<...^>'} := 0;
        %curried{'&prefix:<^>'}   := 0;
        %curried{'&infix:<~~>'}   := 0;
        %curried{'&infix:<=>'}    := 0;
        %curried{'&infix:<:=>'}   := 0;
        %curried{'WHAT'}          := 0;
        %curried{'HOW'}           := 0;
        %curried{'WHO'}           := 0;
        %curried{'WHERE'}         := 0;
        %curried{'&infix:<..>'}   := 1;
        %curried{'&infix:<..^>'}  := 1;
        %curried{'&infix:<^..>'}  := 1;
        %curried{'&infix:<^..^>'} := 1;
        %curried{'&infix:<xx>'}   := 1;
    }
    sub whatever_curry($/, $past, $upto_arity) {
        my $curried := $past.isa(PAST::Op)
                       && ($past<pasttype> ne 'call' || pir::index($past.name, '&infix:') == 0)
                       && (%curried{$past.name} // 2);
        my $i := 0;
        my $whatevers := 0;
        while $curried && $i < $upto_arity {
            $whatevers++ if $past[$i].returns eq 'WhateverCode'
                            || $curried > 1 && $past[$i].returns eq 'Whatever';
            $i++;
        }
        if $whatevers {
            my $i := 0;
            my @params;
            my $block := PAST::Block.new(PAST::Stmts.new(), $past);
            $*ST.cur_lexpad()[0].push($block);
            while $i < $upto_arity {
                my $old := $past[$i];
                if $old.returns eq 'WhateverCode' {
                    my $new := PAST::Op.new( :pasttype<call>, :node($/), $old);
                    my $acount := 0;
                    while $acount < $old.arity {
                        my $pname := '$x' ~ (+@params);
                        @params.push(hash(
                            :variable_name($pname),
                            :nominal_type($*ST.find_symbol(['Mu'])),
                            :is_parcel(1),
                        ));
                        $block[0].push(PAST::Var.new(:name($pname), :scope<lexical_6model>, :isdecl(1)));
                        $new.push(PAST::Var.new(:name($pname), :scope<lexical_6model>));
                        $acount++;
                    }
                    $past[$i] := $new;
                }
                elsif $curried > 1 && $old.returns eq 'Whatever' {
                    my $pname := '$x' ~ (+@params);
                    @params.push(hash(
                        :variable_name($pname),
                        :nominal_type($*ST.find_symbol(['Mu'])),
                        :is_parcel(1),
                    ));
                    $block[0].push(PAST::Var.new(:name($pname), :scope<lexical_6model>, :isdecl(1)));
                    $past[$i] := PAST::Var.new(:name($pname), :scope<lexical_6model>);
                }
                $i++;
            }
            my $signature := create_signature_object(@params, $block);
            add_signature_binding_code($block, $signature, @params);
            my $code := $*ST.create_code_object($block, 'WhateverCode', $signature);
            $past := block_closure(reference_to_code_object($code, $block));
            $past.returns('WhateverCode');
            $past.arity(+@params);
        }
        $past
    }

    sub wrap_return_handler($past) {
        PAST::Op.new(
            :pirop('perl6_type_check_return_value 0P'),
            PAST::Stmts.new( :signature('0Pv'),
                PAST::Op.new(:pasttype<lexotic>, :name<RETURN>,
                    # If we fall off the bottom, decontainerize if
                    # rw not set.
                    PAST::Op.new( :pirop('perl6_decontainerize_return_value PP'), $past )
                ),
                PAST::Op.new(:pasttype<bind_6model>,
                    PAST::Var.new(:name<RETURN>, :scope<lexical>),
                    PAST::Var.new(:name<&EXHAUST>, :scope<lexical>))
            )
        )
    }

    # Works out how to look up a type. If it's not generic we statically
    # resolve it. Otherwise, we punt to a runtime lexical lookup.
    sub instantiated_type(@name, $/) {
        my $type := $*ST.find_symbol(@name);
        my $is_generic := 0;
        try { $is_generic := $type.HOW.is_generic($type) }
        my $past := $is_generic ??
            $*ST.symbol_lookup(@name, $/) !!
            $*ST.get_object_sc_ref_past($type);
        $past<has_compile_time_value> := 1;
        $past<compile_time_value> := $type;
        return $past;
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

    my @prim_spec_ops := ['', 'perl6_box_int__PI', 'perl6_box_num__PN', 'perl6_box_str__PS'];
    my @prim_spec_flags := ['', 'Ii', 'Nn', 'Ss'];
    sub box_native_if_needed($past, $type) {
        my $primspec := pir::repr_get_primitive_type_spec__IP($type);
        if $primspec {
            my $want := PAST::Want.new(
                PAST::Op.new( :pirop(@prim_spec_ops[$primspec]), $past ),
                @prim_spec_flags[$primspec], $past);
            $want<boxable_native> := $primspec;
            return $want;
        }
        else {
            $past
        }
    }

    sub strip_trailing_zeros(str $n) {
        return $n if pir::index($n, '.') < 0;
        while pir::index('_0',nqp::substr($n, -1)) >= 0 {
            $n := pir::chopn__Ssi($n, 1);
        }
        $n;
    }

    sub radcalc($radix, $number, $base?, $exponent?) {
        my int $sign := 1;
        pir::die("Radix '$radix' out of range (2..36)")
            if $radix < 2 || $radix > 36;
        pir::die("You gave us a base for the magnitude, but you forgot the exponent.")
            if pir::defined($base) && !pir::defined($exponent);
        pir::die("You gave us an exponent for the magnitude, but you forgot the base.")
            if !pir::defined($base) && pir::defined($exponent);

        if nqp::substr($number, 0, 1) eq '-' {
            $sign := -1;
            $number := nqp::substr($number, 1);
        }
        if nqp::substr($number, 0, 1) eq '0' {
            my $radix_name := nqp::uc(nqp::substr($number, 1, 1));
            if pir::index('0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ', $radix_name) > $radix {
                $number := nqp::substr($number, 2);

                if      $radix_name eq 'B' {
                    $radix := 2;
                } elsif $radix_name eq 'O' {
                    $radix := 8;
                } elsif $radix_name eq 'D' {
                    $radix := 10;
                } elsif $radix_name eq 'X' {
                    $radix := 16;
                } else {
                    pir::die("Unkonwn radix character '$radix_name' (can be b, o, d, x)");
                }
            }
        }

        $number := strip_trailing_zeros($number);

        my int $iresult  := 0;
        my int $fresult  := 0;
        my int $fdivide  := 1;
        my int $idx      := -1;
        my int $seen_dot := 0;
        while $idx < nqp::chars($number) - 1 {
            $idx++;
            my $current := nqp::substr($number, $idx, 1);
            next if $current eq '_';
            if $current eq '.' {
                $seen_dot := 1;
                next;
            }
            my $i := pir::index('0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ', $current);
            pir::die("Invalid character '$current' in number literal") if $i < 0 || $i >= $radix;
            $iresult := $iresult * $radix + $i;
            $fdivide := $fdivide * $radix if $seen_dot;
        }

        $iresult := $iresult * $sign;

        if pir::defined($exponent) {
            my num $result := nqp::mul_n(nqp::div_n($iresult, $fdivide), nqp::pow_n($base, $exponent));
            return $*ST.add_numeric_constant('Num', $result);
        } else {
            if $seen_dot {
                return $*ST.add_constant('Rat', 'type_new',
                    $*ST.add_numeric_constant('Int', $iresult)<compile_time_value>,
                    $*ST.add_numeric_constant('Int', $fdivide)<compile_time_value>
                );
            } else {
                return $*ST.add_numeric_constant('Int', $iresult);
            }
        }
    }

    sub document($what, $with) {
        unless %*COMPILING<%?OPTIONS><setting> eq 'NULL' {
            my $true := $*ST.add_constant('Int', 'int', 1)<compile_time_value>;
            my $doc  := $*ST.add_constant('Str', 'str', $with)<compile_time_value>;
            $*ST.apply_trait('&trait_mod:<is>', $what, $doc, :docs($true));
        }
    }
}

class Perl6::RegexActions is QRegex::P6Regex::Actions {

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
