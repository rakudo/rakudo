# $Id$
#
# Copyright (C) 2007-2008, The Perl Foundation.

class Perl6::Grammar::Actions ;

##  Change this to be 'Failure' to turn off the Mutable PMC version.
##  Note that to make this work again, you will also need to change:
##   * scoped method, to not do new %0, %1
our $?PERL6SCALAR := 'Perl6Scalar';

method TOP($/) {
    my $past := $( $<statement_block> );
    $past.blocktype('declaration');

    # Attach any initialization code.
    our $?INIT;
    if defined( $?INIT ) {
        $?INIT.unshift(
            PAST::Var.new(
                :name('$def'),
                :scope('lexical'),
                :isdecl(1)
            )
        );
        $?INIT.blocktype('declaration');
        $?INIT.pirflags(':init :load');
        $past.unshift( $?INIT );
        $?INIT := PAST::Block.new(); # For the next eval.
    }

    make $past;
}


method statement_block($/, $key) {
    our $?BLOCK;
    our @?BLOCK;
    our $?BLOCK_SIGNATURED;
    ##  when entering a block, use any $?BLOCK_SIGNATURED if it exists,
    ##  otherwise create an empty block with an empty first child to
    ##  hold any parameters we might encounter inside the block.
    if $key eq 'open' {
        if $?BLOCK_SIGNATURED {
            $?BLOCK := $?BLOCK_SIGNATURED;
            $?BLOCK_SIGNATURED := 0;
            $?BLOCK.symbol('___HAVE_A_SIGNATURE', :scope('lexical'));
        }
        else {
            $?BLOCK := PAST::Block.new( PAST::Stmts.new(), :node($/));
        }
        @?BLOCK.unshift($?BLOCK);
        my $init := $?BLOCK[0];
        unless $?BLOCK.symbol('$_') {
            $init.push( PAST::Var.new( :name('$_'), :isdecl(1) ) );
            $?BLOCK.symbol( '$_', :scope('lexical') );
        }
        unless $?BLOCK.symbol('$/') {
            $init.push( PAST::Var.new( :name('$/'), :isdecl(1) ) );
            $?BLOCK.symbol( '$/', :scope('lexical') );
            $init.push(
                PAST::Op.new(
                    :inline(
                          "    %r = getinterp\n"
                        ~ "    push_eh no_match_to_copy\n"
                        ~ "    %r = %r['lexpad';1]\n"
                        ~ "    pop_eh\n"
                        ~ "    if null %r goto no_match_to_copy\n"
                        ~ "    %r = %r['$/']\n"
                        ~ "    store_lex '$/', %r\n"
                        ~ "  no_match_to_copy:\n"
                    )
                )
            );
        }
        unless $?BLOCK.symbol('$!') {
            $init.push( PAST::Var.new( :name('$!'), :isdecl(1) ) );
            $?BLOCK.symbol( '$!', :scope('lexical') ); }
    }
    if $key eq 'close' {
        my $past := @?BLOCK.shift();
        $?BLOCK := @?BLOCK[0];
        if $past.symbol('___MAYBE_NEED_TOPIC_FIXUP') && !$past.symbol('___HAVE_A_SIGNATURE') {
            if $past[0][0].name() ne '$_' { $/.panic('$_ handling is very poor right now.') };
            $past.symbol('$_', :scope('lexical'));
            $past[0][0].scope('parameter');
            $past[0][0].isdecl(0);
        }
        $past.push($($<statementlist>));
        make $past;
    }
}


method block($/) {
    make $( $<statement_block> );
}


method statementlist($/) {
    my $past := PAST::Stmts.new( :node($/) );
    for $<statement> {
        $past.push( $($_) );
    }
    make $past;
}


method statement($/, $key) {
    my $past;
    if $key eq 'statement_control' {
        $past := $( $<statement_control> );
    }
    elsif $key eq 'null' {
        $past := PAST::Stmts.new();  # empty stmts seem eliminated by TGE
    }
    else {
        my $expr := $( $<expr> );
        if $expr.WHAT() eq 'Block' && !$expr.blocktype() {
            $expr.blocktype('immediate');
        }
        if $key eq 'statement_mod_cond' {
            $past := $( $<statement_mod_cond> );
            $past.push( $expr );
            if $<sml> {
                $expr := $past;
                $key := 'statement_mod_loop';
                $<statement_mod_loop> := $<sml>[0];
            }
        }
        if $key eq 'statement_mod_loop' {
            my $mod := $( $<statement_mod_loop> );
            if $<statement_mod_loop><sym> eq 'for' {
                my $loop :=  PAST::Block.new(
                    PAST::Stmts.new(
                        PAST::Var.new(
                            :name('$_'),
                            :scope('parameter'),
                            :viviself('Failure')
                        ),
                        $expr
                    ),
                    :node( $/ )
                );
                $loop.symbol( '$_', :scope('lexical') );
                $mod.push($loop);
                $past := PAST::Stmts.new( $mod, :node($/) );
            }
            else {
                $mod.push( $expr );
                $past := PAST::Block.new( $mod, :blocktype('immediate'), :node($/) );
            }
        }
        else {
            $past := $expr;
        }
    }
    make $past;
}


method statement_control($/, $key) {
    make $( $/{$key} );
}


method if_statement($/) {
    my $count := +$<EXPR> - 1;
    my $expr  := $( $<EXPR>[$count] );
    my $then  := $( $<block>[$count] );
    $then.blocktype('immediate');
    my $past := PAST::Op.new(
        $expr, $then,
        :pasttype('if'),
        :node( $/ )
    );
    if $<else> {
        my $else := $( $<else>[0] );
        $else.blocktype('immediate');
        $past.push( $else );
    }
    while $count != 0 {
        $count := $count - 1;
        $expr  := $( $<EXPR>[$count] );
        $then  := $( $<block>[$count] );
        $then.blocktype('immediate');
        $past  := PAST::Op.new(
            $expr, $then, $past,
            :pasttype('if'),
            :node( $/ )
        );
    }
    make $past;
}


method unless_statement($/) {
    my $then := $( $<block> );
    $then.blocktype('immediate');
    my $past := PAST::Op.new(
        $( $<EXPR> ), $then,
        :pasttype('unless'),
        :node( $/ )
    );
    make $past;
}


method while_statement($/) {
    my $cond  := $( $<EXPR> );
    my $block := $( $<block> );
    $block.blocktype('immediate');
    make PAST::Op.new( $cond, $block, :pasttype(~$<sym>), :node($/) );
}

method repeat_statement($/) {
    my $cond  := $( $<EXPR> );
    my $block := $( $<block> );
    $block.blocktype('immediate');
    # pasttype is 'repeat_while' or 'repeat_until'
    my $pasttype := 'repeat_' ~ ~$<loop>;
    make PAST::Op.new( $cond, $block, :pasttype($pasttype), :node($/) );
}

method given_statement($/) {
    my $past := $( $<block> );
    $past.blocktype('immediate');

    # Node to assign expression to $_.
    my $expr := $( $<EXPR> );
    my $assign := PAST::Op.new(
        PAST::Var.new( :name('$_') ),
        $( $<EXPR> ),
        :name('infix::='),
        :pasttype('bind'),
        :node($/)
    );

    # Put as first instruction in block (but after .lex $_).
    my $statements := $past[1];
    $statements.unshift( $assign );

    make $past;
}

method when_statement($/) {
    my $block := $( $<block> );
    $block.blocktype('immediate');

    # XXX TODO: push a control exception throw onto the end of the block so we
    # exit the innermost block in which $_ was set.

    # Invoke smartmatch of the expression.
    my $match_past := PAST::Op.new(
        PAST::Var.new( :name('$_') ),
        $( $<EXPR> ),
        :name('infix:~~'),
        :pasttype('call'),
        :node($/)
    );

    # Use the smartmatch result as the condition.
    my $past := PAST::Op.new(
        $match_past, $block,
        :pasttype('if'),
        :node( $/ )
    );
    make $past;
}

method default_statement($/) {
    # Always executed if reached, so just produce the block.
    my $past := $( $<block> );
    $past.blocktype('immediate');
    make $past;
}

method loop_statement($/) {
    if $<eee> ne "" {
        my $init := $( $<e1>[0] );
        my $cond := $( $<e2>[0] );
        my $tail := $( $<e3>[0] );
        my $block := $( $<block> );
        $block.blocktype('immediate');

        my $loop := PAST::Stmts.new(
            $init,
            PAST::Op.new(
                $cond,
                PAST::Stmts.new($block, $tail),
                :pasttype('while'),
                :node($/)
            ),
            :node($/)
        );
        make $loop;
    }
    else {
        my $cond  := PAST::Val.new( :value( 1 ) );
        my $block := $( $<block> );
        $block.blocktype('immediate');
        make PAST::Op.new( $cond, $block, :pasttype('while'), :node($/) );
    }
}

method for_statement($/) {
    my $block := $( $<pblock> );
    $block.blocktype('declaration');
    my $past := PAST::Op.new(
        $( $<EXPR> ),
        $block,
        :pasttype($<sym>),
        :node( $/ )
    );
    make $past;
}

method pblock($/) {
    my $block := $( $<block> );
    make $block;
}

method use_statement($/) {
    my $name := ~$<name>;
    my $past;
    if $name eq 'v6' || $name eq 'lib' {
        $past := PAST::Stmts.new( :node($/) );
    }
    else {
        $past := PAST::Op.new(
            PAST::Val.new( :value($name) ),
            :name('use'),
            :pasttype('call'),
            :node( $/ )
        );
    }
    make $past;
}

method begin_statement($/) {
    my $past := $( $<block> );
    $past.blocktype('declaration');
    my $sub := PAST::Compiler.compile( $past );
    $sub();
    # XXX - should emit BEGIN side-effects, and do a proper return()
    make PAST::Block.new();
}

method end_statement($/) {
    my $past := $( $<block> );
    $past.blocktype('declaration');
    my $sub := PAST::Compiler.compile( $past );
    PIR q<  $P0 = get_hll_global ['Perl6'], '@?END_BLOCKS' >;
    PIR q<  $P1 = find_lex '$sub' >;
    PIR q<  push $P0, $P1 >;
    make $past;
}

method statement_mod_loop($/) {
    my $expr := $( $<EXPR> );
    if ~$<sym> eq 'given' {
        my $assign := PAST::Op.new(
            :name('infix::='),
            :pasttype('bind'),
            :node($/)
        );
        $assign.push(
            PAST::Var.new( :node($/), :name('$_'), :scope('lexical') )
        );
        $assign.push( $expr );

        my $past := PAST::Stmts.new( $assign, :node($/) );
        make $past;
    }
    elsif ~$<sym> eq 'for' {
        my $past := PAST::Op.new(
            $expr,
            :pasttype($<sym>),
            :node( $/ )
        );
        make $past;
    }
    else {
        make PAST::Op.new(
            $expr,
            :pasttype( ~$<sym> ),
            :node( $/ )
        );
    }
}

method statement_mod_cond($/) {
    if ~$<sym> eq 'when' {
        my $expr := $( $<EXPR> );
        my $match_past := PAST::Op.new(
            :name('infix:~~'),
            :pasttype('call'),
            :node($/)
        );
        $match_past.push(
            PAST::Var.new( :node($/), :name('$_'), :scope('lexical') )
        );
        $match_past.push( $expr );

        my $past := PAST::Op.new(
            $match_past,
            :pasttype('if'),
            :node( $/ )
        );
        make $past;
    }
    else {
        make PAST::Op.new(
            $( $<EXPR> ),
            :pasttype( ~$<sym> ),
            :node( $/ )
        );
    }
}


method statement_prefix($/) {
    my $past := $($<statement>);
    my $sym := ~$<sym>;
    if $sym eq 'do' {
        # fall through, just use the statement itself
    }
    ## after the code in the try block is executed, bind $! to Failure,
    ## and set up the code to catch an exception, in case one is thrown
    elsif $sym eq 'try' {
        $past := PAST::Op.new( $past, :pasttype('try') );

        ##  Add a catch node to the try op that captures the
        ##  exception object into $!.
        my $catchpir := "    .get_results (%r, $S0)\n    store_lex '$!', %r";
        $past.push( PAST::Op.new( :inline( $catchpir ) ) );

        ##  Add an 'else' node to the try op that clears $! if
        ##  no exception occurred.
        my $elsepir  := "    new %r, 'Failure'\n    store_lex '$!', %r";
        $past.push( PAST::Op.new( :inline( $elsepir ) ) );
    }
    else {
        $/.panic( $sym ~ ' not implemented');
    }
    make $past;
}


method multi_declarator($/) {
    my $past := $( $<routine_declarator> );
    if $<sym> eq 'multi' {
        our $?PARAM_TYPE_CHECK;
        my @check_list := @($?PARAM_TYPE_CHECK);

        # Go over the parameters and build multi-sig.
        my $pirflags := ~ $past.pirflags();
        $pirflags := $pirflags ~ ' :multi(';
        my $arity := +@check_list;
        my $count := 0;
        while $count != $arity {
            # How many types do we have?
            my $checks := @check_list[$count];
            my $num_checks := +@($checks);
            if $num_checks == 0 {
                # XXX Should be Any, once type hierarchy is fixed up.
                $pirflags := $pirflags ~ '_';
            }
            elsif $num_checks == 1 {
                # At the moment, can only handle a named check.
                my $check_code := $checks[0];
                if $check_code.WHAT() eq 'Op'
                        && $check_code[0].WHAT() eq 'Var' {
                    $pirflags := $pirflags
                        ~ '\'' ~ $check_code[0].name() ~ '\'';
                }
                else {
                    $/.panic(
                        'Can only use type names in a multi,'
                        ~ ' not anonymous constraints.'
                    );
                }
            }
            else {
                $/.panic(
                    'Cannot have more than one type constraint'
                    ~ ' on a parameter in a multi yet.'
                );
            }

            # Comma separator if needed.
            $count := $count + 1;
            if $count != $arity {
                $pirflags := $pirflags ~ ', ';
            }
        }
        $pirflags := $pirflags ~ ')';
        $past.pirflags($pirflags);
    }
    make $past;
}


method routine_declarator($/, $key) {
    if $key eq 'sub' {
        my $past := $($<routine_def>);
        $past.blocktype('declaration');
        $past.node($/);
        make $past;
    }
    elsif $key eq 'method' {
        my $past := $($<method_def>);
        $past.blocktype('declaration');
        $past.pirflags(':method');
        $past.node($/);
        make $past;
    }
}


method enum_declarator($/, $key) {
    # Named enums aren't done yet, just the easy anonymous kind.
    if $<name> {
        $/.panic("Named enums not yet implemented.");
    }
    else {
        # Get the list of values and call anonymous enum constructor.
        my $values := $( $/{$key} );
        make PAST::Op.new(
            :pasttype('call'),
            :name('!anon_enum'),
            $values
        );
    }
}


method routine_def($/) {
    my $past := $( $<block> );
    if $<ident> {
        $past.name( ~$<ident>[0] );
        our $?BLOCK;
        $?BLOCK.symbol(~$<ident>[0], :scope('package'));
    }
    $past.control('return_pir');
    make $past;
}

method method_def($/) {
    my $past := $( $<block> );
    if $<ident> {
        $past.name( ~$<ident>[0] );
    }
    $past.control('return_pir');
    make $past;
}


method signature($/) {
    my $params := PAST::Stmts.new( :node($/) );
    my $type_check := PAST::Stmts.new( :node($/) );
    my $past := PAST::Block.new( $params, :blocktype('declaration') );
    for $/[0] {
        # Add parameter declaration.
        my $parameter := $($_<parameter>);
        my $separator := $_[0];
        $past.symbol($parameter.name(), :scope('lexical'));
        $params.push($parameter);

        # If it is invocant, modify it to be just a lexical and bind self to it.
        if substr($separator, 0, 1) eq ':' {
            # Make sure it's first parameter.
            if +@($params) != 1 {
                $/.panic("There can only be one invocant and it must be the first parameter");
            }

            # Modify.
            $parameter.scope('lexical');
            $parameter.isdecl(1);

            # Bind self to it.
            $past.push(PAST::Op.new(
                :pasttype('bind'),
                PAST::Var.new(
                    :name($parameter.name()),
                    :scope('lexical')
                ),
                PAST::Op.new(
                    :inline('%r = self')
                )
            ));
        }

        # Are we going to take the type of the thing we were passed and bind
        # it to an abstraction parameter?
        if $_<parameter><generic_binder> {
            my $tv_var := $( $_<parameter><generic_binder>[0]<variable> );
            $params.push(PAST::Op.new(
                :pasttype('bind'),
                PAST::Var.new(
                    :name($tv_var.name()),
                    :scope('lexical'),
                    :isdecl(1)
                ),
                PAST::Op.new(
                    :pasttype('callmethod'),
                    :name('WHAT'),
                    PAST::Var.new(
                        :name($parameter.name()),
                        :scope('lexical')
                    )
                )
            ));
            $past.symbol($tv_var.name(), :scope('lexical'));
        }

        # See if we have any traits. For now, we just handle ro, rw and copy.
        my $cont_trait := 'readonly';
        my $cont_traits := 0;
        for $_<parameter><trait> {
            if $_<trait_auxiliary> {
                # Get name of the trait and see if it's one of the special
                # traits we handle in the compiler.
                my $name := ~$_<trait_auxiliary><name>;
                if $name eq 'readonly' {
                    $cont_traits := $cont_traits + 1;
                }
                elsif $name eq 'rw' {
                    $cont_trait := 'rw';
                    $cont_traits := $cont_traits + 1;
                }
                elsif $name eq 'copy' {
                    $cont_trait := 'copy';
                    $cont_traits := $cont_traits + 1;
                }
                else {
                    $/.panic("Cannot apply trait " ~ $name ~ " to parameters yet.");
                }
            }
            else {
                $/.panic("Cannot apply traits to parameters yet.");
            }
        }

        # If we had is copy is rw or some other impossible combination, die.
        if $cont_traits > 1 {
            $/.panic("Can only use one of readonly, rw and copy on a parameter.");
        }

        # Add any type check that is needed. The scheme for this: $type_check
        # is a statement block. We create a block for each parameter, which
        # will be empty if there are no constraints for that parameter. This
        # is so we can later generate a multi-sig from it.
        my $cur_param_types := PAST::Stmts.new();
        if $_<parameter><type_constraint> {
            for $_<parameter><type_constraint> {
                # Just a type name?
                if $_<typename> {
                    $cur_param_types.push(
                        PAST::Op.new(
                            :pasttype('call'),
                            :name('!TYPECHECKPARAM'),
                            $( $_<typename> ),
                            PAST::Var.new(
                                :name($parameter.name()),
                                :scope('lexical')
                            )
                        )
                    );
                }
                else {
                    # We need a block containing the constraint condition.
                    my $past := $( $_<EXPR> );
                    if $past.WHAT() ne 'Block' {
                        # Make block with the expression as its contents.
                        $past := PAST::Block.new(
                            PAST::Stmts.new(),
                            PAST::Stmts.new( $past )
                        );
                    }

                    # Make sure it has a parameter.
                    my $param;
                    my $dollar_underscore;
                    for @($past[0]) {
                        if $_.WHAT() eq 'Var' {
                            if $_.scope() eq 'parameter' {
                                $param := $_;
                            }
                            elsif $_.name() eq '$_' {
                                $dollar_underscore := $_;
                            }
                        }
                    }
                    unless $param {
                        if $dollar_underscore {
                            $dollar_underscore.scope('parameter');
                            $param := $dollar_underscore;
                        }
                        else {
                            $param := PAST::Var.new(
                                :name('$_'),
                                :scope('parameter')
                            );
                            $past[0].push($param);
                        }
                    }

                    # Now we'll just pass this block to the type checker,
                    # since smart-matching a block invokes it.
                    $cur_param_types.push(
                        PAST::Op.new(
                            :pasttype('call'),
                            :name('!TYPECHECKPARAM'),
                            $past,
                            PAST::Var.new(
                                :name($parameter.name()),
                                :scope('lexical')
                            )
                        )
                    );
                }
            }
        }
        $type_check.push($cur_param_types);

        # Handle container type traits.
        if $cont_trait eq 'rw' {
            # We just leave it as it is.
        }
        elsif $cont_trait eq 'readonly' {
            # Create a new container with ro set and bind the parameter to it.
            $past.push(PAST::Op.new(
                :pasttype('bind'),
                PAST::Var.new(
                    :name($parameter.name()),
                    :scope('lexical')
                ),
                PAST::Op.new(
                    :inline(" %r = new 'Perl6Scalar', %0\n" ~
                            " $P0 = get_hll_global ['Bool'], 'True'\n" ~
                            " setprop %r, 'readonly', $P0\n"),
                    PAST::Var.new(
                        :name($parameter.name()),
                        :scope('lexical')
                    )
                )
            ));
        }
        elsif $cont_trait eq 'copy' {
            # Create a new container and copy the value into it..
            $past.push(PAST::Op.new(
                :pasttype('bind'),
                PAST::Var.new(
                :name($parameter.name()),
                :scope('lexical')
                ),
                PAST::Op.new(
                    :inline(" %r = new 'Perl6Scalar'\n" ~
                            " %r.'infix:='(%0)\n"),
                    PAST::Var.new(
                        :name($parameter.name()),
                        :scope('lexical')
                    )
                )
            ));
        }
    }
    $past.arity( +$/[0] );
    if +$/[0] {
        our $?BLOCK_SIGNATURED := $past;
    }
    our $?PARAM_TYPE_CHECK := $type_check;
    $past.push($type_check);
    make $past;
}


method parameter($/) {
    my $past := $( $<param_var> );
    my $sigil := $<param_var><sigil>;
    if $<quant> eq '*' {
        $past.slurpy( $sigil eq '@' || $sigil eq '%' );
        $past.named( $sigil eq '%' );
    }
    else {
        if $<named> eq ':' {          # named
            $past.named(~$<param_var><ident>);
            if $<quant> ne '!' {      #  required (optional is default)
                $past.viviself('Failure');
            }
        }
        else {                        # positional
            if $<quant> eq '?' {      #  optional (required is default)
                $past.viviself('Failure');
            }
        }
    }
    if $<default_value> {
        if $<quant> eq '!' {
            $/.panic("Can't put a default on a required parameter");
        }
        if $<quant> eq '*' {
            $/.panic("Can't put a default on a slurpy parameter");
        }
        $past.viviself( $( $<default_value>[0]<EXPR> ) );
    }
    make $past;
}


method param_var($/) {
    make PAST::Var.new(
        :name(~$/),
        :scope('parameter'),
        :node($/)
    );
}


method special_variable($/) {
    make PAST::Var.new( :node($/), :name(~$/), :scope('lexical') );
}


method expect_term($/, $key) {
    my $past;
    if $key eq '*' {
        # Whatever.
        $past := PAST::Op.new(
            :pasttype('callmethod'),
            :name('new'),
            :node($/),
            :lvalue(1),
            PAST::Var.new(
                :name('Whatever'),
                :scope('package'),
                :node($/)
            )
        );
    }
    else {
        $past := $( $/{$key} );
    }

    if $<postfix> {
        for $<postfix> {
            my $term := $past;
            $past := $($_);
            if $past.name() eq 'infix:,' { $past.name(''); }

            # Check if it's an indirect call.
            if $_<dotty><methodop><variable> {
                # What to call supplied; need to put the invocant second.
                my $meth := $past[0];
                $past[0] := $term;
                $past.unshift($meth);
            }
            elsif $_<dotty><methodop><quote> && $past.pasttype() eq 'callmethod' {
                # First child is something that we evaluate to get the
                # name. Replace it with PIR to call find_method on it.
                my $meth_name := $past[0];
                $past[0] := $term;
                $past.unshift(
                    PAST::Op.new(
                        :inline("$S1000 = %1\n%r = find_method %0, $S1000\n"),
                        $term,
                        $meth_name
                    )
                );
            }
            else {
                $past.unshift($term);
            }
        }
    }
    make $past;
}


method postfix($/, $key) {
    make $( $/{$key} );
}


method dotty($/, $key) {
    my $past;

    if $key eq '.' {
        # Just a normal method call.
        $past := $( $<methodop> );
    }
    elsif $key eq '!' {
        # Private method call. Need to put ! on the start of the name
        # (unless it was call to a code object, in which case we don't do
        # anything more).
        $past := $( $<methodop> );
        if $<methodop><name> {
            $past.name('!' ~ $past.name());
        }
        elsif $<methodop><quote> {
            $past[0] := PAST::Op.new(
                :pasttype('call'),
                :name('infix:~'),
                PAST::Val.new( :value('!') ),
                $past[0]
            );
        }
    }
    elsif $key eq '.*' {
        $past := $( $<methodop> );
        if $/[0] eq '.?' || $/[0] eq '.+' || $/[0] eq '.*' {
            unless $<methodop><name> || $<methodop><quote>  {
                $/.panic("Cannot use " ~ $/[0] ~ " when method is a code ref");
            }
            my $args := $past;
            $past := PAST::Op.new(
                :pasttype('call'),
                :name('infix:' ~ $/[0])
            );
            if $<methodop><name> {
                $past.push(PAST::Val.new( :value(~$args.name()) ));
            }
            for @($args) {
                $past.push($_);
            }
        }
        else {
            $/.panic($/[0] ~ ' method calls not yet implemented');
        }
    }
    elsif $key eq 'VAR' {
        $past := PAST::Op.new(
            :pasttype('call'),
            :name('!VAR'),
            :node($/)
        );
    }

    make $past;
}


method methodop($/, $key) {
    my $past;

    if $key eq 'null' {
        $past := PAST::Op.new();
    }
    else {
        $past := build_call( $( $/{$key} ) );
    }
    $past.pasttype('callmethod');
    $past.node($/);

    if $<name> {
        $past.name(~$<name><ident>[0]);
    }
    elsif $<variable> {
        $past.unshift( $( $<variable> ) );
    }
    else {
        $past.unshift( $( $<quote> ) );
    }

    make $past;
}

method postcircumfix($/, $key) {
    my $past;
    if $key eq '[ ]' {
        $past := PAST::Var.new(
            $( $<semilist> ),
            :scope('keyed_int'),
            :vivibase('Perl6Array'),
            :viviself('Failure'),
            :node( $/ )
        );
    }
    elsif $key eq '( )' {
        $past := build_call( $( $<semilist> ) );
        $past.node($/);
    }
    elsif $key eq '{ }' {
        $past := PAST::Var.new(
            $( $<semilist> ),
            :scope('keyed'),
            :vivibase('Perl6Hash'),
            :viviself('Failure'),
            :node( $/ )
        );
    }
    elsif $key eq '< >' {
        $past := PAST::Var.new(
            $( $<quote_expression> ),
            :scope('keyed'),
            :vivibase('Perl6Hash'),
            :viviself('Failure'),
            :node( $/ )
        );
    }
    else {
        $/.panic("postcircumfix " ~ $key ~ " not yet implemented");
    }
    make $past;
}


method noun($/, $key) {
    my $past;
    if $key eq 'self' {
        $past := PAST::Stmts.new( PAST::Op.new( :inline('%r = self'), :node( $/ ) ) );
    }
    elsif $key eq 'undef' {
        $past := PAST::Op.new(
            :pasttype('callmethod'),
            :name('new'),
            :node($/),
            PAST::Var.new(
                :name('Failure'),
                :scope('package')
            )
        );
    }
    elsif $key eq 'dotty' {
        # Call on $_.
        $past := $( $/{$key} );
        $past.unshift(PAST::Var.new(
            :name('$_'),
            :scope('lexical'),
            :node($/)
        ));
    }
    else {
        $past := $( $/{$key} );
    }
    make $past;
}


sub apply_package_traits($package, $traits) {
    for $traits {
        # Apply any "is" traits through MMD.
        if $_<trait_auxiliary><sym> eq 'is' {
            $package.push(
                PAST::Op.new(
                    :pasttype('call'),
                    :name('trait_auxiliary:is'),
                    PAST::Var.new(
                        :name(~$_<trait_auxiliary><name>),
                        :scope('package')
                    ),
                    PAST::Var.new(
                        :name('$def'),
                        :scope('lexical')
                    )
                )
            );
        }
        elsif $_<trait_auxiliary><sym> eq 'does' {
            # Role.
            $package.push(
                PAST::Op.new(
                    :pasttype('call'),
                    :name('!keyword_does'),
                    PAST::Var.new(
                        :name('$def'),
                        :scope('lexical')
                    ),
                    PAST::Var.new(
                        :name(~$_<trait_auxiliary><role_name><name>),
                        :scope('package')
                    )
                )
            );
        }
        else {
            $traits.panic("Currently only is and does traits are supported on packages.");
        }
    }
}


method package_declarator($/, $key) {
    our $?CLASS;
    our @?CLASS;
    our $?ROLE;
    our @?ROLE;
    our $?PACKAGE;
    our @?PACKAGE;
    our $?GRAMMAR;
    our @?GRAMMAR;

    if $key eq 'open' {
        # Start of a new package. We create an empty PAST::Stmts node for the
        # package definition to be stored in and put it onto the current stack
        # of packages and the stack of its package type.
        my $decl_past := PAST::Stmts.new();
        @?PACKAGE.unshift($?PACKAGE);
        $?PACKAGE := $decl_past;
        if $<sym> eq 'class' {
            @?CLASS.unshift($?CLASS);
            $?CLASS := $decl_past;
        }
        elsif $<sym> eq 'role' {
            @?ROLE.unshift( $?ROLE );
            $?ROLE := $decl_past;
        }
        elsif $<sym> eq 'grammar' {
            @?GRAMMAR.unshift( $?GRAMMAR );
            $?GRAMMAR := $decl_past;
        }
    }
    else {
        # End of declaration. Our PAST will be that made by the package_def or
        # role_def.
        my $past := $( $/{$key} );

        # Restore outer package.
        $?PACKAGE := @?PACKAGE.shift();
        if $<sym> eq 'class' {
            $?CLASS := @?CLASS.shift();
        }
        elsif $<sym> eq 'role' {
            $?ROLE := @?ROLE.shift();
        }
        elsif $<sym> eq 'grammar' {
            $?GRAMMAR := @?GRAMMAR.shift();
        }

        make $past;
    }
}


method package_def($/, $key) {
    our $?PACKAGE;
    our $?CLASS;
    our $?GRAMMAR;
    our $?NS;
    our $?INIT;

    if $key eq 'open' {
        # Start of package definition. Handle class and grammar specially.
        if $?PACKAGE =:= $?CLASS {
            # Start of class definition; create class object to work with.
            $?CLASS.push(
                PAST::Op.new(
                    :pasttype('bind'),
                    PAST::Var.new(
                        :name('$def'),
                        :scope('lexical')
                    ),
                    PAST::Op.new(
                        :pasttype('call'),
                        :name('!keyword_class'),
                        PAST::Val.new( :value(~$<name>) )
                    )
                )
            );
        }
        elsif $?PACKAGE =:= $?GRAMMAR {
            # Start of grammar definition. Create grammar class object.
            $?GRAMMAR.push(
                PAST::Op.new(
                    :pasttype('bind'),
                    PAST::Var.new(
                        :name('$def'),
                        :scope('lexical')
                    ),
                    PAST::Op.new(
                        :pasttype('call'),
                        :name('!keyword_grammar'),
                        PAST::Val.new( :value(~$<name>) )
                    )
                )
            );
        }

        # Also store the current namespace.
        $?NS := $<name><ident>;
    }
    else {
        # Declare the namespace and that the result block holds things that we
        # do "on load".
        my $past := $( $<package_block> );
        $past.namespace($<name><ident>);
        $past.blocktype('declaration');
        $past.pirflags(':init :load');

        if $?PACKAGE =:= $?CLASS {
            # Apply traits.
            apply_package_traits($?CLASS, $<trait>);

            # It's a class. Make proto-object.
            $?CLASS.push(
                PAST::Op.new(
                    :pasttype('callmethod'),
                    :name('register'),
                    PAST::Var.new(
                        :scope('package'),
                        :name('$!P6META'),
                        :namespace('Perl6Object')
                    ),
                    PAST::Var.new(
                        :scope('lexical'),
                        :name('$def')
                    ),
                    PAST::Val.new(
                        :value('Any'),
                        :named( PAST::Val.new( :value('parent') ) )
                    )
                )
            );

            # Attatch any class initialization code to the init code;
            # note that we skip blocks, which are method accessors that
            # we want to put under this block so they get the correct
            # namespace.
            unless defined( $?INIT ) {
                $?INIT := PAST::Block.new();
            }
            for @( $?CLASS ) {
                if $_.WHAT() eq 'Block' {
                    $past.push( $_ );
                }
                else {
                    $?INIT.push( $_ );
                }
            }
        }
        elsif $?PACKAGE =:= $?GRAMMAR {
            # Apply traits.
            apply_package_traits($?GRAMMAR, $<trait>);

            # Make proto-object for grammar.
            $?GRAMMAR.push(
                PAST::Op.new(
                    :pasttype('callmethod'),
                    :name('register'),
                    PAST::Var.new(
                        :scope('package'),
                        :name('$!P6META'),
                        :namespace('Perl6Object')
                    ),
                    PAST::Var.new(
                        :scope('lexical'),
                        :name('$def')
                    ),
                    PAST::Val.new(
                        :value('Grammar'),
                        :named( PAST::Val.new( :value('parent') ) )
                    )
                )
            );

            # Attatch grammar declaration to the init code.
            unless defined( $?INIT ) {
                $?INIT := PAST::Block.new();
            }
            $?INIT.push( $?GRAMMAR );
        }

        make $past;
    }
}


method role_def($/, $key) {
    our $?ROLE;
    our $?NS;
    our $?INIT;

    if $key eq 'open' {
        # Start of role definition. Push on code to create a role object.
        $?ROLE.push(
            PAST::Op.new(
                :pasttype('bind'),
                PAST::Var.new(
                    :name('$def'),
                    :scope('lexical')
                ),
                PAST::Op.new(
                    :pasttype('call'),
                    :name('!keyword_role'),
                    PAST::Val.new( :value(~$<role_name>) )
                )
            )
        );

        # Also store the current namespace.
        $?NS := $<role_name><name><ident>;
    }
    else {
        # Declare the namespace and that the result block holds things that we
        # do "on load".
        my $past := $( $<package_block> );
        $past.namespace($<role_name><name><ident>);
        $past.blocktype('declaration');
        $past.pirflags(':init :load');

        # Apply traits.
        apply_package_traits($?ROLE, $<trait>);

        # Attatch role declaration to the init code, skipping blocks since
        # those are accessors.
        unless defined( $?INIT ) {
            $?INIT := PAST::Block.new();
        }
        for @( $?ROLE ) {
            if $_.WHAT() eq 'Block' {
                $past.push( $_ );
            }
            else {
                $?INIT.push( $_ );
            }
        }

        make $past;
    }
}


method package_block($/, $key) {
    my $past := $( $/{$key} );
    make $past;
}


method variable_decl($/) {
    my $past := $( $<variable> );

    # If it's an attribute declaration, we handle traits elsewhere.
    my $twigil := $<variable><twigil>[0];
    if $<trait> && $twigil ne '.' && $twigil ne '!' {
        for $<trait> {
            my $trait := $_;
            if $trait<trait_auxiliary> {
                my $aux := $trait<trait_auxiliary>;
                my $sym := $aux<sym>;
                if $sym eq 'is' {
                    if $aux<postcircumfix> {
                        $/.panic("'" ~ ~$trait ~ "' not implemented");
                    }
                    else {
                        $past.viviself(~$aux<name>);
                    }
                }
                else {
                    $/.panic("'" ~ $sym ~ "' not implemented");
                }
            }
            elsif $trait<trait_verb> {
                my $verb := $trait<trait_verb>;
                my $sym := $verb<sym>;
                if $sym ne 'handles' {
                    $/.panic("'" ~ $sym ~ "' not implemented");
                }
            }
        }
    }

    make $past;
}


method scoped($/) {
    my $past;
    # Variable declaration?
    if $<variable_decl> {
        $past := $( $<variable_decl> );

        # Unless it's an attribute, emit code to set type and initialize it to
        # the correct proto.
        if $<fulltypename> && $past.WHAT() eq 'Var' {
            my $type_pir := "    %r = new %0, %1\n    setprop %r, 'type', %2\n";
            my $type := build_type($<fulltypename>);
            $past.viviself(
                PAST::Op.new(
                    :inline($type_pir),
                    PAST::Val.new( :value(~$past.viviself()) ),
                    PAST::Op.new(
                        :pasttype('if'),
                        PAST::Op.new(
                            :pirop('isa'),
                            $type,
                            PAST::Val.new( :value("P6protoobject") )
                        ),
                        $type,
                        PAST::Var.new(
                            :name('Failure'),
                            :scope('package')
                        )
                    ),
                    $type
                )
            );
        }
    }
    # Routine declaration?
    else {
        $past := $( $<routine_declarator> );

        # Don't support setting return type yet.
        if $<fulltypename> {
            $/.panic("Setting return type of a routine not yet implemented.");
        }
    }
    make $past;
}


sub declare_attribute($/) {
    # Get the class or role we're in.
    our $?CLASS;
    our $?ROLE;
    our $?PACKAGE;
    our $?BLOCK;
    my $class_def;
    if $?ROLE =:= $?PACKAGE {
        $class_def := $?ROLE;
    }
    else {
        $class_def := $?CLASS;
    }
    unless defined( $class_def ) {
        $/.panic(
                "attempt to define attribute '"
            ~ $name ~ "' outside of class"
        );
    }

    # Is this a role-private or just a normal attribute?
    my $variable := $<scoped><variable_decl><variable>;
    my $name;
    if $<sym> eq 'my' {
        # These are only allowed inside a role.
        unless $class_def =:= $?ROLE {
            $/.panic('Role private attributes can only be declared in a role');
        }

        # We need to name-manage this somehow. We'll do $!rolename!attrname
        # for now; long term, want some UUID. For the block entry, we enter it
        # as $!attrname, add the real name and set the scope as rpattribute,
        # then translate it to the right thing when we see it.
        our $?NS;
        $name := ~$variable<sigil> ~ '!' ~ $?NS[0] ~ '!' ~ ~$variable<name>;
        my $visible_name := ~$variable<sigil> ~ '!' ~ ~$variable<name>;
        my $real_name := '!' ~ $?NS[0] ~ '!' ~ ~$variable<name>;
        $?BLOCK.symbol($visible_name, :scope('rpattribute'), :real_name($real_name));
    }
    else {
        # Register name as attribute scope.
        $name := ~$variable<sigil> ~ '!' ~ ~$variable<name>;
        $?BLOCK.symbol($name, :scope('attribute'));
    }

    # Add attribute to class (always name it with ! twigil).
    $class_def.push(
        PAST::Op.new(
            :pasttype('call'),
            :name('!keyword_has'),
            PAST::Var.new(
                :name('$def'),
                :scope('lexical')
            ),
            PAST::Val.new( :value($name) ),
            build_type($/<scoped><fulltypename>)
        )
    );

    # Is there any "handles" trait verb or an "is rw" or "is ro"?
    my $rw := 0;
    if $<scoped><variable_decl><trait> {
        for $<scoped><variable_decl><trait> {
            if $_<trait_verb><sym> eq 'handles' {
                # Get the methods for the handles and add them to
                # the class
                my $meths := process_handles(
                    $/,
                    $( $_<trait_verb><EXPR> ),
                    $name
                );
                for @($meths) {
                    $class_def.push($_);
                }
            }
            elsif $_<trait_auxiliary><sym> eq 'is' {
                # Just handle rw for now.
                if ~$_<trait_auxiliary><name> eq 'rw' {
                    $rw := 1;
                }
                else {
                    $/.panic("Only 'is rw' trait is implemented for attributes");
                }
            }
            else {
                $/.panic("Only is and handles trait verbs are implemented for attributes");
            }
        }
    }

    # If we have no twigil, make $name as an alias to $!name.
    if $variable<twigil>[0] eq '' {
        $?BLOCK.symbol(
            ~$variable<sigil> ~ ~$variable<name>, :scope('attribute')
        );
    }

    # If we have a . twigil, we need to generate an accessor.
    elsif $variable<twigil>[0] eq '.' {
        my $getset;
        if $rw {
            $getset := PAST::Var.new( :name($name), :scope('attribute') );
        }
        else {
            $getset := PAST::Op.new(
                :inline("    %r = new 'Perl6Scalar', %0\n" ~
                        "    $P0 = get_hll_global [ 'Bool' ], 'True'\n" ~
                        "    setprop %r, 'readonly', $P0\n"),
                PAST::Var.new( :name($name), :scope('attribute') )
            );
        }
        my $accessor := PAST::Block.new(
            PAST::Stmts.new($getset),
            :name(~$variable<name>),
            :blocktype('declaration'),
            :pirflags(':method'),
            :node( $/ )
        );
        $class_def.unshift($accessor);
    }

    # If it's a ! twigil, we're done; otherwise, error.
    elsif $variable<twigil>[0] ne '!' {
        $/.panic(
                "invalid twigil "
            ~ $variable<twigil>[0] ~ " in attribute declaration"
        );
    }
}

method scope_declarator($/) {
    our $?BLOCK;
    my $declarator := $<sym>;
    my $past := $( $<scoped> );

    # What sort of thing are we scoping?
    if $<scoped><variable_decl> {
        # Variable. Now go by declarator or twigil if it's a role-private.
        my $twigil := $<scoped><variable_decl><variable><twigil>[0];
        if $declarator eq 'has' || $declarator eq 'my' && $twigil eq '!' {
            # Attribute declarations need special handling.
            declare_attribute($/);

            # We don't have any PAST at the point of the declaration.
            $past := PAST::Stmts.new();
        }
        else {
            # Has this already been declared?
            my $name := $past.name();
            unless $?BLOCK.symbol($name) {
                #  First declaration
                my $scope := 'lexical';
                $past.isdecl(1);
                if $declarator eq 'our' {
                    $scope := 'package';
                }
                elsif $declarator ne 'my' {
                    $/.panic(
                          "scope declarator '"
                        ~ $declarator ~ "' not implemented"
                    );
                }

                # Add block entry.
                $?BLOCK.symbol($name, :scope($scope));
            }
        }
    }

    # Routine?
    elsif $<scoped><routine_declarator> {
        # What declarator?
        if $declarator eq 'our' {
            # Default, nothing to do.
        }
        elsif $declarator eq 'my' {
            if $<scoped><routine_declarator><sym> eq 'method' {
                # Add ! to start of name.
                $past.name('!' ~ $past.name());
            }
            else {
                $/.panic("Lexically scoped subs not yet implemented.");
            }
        }
        else {
            $/.panic("Cannot apply declarator '" ~ $declarator ~ "' to a routine.");
        }
    }

    # Something else we've not implemetned yet?
    else {
        $/.panic("Don't know how to apply a scope declarator here.");
    }

    make $past;
}


method variable($/, $key) {
    my $past;
    if $key eq 'special_variable' {
        $past := $( $<special_variable> );
    }
    elsif $key eq '$0' {
        $past := PAST::Var.new(
            :scope('keyed_int'),
            :node($/),
            :viviself('Failure'),
            PAST::Var.new(
                :scope('lexical'),
                :name('$/')
            ),
            PAST::Val.new(
                :value(~$<matchidx>),
                :returns('Int')
            )
        );
    }
    elsif $key eq '$<>' {
        $past := $( $<postcircumfix> );
        $past.unshift(PAST::Var.new(
            :scope('lexical'),
            :name('$/'),
            :viviself('Failure')
        ));
    }
    else {
        our $?BLOCK;
        # Handle naming.
        my @ident := $<name><ident>;
        my $name;
        PIR q<  $P0 = find_lex '@ident'  >;
        PIR q<  $P0 = clone $P0          >;
        PIR q<  store_lex '@ident', $P0  >;
        PIR q<  $P1 = pop $P0            >;
        PIR q<  store_lex '$name', $P1   >;

        my $twigil := ~$<twigil>[0];
        my $sigil := ~$<sigil>;
        my $fullname := $sigil ~ $twigil ~ ~$name;

        if $fullname eq '@_' || $fullname eq '%_' {
            unless $?BLOCK.symbol($fullname) {
                $?BLOCK.symbol( $fullname, :scope('lexical') );
                my $var;
                if $sigil eq '@' {
                    $var := PAST::Var.new( :name($fullname), :scope('parameter'), :slurpy(1) );
                }
                else {
                    $var := PAST::Var.new( :name($fullname), :scope('parameter'), :slurpy(1), :named(1) );
                }
                $?BLOCK[0].unshift($var);
            }
        }

        if $twigil eq '^' || $twigil eq ':' {
            if $?BLOCK.symbol('___HAVE_A_SIGNATURE') {
                $/.panic('A signature must not be defined on a sub that uses placeholder vars.');
            }
            $?BLOCK.symbol('___HAS_PLACEHOLDERS', :scope('lexical'));
            unless $?BLOCK.symbol($fullname) {
                $?BLOCK.symbol( $fullname, :scope('lexical') );
                $?BLOCK.arity( +$?BLOCK.arity() + 1 );
                my $var;
                if $twigil eq ':' {
                    $var := PAST::Var.new( :name($fullname), :scope('parameter'), :named( ~$name ) );
                }
                else {
                    $var := PAST::Var.new( :name($fullname), :scope('parameter') );
                }
                my $block := $?BLOCK[0];
                my $i := +@($block);
                my $done := 0;
                while $i >= 0 && !$done{
                    my $minusblock;
                    PIR q<  $P0 = find_lex '$i'  >;
                    PIR q<  $P1 = find_lex '$block'  >;
                    PIR q<  $I0 = $P0  >;
                    PIR q<  $I0 = $I0 - 1  >;
                    PIR q<  set $P2, $P1[$I0]  >;
                    PIR q<  store_lex '$minusblock', $P2  >;
                    # if $var<name> gt $block[$i-1]<name> ...
                    if $var<name> gt $minusblock<name> || $i == 0 {
                        # $block[$i] := $var;
                        PIR q<  $P0 = find_lex '$block'   >;
                        PIR q<  $P1 = find_lex '$i'   >;
                        PIR q<  $P2 = find_lex '$var'   >;
                        PIR q<  $I0 = $P1 >;
                        PIR q<  set $P0[$I0], $P2 >;
                        $done := 1;
                    }
                    else {
                        #$block[$i] := $block[$i-1];
                        PIR q<  $P0 = find_lex '$block'   >;
                        PIR q<  $P1 = find_lex '$i'   >;
                        PIR q<  $I0 = $P1 >;
                        PIR q<  $I1 = $I0 - 1 >;
                        PIR q<  set $P2, $P0[$I1] >;
                        PIR q<  set $P0[$I0], $P2 >;
                    }
                    $i--;
                }
            }
        }

        if $fullname eq '$_' && !$?BLOCK.symbol('___HAVE_A_SIGNATURE') {
            $?BLOCK.symbol('___MAYBE_NEED_TOPIC_FIXUP', :scope('lexical'));
        }

        # If it's $.x, it's a method call, not a variable.
        if $twigil eq '.' {
            $past := PAST::Op.new(
                :node($/),
                :pasttype('callmethod'),
                :name($name),
                PAST::Op.new(
                    :inline('%r = self')
                )
            );
        }
        else {
            # Variable. [!:^] twigil should be kept in the name.
            if $twigil eq '!' || $twigil eq ':' || $twigil eq '^' { $name := $twigil ~ ~$name; }

            # All but subs should keep their sigils.
            my $sigil := '';
            if $<sigil> ne '&' {
                $sigil := ~$<sigil>;
            }

            # If we have no twigil, but we see the name noted as an attribute in
            # an enclosing scope, add the ! twigil anyway; it's an alias.
            if $twigil eq '' {
                our @?BLOCK;
                for @?BLOCK {
                    if defined( $_ ) {
                        my $sym_table := $_.symbol($sigil ~ $name);
                        if defined( $sym_table )
                                && $sym_table<scope> eq 'attribute' {
                            $name := '!' ~ $name;
                        }
                    }
                }
            }

            # If it's a role-private attribute, fix up the name.
            if $twigil eq '!' {
                our @?BLOCK;
                for @?BLOCK {
                    if defined( $_ ) {
                        my $sym_table := $_.symbol($sigil ~ $name);
                        if defined( $sym_table )
                                && $sym_table<scope> eq 'rpattribute' {
                            $name := $sym_table<real_name>;
                        }
                    }
                }
            }

            $past := PAST::Var.new(
                :name( $sigil ~ $name ),
                :node($/)
            );
            if @ident || $twigil eq '*' {
                $past.namespace(@ident);
                $past.scope('package');
            }

            # If it has a ! twigil, give it attribute scope.
            if $twigil eq '!' {
                $past.scope('attribute');
            }

            # If we have something with an & sigil see if it has any entries
            # in the enclosing blocks; otherwise, default to package.
            if $<sigil> eq '&' {
                $past.scope('package');
                our @?BLOCK;
                for @?BLOCK {
                    if defined($_) {
                        my $sym_table := $_.symbol($name);
                        if defined($sym_table) && defined($sym_table<scope>) {
                            $past.scope( $sym_table<scope> );
                        }
                    }
                }
            }

            my $container_type;
            if    $sigil eq '@' { $container_type := 'Perl6Array'  }
            elsif $sigil eq '%' { $container_type := 'Perl6Hash'   }
            else                { $container_type := $?PERL6SCALAR }
            $past.viviself($container_type);
        }
    }
    make $past;
}


method circumfix($/, $key) {
    my $past;
    if $key eq '( )' {
        $past := $<statementlist><statement>
                     ?? $( $<statementlist> )
                     !! PAST::Op.new(:name('list'));
    }
    if $key eq '[ ]' {
        $past := PAST::Op.new(:name('!Arrayref'), :node($/) );
        if $<statementlist><statement> { $past.push( $( $<statementlist> ) ); }
    }
    elsif $key eq '{ }' {
        $past := $( $<pblock> );
    }
    elsif $key eq '$( )' {
        ##  Context - is just calling .item, .list etc on whatever we
        ##  got made by the expression in the brackets.
        my $expr := $( $<semilist> );
        my $method;
        if $<sigil> eq '$' {
            $method := 'item';
        }
        elsif $<sigil> eq '@' {
            $method := 'list';
        }
        elsif $<sigil> eq '%' {
            $method := 'hash';
        }
        else {
            $/.panic("Use of " ~ $<sigil> ~ " as contextualizer not yet implemented.");
        }
        $past := PAST::Op.new(
            :pasttype('callmethod'),
            :name($method),
            :node($/),
            $expr
        );
    }
    make $past;
}


method value($/, $key) {
    make $( $/{$key} );
}


method number($/, $key) {
    make $( $/{$key} );
}


##  for a variety of reasons, this is easier in PIR than NQP for now.
##  NQP doesn't have assign yet, and Perl6Str is lighter-weight than Str.
method integer($/) {
    my $str;
    PIR q<  $P0 = find_lex '$/'   >;
    PIR q<  $S0 = $P0             >;
    PIR q<  $P1 = new 'Perl6Str'  >;
    PIR q<  assign $P1, $S0       >;
    PIR q<  store_lex '$str', $P1 >;
    make PAST::Val.new(
        :value( +$str ),
        :returns('Int'),
        :node( $/ )
    );
}


method dec_number($/) {
    make PAST::Val.new( :value( ~$/ ), :returns('Num'), :node( $/ ) );
}

method radint($/, $key) {
    make $( $/{$key} );
}

method rad_number($/) {
    my $radix    := ~$<radix>;
    my $intpart  := ~$<intpart>;
    my $fracpart := ~$<fracpart>;
    my $base;
    my $exp;
    if defined( $<base>[0] ) { $base := $<base>[0].text(); }
    if defined( $<exp>[0] ) { $exp := $<exp>[0].text(); }
    if ~$<postcircumfix> {
        my $radcalc := $( $<postcircumfix> );
        $radcalc.name('radcalc');
        $radcalc.pasttype('call');
        $radcalc.unshift( PAST::Val.new( :value( $radix ), :node( $/ ) ) );
        make $radcalc;
    }
    else{
        my $return_type := 'Int';
        if $fracpart { $return_type := 'Num'; }
        make PAST::Val.new(
            :value( radcalc( $radix, $intpart, $fracpart, ~$base, ~$exp ) ),
            :returns($return_type),
            :node( $/ )
        );
    }
}


method quote($/) {
    make $( $<quote_expression> );
}

method quote_expression($/, $key) {
    my $past;
    if $key eq 'quote_regex' {
        our $?NS;
        $past := PAST::Block.new(
            $<quote_regex>,
            :compiler('PGE::Perl6Regex'),
            :namespace($?NS),
            :blocktype('declaration'),
            :node( $/ )
        );
    }
    elsif $key eq 'quote_concat' {
        if +$<quote_concat> == 1 {
            $past := $( $<quote_concat>[0] );
        }
        else {
            $past := PAST::Op.new(
                :name('list'),
                :pasttype('call'),
                :node( $/ )
            );
            for $<quote_concat> {
                $past.push( $($_) );
            }
        }
    }
    make $past;
}


method quote_concat($/) {
    my $terms := +$<quote_term>;
    my $count := 1;
    my $past := $( $<quote_term>[0] );
    while ($count != $terms) {
        $past := PAST::Op.new(
            $past,
            $( $<quote_term>[$count] ),
            :pirop('n_concat'),
            :pasttype('pirop')
        );
        $count := $count + 1;
    }
    make $past;
}


method quote_term($/, $key) {
    my $past;
    if ($key eq 'literal') {
        $past := PAST::Val.new(
            :value( ~$<quote_literal> ),
            :returns('Perl6Str'), :node($/)
        );
    }
    elsif ($key eq 'variable') {
        $past := $( $<variable> );
    }
    elsif ($key eq 'circumfix') {
        $past := $( $<circumfix> );
        $past.blocktype('immediate');
    }
    make $past;
}


method typename($/) {
    # Extract shortname part of identifier, if there is one.
    my $ns := $<name><ident>;
    my $shortname;
    PIR q<    $P0 = find_lex '$ns'         >;
    PIR q<    $P0 = clone $P0              >;
    PIR q<    $P1 = pop $P0                >;
    PIR q<    store_lex '$ns', $P0         >;
    PIR q<    store_lex '$shortname', $P1  >;

    # Create default PAST node for package lookup of type.
    my $past := PAST::Var.new(
        :name($shortname),
        :namespace($ns),
        :scope('package'),
        :node($/),
        :viviself('Failure')
    );

    # If there's no namespace, could be lexical abstraction type.
    if +@($ns) == 0 {
        # See if we got lexical with the right name.
        our @?BLOCK;
        my $name := '::' ~ $shortname;
        for @?BLOCK {
            if defined($_) {
                my $sym_table := $_.symbol($name);
                if defined($sym_table) && defined($sym_table<scope>) {
                    $past.name( $name );
                    $past.scope( $sym_table<scope> );
                }
            }
        }
    }

    make $past;
}


method subcall($/, $key) {
    my $past;
    if $key eq 'subcall' {
        $past := build_call( $( $<semilist> ) );
        $past.name( ~$<ident> );
        $past.node( $/ );
    }
    elsif $key eq 'VAR' {
        $past := PAST::Op.new(
            :pasttype('call'),
            :name('!VAR'),
            :node($/),
            $( $<variable> )
        );
    }
    make $past;
}


method semilist($/) {
    my $past := $<EXPR>
                    ?? $( $<EXPR>[0] )
                    !! PAST::Op.new( :node($/), :name('infix:,') );
    make $past;
}


method listop($/, $key) {
    my $past;
    if $key eq 'arglist' {
        $past := build_call( $( $<arglist> ) );
    }
    if $key eq 'noarg' {
        $past := PAST::Op.new( :pasttype('call') );
    }
    $past.name( ~$<sym> );
    $past.node($/);
    make $past;
}


method arglist($/) {
    make $($<EXPR>);
}


method EXPR($/, $key) {
    if $key eq 'end' {
        make $($<expr>);
    }
    elsif ~$<type> eq 'infix:.=' {
        my $invocant  := $( $/[0] );
        my $call      := $( $/[1] );

        # Check that we have a sub call.
        if $call.WHAT() ne 'Op' || $call.pasttype() ne 'call' {
            $/.panic('.= must have a call on the right hand side');
        }

        # Make a duplicate of the target node to receive result
        my $target := PAST::Var.new(
            :name($invocant.name()),
            :scope($invocant.scope()),
            :lvalue(1)
        );

        # Change call node to a callmethod and add the invocant
        $call.pasttype('callmethod');
        $call.unshift($invocant);

        # and assign result to target
        my $past := PAST::Op.new(
            :inline("    %r = %1.'infix:='(%0)"),
            :node($/),
            $call,
            $target
        );

        make $past;
    }
    elsif ~$<type> eq 'infix:does' {
        # If the RHS is a subcall, need to handle it specially, since this is
        # not really a call, but supplying value to init first attribute with.
        my $lhs := $( $/[0] );
        my $rhs := $( $/[1] );
        my $past := PAST::Op.new(
            :pasttype('call'),
            :name('infix:does'),
            $lhs
        );
        if $rhs.WHAT() eq 'Op' && $rhs.pasttype() eq 'call' {
            # Make sure we only have one initialization value.
            if +@($rhs) != 1 {
                $/.panic("Role initialization can only supply a value for one attribute");
            }

            # Extract role name.
            $past.push(PAST::Var.new(
                :name($rhs.name()),
                :scope('package')
            ));

            # Push on initialization value.
            $past.push($rhs[0]);
        }
        else {
            $past.push($rhs);
        }
        make $past;
    }
    else {
        my $past := PAST::Op.new(
            :node($/),
            :name($<type>),
            :opattr($<top>)
        );
        if $<top><subname> { $past.name(~$<top><subname>); }
        for @($/) {
            unless +$_.from() == +$_.to() { $past.push( $($_) ) };
        }

        make $past;
    }
}


method regex_declarator($/, $key) {
    make $( $/{$key} );
}


method regex_declarator_regex($/) {
    my $past := $( $<quote_expression> );
    $past.name( ~$<ident>[0] );
    make $past;
}


method regex_declarator_token($/) {
    my $past := $( $<quote_expression> );
    $past.compiler_args( :ratchet(1) );
    $past.name( ~$<ident>[0] );
    make $past;
}


method regex_declarator_rule($/) {
    my $past := $( $<quote_expression> );
    $past.compiler_args( :s(1), :ratchet(1) );
    $past.name( ~$<ident>[0] );
    make $past;
}


method type_declarator($/) {
    # We need a block containing the constraint condition.
    my $past := $( $<EXPR> );
    if $past.WHAT() ne 'Block' {
        # Make block with a smart match of the the expression as its contents.
        $past := PAST::Block.new(
            PAST::Stmts.new(
                PAST::Var.new(
                    :scope('parameter'),
                    :name('$_')
                )
            ),
            PAST::Stmts.new(
                PAST::Op.new(
                    :pasttype('callmethod'),
                    :name('ACCEPTS'),
                    $past,
                    PAST::Var.new(
                        :scope('lexical'),
                        :name('$_')
                    )
                )
            )
        );
    }

    # Make sure it has a parameter and keep hold of it if found.
    my $param;
    my $dollar_underscore;
    for @($past[0]) {
        if $_.WHAT() eq 'Var' {
            if $_.scope() eq 'parameter' {
                $param := $_;
            }
            elsif $_.name() eq '$_' {
                $dollar_underscore := $_;
            }
        }
    }
    unless $param {
        if $dollar_underscore {
            $dollar_underscore.scope('parameter');
            $param := $dollar_underscore;
        }
        else {
            $param := PAST::Var.new(
                :name('$_'),
                :scope('parameter')
            );
            $past[0].push($param);
        }
    }

    # Do we have an existing constraint to check?
    if $<typename> {
        my $new_cond := $past[1];
        my $prev_cond := $( $<typename>[0] );
        $past[1] := PAST::Op.new(
            :pasttype('if'),
            PAST::Op.new(
                :pasttype('callmethod'),
                :name('ACCEPTS'),
                $prev_cond,
                PAST::Var.new(
                    :name($param.name())
                )
            ),
            $new_cond
        )
    }

    # Set block details.
    $past.node($/);

    # Now we need to create the block wrapper class.
    $past := PAST::Op.new(
        :pasttype('callmethod'),
        :name('!create'),
        PAST::Var.new(
            :name('Subset'),
            :scope('package')
        ),
        PAST::Val.new( :value(~$<name>) ),
        $past
    );

    make $past;
}


method fatarrow($/) {
    my $past := PAST::Op.new(
        :node($/),
        :pasttype('call'),
        :name('infix:=>'),
        :returns('Pair'),
        PAST::Val.new( :value(~$<key>) ),
        $( $<val> )
    );
    make $past;
}


method colonpair($/, $key) {
    my $pair_key;
    my $pair_val;

    if $key eq 'false' {
        $pair_key := PAST::Val.new( :value(~$<ident>) );
        $pair_val := PAST::Val.new( :value(0), :returns('Int') );
    }
    elsif $key eq 'value' {
        $pair_key := PAST::Val.new( :value(~$<ident>) );
        if $<postcircumfix> {
            $pair_val := $( $<postcircumfix>[0] );
            if $pair_val.name() ne 'infix:,' || +@($pair_val) == 1 {
                $pair_val := $pair_val[0];
            }
        }
        else {
            $pair_val := PAST::Val.new( :value(1), :returns('Int') );
        }
    }
    elsif $key eq 'varname' {
        if $<desigilname><name> {
            $pair_key := PAST::Val.new( :value( ~$<desigilname> ) );
            $pair_val := PAST::Var.new(
                :name( ~$<sigil> ~ ~$<twigil> ~ ~$<desigilname> )
            );
        }
        else {
            $/.panic('complex varname colonpair case not yet implemented');
        }
    }
    else {
        $/.panic($key ~ " pairs not yet implemented.");
    }

    my $past := PAST::Op.new(
        :node($/),
        :pasttype('call'),
        :name('infix:=>'),
        :returns('Pair'),
        $pair_key,
        $pair_val
    );
    make $past;
}


method capterm($/) {
    # We will create the capture object, passing the things supplied.
    my $past := build_call( $( $<capture> ) );
    $past.name('infix:\\( )');
    $past.unshift( PAST::Op.new( :inline('    null %r') ) );
    make $past;
}


method capture($/) {
    make $( $<EXPR> );
}


# Used by all calling code to process arguments into the correct form.
sub build_call($args) {
    if $args.WHAT() ne 'Op' || $args.name() ne 'infix:,' {
        $args := PAST::Op.new( :node($args), :name('infix:,'), $args);
    }
    my $i := 0;
    my $elems := +@($args);
    while $i < $elems {
        my $x := $args[$i];
        if $x.returns() eq 'Pair' {
            $x[1].named($x[0]);
            $args[$i] := $x[1];
        }
        $i++;
    }
    $args.pasttype('call');
    $args;
}


# Processes a handles expression to produce the appropriate method(s).
sub process_handles($/, $expr, $attr_name) {
    my $past := PAST::Stmts.new();

    # What type of expression do we have?
    if $expr.WHAT() eq 'Val' && $expr.returns() eq 'Perl6Str' {
        # Just a single string mapping.
        my $name := ~$expr.value();
        $past.push(make_handles_method($/, $name, $name, $attr_name));
    }
    elsif $expr.WHAT() eq 'Op' && $expr.returns() eq 'Pair' {
        # Single pair.
        $past.push(make_handles_method_from_pair($/, $expr, $attr_name));
    }
    elsif $expr.WHAT() eq 'Op' && $expr.pasttype() eq 'call' &&
          $expr.name() eq 'list' {
        # List of something, but what is it?
        for @($expr) {
            if $_.WHAT() eq 'Val' && $_.returns() eq 'Perl6Str' {
                # String value.
                my $name := ~$_.value();
                $past.push(make_handles_method($/, $name, $name, $attr_name));
            }
            elsif $_.WHAT() eq 'Op' && $_.returns() eq 'Pair' {
                # Pair.
                $past.push(make_handles_method_from_pair($/, $_, $attr_name));
            }
            else {
                $/.panic(
                    'Only a list of constants or pairs can be used in handles'
                );
            }
        }
    }
    elsif $expr.WHAT() eq 'Stmts' && $expr[0].name() eq 'infix:,' {
        # Also a list, but constructed differently.
        for @($expr[0]) {
            if $_.WHAT() eq 'Val' && $_.returns() eq 'Perl6Str' {
                # String value.
                my $name := ~$_.value();
                $past.push(make_handles_method($/, $name, $name, $attr_name));
            }
            elsif $_.WHAT() eq 'Op' && $_.returns() eq 'Pair' {
                # Pair.
                $past.push(make_handles_method_from_pair($/, $_, $attr_name));
            }
            else {
                $/.panic(
                    'Only a list of constants or pairs can be used in handles'
                );
            }
        }
    }
    else {
        $/.panic('Illegal or unimplemented use of handles');
    }

    $past
}


# Produces a handles method.
sub make_handles_method($/, $from_name, $to_name, $attr_name) {
    PAST::Block.new(
        :name($from_name),
        :pirflags(':method'),
        :blocktype('declaration'),
        :node($/),
        PAST::Var.new(
            :name('@a'),
            :scope('parameter'),
            :slurpy(1)
        ),
        PAST::Var.new(
            :name('%h'),
            :scope('parameter'),
            :named(1),
            :slurpy(1)
        ),
        PAST::Op.new(
            :name($to_name),
            :pasttype('callmethod'),
            PAST::Var.new(
                :name($attr_name),
                :scope('attribute')
            ),
            PAST::Var.new(
                :name('@a'),
                :scope('lexical'),
                :flat(1)
            ),
            PAST::Var.new(
                :name('%h'),
                :scope('lexical'),
                :flat(1),
                :named(PAST::Val.new( :value(1) ))
            )
        )
    )
}


# Makes a handles method from a pair.
sub make_handles_method_from_pair($/, $pair, $attr_name) {
    my $meth;

    # Single pair mapping. Check we have string name and value.
    my $key := $pair[1];
    my $value := $pair[2];
    if $key.WHAT() eq 'Val' && $value.WHAT() eq 'Val' {
        my $from_name := ~$key.value();
        my $to_name := ~$value.value();
        $meth := make_handles_method($/, $from_name, $to_name, $attr_name);
    }
    else {
        $/.panic('Only constants may be used in a handles pair argument.');
    }

    $meth
}


# This takes an array of match objects of type constraints and builds a type
# representation out of them.
sub build_type($cons_pt) {
    # Build the type constraints list for the variable.
    my $num_types := 0;
    my $type_cons := PAST::Op.new();
    for $cons_pt {
        $type_cons.push( $( $_<typename> ) );
        $num_types := $num_types + 1;
    }

    # If there were none, it's Object.
    if $num_types == 0 {
        $type_cons.push(PAST::Var.new(
            :name('Object'),
            :scope('package')
        ));
        $num_types := 1;
    }

    # Now need to apply the type constraints. How many are there?
    if $num_types == 1 {
        # Just the first one.
        $type_cons := $type_cons[0];
    }
    else {
        # Many; make an and junction of types.
        $type_cons.pasttype('call');
        $type_cons.name('all');
    }

    $type_cons
}


# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
