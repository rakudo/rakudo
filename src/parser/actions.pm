# Copyright (C) 2007-2008, The Perl Foundation.
# $Id$

class Perl6::Grammar::Actions ;

method TOP($/) {
    my $past := $( $<statement_block> );
    $past.blocktype('declaration');
    declare_implicit_var($past, '$_', 'new');
    declare_implicit_var($past, '$!', 'new');
    declare_implicit_var($past, '$/', 'new');

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

    #  Make sure we have the interpinfo constants.
    $past.unshift( PAST::Op.new( :inline('.include "interpinfo.pasm"') ) );

    #  Add code to load perl6.pbc if it's not already present
    my $loadinit := $past.loadinit();
    $loadinit.unshift(
        PAST::Op.new( :inline('$P0 = compreg "Perl6"',
                              'unless null $P0 goto have_perl6',
                              'load_bytecode "perl6.pbc"',
                              'have_perl6:')
        )
    );
    

    #  convert the last operation of the block into a .return op
    #  so that :load block below isn't used as return value
    $past.push( PAST::Op.new( $past.pop(), :pirop('return') ) );
    #  automatically invoke mainline on :load (but not :init)
    $past.push(
        PAST::Block.new(
            PAST::Op.new(
                :inline(
                    '$P0 = interpinfo .INTERPINFO_CURRENT_SUB',
                    '$P0 = $P0."get_outer"()',
                    '$P0()'
                )
            ),
            :pirflags(':load')
        )
    );

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
    }
    if $key eq 'close' {
        my $past := @?BLOCK.shift();
        $?BLOCK := @?BLOCK[0];
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
        elsif $key eq 'statement_mod_loop' {
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
    declare_implicit_immediate_vars($then);
    my $past := PAST::Op.new(
        $expr, $then,
        :pasttype('if'),
        :node( $/ )
    );
    if $<else> {
        my $else := $( $<else>[0] );
        $else.blocktype('immediate');
        declare_implicit_immediate_vars($else);
        $past.push( $else );
    }
    while $count != 0 {
        $count := $count - 1;
        $expr  := $( $<EXPR>[$count] );
        $then  := $( $<block>[$count] );
        $then.blocktype('immediate');
        declare_implicit_immediate_vars($then);
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
    declare_implicit_immediate_vars($then);
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
    declare_implicit_immediate_vars($block);
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
    my $block := $( $<pblock> );
    $block.blocktype('declaration');
    declare_implicit_function_vars($block);
    ##  call the block using the expression as an argument
    my $past := PAST::Op.new(
        :pasttype('call'),
        $block,
        $( $<EXPR> )
    );
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
    declare_implicit_function_vars($block);
    my $past := PAST::Op.new(
        PAST::Op.new(:name('list'), $($<EXPR>)),
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
            PAST::Op.new($expr, :name('list')),
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
    ##  after the code in the try block is executed, bind $! to Failure,
    ##  and set up the code to catch an exception, in case one is thrown
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


method multi_declarator($/, $key) {
    my $past := $( $/{$key} );

    # If we just got a routine_def, make it a sub.
    if $key eq 'routine_def' {
        create_sub($/, $past);
    }

    # If it was multi, then emit a :multi and a type list.
    if $<sym> eq 'multi' {
        our $?PARAM_TYPE_CHECK;
        my @check_list;
        if $?PARAM_TYPE_CHECK {
            @check_list := @($?PARAM_TYPE_CHECK);
            $?PARAM_TYPE_CHECK := 0;
        }

        # Go over the parameters and build multi-sig.
        my $pirflags := ~ $past.pirflags();
        $pirflags := $pirflags ~ ' :multi(';
        my $arity := +@check_list;
        my $count := 0;
        if $<routine_declarator><sym> eq 'method' {
            # For methods, need to have a slot in the multi list for the
            # invocant. XXX could be a type constraint in the sig on self.
            $pirflags := $pirflags ~ '_';
            if $arity {
                $pirflags := $pirflags ~ ', ';
            }
        }
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
    my $past;
    if $key eq 'sub' {
        $past := $($<routine_def>);
        create_sub($/, $past);
    }
    elsif $key eq 'method' {
        $past := $($<method_def>);

        # Add declaration of leixcal self.
        $past[0].unshift(PAST::Op.new(
            :pasttype('bind'),
            PAST::Var.new(
                :name('self'),
                :scope('lexical'),
                :isdecl(1)
            ),
            PAST::Var.new( :name('self'), :scope('register') )
        ));

        # Set up the block details.
        $past.blocktype('method');
        set_block_proto($past, 'Method');
        if $<method_def><multisig> {
            set_block_sig($past, $( $<method_def><multisig>[0]<signature> ));
        }
        $past := add_method_to_class($past);
    }
    $past.node($/);
    if (+@($past[1])) {
        declare_implicit_var($past, '$_', 'new');
        declare_implicit_var($past, '$!', 'new');
        declare_implicit_var($past, '$/', 'new');
    }
    else {
        $past[1].push( PAST::Op.new( :name('list') ) );
    }
    make $past;
}


method enum_declarator($/, $key) {
    my $values := $( $/{$key} );

    if $<name> {
        # It's a named enumeration. First, we will get a mapping of all the names
        # we will introduce with this enumeration to their values. We'll compute
        # these at compile time, so then we can build as much of the enum as possible
        # as PAST at compile time too. Note that means that, like a BEGIN block, we
        # will compile, run and get the return value now.
        my $block := PAST::Block.new(
            :blocktype('declaration'),
            PAST::Stmts.new(
                PAST::Op.new(
                    :pasttype('call'),
                    :name('!anon_enum'),
                    $values
                )
            )
        );
        my $getvals_sub := PAST::Compiler.compile( $block );
        my %values := $getvals_sub();

        # Now we need to emit an role of the name of the enum containing:
        #  * One attribute with the same name as the enum
        #  * A method of the same name as the enum
        #  * Methods for each name introduced by the enum that compare the
        #    attribute with the value of that name.
        my $role_past := PAST::Stmts.new(
            PAST::Op.new(
                :pasttype('bind'),
                PAST::Var.new(
                    :name('$def'),
                    :scope('lexical')
                ),
                PAST::Op.new(
                    :pasttype('call'),
                    :name('!keyword_role'),
                    PAST::Val.new( :value(~$<name>[0]) )
                )
            ),
            PAST::Op.new(
                :pasttype('call'),
                :name('!keyword_has'),
                PAST::Var.new(
                    :name('$def'),
                    :scope('lexical')
                ),
                PAST::Val.new( :value("$!" ~ ~$<name>[0]) ),
                # XXX Set declared type here, when we parse that.
                PAST::Var.new(
                    :name('Object'),
                    :scope('package')
                )
            ),
            PAST::Op.new(
                :pasttype('callmethod'),
                :name('add_method'),
                PAST::Var.new(
                    :name('$def'),
                    :scope('lexical')
                ),
                PAST::Val.new( :value(~$<name>[0]) ),
                make_accessor($/, undef, "$!" ~ ~$<name>[0], 1, 'attribute')
            )
        );
        for %values.keys() {
            # Method for this value.
            $role_past.push(PAST::Op.new(
                :pasttype('callmethod'),
                :name('add_method'),
                PAST::Var.new(
                    :name('$def'),
                    :scope('lexical')
                ),
                PAST::Val.new( :value($_) ),
                PAST::Block.new(
                    :blocktype('declaration'),
                    :pirflags(':method'),
                    PAST::Stmts.new(
                        PAST::Op.new(
                            :pasttype('call'),
                            :name('infix:eq'), # XXX not generic enough
                            PAST::Var.new(
                                :name("$!" ~ ~$<name>[0]),
                                :scope('attribute')
                            ),
                            PAST::Val.new( :value(%values{$_}) )
                        )
                    )
                )
            ));
        }

        # Now we emit code to create a class for the enum that does the role
        # that we just defined. Note $def in the init code refers to this
        # class from now on. Mark the class as an enum.
        my $class_past := PAST::Stmts.new(
            PAST::Op.new(
                :pasttype('bind'),
                PAST::Var.new(
                    :name('$def'),
                    :scope('lexical')
                ),
                PAST::Op.new(
                    :pasttype('call'),
                    :name('!keyword_enum'),
                    PAST::Var.new(
                        :name('$def'),
                        :scope('lexical')
                    )
                )
            ),
            PAST::Op.new(
                :inline('    setprop %0, "enum", %1'),
                PAST::Var.new(
                    :name('$def'),
                    :scope('lexical')
                ),
                PAST::Val.new(
                    :value(1),
                    :returns('Int')
                )
            )
        );

        # Want to give the class an invoke method that returns the enum value,
        # and get_string, get_number and get_integer v-table overrides to we
        # can get data from it..
        $class_past.push(PAST::Op.new(
            :pasttype('callmethod'),
            :name('add_method'),
            PAST::Var.new(
                :scope('lexical'),
                :name('$def')
            ),
            PAST::Val.new( :value('invoke') ),
            PAST::Block.new(
                :blocktype('declaration'),
                :pirflags(":method"),
                PAST::Var.new(
                    :name("$!" ~ ~$<name>[0]),
                    :scope('attribute')
                )
            ),
            PAST::Val.new(
                :value(1),
                :named( PAST::Val.new( :value('vtable') ) )
            )
        ));
        $class_past.push(PAST::Op.new(
            :pasttype('callmethod'),
            :name('add_method'),
            PAST::Var.new(
                :scope('lexical'),
                :name('$def')
            ),
            PAST::Val.new( :value('get_string') ),
            PAST::Block.new(
                :blocktype('declaration'),
                :pirflags(":method"),
                PAST::Op.new(
                    :pasttype('call'),
                    :name('prefix:~'),
                    PAST::Var.new(
                        :name("$!" ~ ~$<name>[0]),
                        :scope('attribute')
                    )
                )
            ),
            PAST::Val.new(
                :value(1),
                :named( PAST::Val.new( :value('vtable') ) )
            )
        ));
        $class_past.push(PAST::Op.new(
            :pasttype('callmethod'),
            :name('add_method'),
            PAST::Var.new(
                :scope('lexical'),
                :name('$def')
            ),
            PAST::Val.new( :value('get_integer') ),
            PAST::Block.new(
                :blocktype('declaration'),
                :pirflags(":method"),
                PAST::Op.new(
                    :pasttype('call'),
                    :name('prefix:+'),
                    PAST::Var.new(
                        :name("$!" ~ ~$<name>[0]),
                        :scope('attribute')
                    )
                )
            ),
            PAST::Val.new(
                :value(1),
                :named( PAST::Val.new( :value('vtable') ) )
            )
        ));
        $class_past.push(PAST::Op.new(
            :pasttype('callmethod'),
            :name('add_method'),
            PAST::Var.new(
                :scope('lexical'),
                :name('$def')
            ),
            PAST::Val.new( :value('get_number') ),
            PAST::Block.new(
                :blocktype('declaration'),
                :pirflags(":method"),
                PAST::Op.new(
                    :pasttype('call'),
                    :name('prefix:+'),
                    PAST::Var.new(
                        :name("$!" ~ ~$<name>[0]),
                        :scope('attribute')
                    )
                )
            ),
            PAST::Val.new(
                :value(1),
                :named( PAST::Val.new( :value('vtable') ) )
            )
        ));

        # Now we need to create instances of each of these and install them
        # in a package starting with the enum's name, plus an alias to them
        # in the current package.
        for %values.keys() {
            # Instantiate with value.
            $class_past.push(PAST::Op.new(
                :pasttype('bind'),
                PAST::Var.new(
                    :name($_),
                    :namespace(~$<name>[0]),
                    :scope('package')
                ),
                PAST::Op.new(
                    :pasttype('callmethod'),
                    :name('new'),
                    PAST::Var.new(
                        :name('$def'),
                        :scope('lexical')
                    ),
                    PAST::Val.new(
                        :value(%values{$_}),
                        :named( PAST::Val.new( :value("$!" ~ ~$<name>[0]) ) )
                    )
                )
            ));

            # Add alias in current package.
            # XXX Need to do collision detection, once we've a registry.
            $class_past.push(PAST::Op.new(
                :pasttype('bind'),
                PAST::Var.new(
                    :name($_),
                    :scope('package')
                ),
                PAST::Var.new(
                    :name($_),
                    :namespace(~$<name>[0]),
                    :scope('package')
                )
            ));
        }

        # Assemble all that we build into a statement list and then place it
        # into the init code.
        our $?INIT;
        unless defined( $?INIT ) {
            $?INIT := PAST::Block.new();
        }
        $?INIT.push(PAST::Stmts.new(
            $role_past,
            $class_past
        ));

        # Finally, since it's a decl, we don't have anything to emit at this
        # point; just hand back empty statements block.
        make PAST::Stmts.new();
    }
    else {
        # Emit runtime call anonymous enum constructor.
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
    # In here, we build a signature object and optionally some other things
    # if $?SIG_BLOCK_NOT_NEEDED is not set to a true value.
    # * $?BLOCK_SIGNATURED ends up containing the PAST tree for a block that
    #   takes and binds the parameters. This is used for generating subs,
    #   methods and so forth.
    # * $?PARAM_TYPE_CHECK is used to export details of the types from here
    #   so that the multi plurality declarator can make use of them.

    # Initialize PAST for the signatured block, if we're going to have it.
    our $?SIG_BLOCK_NOT_NEEDED;
    my $params;
    my $type_check;
    my $block_past;
    unless $?SIG_BLOCK_NOT_NEEDED {
        $params := PAST::Stmts.new( :node($/) );
        $block_past := PAST::Block.new( $params, :blocktype('declaration') );
        $type_check := PAST::Stmts.new( :node($/) );
    }

    # Initialize PAST for constructing the signature object.
    my $sig_past := PAST::Op.new(
        :pasttype('callmethod'),
        :name('!create'),
        PAST::Var.new(
            :name('Signature'),
            :scope('package'),
            :namespace(list())
        )
    );

    # Go through the parameters.
    for $/[0] {
        my $parameter := $($_<parameter>);
        my $separator := $_[0];

        # Add parameter declaration to the block, if we're producing one.
        unless $?SIG_BLOCK_NOT_NEEDED {
            # Register symbol and put parameter PAST into the node.
            $block_past.symbol($parameter.name(), :scope('lexical'));
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
                $params.push(PAST::Op.new(
                    :pasttype('bind'),
                    PAST::Var.new(
                        :name($parameter.name()),
                        :scope('lexical')
                    ),
                    PAST::Var.new( :name('self'), :scope('register') )
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
                $block_past.symbol($tv_var.name(), :scope('lexical'));
            }
        }

        # Now start making a descriptor for the signature.
        my $descriptor := sig_descriptor_create();
        $sig_past.push($descriptor);
        sig_descriptor_set($descriptor, 'name',
            PAST::Val.new( :value(~$parameter.name()) ));
        if $parameter.named() {
            sig_descriptor_set($descriptor, 'named',
                PAST::Val.new( :value(~$parameter.named()) ));
        }
        if $parameter.viviself() {
            sig_descriptor_set($descriptor, 'optional', PAST::Val.new( :value(1) ));
        }
        if $parameter.slurpy() {
            sig_descriptor_set($descriptor, 'slurpy', PAST::Val.new( :value(1) ));
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
                my $type_obj;

                # Just a type name?
                if $_<typename> {
                    $type_obj := PAST::Op.new(
                        :pasttype('call'),
                        :name('!TYPECHECKPARAM'),
                        $( $_<typename> ),
                        PAST::Var.new(
                            :name($parameter.name()),
                            :scope('lexical')
                        )
                    );
                }
                else {
                    $type_obj := make_anon_subset($( $_<EXPR> ), $parameter);
                }

                # Add it to the types list.
                $cur_param_types.push($type_obj);
            }
        }

        # Add any post-constraints too.
        for $_<parameter><post_constraint> {
            my $type_obj := make_anon_subset($( $_<EXPR> ), $parameter);
            $cur_param_types.push($type_obj);
        }

        # For blocks, we just collect the check into the list of all checks.
        unless $?SIG_BLOCK_NOT_NEEDED {
            $type_check.push($cur_param_types);
        }

        # For signatures, we build a list from the constraints and store it.
        my $sig_type_cons := PAST::Stmts.new(
            PAST::Op.new(
                :inline('    $P2 = new "List"')
            ),
            PAST::Stmts.new(),
            PAST::Op.new(
                :inline('    %r = $P2')
            )
        );
        for @($cur_param_types) {
            # Just want the type, not the call to the checker.
            $sig_type_cons[1].push(PAST::Op.new(
                :inline('    push $P2, %0'),
                $_[0]
            ));
        }
        sig_descriptor_set($descriptor, 'constraints', $sig_type_cons);

        # If we're making a block, emit code for trait types.
        unless $?SIG_BLOCK_NOT_NEEDED {
            if $cont_trait eq 'rw' {
                # We just leave it as it is.
            }
            elsif $cont_trait eq 'readonly' {
                # Create a new container with ro set and bind the parameter to it.
                $params.push(PAST::Op.new(
                    :pasttype('bind'),
                    PAST::Var.new(
                        :name($parameter.name()),
                        :scope('lexical')
                    ),
                    PAST::Op.new(
                        :inline(
                            '    %r = new "Perl6Scalar", %0',
                            '    $P0 = get_hll_global ["Bool"], "True"',
                            '    setprop %r, "readonly", $P0'
                        ),
                        PAST::Var.new(
                            :name($parameter.name()),
                            :scope('lexical')
                        )
                    )
                ));
            }
            elsif $cont_trait eq 'copy' {
                # Create a new container and copy the value into it..
                $params.push(PAST::Op.new(
                    :pasttype('bind'),
                    PAST::Var.new(
                    :name($parameter.name()),
                    :scope('lexical')
                    ),
                    PAST::Op.new(
                        :inline(
                            '    %r = new "Perl6Scalar"',
                            '    %r."infix:="(%0)'
                        ),
                        PAST::Var.new(
                            :name($parameter.name()),
                            :scope('lexical')
                        )
                    )
                ));
            }
        }
    }

    # Finish setting up the signatured block, if we're making one.
    unless $?SIG_BLOCK_NOT_NEEDED {
        $block_past.arity( +$/[0] );
        our $?BLOCK_SIGNATURED := $block_past;
        our $?PARAM_TYPE_CHECK := $type_check;
        $params.push($type_check);
    }

    # Hand back the PAST to construct a signature object.
    make $sig_past;
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
    if $<twigil> && $<twigil>[0] ne '.' && $<twigil>[0] ne '!' {
        $/.panic('Invalid twigil used in signature parameter.');
    }
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

            if  $past.isa(PAST::Op)
                && $past.pasttype() eq 'callmethod'
                && !$past.name() {
                    # indirect call, invocant needs to be second arg
                    my $meth := $past[0];
                    $past[0] := $term;
                    $past.unshift($meth);
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
        if $/[0] eq '.?' || $/[0] eq '.+' || $/[0] eq '.*' || $/[0] eq '.^' {
            my $name := $past.name();
            unless $name {
                $/.panic("Cannot use " ~ $/[0] ~ " when method is a code ref");
            }
            unless $name.isa(PAST::Node) {
                $name := PAST::Val.new( :value($name) );
            }
            $past.unshift($name);
            $past.name('!' ~ $/[0]);
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
        $past.name( $( $<quote> ) );
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
        $past := PAST::Var.new(
            :name('self'),
            :scope('lexical'),
            :node($/)
        );
    }
    elsif $key eq 'dotty' {
        # Call on $_.
        $past := $( $/{$key} );
        $past.unshift(PAST::Var.new(
            :name('$_'),
            :scope('lexical'),
            :viviself('Failure'),
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
                        :scope('package'),
                        :viviself('Undef')
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
                        :name(~$_<trait_auxiliary><name>),
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
            # Start of class definition; make PAST to create class object.
            my $class_def := PAST::Op.new(
                :pasttype('bind'),
                PAST::Var.new(
                    :name('$def'),
                    :scope('lexical')
                ),
                PAST::Op.new(
                    :pasttype('call'),
                    :name('!keyword_class')
                )
            );

            # Add a name, if we have one.
            if $<name> {
                $class_def[1].push( PAST::Val.new( :value(~$<name>[0]) ) );
            }

            $?CLASS.push($class_def);
        }
        elsif $?PACKAGE =:= $?GRAMMAR {
            # Anonymous grammars not supported.
            unless $<name> {
                $/.panic('Anonymous grammars not supported');
            }

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
                        PAST::Val.new( :value(~$<name>[0]) )
                    )
                )
            );
        }
        else {
            # Anonymous modules not supported.
            unless $<name> {
                $/.panic('Anonymous modules not supported');
            }
        }

        # Also store the current namespace, if we're not anonymous.
        if $<name> {
            $?NS := $<name>[0]<ident>;
        }
    }
    else {
        # XXX For now, to work around the :load :init not being allowed to be
        # an outer bug, we will enclose the actual package block inside an
        # immediate block of its own.
        my $inner_block := $( $<package_block> );
        $inner_block.blocktype('immediate');
        my $past := PAST::Block.new(
            $inner_block
        );

        # Declare the namespace and that the result block holds things that we
        # do "on load".
        if $<name> {
            $past.namespace($<name>[0]<ident>);
        }
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

            # If this is an anonymous class, the block doesn't want to be a
            # :init :load, and it's going to contain the class definition, so
            # we need to declare the lexical $def.
            unless $<name> {
                $past.pirflags('');
                $past.blocktype('immediate');
                $past[0].push(PAST::Var.new(
                    :name('$def'),
                    :scope('lexical'),
                    :isdecl(1)
                ));
            }

            # Attatch any class initialization code to the init code;
            # note that we skip blocks, which are method accessors that
            # we want to put under this block so they get the correct
            # namespace. If it's an anonymous class, everything goes into
            # this block.
            unless defined( $?INIT ) {
                $?INIT := PAST::Block.new();
            }
            for @( $?CLASS ) {
                if $_.WHAT() eq 'Block' || !$<name> {
                    $past[0].push( $_ );
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
                    PAST::Val.new( :value(~$<name>) )
                )
            )
        );

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


method variable_declarator($/) {
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
    if $<declarator><variable_declarator> {
        $past := $( $<declarator><variable_declarator> );

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

    # Variable declaration, but with a signature?
    elsif $<declarator><signature> {
        if $<fulltypename> {
            $/.panic("Distributing a type across a signature at declaration unimplemented.");
        }
        $past := $( $<declarator><signature> );
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


sub declare_attribute($/, $sym, $variable_sigil, $variable_twigil, $variable_name) {
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
                "attempt to define attribute '" ~ $name ~ "' outside of class"
        );
    }

    # Is this a role-private or just a normal attribute?
    my $name;
    if $sym eq 'my' {
        # These are only allowed inside a role.
        unless $class_def =:= $?ROLE {
            $/.panic('Role private attributes can only be declared in a role');
        }

        # We need to name-manage this somehow. We'll do $!rolename!attrname
        # for now; long term, want some UUID. For the block entry, we enter it
        # as $!attrname, add the real name and set the scope as rpattribute,
        # then translate it to the right thing when we see it.
        our $?NS;
        $name := ~$variable_sigil ~ '!' ~ $?NS[0] ~ '!' ~ ~$variable_name;
        my $visible_name := ~$variable_sigil ~ '!' ~ ~$variable_name;
        my $real_name := '!' ~ $?NS[0] ~ '!' ~ ~$variable_name;
        $?BLOCK.symbol($visible_name, :scope('rpattribute'), :real_name($real_name));
    }
    else {
        # Register name as attribute scope.
        $name := ~$variable_sigil ~ '!' ~ ~$variable_name;
        $?BLOCK.symbol($name, :scope('attribute'));
    }

    # Add attribute to class (always name it with ! twigil).
    if $/<scoped><fulltypename> {
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
    }
    else {
        $class_def.push(
            PAST::Op.new(
                :pasttype('call'),
                :name('!keyword_has'),
                PAST::Var.new(
                    :name('$def'),
                    :scope('lexical')
                ),
                PAST::Val.new( :value($name) )
            )
        );
    }

    # Is there any "handles" trait verb or an "is rw" or "is ro"?
    my $rw := 0;
    if $<scoped><declarator><variable_declarator><trait> {
        for $<scoped><declarator><variable_declarator><trait> {
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

    # Twigil handling.
    if $variable_twigil eq '.' {
        # We have a . twigil, so we need to generate an accessor.
        my $accessor := make_accessor($/, ~$variable_name, $name, $rw, 'attribute');
        $class_def.push(add_method_to_class($accessor));
    }
    elsif $variable_twigil eq '!' {
        # Don't need to do anything.
    }
    elsif $variable_twigil eq '' {
        # We have no twigil, make $name as an alias to $!name.
        $?BLOCK.symbol(
            ~$variable_sigil ~ ~$variable_name, :scope('attribute')
        );
    }
    else {
        # It's a twigil that you canny use in an attribute declaration.
        $/.panic(
                "invalid twigil "
            ~ $variable_twigil ~ " in attribute declaration"
        );
    }
}

method scope_declarator($/) {
    our $?BLOCK;
    my $declarator := $<sym>;
    my $past := $( $<scoped> );

    # What sort of thing are we scoping?
    if $<scoped><declarator><variable_declarator> {
        our $?PACKAGE;
        our $?ROLE;
        our $?CLASS;

        # Variable. If it's declared with "has" it is always an attribute. If
        # it is declared with "my" inside a role and has the ! twigil, it is
        # a role private attribute.
        my $variable := $<scoped><declarator><variable_declarator><variable>;
        my $twigil := $variable<twigil>[0];
        my $role_priv := $?ROLE =:= $?PACKAGE && $declarator eq 'my' && $twigil eq '!';
        if $declarator eq 'has' || $role_priv {
            # Attribute declarations need special handling.
            my $sigil := ~$<scoped><declarator><variable_declarator><variable><sigil>;
            my $twigil := ~$<scoped><declarator><variable_declarator><variable><twigil>[0];
            my $name := ~$<scoped><declarator><variable_declarator><variable><name>;
            declare_attribute($/, $declarator, $sigil, $twigil, $name);

            # We don't have any PAST at the point of the declaration.
            $past := PAST::Stmts.new();
        }

        # If we're in a class and have something declared with a sigil, then
        # we need to generate an accessor method and emit that along with the
        # lexical declaration itself.
        elsif ($twigil eq '.' || $twigil eq '!') && $?CLASS =:= $?PACKAGE {
            # This node is just the variable declaration; also register it in
            # the symbol table.
            my $orig_past := $past;
            $past := PAST::Var.new(
                :name(~$variable<sigil> ~ '!' ~ ~$variable<name>),
                :scope('lexical'),
                :isdecl(1),
                :viviself(container_type(~$variable<sigil>))
            );
            $?BLOCK.symbol($past.name(), :scope('lexical'));

            # Now generate accessor, if it's public.
            if $twigil eq '.' {
                $?CLASS.push(make_accessor($/, $orig_past.name(), $past.name(), 1, 'lexical'));
            }
        }

        # Otherwise, just a normal variable declaration.
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

    # Signature.
    elsif $<scoped><declarator><signature> {
        # We'll emit code to declare each of the parameters, then we'll have
        # the declaration evaluate to the signature object, thus allowing an
        # assignment to it.
        my @declare := sig_extract_declarables($/, $past);
        $past := PAST::Stmts.new($past);
        for @declare {
            # Work out sigil and twigil.
            my $sigil := substr($_, 0, 1);
            my $twigil := substr($_, 1, 1);
            my $desigilname;
            if $twigil eq '.' || $twigil eq '!' {
                $desigilname := substr($_, 2);
            }
            else {
                $twigil := '';
                $desigilname := substr($_, 1);
            }

            # Decide by declarator.
            if $declarator eq 'my' || $declarator eq 'our' {
                # Add declaration code.
                my $scope;
                if $declarator eq 'my' {
                    $scope := 'lexical'
                }
                else {
                    $scope := 'package';
                }
                $past.unshift(PAST::Var.new(
                    :name($_),
                    :isdecl(1),
                    :scope($scope),
                    :viviself('Perl6Scalar')
                ));

                # Add block entry.
                $?BLOCK.symbol($_, :scope($scope));
            } elsif $declarator eq 'has' {
                declare_attribute($/, $declarator, $sigil, $twigil, $desigilname);
            }
            else {
                $/.panic("Scope declarator " ~ $declarator ~ " unimplemented with signatures.");
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
            unless $?BLOCK.symbol($fullname) {
                $?BLOCK.symbol( $fullname, :scope('lexical') );
                $?BLOCK.arity( +$?BLOCK.arity() + 1 );
                my $var := PAST::Var.new(:name($fullname), :scope('parameter'));
                if $twigil eq ':' { $var.named( ~$name ); }
                my $block := $?BLOCK[0];
                my $i := +@($block);
                while $i > 0 && $block[$i-1]<name> gt $fullname {
                    $block[$i] := $block[$i-1];
                    $i--;
                }
                $block[$i] := $var;
            }
        }

        # If it's $.x, it's a method call, not a variable.
        if $twigil eq '.' {
            $past := PAST::Op.new(
                :node($/),
                :pasttype('callmethod'),
                :name($name),
                PAST::Var.new(
                    :name('self'),
                    :scope('lexical'),
                    :node($/)
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

            # If it has a ! twigil, give it attribute scope and add self.
            if $twigil eq '!' {
                $past.scope('attribute');
                $past.unshift(PAST::Var.new(
                    :name('self'),
                    :scope('lexical')
                ));
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

            $past.viviself(container_type($sigil));
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
        $past := PAST::Op.new(:name('circumfix:[ ]'), :node($/) );
        if $<statementlist><statement> { $past.push( $( $<statementlist> ) ); }
    }
    elsif $key eq '{ }' {
        # If it is completely empty or consists of a single list, the first
        # element of which is either a hash or a pair, it's a hash constructor.
        $past := $( $<pblock> );
        my $is_hash := 0;
        if +@($past) == 2 && +@($past[0]) == 0 {
            if +@($past[1]) == 0 {
                # Empty block, so a hash.
                $is_hash := 1;
            }
            elsif +@($past[1]) == 1 && $past[1][0].WHAT() eq 'Op' {
                if $past[1][0].name() eq 'infix:=>' {
                    # Block with just one pair in it, so a hash.
                    $is_hash := 1;
                }
                elsif $past[1][0].name() eq 'infix:,' {
                    # List, but first elements must be...
                    if $past[1][0][0].WHAT() eq 'Op' &&
                            $past[1][0][0].name() eq 'infix:=>' {
                        # ...a Pair
                        $is_hash := 1;
                    }
                    elsif $past[1][0][0].WHAT() eq 'Var' &&
                            substr($past[1][0][0].name(), 0, 1) eq '%' {
                        # ...or a hash.
                        $is_hash := 1
                    }
                }
            }
        }
        if $is_hash {
            my @children := @($past[1]);
            $past := PAST::Op.new(
                :pasttype('call'),
                :name('hash'),
                :node($/)
            );
            for @children {
                $past.push($_);
            }
        }
        else {
            declare_implicit_function_vars($past);
        }
    }
    elsif $key eq '$( )' {
        my $method := contextualizer_name($/, $<sigil>);
        $past := PAST::Op.new(
            :pasttype('callmethod'),
            :name($method),
            :node($/),
            $( $<semilist> )
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
        if $past.WHAT() eq 'Block' {
            $past.blocktype('immediate');
        }
    }
    make $past;
}


method typename($/) {
    # Extract shortname part of identifier, if there is one.
    my $ns := $<name><ident>.clone();
    my $shortname := $ns.pop();

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


method term($/, $key) {
    my $past;
    if $key eq 'func args' {
        $past := build_call( $( $<semilist> ) );
        $past.name( ~$<ident> );
    }
    elsif $key eq 'listop args' {
        $past := build_call( $( $<arglist> ) );
        $past.name( ~$<ident> );
    }
    elsif $key eq 'listop noarg' {
        $past := PAST::Op.new( :name( ~$<ident> ), :pasttype('call') );
    }
    elsif $key eq 'VAR' {
        $past := PAST::Op.new(
            :name('!VAR'),
            :pasttype('call'),
            $( $<variable> )
        );
    }
    elsif $key eq 'sigil' {
        my $method := contextualizer_name($/, $<sigil>);

        $past := PAST::Op.new(
            :pasttype('callmethod'),
            :name($method),
            :node($/),
            $( $<arglist> )
        );
    }
    else { $past := $( $/{$key} ); }
    $past.node($/);
    make $past;
}


method semilist($/) {
    my $past := $<EXPR>
                    ?? $( $<EXPR>[0] )
                    !! PAST::Op.new( :node($/), :name('infix:,') );
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
    elsif ~$<type> eq 'infix:does' || ~$<type> eq 'infix:but' {
        my $past := PAST::Op.new(
            $( $/[0] ),
            :pasttype('call'),
            :name(~$<type>),
            :node($/)
        );
        my $rhs := $( $/[1] );
        if $rhs.HOW().isa($rhs, PAST::Op) && $rhs.pasttype() eq 'call' {
            # Make sure we only have one initialization value.
            if +@($rhs) > 2 {
                $/.panic("Role initialization can only supply a value for one attribute");
            }
            # Push role name and argument onto infix:does or infix:but.
            $past.push($rhs[0]);
            $past.push($rhs[1]);
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
    $past.name('prefix:\\');
    make $past;
}


method capture($/) {
    make $( $<EXPR> );
}


method sigterm($/) {
    my $past := $( $/<signature> );
    make $past;
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


sub declare_implicit_var($block, $name, $type) {
    unless $block.symbol($name) {
        my $var := PAST::Var.new( :name($name), :isdecl(1) );
        $var.scope($type eq 'parameter' ?? 'parameter' !! 'lexical');
        if $type eq 'new' {
            $var.viviself( 'Perl6Scalar' );
        }
        else {
            my $opast := PAST::Op.new(
                :name('!OUTER'),
                PAST::Val.new( :value($name) )
            );
            $var.viviself($opast);
        }
        $block[0].push($var);
        $block.symbol($name, :scope('lexical') );
    }
}


sub declare_implicit_function_vars($block) {
    declare_implicit_var($block, '$_',
        defined($block.arity()) ?? 'outer' !! 'parameter');
    declare_implicit_var($block, '$!', 'outer');
    declare_implicit_var($block, '$/', 'outer');
}


sub declare_implicit_immediate_vars($block) {
    declare_implicit_var($block, '$_', 'outer');
    declare_implicit_var($block, '$!', 'outer');
    declare_implicit_var($block, '$/', 'outer');
}


sub contextualizer_name($/, $sigil) {
    ##  Contextualizing is calling .item, .list, .hash, etc.
    ##  on the expression in the brackets
    my $method;
    if    $sigil eq '$' { $method := 'item'; }
    elsif $sigil eq '@' { $method := 'list'; }
    elsif $sigil eq '%' { $method := 'hash'; }
    else {
        $/.panic("Use of contextualizer " ~ $sigil ~ " not implemented.");
    }
    $method
}


sub container_type($sigil) {
    if    $sigil eq '@' { return 'Perl6Array'  }
    elsif $sigil eq '%' { return 'Perl6Hash'   }
    else                { return 'Perl6Scalar' }
}


# Processes a handles expression to produce the appropriate method(s).
sub process_handles($/, $expr, $attr_name) {
    my $past := PAST::Stmts.new();

    # What type of expression do we have?
    if $expr.WHAT() eq 'Val' && $expr.returns() eq 'Perl6Str' {
        # Just a single string mapping.
        my $name := ~$expr.value();
        my $method := make_handles_method($/, $name, $name, $attr_name);
        $past.push(add_method_to_class($method));
    }
    elsif $expr.WHAT() eq 'Op' && $expr.returns() eq 'Pair' {
        # Single pair.
        my $method := make_handles_method_from_pair($/, $expr, $attr_name);
        $past.push(add_method_to_class($method));
    }
    elsif $expr.WHAT() eq 'Op' && $expr.pasttype() eq 'call' &&
          $expr.name() eq 'list' {
        # List of something, but what is it?
        for @($expr) {
            if $_.WHAT() eq 'Val' && $_.returns() eq 'Perl6Str' {
                # String value.
                my $name := ~$_.value();
                my $method := make_handles_method($/, $name, $name, $attr_name);
                $past.push(add_method_to_class($method));
            }
            elsif $_.WHAT() eq 'Op' && $_.returns() eq 'Pair' {
                # Pair.
                my $method := make_handles_method_from_pair($/, $_, $attr_name);
                $past.push(add_method_to_class($method));
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
                my $method := make_handles_method($/, $name, $name, $attr_name);
                $past.push(add_method_to_class($method));
            }
            elsif $_.WHAT() eq 'Op' && $_.returns() eq 'Pair' {
                # Pair.
                my $method := make_handles_method_from_pair($/, $_, $attr_name);
                $past.push(add_method_to_class($method));
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
    my $key := $pair[0];
    my $value := $pair[1];
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


# Takes a block and turns it into a sub.
sub create_sub($/, $past) {
    $past.blocktype('declaration');
    set_block_proto($past, 'Sub');
    if $<routine_def><multisig> {
        #set_block_sig($past, $( $<routine_def><multisig>[0]<signature> ));
    }
}


# Set the proto object type of a block.
sub set_block_proto($block, $type) {
    my $loadinit := $block.loadinit();
    $loadinit.push(
        PAST::Op.new(
            :inline('setprop %0, "$!proto", %1'),
            PAST::Var.new( :name('block'), :scope('register') ),
            PAST::Var.new( :name($type), :scope('package') )
        )
    );
}


# Associate a signature object with a block.
sub set_block_sig($block, $sig_obj) {
    my $loadinit := $block.loadinit();
    $loadinit.push(
        PAST::Op.new(
            :inline('setprop %0, "$!signature", %1'),
            PAST::Var.new( :name('block'), :scope('register') ),
            $sig_obj
        )
    );
}


# Creates a signature descriptor (for now, just a hash).
sub sig_descriptor_create() {
    PAST::Stmts.new(
        PAST::Op.new( :inline('    $P1 = new "Hash"') ),
        PAST::Stmts.new(),
        PAST::Op.new( :inline('    %r = $P1') )
    )
}

# Sets a given value in the signature descriptor.
sub sig_descriptor_set($descriptor, $name, $value) {
    $descriptor[1].push(PAST::Op.new(
        :inline('    $P1[%0] = %1'),
        PAST::Val.new( :value(~$name) ),
        $value
    ));
}

# Returns a list of variables from a signature that we are to declare. Panics
# if the signature is too complex to unpack.
sub sig_extract_declarables($/, $sig_setup) {
    # Just make sure it's what we expect.
    if $sig_setup.WHAT() ne 'Op' || $sig_setup.pasttype() ne 'callmethod' ||
       $sig_setup[0].name() ne 'Signature' {
        $/.panic("sig_extract_declarables was not passed signature declaration PAST!");
    }

    # Now go through what signature and extract what to declare.
    my @result := list();
    my $first := 1;
    for @($sig_setup) {
        if $first {
            # Skip over invocant.
            $first := 0;
        }
        else {
            # If it has a name, we're fine; if not, it's something odd - give
            # it a miss for now.
            my $found_name := undef;
            for @($_[1]) {
                if $_[0].value() eq 'name' {
                    $found_name := ~$_[1].value();
                }
            }
            if defined($found_name) {
                @result.push($found_name);
            }
            else {
                $/.panic("Signature too complex for LHS of assignment.");
            }
        }
    }
    @result
}

# Generates a setter/getter method for an attribute in a class or role.
sub make_accessor($/, $method_name, $attr_name, $rw, $scope) {
    my $getset;
    if $rw {
        $getset := PAST::Var.new( :name($attr_name), :scope($scope) );
    }
    else {
        $getset := PAST::Op.new(
            :inline(
                '    %r = new "Perl6Scalar", %0',
                '    $P0 = get_hll_global [ "Bool" ], "True"',
                '    setprop %r, "readonly", $P0'
            ),
            PAST::Var.new( :name($attr_name), :scope($scope) )
        );
    }
    my $accessor := PAST::Block.new(
        PAST::Stmts.new($getset),
        :blocktype('declaration'),
        :name($method_name),
        :pirflags(':method'),
        :node( $/ )
    );
    $accessor
}


# Adds the given method to the current class. This just returns the method that
# is passed to it if the current class is named; in the case that it is anonymous
# we need instead to emit an add_method call and remove the methods name so it
# doesn't pollute the namespace.
sub add_method_to_class($method) {
    our $?CLASS;
    our $?PACKAGE;
    if $?CLASS =:= $?PACKAGE && +@($?CLASS[0][1]) == 0 {
        # Create new PAST::Block - can't work out how to unset the name of an
        # existing one.
        my $new_method := PAST::Block.new(
            :blocktype($method.blocktype()),
            :pirflags($method.pirflags())
        );
        for @($method) {
            $new_method.push($_);
        }

        # Put call to add method into the class definition.
        $?CLASS.push(PAST::Op.new(
            :pasttype('callmethod'),
            :name('add_method'),
            PAST::Var.new(
                :name('$def'),
                :scope('lexical')
            ),
            PAST::Val.new( :value($method.name()) ),
            $new_method
        ));

        $new_method
    }
    else {
        $method
    }
}

# Creates an anonymous subset type.
sub make_anon_subset($past, $parameter) {
    # We need a block containing the constraint condition.
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
    return PAST::Op.new(
        :pasttype('call'),
        :name('!TYPECHECKPARAM'),
        PAST::Op.new(
            :inline('    %r = newclosure %0'),
            $past
        ),
        PAST::Var.new(
            :name($parameter.name()),
            :scope('lexical')
        )
    );
}

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
