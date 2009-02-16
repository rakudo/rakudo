# Copyright (C) 2007-2008, The Perl Foundation.
# $Id$

class Perl6::Grammar::Actions ;

# The %?CLASSMAP hash is used to identify those classes where we
# "lie" about the class name in order to work around RT #43419 / TT #71.
# When those are fixed and we can use the "true" Perl 6 classnames,
# this can be removed.  (See also the C<package_def> method below.)
our %?CLASSMAP;
%?CLASSMAP<Object>  := 'Perl6Object';
%?CLASSMAP<Array>   := 'Perl6Array';
%?CLASSMAP<Hash>    := 'Perl6Hash';
%?CLASSMAP<Pair>    := 'Perl6Pair';
%?CLASSMAP<Complex> := 'Perl6Complex';

method TOP($/) {
    my $past := $( $<statement_block> );
    $past.blocktype('declaration');
    declare_implicit_routine_vars($past);
    $past.lexical(0);

    #  Make sure we have the interpinfo constants.
    $past.unshift( PAST::Op.new( :inline('.include "interpinfo.pasm"') ) );

    # Set package for unit mainline
    $past.unshift(set_package_magical());

    # Create the unit's startup block.
    my $main := PAST::Block.new( :pirflags(':main') );
    $main.loadinit().push(
        PAST::Op.new( :inline('$P0 = compreg "Perl6"',
                              'unless null $P0 goto have_perl6',
                              'load_bytecode "perl6.pbc"',
                              'have_perl6:')
        )
    );

   # call the unit mainline, passing any arguments, and return
   # the result.  We force a tailcall here because we need a
   # :load sub (below) to occur last in the generated output, but don't
   # want it to be treated as the module's return value.
   $main.push(
       PAST::Op.new( :pirop('tailcall'),
           PAST::Op.new( :pirop('find_name'), '!UNIT_START' ),
           $past,
           PAST::Var.new( :scope('parameter'), :name('@_'), :slurpy(1) )
       )
    );

    # generate a :load sub that invokes this one, but does so _last_
    # (e.g., at the end of a load_bytecode operation)
    $main.push(
        PAST::Block.new( :pirflags(':load'), :blocktype('declaration'),
            PAST::Op.new(
                :inline( '.include "interpinfo.pasm"',
                         '$P0 = interpinfo .INTERPINFO_CURRENT_SUB',
                         '$P0 = $P0."get_outer"()',
                         '$P0()'
                )
            )
        )
    );
    $main.push( PAST::Stmts.new() );

    make $main;
}


method statement_block($/, $key) {
    our @?BLOCK;
    our $?BLOCK_OPEN;
    ##  when entering a block, use any $?BLOCK_OPEN if it exists,
    ##  otherwise create an empty block with an empty first child to
    ##  hold any parameters we might encounter inside the block.
    if $key eq 'open' {
        if $?BLOCK_OPEN {
            @?BLOCK.unshift( $?BLOCK_OPEN );
            $?BLOCK_OPEN := 0;
        }
        else {
            @?BLOCK.unshift( PAST::Block.new( PAST::Stmts.new(), :node($/)));
        }
    }
    if $key eq 'close' {
        my $past := @?BLOCK.shift();
        $past.push($($<statementlist>));
        make $past;
    }
}


method block($/) {
    my $past := $( $<statement_block> );
    unless $past<pkgdecl> {
        set_block_type($past, 'Block');
    }
    make $past;
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
    if $key eq 'control' {
        $past := $( $<statement_control> );
    }
    elsif $key eq 'null' {
        $past := PAST::Stmts.new();
    }
    else {
        my $sml;
        $past := $( $<expr> );
        if $past.isa(PAST::Block) && !$past.blocktype() {
            $past.blocktype('immediate');
        }
        if $key eq 'mod_cond' {
            my $body := $past;
            $past := $( $<statement_mod_cond> );
            $past.push( $body );
            $sml := $<statement_mod_loop>[0];
        }
        if $key eq 'mod_loop' { $sml := $<statement_mod_loop>; }
        if $sml {
            my $body := $past;
            if $sml<sym> eq 'for' {
                if !$body.isa(PAST::Block) {
                    $body := PAST::Block.new( PAST::Stmts.new(), $body );
                    $body.blocktype('immediate');
                }
                declare_implicit_function_vars( $body );
            }
            $past := $( $sml );
            $past.push( $body );
        }
    }
    make $past;
}


method statement_control($/, $key) {
    make $( $/{$key} );
}


method if_statement($/) {
    my $count := +$<xblock> - 1;
    my $past  := $( $<xblock>[$count] );
    declare_implicit_block_vars($past[1], 0);
    ## add any 'else' clause
    if $<pblock> {
        my $else := $( $<pblock>[0] );
        $else.blocktype('immediate');
        declare_implicit_block_vars($else, 0);
        $past.push( $else );
    }
    ## build if/then/elsif structure
    while $count != 0 {
        $count--;
        my $else := $past;
        $past := $( $<xblock>[$count] );
        declare_implicit_block_vars($past[1], 0);
        $past.push($else);
    }
    make $past;
}

method unless_statement($/) {
    my $past := $( $<xblock> );
    $past.pasttype('unless');
    declare_implicit_block_vars($past[1], 0);
    make $past;
}

method while_statement($/) {
    my $past := $( $<xblock> );
    $past.pasttype(~$<sym>);
    declare_implicit_block_vars($past[1], 0);
    make $past;
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
    my $past := $( $<xblock> );
    $past.push( $past.shift() );              # swap <EXPR> and <pblock>
    $past[0].blocktype('declaration');
    declare_implicit_function_vars($past[0]);
    $past.pasttype('call');
    make $past;
}

method when_statement($/) {
    my $block := $( $<block> );
    $block.blocktype('immediate');

    # Push a handler onto the innermost block so that we can exit if we
    # successfully match
    when_handler_helper($block);

    # Invoke smartmatch of the expression.
    my $match_past := process_smartmatch(
        PAST::Var.new( :name('$_') ),
        $( $<EXPR> ),
        $<EXPR><expr>
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
    my $block := $( $<block> );
    $block.blocktype('immediate');

    # Push a handler onto the innermost block so that we can exit if we
    # successfully match
    when_handler_helper($block);

    make $block;
}

sub when_handler_helper($block) {
    our @?BLOCK;
    my $?BLOCK := @?BLOCK[0];
    # XXX TODO: This isn't quite the right way to check this...
    unless $?BLOCK.handlers() {
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
        $?BLOCK.handlers(@handlers);
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

method loop_statement($/) {
    my $block := $( $<block> );
    $block.blocktype('immediate');
    my $cond  := $<e2> ?? $( $<e2>[0] ) !! 1;
    my $loop := PAST::Op.new( $cond, $block, :pasttype('while'), :node($/) );
    if $<e3> {
        $loop.push( $( $<e3>[0] ) );
    }
    if $<e1> {
        $loop := PAST::Stmts.new( $( $<e1>[0] ), $loop, :node($/) );
    }
    make $loop;
}

method for_statement($/) {
    my $past := $( $<xblock> );
    $past.pasttype('for');
    $past[0] := PAST::Op.new(:name('list'), $past[0]);
    declare_implicit_function_vars($past[1]);
    make $past;
}

method pblock($/) {
    my $block := $( $<block> );
    ##  Add a call to !SIGNATURE_BIND to fixup params and do typechecks.
    if $block<signature> {
        $block[0].push(
            PAST::Op.new( :pasttype('call'), :name('!SIGNATURE_BIND') )
        );
        if $<lambda>[0] eq '<->' {
            $block.loadinit().push(PAST::Op.new(
                :pasttype('callmethod'),
                :name('!make_parameters_rw'),
                PAST::Var.new(
                    :name('signature'),
                    :scope('register')
                )
            ));
        }
    }
    ##  If block has no statements, need to return an undef (so we don't
    ##  get a null PMC access) if it's a lambda (in the non-lambda case,
    ##  it may be a Hash composer).
    if $<lambda> {
        prevent_null_return($block);
    }
    make $block;
}

method xblock($/) {
    my $pblock := $( $<pblock> );
    $pblock.blocktype('immediate');
    prevent_null_return($pblock);
    my $past := PAST::Op.new(
        $( $<EXPR> ), $pblock,
        :pasttype('if'),
        :node( $/ )
    );
    make $past;
}

method use_statement($/) {
    my $name := ~$<name>;
    my $past;
    if $name ne 'v6' && $name ne 'lib' {
        ##  Create a loadinit node so the use module is loaded
        ##  when this module is loaded...
        our @?BLOCK;
        @?BLOCK[0].loadinit().push(
            PAST::Op.new(
                PAST::Val.new( :value($name) ),
                :name('use'),
                :pasttype('call'),
                :node( $/ )
            )
        );
        ##  ...and load it immediately to get its BEGIN semantics
        ##  and symbols for the current compilation.
        use($name);
    }
    $past := PAST::Stmts.new( :node($/) );
    make $past;
}

method begin_statement($/) {
    my $past := $( $<block> );
    $past.blocktype('declaration');
    declare_implicit_routine_vars($past);                  # FIXME
    my $sub := PAST::Compiler.compile( $past );
    $sub();
    # XXX - should emit BEGIN side-effects, and do a proper return()
    make PAST::Block.new();
}

method end_statement($/) {
    my $past := $( $<block> );
    $past.blocktype('declaration');
    declare_implicit_routine_vars($past);
    $past.loadinit().push(
        PAST::Op.new(
            :pasttype('callmethod'),
            :name('push'),
            PAST::Var.new(
                :namespace('Perl6'),
                :name('@?END_BLOCKS'),
                :scope('package')
            ),
            PAST::Var.new(
                :name('block'),
                :scope('register')
            )
        )
    );
    make $past;
}

method catch_statement($/) {
    my $past := $( $<block> );
    $past.blocktype('immediate');
    $past := PAST::Stmts.new(
        PAST::Op.new(
            :pasttype('bind'),
            PAST::Var.new( :name('$_'), :scope('lexical') ),
            PAST::Var.new( :name('exception'), :scope('register') )
        ),
        PAST::Op.new(
            :pasttype('bind'),
            PAST::Var.new( :name('$!'), :scope('lexical') ),
            PAST::Var.new( :name('exception'), :scope('register') )
        ),
        $past
    );
    our @?BLOCK;
    my $?BLOCK := @?BLOCK[0];
    my $eh := PAST::Control.new( $past );
    my @handlers;
    if $?BLOCK.handlers() {
        @handlers := $?BLOCK.handlers();
    }
    @handlers.unshift($eh);
    $?BLOCK.handlers(@handlers);
    make PAST::Stmts.new();
}

method control_statement($/) {
    my $past := $( $<block> );
    $past.blocktype('immediate');
    $past := PAST::Stmts.new(
        PAST::Op.new(
            :pasttype('bind'),
            PAST::Var.new( :name('$_'), :scope('lexical') ),
            PAST::Var.new( :name('exception'), :scope('register') )
        ),
        PAST::Op.new(
            :pasttype('bind'),
            PAST::Var.new( :name('$!'), :scope('lexical') ),
            PAST::Var.new( :name('exception'), :scope('register') )
        ),
        $past
    );
    our @?BLOCK;
    my $?BLOCK := @?BLOCK[0];
    my $eh := PAST::Control.new(
        $past,
        :handle_types('CONTROL')
    );
    my @handlers;
    if $?BLOCK.handlers() {
        @handlers := $?BLOCK.handlers();
    }
    @handlers.unshift($eh);
    $?BLOCK.handlers(@handlers);
    make PAST::Stmts.new();
}

method statement_mod_loop($/) {
    my $expr := $( $<EXPR> );
    my $sym := ~$<sym>;

    if $sym eq 'given' {
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
    elsif $sym eq 'for' {
        my $past := PAST::Op.new(
            PAST::Op.new($expr, :name('list')),
            :pasttype($sym),
            :node( $/ )
        );
        make $past;
    }
    else {
        make PAST::Op.new(
            $expr,
            :pasttype( $sym ),
            :node( $/ )
        );
    }
}


method statement_mod_cond($/) {
    my $sym := ~$<sym>;
    my $expr := $( $<EXPR> );
    if $sym eq 'when' {
        $expr := PAST::Op.new(
                     PAST::Var.new( :name('$_'), :scope('lexical') ),
                     $expr,
                     :name('infix:~~'),
                     :pasttype('call'),
                     :node($/)
                 );
        $sym := 'if';
    }
    make PAST::Op.new( $expr, :pasttype($sym), :node($/) );
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
        my $catchpir := "    .get_results (%r)\n    store_lex '$!', %r";
        $past.push( PAST::Op.new( :inline( $catchpir ) ) );

        ##  Add an 'else' node to the try op that clears $! if
        ##  no exception occurred.
        my $elsepir  := "    new %r, 'Failure'\n    store_lex '$!', %r";
        $past.push( PAST::Op.new( :inline( $elsepir ) ) );
    }
    elsif $sym eq 'gather' {
        if !$past.isa(PAST::Block) {
            $past := PAST::Block.new($past)
        }
        $past.blocktype('declaration');
        $past := PAST::Op.new( $past, :pasttype('call'),
                               :name('gather'), :node($/) );
    }
    else {
        $/.panic( $sym ~ ' not implemented');
    }
    make $past;
}


method multi_declarator($/) {
    my $sym  := ~$<sym>;
    my $past :=  $<declarator> ?? $( $<declarator> ) !! $( $<routine_def> );

    if $past.isa(PAST::Block) {
        # If we have a multi declarator, must have a named routine too.
        if $sym ne "" && $past.name() eq "" {
            $/.panic("'" ~ $<sym> ~ "' can only be used on named routines");
        }

        # If we're declaring a multi or a proto, flag the sub as :multi,
        # and transform the sub's container to a Perl6MultiSub.
        if $sym eq 'multi' || $sym eq 'proto' {
            transform_to_multi($past);
        }

        # Protos also need the proto property setting on them, plus we note
        # that we have one in scope.
        if $<sym> eq 'proto' {
            $past.loadinit().push(
                PAST::Op.new(:inline('    setprop block, "proto", %0'), 1)
            );
            our @?BLOCK;
            @?BLOCK[0].symbol($past.name(), :does_callable(1), :is_proto(1));
        }

        # If it's just a routine, need to mark it as a sub and make sure we
        # bind its signature.
        if $<routine_def> {
            set_block_type($past, 'Sub');
            $past[0].push(
                PAST::Op.new( :pasttype('call'), :name('!SIGNATURE_BIND') )
            );
        }
    }

    make $past;
}


method enum_declarator($/, $key) {
    my $values := $( $/{$key} );

    my $name := ~$<name>[0];
    if $name {
        # It's a named enumeration. Ensure the type isn't already declared.
        if $/.type_redeclaration() {
            $/.panic("Re-declaration of type " ~ $name);
        }

        # Get a mapping of all the names we will introduce with this enumeration to their
        # values. We'll compute these at compile time, so then we can build as much of the
        # enum as possible as PAST at compile time too. Note that means that, like a
        # BEGIN block, we will compile, run and get the return value now.
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

        # Now we need to emit a role of the name of the enum containing:
        #  * One attribute with the same name as the enum
        #  * A method of the same name as the enum
        #  * Methods for each name introduced by the enum that compare the
        #    attribute with the value of that name.
        my $role_past := PAST::Stmts.new(
            PAST::Op.new(
                :pasttype('bind'),
                PAST::Var.new(
                    :name('def'),
                    :scope('register'),
                    :isdecl(1)
                ),
                PAST::Op.new(
                    :pasttype('call'),
                   :name('!keyword_role'),
                    PAST::Val.new( :value($name) )
                )
            ),
            PAST::Op.new(
                :pasttype('call'),
                :name('!keyword_has'),
                PAST::Op.new(
                    :pasttype('callmethod'),
                    :name('!select'),
                    PAST::Var.new(
                        :name('def'),
                        :scope('register')
                    )
                ),
                PAST::Val.new( :value("$!" ~ $name) ),
                # XXX Set declared type here, when we parse that.
                PAST::Var.new(
                    :name('Object'),
                    :scope('package')
                )
            ),
            PAST::Op.new(
                :pasttype('callmethod'),
                :name('add_method'),
                PAST::Op.new(
                    :pasttype('callmethod'),
                    :name('!select'),
                    PAST::Var.new(
                        :name('def'),
                        :scope('register')
                    )
                ),
                PAST::Val.new( :value($name) ),
                make_accessor($/, undef, "$!" ~ $name, 1, 'attribute')
            )
        );
        for %values.keys() {
            # Method for this value.
            $role_past.push(PAST::Op.new(
                :pasttype('callmethod'),
                :name('add_method'),
                PAST::Op.new(
                    :pasttype('callmethod'),
                    :name('!select'),
                    PAST::Var.new(
                        :name('def'),
                        :scope('register')
                    )
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
                                :name("$!" ~ $name),
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
                    :name('class_def'),
                    :scope('register'),
                    :isdecl(1)
                ),
                PAST::Op.new(
                    :pasttype('call'),
                    :name('!keyword_enum'),
                    PAST::Op.new(
                        :pasttype('callmethod'),
                        :name('!select'),
                        PAST::Var.new(
                            :name('def'),
                            :scope('register')
                        )
                    )
                )
            ),
            PAST::Op.new(
                :inline('    setprop %0, "enum", %1'),
                PAST::Var.new(
                    :name('class_def'),
                    :scope('register')
                ),
                PAST::Var.new(
                    :name('def'),
                    :scope('register')
                )
            )
        );

        # Want to give the class an invoke method that returns the enum value,
        # and get_string, get_number and get_integer v-table overrides to we
        # can get data from it..
        $class_past.push(PAST::Op.new(
            :pasttype('callmethod'),
            :name('add_vtable_override'),
            PAST::Var.new(
                :scope('register'),
                :name('class_def')
            ),
            'invoke',
            PAST::Block.new(
                :blocktype('declaration'),
                :pirflags(":method"),
                PAST::Var.new(
                    :name("$!" ~ $name),
                    :scope('attribute')
                )
            )
        ));
        $class_past.push(PAST::Op.new(
            :pasttype('callmethod'),
            :name('add_vtable_override'),
            PAST::Var.new(
                :scope('register'),
                :name('class_def')
            ),
            'get_string',
            PAST::Block.new(
                :blocktype('declaration'),
                :pirflags(":method"),
                PAST::Op.new(
                    :pasttype('call'),
                    :name('prefix:~'),
                    PAST::Var.new(
                        :name("$!" ~ $name),
                        :scope('attribute')
                    )
                )
            )
        ));
        $class_past.push(PAST::Op.new(
            :pasttype('callmethod'),
            :name('add_vtable_override'),
            PAST::Var.new(
                :scope('register'),
                :name('class_def')
            ),
            'get_integer',
            PAST::Block.new(
                :blocktype('declaration'),
                :pirflags(":method"),
                PAST::Op.new(
                    :pasttype('call'),
                    :name('prefix:+'),
                    PAST::Var.new(
                        :name("$!" ~ $name),
                        :scope('attribute')
                    )
                )
            )
        ));
        $class_past.push(PAST::Op.new(
            :pasttype('callmethod'),
            :name('add_vtable_override'),
            PAST::Var.new(
                :scope('register'),
                :name('class_def')
            ),
            'get_number',
            PAST::Block.new(
                :blocktype('declaration'),
                :pirflags(":method"),
                PAST::Op.new(
                    :pasttype('call'),
                    :name('prefix:+'),
                    PAST::Var.new(
                        :name("$!" ~ $name),
                        :scope('attribute')
                    )
                )
            )
        ));

        # Now we need to create instances of each of these and install them
        # in a package starting with the enum's name, plus an alias to them
        # in the current package. Register the symbols in the current block
        # as we go.
        our @?BLOCK;
        for %values.keys() {
            # Instantiate with value.
            $class_past.push(PAST::Op.new(
                :pasttype('bind'),
                PAST::Var.new(
                    :name($_),
                    :namespace($name),
                    :scope('package')
                ),
                PAST::Op.new(
                    :pasttype('callmethod'),
                    :name('new'),
                    PAST::Var.new(
                        :name('class_def'),
                        :scope('register')
                    ),
                    PAST::Val.new(
                        :value(%values{$_}),
                        :named( PAST::Val.new( :value("$!" ~ $name) ) )
                    )
                )
            ));
            @?BLOCK[0].symbol($name ~ '::' ~ $_, :does_abstraction(1));

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
                    :namespace($name),
                    :scope('package')
                )
            ));
            @?BLOCK[0].symbol($_, :does_abstraction(1));
        }

        # Assemble all that we build into a statement list and then place it
        # into the init code.
        my $loadinit := @?BLOCK[0].loadinit();
        $loadinit.push($role_past);
        $loadinit.push($class_past);

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


method routine_declarator($/, $key) {
    my $past;
    if $key eq 'sub' {
        $past := $($<routine_def>);
        set_block_type($past, 'Sub');
    }
    elsif $key eq 'method' {
        $past := $($<method_def>);
        set_block_type($past, 'Method');
    }
    elsif $key eq 'submethod' {
        $past := $($<method_def>);
        set_block_type($past, 'Submethod');
    }
    $past.node($/);
    if (+@($past[1])) {
        declare_implicit_routine_vars($past);
    }
    else {
        $past[1].push( PAST::Op.new( :name('list') ) );
    }
    ##  Add a call to !SIGNATURE_BIND to fixup params and do typechecks.
    $past[0].push(
        PAST::Op.new( :pasttype('call'), :name('!SIGNATURE_BIND') )
    );
    ##  If we have a proto in scope of this name, then we need to make this a
    ##  multi.
    if $past.name() ne "" {
        my $sym := outer_symbol($past.name());
        if $sym && $sym<does_callable> && $sym<is_proto> {
            transform_to_multi($past);
        }
    }
    make $past;
}


method routine_def($/) {
    my $block := $( $<block> );
    $block.blocktype('declaration');
    if $<deflongname> {
        my $name := ~$<deflongname>[0];
        $block.name( $name );
        our @?BLOCK;
        @?BLOCK[0].symbol( $name, :scope('package') );
    }
    $block.control('return_pir');
    block_signature($block);

    if $<trait> {
        my $loadinit := $block.loadinit();
        my $blockreg := PAST::Var.new( :name('block'), :scope('register') );
        for @($<trait>) {
            #  Trait nodes come in as PAST::Op( :name('list') ).
            #  We just modify them to call !sub_trait and add
            #  'block' as the first argument.
            my $trait := $( $_ );
            $trait.name('!sub_trait');
            $trait.unshift($blockreg);
            $loadinit.push($trait);
        }
    }
    make $block;
}


method method_def($/) {
    my $block := $( $<block> );
    $block.blocktype('method');

    if $<longname> {
        $block.name( ~$<longname> );
    }

    # Add lexical 'self'.
    $block[0].unshift(
        PAST::Var.new( :name('self'), :scope('lexical'), :isdecl(1),
            :viviself( PAST::Var.new( :name('self'), :scope('register' ) ) )
        )
    );

    $block.control('return_pir');
    block_signature($block);
    # Ensure there's an invocant in the signature.
    $block.loadinit().push(PAST::Op.new(
        :pasttype('callmethod'),
        :name('!add_implicit_self'),
        PAST::Var.new( :name('signature'), :scope('register') )
    ));

    if $<trait> {
        my $loadinit := $block.loadinit();
        my $blockreg := PAST::Var.new( :name('block'), :scope('register') );
        for @($<trait>) {
            #  Trait nodes come in as PAST::Op( :name('list') ).
            #  We just modify them to call !sub_trait and add
            #  'block' as the first argument.
            my $trait := $( $_ );
            $trait.name('!sub_trait');
            $trait.unshift($blockreg);
            $loadinit.push($trait);
        }
    }

    make $block;
}


method trait($/) {
    my $past;
    if $<trait_auxiliary> {
        $past := $( $<trait_auxiliary> );
    }
    elsif $<trait_verb> {
        $past := $( $<trait_verb> );
    }
    make $past;
}


method trait_auxiliary($/) {
    my $sym   := ~$<sym>;
    my $trait := PAST::Op.new( :name('infix:,'), 'trait_auxiliary:' ~ $sym);
    if $sym eq 'is' {
        $trait.push( ~$<name> );
        if $<postcircumfix> {
            my $arg := $( $<postcircumfix>[0] );
            $arg.name('!capture');
            $trait.push($arg);
        }
    }
    elsif $sym eq 'does' {
        $trait.push( ~$<name> );
        if $<EXPR> {
            for @(build_call($( $<EXPR>[0] ))) {
                if $_.returns() eq 'Pair' {
                    $_[1].named($_[0]);
                    $trait.push($_[0]);
                }
                else {
                    $trait.push($_);
                }
            }
        }
    }
    make $trait;
}


method trait_verb($/) {
    my $sym := ~$<sym>;
    my $value;
    if $sym eq 'handles' { $value := $( $<EXPR> ); }
    else { $value := $( $<typename> ); }
    make PAST::Op.new( :name('infix:,'), 'trait_verb:' ~ $sym, $value );
}


method signature($/, $key) {
    our @?BLOCK;
    if $key eq 'open' {
        our $?BLOCK_OPEN;
        my $sigpast := PAST::Op.new( :pasttype('stmts'), :node($/) );
        my $block;
        if $?BLOCK_OPEN {
            $block := $?BLOCK_OPEN;
            $?BLOCK_OPEN := 0;
            $block.unshift( $sigpast);
        }
        else {
            $block := PAST::Block.new( $sigpast, :blocktype('declaration') );
        }
        $block<explicit_signature> := 1;
        @?BLOCK.unshift($block);
    }
    else {
        my $block    := @?BLOCK.shift();
        my $sigpast := $block[0];
        my $loadinit := $block.loadinit();
        my $sigobj   := PAST::Var.new( :name('signature'), :scope('register') );

        block_signature($block);

        ##  loop through parameters of signature
        my $arity := $<parameter> ?? +@($<parameter>) !! 0;
        $block.arity($arity);
        my $i                  := 0;
        my $multi_inv_suppress := 0;
        while $i < $arity {
            my $var    := $( $<parameter>[$i] );
            my $name   := $var.name();

            if $var<type_binding> {
                $sigpast.push( $var<type_binding> );
            }

            ##  add parameter to the signature object
            my $sigparam := PAST::Op.new( :pasttype('callmethod'),
                                :name('!add_param'), $sigobj, $name );

            ##  if it's named or optional, note that in the signature object
            if $var.named() ne "" {
                $sigparam.push(PAST::Val.new( :value($var.named()), :named('named') ));
            }
            if $var.viviself() {
                $sigparam.push(PAST::Val.new( :value(1), :named('optional') ));
            }

            ##  add any typechecks
            my $type := $var<type>;
            if +@($type) > 0 {
                $type.named('type');
                $sigparam.push($type);
            }

            ##  add traits (we're not using this yet.)
            my $trait := $var<trait>;
            if $trait {
                $trait.named('trait');
                $sigparam.push($trait);
            }

            my $readtype := trait_readtype( $var<traitlist> );
            if $readtype eq 'CONFLICT' {
                $<parameter>[$i].panic(
                    "Can use only one of readonly, rw, and copy on "
                    ~ $name ~ " parameter"
                );
            }
            if $readtype {
                $sigparam.push(PAST::Val.new(:value($readtype),:named('readtype')));
            }

            ##  if it's an invocant, flag it as such and make the var be a
            ##  lexical that has self register bound to it
            if $<param_sep>[$i][0] eq ':' {
                if $i == 0 {
                    $sigparam.push(PAST::Val.new( :value(1), :named('invocant')));
                    $var.scope('lexical');
                    $var.isdecl(1);
                    $var.viviself(
                        PAST::Var.new( :name('self'), :scope('register') )
                    )
                }
                else {
                    $/.panic("Can only use : separator to denote invocant after first parameter.");
                }
            }

            ##  handle end of multi-invocant sequence
            if ($multi_inv_suppress) {
                $sigparam.push(PAST::Val.new(:value(0),:named('multi_invocant')));
            }
            if $<param_sep>[$i][0] eq ';;' { $multi_inv_suppress := 1; }

            ##  add var node to block
            $sigpast.push( $var );

            $loadinit.push($sigparam);
            $i++;
        }

        ##  restore block stack and return signature ast
        our $?BLOCK_OPEN;
        $?BLOCK_OPEN := $block;
        make $sigpast;
    }
}


method type_constraint($/) {
    my $past;
    if $<fulltypename> {
        $past := $( $<fulltypename> );
    }
    else {
        $past := make_anon_subtype($( $<EXPR> ));
    }
    make $past;
}


method post_constraint($/) {
    my $past := make_anon_subtype($( $<EXPR> ));
    make $past;
}


method parameter($/) {
    my $var   := $( $<param_var> );
    my $sigil := $<param_var><sigil>;
    my $quant := $<quant>;

    ##  if it was type a type capture and nothing else, need to make a PAST::Var
    unless $<param_var> {
        unless $<type_constraint> == 1 {
            $/.panic("Invalid signature; cannot have two consecutive parameter separators.");
        }
        our @?BLOCK;
        my $name := ~$<type_constraint>[0];
        $var     := PAST::Var.new( :scope('parameter') );
        $var.name($var.unique());
        @?BLOCK[0].symbol( $var.name(), :scope('lexical') );
    }

    ##  handle slurpy and optional flags
    if $quant eq '*' {
        $var.slurpy( $sigil eq '@' || $sigil eq '%' );
        $var.named( $sigil eq '%' );
    }
    elsif $<named> eq ':' {          # named
        $var.named(~$<param_var><identifier>);
        if $quant ne '!' {      #  required (optional is default)
            $var.viviself(container_itype($sigil));
        }
    }
    elsif $quant eq '?' {           # positional optional
        $var.viviself(container_itype($sigil));
    }

    ##  handle any default value
    if $<default_value> {
        if $quant eq '!' {
            $/.panic("Can't put a default on a required parameter");
        }
        if $quant eq '*' {
            $/.panic("Can't put a default on a slurpy parameter");
        }
        $var.viviself( $( $<default_value>[0]<EXPR> ) );
    }

    ##  keep track of any type constraints
    my $typelist := PAST::Op.new( :name('all'), :pasttype('call') );
    $var<type> := $typelist;
    if $<type_constraint> {
        for @($<type_constraint>) {
            my $type_past := $( $_ );
            if $type_past.isa(PAST::Var) && $type_past.scope() eq 'lexical' {
                our @?BLOCK;
                # Lexical type constraint.
                if $type_past.isdecl() {
                    # If it's a declaration, we need to initialize it.
                    $type_past.viviself(
                        PAST::Op.new( :pasttype('callmethod'), :name('WHAT'),
                            PAST::Var.new( :name($var.name()) )
                        )
                    );
                    $var<type_binding> := $type_past;
                    @?BLOCK[0].symbol( $type_past.name(), :scope('lexical') );
                }
                else {
                    # we need to thunk it
                    my $thunk := PAST::Op.new(
                        :name('ACCEPTS'), :pasttype('callmethod'),
                        $type_past,
                        PAST::Var.new( :name('$_'), :scope('parameter') )
                    );
                    $thunk := PAST::Block.new($thunk, :blocktype('declaration'));
                    @?BLOCK[0].push($thunk);
                    $type_past := PAST::Val.new( :value($thunk) );
                    $typelist.push( $type_past );
                }
            }
            else {
                $typelist.push( $type_past );
            }
        }
    }
    if $<post_constraint> {
        for @($<post_constraint>) {
            $typelist.push($( $_ ));
        }
    }

    if $<trait> {
        my $traitlist := PAST::Op.new( :name('infix:,'), :pasttype('call') );
        $var<traitlist> := $traitlist;
        for @($<trait>) { $traitlist.push( $( $_ ) ); }
    }

    make $var;
}


method param_var($/) {
    my $sigil  := ~$<sigil>;
    my $twigil := ~$<twigil>[0];
    if $sigil eq '&' { $sigil := ''; }
    my $name := $sigil ~ $twigil ~ ~$<identifier>;
    if $twigil eq '.' {
        $name := $sigil ~ '!' ~ $<identifier>;
    }
    elsif $twigil && $twigil ne '!' {
        $/.panic('Invalid twigil used in signature parameter.');
    }
    my $var := PAST::Var.new(
        :name($name),
        :scope('parameter'),
        :node($/)
    );
    $var<twigil> := $twigil;
    $var<itype>  := container_itype( $<sigil> );
    # Declare symbol as lexical in current (signature) block.
    # This is needed in case any post_constraints try to reference
    # this new param_var.
    our @?BLOCK;
    @?BLOCK[0].symbol( $name, :scope('lexical') );

    make $var;
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
                :namespace(list()),
                :scope('package'),
                :node($/)
            )
        );
    }
    else {
        $past := $( $/{$key} );
    }

    if $<post> {
        for $<post> {
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
            elsif $past<invocant_holder> {
                $past<invocant_holder>.unshift($term);
            }
            else {
                $past.unshift($term);
            }
        }
    }
    make $past;
}


method post($/, $key) {
    make $( $/{$key} );
}


method dotty($/, $key) {
    my $past;

    if $key eq '.' {
        # Just a normal method call.
        $past := $( $<dottyop> );
    }
    elsif $key eq '!' {
        # Private method call. Need to put ! on the start of the name
        # (unless it was call to a code object, in which case we don't do
        # anything more).
        $past := $( $<methodop> );
        my $methodop := $<methodop>;
        if $methodop<name> {
            $past.name('!' ~ $past.name());
        }
        elsif $methodop<quote> {
            $past.name(
                PAST::Op.new(
                    :pasttype('call'),
                    :name('infix:~'),
                    '!',
                    $past.name()
                )
            );
        }
    }
    elsif $key eq '.*' {
        $past := $( $<dottyop> );
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

    # We actually need to send dispatches for named method calls (other than .*)
    # through .HOW.dispatch.
    if $key ne '.*' && $past.pasttype() eq 'callmethod' && $past.name() ne "" {
        $past.unshift($past.name());
        $past.name('dispatch');
        my $inv_var := PAST::Var.new( :scope('register') );
        $past.unshift($inv_var);
        $past.unshift(PAST::Op.new(
            :pasttype('callmethod'),
            :name('HOW'),
            $inv_var
        ));
        my $inv_set_node := PAST::Op.new(
            :pasttype('bind'),
            $inv_var,
            PAST::Stmts.new()
        );
        $past := PAST::Stmts.new( $inv_set_node, $past );
        $past<invocant_holder> := $inv_set_node[1];
    }
    else {
        $past<invocant_holder> := $past;
    }

    make $past;
}


method dottyop($/, $key) {
    make $( $/{$key} );
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
        $past.name(~$<name>);
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
        $past := PAST::Op.new( :name('postcircumfix:[ ]'), :node($/) );
        if $<semilist><EXPR> {
            my $slice := $( $<semilist> );
            $past.push( PAST::Block.new( $slice, :blocktype('declaration') ) );
        }
    }
    elsif $key eq '( )' {
        $past := build_call( $( $<semilist> ) );
        $past.node($/);
    }
    elsif $key eq '{ }' {
        $past := build_call( $( $<semilist> ) );
        $past.node($/);
        $past.name('postcircumfix:{ }');
    }
    elsif $key eq '< >' {
        $past := build_call( $( $<quote_expression> ) );
        $past.node($/);
        $past.name('postcircumfix:{ }');
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
        $past<invocant_holder>.unshift(PAST::Var.new(
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


method package_declarator($/, $key) {
    our @?PKGDECL;
    our @?NS;
    my $sym := ~$<sym>;
    my $past;
    if $key eq 'open' {
        our $?BLOCK_OPEN;
        $?BLOCK_OPEN := PAST::Block.new( PAST::Stmts.new(), :node($/) );
        $?BLOCK_OPEN<pkgdecl> := $sym;
        @?PKGDECL.unshift( $sym );
    }
    else {
        make $( $<package_def> );
        @?PKGDECL.shift();
    }
}


method package_def($/, $key) {
    our @?PKGDECL;
    my $?PKGDECL := @?PKGDECL[0];
    our @?NS;

    if $key eq 'panic' {
        $/.panic("Unable to parse " ~ $?PKGDECL ~ " definition");
    }

    # At block opening, unshift module name (fully qualified) onto @?NS; otherwise,
    # shift it off.
    if $key eq 'open' {
        my $fqname := +@?NS ?? @?NS[0] ~ '::' ~ ~$<module_name>[0] !! ~$<module_name>[0];
        @?NS.unshift($fqname);
        return 0;
    }
    else {
        @?NS.shift();
    }

    my $block := $( $/{$key} );
    $block.lexical(0);

    my $modulename := $<module_name>
                         ?? ~$<module_name>[0] !!
                         $block.unique('!ANON');
    if +@?NS > 0 {
        $modulename := @?NS[0] ~ '::' ~ $modulename;
    }

    # See note at top of file for %?CLASSMAP.
    if %?CLASSMAP{$modulename} { $modulename := %?CLASSMAP{$modulename}; }

    if $?PKGDECL eq 'role' {
        # Parametric roles need to have their bodies evaluated per type-
        # parmeterization, and are "invoked" by 'does'. We make them
        # multis, and ensure they have a signature. XXX Need to put
        # $?CLASS as first item in signature always too.
        $block.blocktype('declaration');

        # Also need to put this (possibly parameterized) role into the
        # set of possible roles.
        $block.loadinit().push(
            PAST::Op.new( :name('!ADDTOROLE'), :pasttype('call'),
                PAST::Var.new( :name('block'), :scope('register') )
            )
        );

        # And if there's no signature, make sure we set one up and add [] to
        # the namespace name.
        if substr($modulename, -1, 1) ne ']' {
            $modulename := $modulename ~ '[]';
            block_signature($block);
        }
    }
    elsif $key eq 'block' {
        # A normal block acts like a BEGIN and is executed ASAP.
        $block.blocktype('declaration');
        $block.pirflags(':load :init');
    }
    elsif $key eq 'statement_block' {
        # file-level blocks have their contents as the compunit mainline
        if !$<module_name> {
            $/.panic("Compilation unit cannot be anonymous");
        }
        $block.blocktype('immediate');
    }

    if ($modulename) {
        $block.namespace( Perl6::Compiler.parse_name($modulename) );
    }

    #  Create a node at the beginning of the block's initializer
    #  for package initializations
    my $init := PAST::Stmts.new();
    $block[0].unshift( $init );

    #  Normally we would create the metaclass object first,
    #  but if there's an "is also" trait we want to do a class
    #  lookup instead.  So we do the trait processing first
    #  (scanning for 'is also' as we go), and then decide how
    #  to obtain the metaclass.

    #  Add any traits coming from the package declarator.
    #  Traits in the body have already been added to the block.
    our $?METACLASS;
    if $<trait> {
        for @($<trait>) {
            #  Trait nodes come in as PAST::Op( :name('list') ).
            #  We just modify them to call !meta_trait and add
            #  the metaclass as the first argument.
            my $trait := $( $_ );
            if $trait[1] eq 'also' { $block<isalso> := 1; }
            else {
                ##  If it is a trait_auxiliary:does or a trait_auxiliary:is we
                ##  should check the name is a type.
                if $trait[0] eq 'trait_auxiliary:is' || $trait[0] eq 'trait_auxiliary:does' {
                    unless $/.is_type($trait[1]) {
                        $_.panic("The type " ~ $trait[1] ~ " does not exist.");
                    }
                }
                $trait.name('!meta_trait');
                $trait.unshift($?METACLASS);
                $init.push($trait);
            }
        }
    }

    #  If it's not an "is also", have a name and aren't a role (since they can
    #  have many declarations) we need to check it's not a duplicate.
    if !$block<isalso> && $<module_name> && $?PKGDECL ne 'role' {
        if $/.type_redeclaration() {
            $/.panic("Re-declaration of type " ~ ~$<module_name>[0]);
        }
    }

    ##  If it is an "is also", check that the type did already exist.
    if $block<isalso> && !$/.type_redeclaration() {
        $/.panic("Cannot use 'is also' on non-existent class " ~ ~$<module_name>[0]);
    }

    #  At the beginning, create the "class/module/grammar/role/etc"
    #  metaclass handle on which we do the other operations.
    $init.unshift(
        PAST::Op.new( :pasttype('bind'),
            PAST::Var.new(:name('metaclass'), :scope('register'), :isdecl(1) ),
            PAST::Op.new(:name('!meta_create'),
                $?PKGDECL, $modulename, +$block<isalso>
            )
        )
    );

    #  ...and at the end of the block's initializer (after any other
    #  items added by the block), we finalize the composition.
    if $?PKGDECL eq 'role' {
        #  For a role, we now need to produce a new one which clones the original,
        #  but without the methods. Then we need to add back the methods. We emit
        #  PIR here to do it rather than doing a call, since we need to call
        #  new_closure from the correct scope.
        $block[0].push(PAST::Op.new(:inline(
                '    "!meta_compose"(%0)',
                '    .local pmc orig_role, meths, meth_iter',
                '    orig_role = getprop "$!orig_role", %0',
                '    meths = orig_role."methods"()',
                '    meth_iter = iter meths',
                '  it_loop:',
                '    unless meth_iter goto it_loop_end',
                '    $S0 = shift meth_iter',
                '    $P0 = meths[$S0]',
                '    $P1 = getprop "$!signature", $P0',
                '    $P0 = newclosure $P0',
                '    setprop $P0, "$!signature", $P1',
                '    %0."add_method"($S0, $P0)',
                '    goto it_loop',
                '  it_loop_end:',
                '    .return (%0)'
            ),
            $?METACLASS
        ));
    }
    elsif $<module_name> eq "" && ($?PKGDECL eq 'class' || $?PKGDECL eq 'grammar') {
        #  We need to keep the proto around and return it at the end of
        #  initialization for anonymous classes.
        $block[0].push(PAST::Op.new(
            :pasttype('bind'),
            PAST::Var.new(:name('proto_store'), :scope('register'), :isdecl(1)),
            PAST::Op.new( :name('!meta_compose'), $?METACLASS)
        ));
        $block.push(PAST::Var.new(:name('proto_store'), :scope('register')));
        $block.blocktype('immediate');
        $block.pirflags('');
    }
    else {
        $block[0].push( PAST::Op.new( :name('!meta_compose'), $?METACLASS) );
    }

    make $block;
}


method scope_declarator($/) {
    our @?BLOCK;
    my $block := @?BLOCK[0];
    my $sym   := ~$<sym>;
    my $past  := $( $<scoped> );
    my $scope := 'lexical';
    if    $sym eq 'our' { $scope := 'package'; }
    elsif $sym eq 'has' { $scope := 'attribute'; }

    #  Private methods get a leading !.
    if $scope eq 'lexical' && $past.isa(PAST::Block)
        && $past.blocktype() eq 'method' {
            $past.name( '!' ~ $past.name());
    }

    #  If we have a single variable, we temporarily pack it into
    #  a PAST::Op node (like a signature of one variable) and
    #  let the PAST::Op code below handle it.  It then gets
    #  unpacked at the end.
    if $past.isa(PAST::Var) {
        $past := PAST::Op.new( $past );
    }

    if $past.isa(PAST::Op) {
        my $i := 0;
        for @($past) {
            if $_.isa(PAST::Var) && !$_<redecl> {
                my $var := $_;

                # This is a variable declaration, so we set the scope in
                # the block's symbol table as well as the variable itself.
                $block.symbol( $var.name(), :scope($scope) );
                $var.scope($scope);
                $var.isdecl(1);
                if $scope eq 'package' { $var.lvalue(1); }
                my $init_value := $var.viviself();
                my $type;
                if +@($var<type>) { $type := $var<type>[0]; }  # FIXME

                # If the var has a '.' twigil, we need to create an
                # accessor method for it in the block (class/grammar/role)
                if $var<twigil> eq '.' {
                    my $method := PAST::Block.new( :blocktype('method') );
                    $method.name( substr($var.name(), 2) );
                    my $value := PAST::Var.new( :name($var.name()) );
                    my $readtype := trait_readtype( $var<traitlist> ) || 'readonly';
                    if $readtype eq 'CONFLICT' {
                        $<scoped>.panic(
                            "Can use only one of readonly, rw, and copy on "
                            ~ $var.name() ~ " parameter"
                        );
                    }
                    elsif $readtype ne 'rw' {
                        $value := PAST::Op.new( :pirop('new PsP'),
                                      'ObjectRef', $value);
                        $value := PAST::Op.new( :pirop('setprop'),
                                      $value, 'readonly', 1);
                    }
                    $method.push( $value );
                    $block[0].push($method);
                }

                if $scope eq 'attribute' {
                    # If no twigil, we need a twigiled entry of
                    # the attribute in the block's symbol table.
                    if $var<twigil> eq '' {
                        my $sigil := substr($var.name(), 0, 1);
                        my $name  := substr($var.name(), 1);
                        $block.symbol( $sigil ~ '!' ~ $name, :scope($scope));
                    }
                    my $pkgdecl := $block<pkgdecl>;
                    unless $pkgdecl eq 'class' || $pkgdecl eq 'role'
                            || $pkgdecl eq 'grammar' {
                        $/.panic("Attempt to define attribute " ~ $var.name() ~
                                 " outside of class, role, or grammar");
                    }
                    # Attribute declaration.  Add code to the beginning
                    # of the block (really class/grammar/role) to
                    # create the attribute.
                    our $?METACLASS;
                    my $has := PAST::Op.new( :name('!meta_attribute'),
                                   $?METACLASS, $var.name(), $var<itype> );
                    if $type { $type.named('type'); $has.push($type); }
                    if $init_value {
                        $init_value.named('init_value');
                        $has.push($init_value);
                    }
                    if $var<traitlist> {
                        $var<traitlist>.named('traitlist');
                        $has.push($var<traitlist>);
                    }
                    $block[0].push( $has );
                }
                else {
                    # $scope eq 'package' | 'lexical'
                    my $viviself := PAST::Op.new( :pirop('new PsP'), $var<itype> );
                    if $init_value { $viviself.push( $init_value ); }
                    $var.viviself( $viviself );
                    if $type {
                        $var := PAST::Op.new( :pirop('setprop'),
                                              $var, 'type', $type );
                    }
                }
                $past[$i] := $var;
            }
            $i++;
        }
        if $scope eq 'attribute' {
            $past.pasttype('null');
            $past<scopedecl> := $scope;
        }
        elsif +@($past) == 1 { $past := $past[0]; }
        else { $past.name('infix:,'); $past.pasttype('call'); }
    }
    make $past;
}


method scoped($/) {
    my $past;
    if $<declarator> {
        $past := $( $<declarator> );
    }
    elsif $<multi_declarator> {
        $past := $( $<multi_declarator> );
        if $past.isa(PAST::Var) {
            my $type := $past<type>;
            for @($<fulltypename>) {
                $type.push( $( $_ ) );
            }
            $past.viviself( $( $<fulltypename>[0] ).clone() );
        }
    }
    make $past;
}


method declarator($/) {
    my $past;
    if $<variable_declarator> {
        $past := $( $<variable_declarator> );
    }
    elsif $<signature> {
        $past := $( $<signature> );
        our $?BLOCK_OPEN;
        $?BLOCK_OPEN := 0;
    }
    elsif $<routine_declarator> {
        $past := $( $<routine_declarator> );
    }
    make $past;
}


method variable_declarator($/) {
    our @?BLOCK;
    my $var    := $( $<variable> );

    ##  The $<variable> subrule might've saved a PAST::Var node for
    ##  us (e.g., $.x), if so, use it instead.

    if $var<vardecl> { $var := $var<vardecl>; }
    my $name   := $var.name();
    my $symbol := @?BLOCK[0].symbol( $name );
    if $symbol<scope> eq 'lexical' {
        warn("Redeclaration of variable " ~ $name);
        $var<redecl> := 1;
        $var.isdecl(0);
    }
    else {
        $var.isdecl(1);
        $var<type>  := PAST::Op.new( :name('and'), :pasttype('call') );
        $var<itype> := container_itype($<variable><sigil>);

        if $<trait> {
            my $traitlist := PAST::Op.new( :name('infix:,'), :pasttype('call') );
            $var<traitlist> := $traitlist;
            for @($<trait>) { $traitlist.push( $( $_ ) ); }
        }
    }

    make $var;
}

method variable($/, $key) {
    my $var;
    our @?BLOCK;
    my $?BLOCK := @?BLOCK[0];
    if $key eq 'desigilname' {
        my $sigil    := ~$<sigil>;
        if $sigil eq '&' { $sigil := ''; }
        my $twigil   := ~$<twigil>[0];
        my @ns       := Perl6::Compiler.parse_name( $<desigilname> );
        my $name     := ~@ns.pop();
        my $varname  := $sigil ~ $twigil ~ $name;

        # If no twigil, but varname is 'attribute' in outer scope,
        # it's really a private attribute and implies a '!' twigil
        if !$twigil {
            my $sym := outer_symbol($varname);
            if $sym && $sym<scope> eq 'attribute' {
                $twigil  := '!';
                $varname := $sigil ~ $twigil ~ $name;
            };
        }

        # If twigil is ^ or :, it's a placeholder var.  Create the
        # parameter for the block if one doesn't already exist.
        if $twigil eq '^' || $twigil eq ':' {
            if $?BLOCK<explicit_signature> {
                $/.panic("Cannot use placeholder var in block with signature.");
            }
            $varname := $sigil ~ $name;
            unless $?BLOCK.symbol($varname) {
                $?BLOCK.symbol( $varname, :scope('lexical') );
                $?BLOCK.arity( +$?BLOCK.arity() + 1 );
                my $param := PAST::Var.new(:name($varname), :scope('parameter'));
                if $twigil eq ':' { $param.named( $name ); }
                my $blockinit := $?BLOCK[0];
                my $i := +@($blockinit);
                while $i > 0 && $blockinit[$i-1].name() gt $varname {
                    $blockinit[$i] := $blockinit[$i-1];
                    $i--;
                }
                $blockinit[$i] := $param;

                ##  add to block's signature
                block_signature($?BLOCK);
                $?BLOCK.loadinit().push(
                    PAST::Op.new( :pasttype('callmethod'), :name('!add_param'),
                        PAST::Var.new( :name('signature'), :scope('register') ),
                        $varname
                    )
                );
            }
            ## use twigil-less form afterwards
            $twigil := '';
        }

        $var := PAST::Var.new( :name($varname), :node($/) );
        if $twigil { $var<twigil> := $twigil; }

        # If namespace qualified or has a '*' twigil, it's a package var.
        if @ns || $twigil eq '*' {
            $twigil := '';
            $varname := $sigil ~ $name;
            $var.name($varname);
            $var.namespace(@ns);
            $var.scope('package');
            $var.viviself( container_itype($sigil) );
        }

        ## @_ and %_ add a slurpy param to the block
        if $varname eq '@_' || $varname eq '%_' {
            unless $?BLOCK.symbol($varname) {
                $?BLOCK.symbol( $varname, :scope('lexical') );
                my $param := PAST::Var.new( :name($varname),
                                            :scope('parameter'),
                                            :slurpy(1) );
                if $sigil eq '%' { $param.named(1); }
                $?BLOCK[0].unshift($param);
            }
        }

        # Until PCT has 'name' scope, we handle lexical/package lookup here.
        if $<sigil> eq '&' {
            my $sym := outer_symbol($varname);
            $var.scope( ($sym && $sym<scope>) || 'package');
        }

        # The ! twigil always implies attribute scope.
        if $twigil eq '!' {
            $var.scope('attribute');
        }

        # ! and . twigils may need 'self' for attribute lookup ...
        if $twigil eq '!' || $twigil eq '.' {
            $var.unshift( PAST::Var.new( :name('self'), :scope('lexical') ) );
        }

        # ...but return . twigil as a method call, saving the
        # PAST::Var node in $var<vardecl> where it can be easily
        # retrieved by <variable_declarator> if we're called from there.
        if $twigil eq '.' {
            my $vardecl := $var;
            $vardecl.name( $sigil ~ '!' ~ $name );
            $var := PAST::Op.new( :node($/), :pasttype('callmethod'),
                :name($name),
                PAST::Var.new( :name('self'), :scope('lexical') )
            );
            $var<vardecl> := $vardecl;
        }
    }
    elsif $key eq 'special_variable' {
        $var := $( $<special_variable> );
    }
    elsif $key eq '$0' {
        $var := PAST::Var.new(
                    :scope('keyed_int'),
                    :node($/),
                    :viviself('Failure'),
                    PAST::Var.new( :scope('lexical'), :name('$/') ),
                    +$<matchidx> );
    }
    elsif $key eq '$<>' {
        $var := $( $<postcircumfix> );
        $var.unshift( PAST::Var.new( :scope('lexical'), :name('$/'),
                                     :viviself('Failure'), :node($/) )
        );
    }
    make $var;
}


method special_variable($/) {
    make PAST::Var.new( :node($/), :name(~$/), :scope('lexical') );
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
            elsif +@($past[1]) == 1 && $past[1][0].isa(PAST::Op) {
                if $past[1][0].name() eq 'infix:=>' {
                    # Block with just one pair in it, so a hash.
                    $is_hash := 1;
                }
                elsif $past[1][0].name() eq 'infix:,' {
                    # List, but first elements must be...
                    if $past[1][0][0].isa(PAST::Op) &&
                            $past[1][0][0].name() eq 'infix:=>' {
                        # ...a Pair
                        $is_hash := 1;
                    }
                    elsif $past[1][0][0].isa(PAST::Var) &&
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
                :name('circumfix:{ }'),
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
        my $call_on := $( $<semilist> );
        if $call_on.name() eq 'infix:,' && +@($call_on) == 0 {
            $call_on := PAST::Var.new(
                :name('$/'),
                :scope('lexical')
            );
        }
        $past := PAST::Op.new(
            :pasttype('callmethod'),
            :name($method),
            :node($/),
            $call_on
        );
    }
    make $past;
}


method value($/, $key) {
    make $( $/{$key} );
}


method typename($/) {
    # Extract shortname part of identifier, if there is one.
    my $ns := Perl6::Compiler.parse_name($<name>);
    my $shortname := $ns.pop();

    my $past := PAST::Var.new( :name($shortname), :namespace($ns), :node($/) );

    my $scope := '';
    our @?BLOCK;
    if +$ns == 0 && @?BLOCK {
        for @?BLOCK {
            if defined($_) && !$scope {
                my $sym := $_.symbol($shortname);
                if defined($sym) && $sym<scope> { $scope := $sym<scope>; }
            }
        }
    }

    $past.scope($scope || 'package');
    make $past;
}


method fulltypename($/) {
    my $past := $( $<typename> );
    if substr( $<typename>.text(), 0, 2) eq '::' {
        $past.isdecl(1);
        $past.scope('lexical');
    }
    if $<fulltypename> {
        $past := PAST::Op.new(
            :pasttype('call'),
            :name('postcircumfix:[ ]'),
            $past,
            PAST::Block.new( $( $<fulltypename>[0] ), :blocktype('declaration') )
        );
    }
    make $past;
}


method number($/, $key) {
    make $( $/{$key} );
}


##  for a variety of reasons, this is easier in PIR than NQP for now.
##  NQP doesn't have assign yet, and Str is lighter-weight than Str.
method integer($/) {
    my $str;
    PIR q<  $P0 = find_lex '$/'   >;
    PIR q<  $S0 = $P0             >;
    PIR q<  $P1 = new 'Str'  >;
    PIR q<  assign $P1, $S0       >;
    PIR q<  store_lex '$str', $P1 >;
    make PAST::Val.new(
        :value( +$str ),
        :returns('Int'),
        :node( $/ )
    );
}


method dec_number($/) {
    my $str;
    PIR q<  $P0 = find_lex '$/'   >;
    PIR q<  $S0 = $P0             >;
    PIR q<  $P1 = new 'Str'  >;
    PIR q<  assign $P1, $S0       >;
    PIR q<  store_lex '$str', $P1 >;
    make PAST::Val.new(
        :value( +$str ),
        :returns('Num'),
        :node( $/ )
    );
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
    if $key eq 'quote_concat' {
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
    elsif $key eq 'quote_regex' {
        $past := PAST::Block.new(
            $<quote_regex>,
            :compiler('PGE::Perl6Regex'),
            :blocktype('declaration'),
            :node( $/ )
        );
    }
    elsif $key eq 'quote_pir' {
        $past := PAST::Op.new( :inline( $<quote_pir> ), :node($/) );
    }
    make $past;
}


method quote_concat($/) {
    my $quote_term := $<quote_term>;
    my $terms := +$quote_term;
    my $count := 1;
    my $past := $( $quote_term[0] );
    while ($count != $terms) {
        $past := PAST::Op.new(
            $past,
            $( $quote_term[$count] ),
            :pirop('concat'),
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
            :returns('Str'), :node($/)
        );
    }
    elsif ($key eq 'variable') {
        $past := PAST::Op.new( $( $<variable> ), :name('prefix:~'), :pasttype('call') );
    }
    elsif ($key eq 'circumfix') {
        $past := $( $<circumfix> );
        if $past.isa(PAST::Block) {
            $past.blocktype('immediate');
        }
        $past := PAST::Op.new( $past, :name('prefix:~'), :pasttype('call') );
    }
    make $past;
}


method term($/, $key) {
    my $past;

    my @ns;
    my $short_name;
    if $<name> {
        @ns := Perl6::Compiler.parse_name(~$<name>);
        $short_name := @ns.pop();
    }

    if $key eq 'noarg' {
        if @ns {
            $past := PAST::Op.new(
                PAST::Var.new(
                    :name($short_name),
                    :namespace(@ns),
                    :scope('package')
                ),
                :pasttype('call')
            );
        }
        else {
            if $short_name eq 'print' || $short_name eq 'say' {
                $/.panic($short_name ~ ' requires an argument');
            }
            $past := PAST::Op.new( :name( $short_name ), :pasttype('call') );
        }
    }
    elsif $key eq 'args' {
        $past := $($<args>);
        if @ns {
            $past.unshift(PAST::Var.new(
                :name($short_name),
                :namespace(@ns),
                :scope('package')
            ));
        }
        else {
            if +@($past) == 0 && ($short_name eq 'print' || $short_name eq 'say') {
                $/.panic($short_name ~ ' requires an argument');
            }
            $past.name( $short_name );
        }
    }
    elsif $key eq 'func args' {
        $past := build_call( $( $<semilist> ) );
        if @ns {
            $past.unshift(PAST::Var.new(
                :name($short_name),
                :namespace(@ns),
                :scope('package')
            ));
        }
        else {
            $past.name( $short_name );
        }
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


method args($/, $key) {
    my $past := build_call( $key eq 'func args'
        ?? $($<semilist>)
        !! $($<arglist>)
    );
    make $past;
}


method semilist($/) {
    my $past := $<EXPR>
        ?? $( $<EXPR>[0] )
        !! PAST::Op.new( :node($/), :name('infix:,') );
    make $past;
}


method arglist($/) {
    my $past := $<EXPR>
        ?? $( $<EXPR> )
        !! PAST::Op.new( :node($/), :name('infix:,') );
    make $past;
}


method EXPR($/, $key) {
    my $type := ~$<type>;

    if $key eq 'end' {
        make $($<expr>);
    }
    elsif ~$type eq 'infix:=' {
        my $lhs := $( $/[0] );
        my $rhs := $( $/[1] );
        my $past;

        if $lhs<scopedecl> eq 'attribute' {
            $rhs.named('init_value');
            our $?METACLASS;
            $past := PAST::Op.new( :name('!meta_attribute'),
                         $?METACLASS, $lhs[0].name(), $rhs
            );
            our @?BLOCK;
            @?BLOCK[0][0].push($past);
            $past := PAST::Stmts.new();
        }
        else {
            # Just a normal assignment.
            $past := PAST::Op.new(
                :pasttype('call'),
                :name('infix:='),
                :lvalue(1),
                $lhs,
                $rhs
            );
        }

        make $past;
    }
    elsif ~$type eq 'infix:.=' {
        my $invocant  := $( $/[0] );
        my $call      := $( $/[1] );

        # Check that we have a sub call.
        if !$call.isa(PAST::Op) || $call.pasttype() ne 'call' {
            $/[0].panic('.= must have a call on the right hand side');
        }

        # Change call node to a callmethod.
        $call.pasttype('callmethod');

        # We only want to evaluate invocant once; stash it in a register.
        $call.unshift(PAST::Op.new(
            :pasttype('bind'),
            PAST::Var.new(
                :name('detemp'),
                :scope('register'),
                :isdecl(1)
            ),
            $invocant
        ));

        # Do call, then assignment to target container.
        my $past := PAST::Op.new(
            :inline("    %r = 'infix:='(%1, %0)"),
            :node($/),
            $call,
            PAST::Var.new(
                :name('detemp'),
                :scope('register')
            )
        );

        make $past;
    }
    elsif ~$type eq 'infix:does' || ~$type eq 'infix:but' {
        my $past := PAST::Op.new(
            $( $/[0] ),
            :pasttype('call'),
            :name(~$type),
            :node($/)
        );
        my $rhs := $( $/[1] );
        if $rhs.isa(PAST::Op) && $rhs.pasttype() eq 'call' {
            # Make sure we only have one initialization value.
            if +@($rhs) > 2 {
                $/[0].panic("Role initialization can only supply a value for one attribute");
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
    elsif ~$type eq 'infix:~~' {
        # Smart-match. We need to detect and specially dispatch a few special forms; the
        # rest fall through to a call to .ACCEPTS.
        my $lhs := $( $/[0] );
        my $rhs := $( $/[1] );
        make process_smartmatch($lhs, $rhs, $/[1]);
    }
    elsif ~$type eq 'prefix:|' {
        # Need to make it flatten the argument.
        my $past := $( $/[0] );
        $past.flat(1);
        make $past;
    }
    else {
        my $past := PAST::Op.new(
            :node($/),
            :name($type),
            :opattr($<top>)
        );
        if $<top><subname> { $past.name(~$<top><subname>); }
        for @($/) {
            unless +$_.from() == +$_.to() { $past.push( $($_) ) };
        }

        make $past;
    }
}


method regex_declarator($/) {
    my $sym  := ~$<sym>;
    my $past := $( $<regex_def> );
    if $sym eq 'token'
        { $past.compiler_args( :grammar(''), :ratchet(1) ); }
    elsif $sym eq 'rule'
        { $past.compiler_args( :grammar(''), :s(1), :ratchet(1) ); }
    else
        { $past.compiler_args( :grammar('') ); }
    make $past;
}

method regex_def($/) {
    my $past := $( $<regex_block> );
    $past.name( ~$<deflongname>[0] );
    make $past;
}

method regex_block($/) {
    make $( $<quote_expression> );
}


method type_declarator($/) {
    # Make sure it's not a re-declaration.
    if $/.type_redeclaration() {
        $/.panic("Re-declaration of type " ~ ~$<name>);
    }

    # We need a block containing the constraint condition.
    my $past := $( $<EXPR> );
    my $param_name := '$_';
    if (!$past.isa(PAST::Block) || $past.compiler() eq 'PGE::Perl6Regex') {
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
        if $_.isa(PAST::Var) {
            if $_.scope() eq 'parameter' {
                $param := $_;
                $param_name := $param.name();
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

    # If it doesn't have a signature, give it one.
    unless $past<signature> {
        block_signature($past);
        $past.loadinit().push(PAST::Op.new(
            :pasttype('callmethod'),
            :name('!add_param'),
            PAST::Var.new( :name('signature'), :scope('register') ),
            $param_name
        ));
        $past[0].push(PAST::Op.new( :pasttype('call'), :name('!SIGNATURE_BIND') ));
    }

    # Create subset type.
    my @name := Perl6::Compiler.parse_name($<name>);
    $past := PAST::Op.new(
        :node($/),
        :pasttype('bind'),
        PAST::Var.new(
            :name(@name.pop()),
            :namespace(@name),
            :scope('package')
        ),
        PAST::Op.new(
            :pasttype('call'),
            :name('!CREATE_SUBSET_TYPE'),
            $<fulltypename> ??
                $( $<fulltypename>[0] )
                !!
                PAST::Var.new(
                    :name('Any'),
                    :scope('package')
                ),
            $past
        )
    );

    # Put this code in loadinit, so the type is created early enough,
    # then this node results in an empty statement node.
    our @?BLOCK;
    @?BLOCK[0].loadinit().push($past);

    make PAST::Stmts.new();
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
        $pair_key := PAST::Val.new( :value(~$<identifier>) );
        $pair_val := PAST::Val.new( :value(0), :returns('Int') );
    }
    elsif $key eq 'value' {
        $pair_key := PAST::Val.new( :value(~$<identifier>) );
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
        if $<desigilname><longname> {
            $pair_key := PAST::Val.new( :value( $<desigilname>.text() ) );
            $pair_val := PAST::Var.new(
                :name( ~$<sigil> ~ ~$<twigil> ~ $<desigilname>.text() )
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


# search through outer blocks for a symbol table entry
sub outer_symbol($name) {
    our @?BLOCK;
    my $symbol;
    for @?BLOCK {
        $symbol := $_.symbol($name);
        if $symbol { return $symbol; }
    }
    return $symbol;
}


# Used by all calling code to process arguments into the correct form.
sub build_call($args) {
    if !$args.isa(PAST::Op) || $args.name() ne 'infix:,' {
        $args := PAST::Op.new( :node($args), $args);
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
    $args.name('');
    $args;
}


sub declare_implicit_routine_vars($block) {
    for ('$_', '$/', '$!') {
        unless $block.symbol($_) {
            $block[0].push( PAST::Var.new( :name($_),
                                           :scope('lexical'),
                                           :isdecl(1),
                                           :viviself('Failure') ) );
            $block.symbol($_, :scope('lexical') );
        }
    }
}


sub declare_implicit_block_vars($block, $tparam) {
    $block[0].push( PAST::Op.new(
                        :inline('    .local pmc outerlex',
                                '    getinterp $P0',
                                '    set outerlex, $P0["outer";"lexpad";1]')));
    for ('$_', '$/', '$!') {
        unless $block.symbol($_) {
            my $lex := PAST::Op.new(:inline('    set %r, outerlex["'~$_~'"]'));
            my $scope := ($tparam && $_ eq '$_') ?? 'parameter' !! 'lexical';
            $block[0].push(
                PAST::Var.new( :name($_),
                               :scope($scope),
                               :isdecl(1),
                               :viviself($lex)
                )
            );
            $block.symbol($_, :scope('lexical') );
        }
    }
}

sub declare_implicit_function_vars($block) {
    declare_implicit_block_vars($block, !defined($block.arity()));
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


sub container_itype($sigil) {
    if    $sigil eq '@' { return 'Perl6Array'  }
    elsif $sigil eq '%' { return 'Perl6Hash'   }
    else                { return 'Perl6Scalar' }
}


sub trait_readtype($traitpast) {
    my $readtype;
    if $traitpast {
        for @($traitpast) {
            my $tname := $_[1];
            if $tname eq 'readonly' || $tname eq 'rw' || $tname eq 'copy' {
                $readtype := $readtype ?? 'CONFLICT' !! $tname;
            }
        }
    }
    $readtype;
}


# Produces a handles method.
# Generates a setter/getter method for an attribute in a class or role.
sub make_accessor($/, $method_name, $attr_name, $rw, $scope) {
    my $getset;
    if $rw {
        $getset := PAST::Var.new( :name($attr_name), :scope($scope) );
    }
    else {
        $getset := PAST::Op.new(
            :inline(
                '    %r = new "ObjectRef", %0',
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


# Creates an anonymous subset type.
sub make_anon_subtype($past) {
    my $param_name := '$_';

    # We need a block containing the constraint condition and do smart-match
    # it against $_.
    if !$past.isa(PAST::Block) || $past.compiler() eq 'PGE::Perl6Regex' {
        $past := PAST::Block.new(
            PAST::Stmts.new(
                PAST::Var.new(
                    :name('$_'),
                    :scope('parameter')
                )
            ),
            PAST::Stmts.new( 
                PAST::Op.new(
                    :name('infix:~~'),
                    :pasttype('call'),
                    PAST::Var.new( :name('$_'), :scope('lexical') ),
                    $past
                )
            )
        );
    }
    else {
        # Make the block we have has a parameter.
        my $param;
        my $dollar_underscore;
        for @($past[0]) {
            if $_.isa(PAST::Var) {
                if $_.scope() eq 'parameter' {
                    $param := $_;
                    $param_name := $param.name();
                }
                elsif $_.name() eq '$_' {
                    $dollar_underscore := $_;
                }
            }
        }
        unless $param {
            if $dollar_underscore {
                $dollar_underscore.scope('parameter');
            }
            else {
                $past[0].push(PAST::Var.new(
                    :name('$_'),
                    :scope('parameter')
                ));
            }
        }
    }

    # Add signature to make sure parameter is read-only, unless block is
    # already sig'd.
    unless $past<signature> {
        block_signature($past);
        $past.loadinit().push(PAST::Op.new(
            :pasttype('callmethod'),
            :name('!add_param'),
            PAST::Var.new( :name('signature'), :scope('register') ),
            $param_name
        ));
        $past[0].push(PAST::Op.new( :pasttype('call'), :name('!SIGNATURE_BIND') ));
    }

    return $past;
}


# Returns the code to set $?PACKAGE to the current package.
sub set_package_magical() {
    return PAST::Var.new(
            :name('$?PACKAGE'),
            :scope('lexical'),
            :isdecl(1),
            :viviself(PAST::Op.new(:pirop('get_namespace P')))
    );
}


sub block_signature($block) {
    unless $block<signature> {
        $block.loadinit().push(
            PAST::Op.new( :inline('    .local pmc signature',
                                  '    signature = new ["Signature"]',
                                  '    setprop block, "$!signature", signature')
            )
        );
        $block<signature> := 1;
    }
}


# Adds to the loadinit to set the type of a block.
sub set_block_type($block, $type) {
    # If the block already has a type node, edit it.
    if $block<block_class_type> {
        $block<block_class_type>[1] := $type;
    }
    else {
        my $set_type := PAST::Op.new(
            :pasttype('call'),
            :name('!fixup_routine_type'),
            PAST::Var.new( :name('block'), :scope('register') ),
            $type
        );
        $block<block_class_type> := $set_type;
        $block.loadinit().push($set_type);
    }
}


# Makes a routine into a multi, if it isn't already one.
sub transform_to_multi($past) {
    unless $past<multi_flag> {
        my $pirflags := ~$past.pirflags();
        $past.pirflags( $pirflags ~ ' :multi()' );
        $past.loadinit().push(
            PAST::Op.new( :name('!TOPERL6MULTISUB'), :pasttype('call'),
                PAST::Var.new( :name('block'), :scope('register') )
            )
        );
        $past<multi_flag> := 1;
    }
}


# Hanldes syntactic forms of smart-matching (factored out here since it's used
# by infix:~~ and the when statement.
sub process_smartmatch($lhs, $rhs, $rhs_pt) {
    if $rhs_pt<noun><dotty> {
        # Method truth - just call RHS.
        $rhs<invocant_holder>[0] := $lhs;
        return PAST::Op.new(
            :pasttype('call'),
            :name('prefix:?'),
            $rhs
        );
    }
    else {
        return PAST::Op.new(
            :pasttype('call'),
            :name('infix:~~'),
            $lhs, $rhs
        );
    }
}


# Gives a block an undef to return if it has no statements, to prevent Null
# PMCs being handed back.
sub prevent_null_return($block) {
    if +@($block[1]) == 0 {
        $block[1].push(PAST::Op.new(
            :pasttype('call'),
            :name('undef')
        ));
    }
}

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
