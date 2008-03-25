# $Id$
#
# Copyright (C) 2007, The Perl Foundation.

class Perl6::Grammar::Actions ;

method TOP($/) {
    my $past := $( $<statement_block> );
    $past.blocktype('declaration');

    # Attatch any initialization code.
    our $?INIT;
    if defined( $?INIT ) {
        $?INIT.unshift(PAST::Var.new(
            :name('$def'),
            :scope('lexical'),
            :isdecl(1)
        ));
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
            $init.push( PAST::Op.new(
                :inline("    %r = getinterp\n" ~
                        "    %r = %r['lexpad';1]\n" ~
                        "    if null %r goto no_match_to_copy\n" ~
                        "    %r = %r['$/']\n" ~
                        "    store_lex '$/', %r\n" ~
                        "  no_match_to_copy:\n")
            ));
        }
        unless $?BLOCK.symbol('$!') {
            $init.push( PAST::Var.new( :name('$!'), :isdecl(1) ) );
            $?BLOCK.symbol( '$!', :scope('lexical') ); }
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
    else {
        my $expr := $( $<expr> );
        if $expr.WHAT() eq 'Block' && !$expr.blocktype() {
            $expr.blocktype('immediate');
        }
        if $key eq 'statement_mod_cond' {
            $past := $( $<statement_mod_cond> );
            $past.push( $expr );
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
    my $past := PAST::Op.new( $expr, $then,
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
        $past  := PAST::Op.new( $expr, $then, $past,
                               :pasttype('if'),
                               :node( $/ )
                             );
    }
    make $past;
}


method unless_statement($/) {
    my $then := $( $<block> );
    $then.blocktype('immediate');
    my $past := PAST::Op.new( $( $<EXPR> ), $then,
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
    my $assign := PAST::Op.new( :name('infix::='),
                                :pasttype('bind'),
                                :node($/)
                              );
    $assign.push( PAST::Var.new( :node($/), :name('$_'), :scope('lexical') ) );
    $assign.push( $expr );

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
    my $expr := $( $<EXPR> );
    my $match_past := PAST::Op.new( :name('infix:~~'),
                                    :pasttype('call'),
                                    :node($/)
                                  );
    $match_past.push( PAST::Var.new( :node($/), :name('$_'), :scope('lexical') ) );
    $match_past.push( $expr );

    # Use the smartmatch result as the condition.
    my $past := PAST::Op.new( $match_past, $block,
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

method for_statement($/) {
    my $block := $( $<pblock> );
    $block.blocktype('declaration');
    my $past := PAST::Op.new( $( $<EXPR> ), $block,
                            :pasttype($<sym>),
                            :node( $/ )
                            );
    make $past;
}

method pblock($/) {
    our $?BLOCK_SIGNATURED;
    unless $<signature> {
        $?BLOCK_SIGNATURED :=
            PAST::Block.new(
                PAST::Stmts.new(
                    PAST::Var.new(
                        :name('$_'),
                        :scope('parameter'),
                        :viviself('Undef')
                    )
                ),
                :node( $/ )
            );
        $?BLOCK_SIGNATURED.symbol( '$_', :scope('lexical') );
    }
    make $?BLOCK_SIGNATURED;
}

method use_statement($/) {
    my $name := ~$<name>;
    my $past;
    if $name eq 'v6' || $name eq 'lib' {
        $past := PAST::Stmts.new( :node($/) );
    }
    else {
        $past := PAST::Op.new( PAST::Val.new( :value( $name ) ),
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

method statement_mod_cond($/) {
    make PAST::Op.new( $( $<EXPR> ),
                       :pasttype( ~$<sym> ),
                       :node( $/ )
                     );
}


method statement_prefix($/) {
    my $past := $($<statement>);
    my $sym := ~$<sym>;
    if $sym eq 'do' {
        # fall through, just use the statement itself
    }
    ## after the code in the try block is executed, bind $! to Undef,
    ## and set up the code to catch an exception, in case one is thrown
    elsif $sym eq 'try' {
        ##  Set up code to execute <statement> as a try node, and
        ##  set $! to Undef if successful.
        my $exitpir  := "    new %r, 'Undef'\n    store_lex '$!', %r";
        my $try := PAST::Stmts.new( $past ,
                                    PAST::Op.new( :inline( $exitpir ) ) );
        $past := PAST::Op.new( $try, :pasttype('try') );

        ##  Add a catch node to the try op that captures the
        ##  exception object into $!.
        my $catchpir := "    .get_results (%r, $S0)\n    store_lex '$!', %r";
        $past.push( PAST::Op.new( :inline( $catchpir ) ) );
    }
    else {
        $/.panic( $sym ~ ' not implemented');
    }
    make $past;
}


method plurality_declarator($/) {
    my $past := $( $<routine_declarator> );
    if $<sym> eq 'multi' {
        my $pirflags := ~ $past.pirflags();
        my $arity := $past.arity();
        if    $arity == 0 { $pirflags := $pirflags ~ ' :multi()'; }
        elsif $arity == 1 { $pirflags := $pirflags ~ ' :multi(_)'; }
        else {
            $pirflags := $pirflags ~ ' :multi(_';
            my $count := 1;
            while $count != $arity {
                $pirflags := $pirflags ~ ',_';
                $count := $count + 1;
            }
            $pirflags := $pirflags ~ ')';
        }
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


method routine_def($/) {
    my $past := $( $<block> );
    if $<ident> {
        $past.name( ~$<ident>[0] );
        our $?BLOCK;
        $?BLOCK.symbol(~$<ident>[0], :scope('package'));
    }
    make $past;
}

method method_def($/) {
    my $past := $( $<block> );
    if $<ident> {
        $past.name( ~$<ident>[0] );
    }
    make $past;
}

method signature($/) {
    my $params := PAST::Stmts.new( :node($/) );
    my $past := PAST::Block.new( $params, :blocktype('declaration') );
    for $/[0] {
        my $parameter := $($_<parameter>);
        $past.symbol($parameter.name(), :scope('lexical'));
        $params.push($parameter);
    }
    $past.arity( +$/[0] );
    our $?BLOCK_SIGNATURED := $past;
    make $past;
}


method parameter($/, $key) {
    my $past := $( $<param_var> );
    my $sigil := $<param_var><sigil>;
    if $key eq 'slurp' {              # slurpy
        $past.slurpy( $sigil eq '@' || $sigil eq '%' );
        $past.named( $sigil eq '%' );
    }
    else {
        if $<named> eq ':' {          # named
            $past.named(~$<param_var><ident>);
            if $<quant> ne '!' {      #  required (optional is default)
                $past.viviself('Undef');
            }
        }
        else {                        # positional
            if $<quant> eq '?' {      #  optional (required is default)
                $past.viviself('Undef');
            }
        }
    }
    make $past;
}


method param_var($/) {
    make PAST::Var.new( :name(~$/),
                        :scope('parameter'),
                        :node($/)
                      );
}


method special_variable($/) {
    make PAST::Var.new( :node($/), :name(~$/), :scope('lexical') );
}


method term($/, $key) {
    my $past := $( $/{$key} );
    if $<postfix> {
        for $<postfix> {
            my $term := $past;
            $past := $($_);

            # Check if it's an indirect call.
            if $_<methodop><variable> {
                # What to call supplied; need to put the invocant second.
                my $meth := $past[0];
                $past[0] := $term;
                $past.unshift($meth);
            }
            elsif $_<methodop><quote> {
                # First child is something that we evaluate to get the
                # name. Replace it with PIR to call find_method on it.
                my $meth_name := $past[0];
                $past[0] := $term;
                $past.unshift( PAST::Op.new(
                    :inline("$S1000 = %1\n%r = find_method %0, $S1000\n"),
                    $term,
                    $meth_name
                ));
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

method methodop($/, $key) {
    my $past;

    if $key eq 'null' {
        $past := PAST::Op.new();
    }
    else {
        $past := PAST::Op.new();
        my $args := $( $/{$key} );
        process_arguments($past, $args);
    }
    $past.pasttype('callmethod');
    $past.node($/);

    if $<ident> {
        $past.name(~$<ident>);
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
        my $semilist := $( $<semilist> );
        $past := PAST::Var.new( $semilist[0],
                                :scope('keyed'),
                                :vivibase('List'),
                                :viviself('Undef'),
                                :node( $/ )
                              );
    }
    elsif $key eq '( )' {
        my $semilist := $( $<semilist> );
        $past := PAST::Op.new( :node($/), :pasttype('call') );
        process_arguments($past, $semilist);
    }
    elsif $key eq '{ }' {
        my $semilist := $( $<semilist> );
        $past := PAST::Var.new( $semilist[0],
                                :scope('keyed'),
                                :vivibase('Hash'),
                                :viviself('Undef'),
                                :node( $/ )
                              );
    }
    elsif $key eq '< >' {
        $past := PAST::Var.new( $( $<quote_expression> ),
                                :scope('keyed'),
                                :vivibase('Hash'),
                                :viviself('Undef'),
                                :node( $/ )
                              );
    }
    else
    {
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
    elsif $key eq 'methodop' {
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


method package_declarator($/, $key) {
    our $?INIT;
    our $?CLASS;
    our @?CLASS;
    our $?ROLE;
    our @?ROLE;
    our $?PACKAGE;
    our @?PACKAGE;

    if $key eq 'open' {
        # Start of the block; if it's a class or role, need to make $?CLASS or
        # $?ROLE available for storing current definition in.
        if $<sym> eq 'class' || $<sym> eq 'role' {
            my $decl_past := PAST::Stmts.new();

            # If it's a class...
            if $<sym> eq 'class' {
                # Call method to create the class.
                $decl_past.push(PAST::Op.new(
                    :pasttype('bind'),
                    PAST::Var.new(
                        :name('$def'),
                        :scope('lexical')
                    ),
                    PAST::Op.new(
                        :pasttype('callmethod'),
                        :name('!keyword_class'),
                        PAST::Var.new(
                            :name('Perl6Object'),
                            :scope('package')
                        ),
                        PAST::Val.new( :value(~$<name>) )
                    )
                ));

                # Put current class, if any, on @?CLASS list so we can handle
                # nested classes.
                @?CLASS.unshift( $?CLASS );
                $?CLASS := $decl_past;

                # Set it as the current package.
                @?PACKAGE.unshift( $?PACKAGE );
                $?PACKAGE := $?CLASS;
            }

            # If it's a role...
            elsif $<sym> eq 'role' {
                # Call method to create the role.
                $decl_past.push(PAST::Op.new(
                    :pasttype('bind'),
                    PAST::Var.new(
                        :name('$def'),
                        :scope('lexical')
                    ),
                    PAST::Op.new(
                        :pasttype('callmethod'),
                        :name('!keyword_role'),
                        PAST::Var.new(
                            :name('Perl6Object'),
                            :scope('package')
                        ),
                        PAST::Val.new( :value(~$<name>) )
                    )
                ));

                # Put current role, if any, on @?ROLE list so we can handle
                # nested roles.
                @?ROLE.unshift( $?ROLE );
                $?ROLE := $decl_past;

                # Set it as the current package.
                @?PACKAGE.unshift( $?PACKAGE );
                $?PACKAGE := $?ROLE;
            }

            # Apply any traits and do any roles.
            my $does_pir;
            for $<trait_or_does> {
                if $_<trait> {
                    # Apply the trait.
                    if $_<trait><trait_auxiliary><sym> eq 'is' {
                        $?PACKAGE.push(PAST::Op.new(
                            :pasttype('call'),
                            :name('trait_auxiliary:is'),
                            PAST::Var.new(
                                :name(~$_<trait><trait_auxiliary><ident>),
                                :scope('package')
                            ),
                            PAST::Var.new(
                                :name('$def'),
                                :scope('lexical')
                            )
                        ));
                    }
                }
                elsif $_<sym> eq 'does' {
                    # Role.
                    $?PACKAGE.push(PAST::Op.new(
                        :pasttype('callmethod'),
                        :name('!keyword_does'),
                        PAST::Var.new(
                            :name('Perl6Object'),
                            :scope('package')
                        ),
                        PAST::Var.new(
                            :name('$def'),
                            :scope('lexical')
                        ),
                        PAST::Val.new( :value(~$_<name>) )
                    ));
                }
            }
        }
        else {
            # It's a module. We need a way to mark that the current package is
            # not a role or a class, so we put the current one on the array and
            # set $?PACKAGE to undef.
            @?PACKAGE.unshift( $?PACKAGE );
            $?PACKAGE := undef;
        }
    }
    else {
        my $past := $( $/{$key} );

        # Declare the namespace and that this is something we do
        # "on load".
        $past.namespace($<name><ident>);
        $past.blocktype('declaration');
        $past.pirflags(':init :load');

        if $<sym> eq 'class' {
            # Make proto-object.
            $?CLASS.push(PAST::Op.new(
                :pasttype('call'),
                PAST::Var.new(
                    :scope('package'),
                    :namespace('Perl6Object'),
                    :name('make_proto')
                ),
                PAST::Var.new(
                    :scope('lexical'),
                    :name('$def')
                ),
                PAST::Val.new( :value(~$<name>) )
            ));

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

            # Restore outer class.
            $?CLASS := @?CLASS.shift();
        }
        elsif $<sym> eq 'role' {
            # Attatch role declaration to the init code.
            unless defined( $?INIT ) {
                $?INIT := PAST::Block.new();
            }
            $?INIT.push( $?ROLE );

            # Restore outer role.
            $?ROLE := @?ROLE.shift();
        }

        # Restore outer package.
        $?PACKAGE := @?PACKAGE.shift();

        make $past;
    }
}


method variable_decl($/) {
    my $past := $( $<variable> );
    if $<trait> {
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
                        $past.viviself($aux<ident>);
                    }
                }
                else {
                    $/.panic("'" ~ $sym ~ "' not implemented");
                }
            }
            elsif $trait<trait_verb> {
                my $verb := $trait<trait_verb>;
                my $sym := $verb<sym>;
                $/.panic("'" ~ $sym ~ "' not implemented");
            }
        }
    }
    make $past;
}


method scoped($/) {
    my $past := $( $<variable_decl> );
    make $past;
}


method scope_declarator($/) {
    my $past := $( $<scoped> );
    my $name := $past.name();
    our $?BLOCK;
    unless $?BLOCK.symbol($name) {
        my $scope := 'lexical';
        my $declarator := $<declarator>;
        if $declarator eq 'my' {
            $past.isdecl(1);
        }
        elsif $declarator eq 'our' {
            $scope := 'package';
            $past.isdecl(1);
        }
        elsif $declarator eq 'has' {
            # Set that it's attribute scope.
            $scope := 'attribute';

            # Get the class or role we're in.
            our $?CLASS;
            our $?ROLE;
            our $?PACKAGE;
            my $class_def;
            if $?ROLE =:= $?PACKAGE {
                $class_def := $?ROLE;
            }
            else {
                $class_def := $?CLASS;
            }
            unless defined( $class_def ) {
                $/.panic("attempt to define attribute '" ~ $name ~ "' outside of class");
            }

            # Generate PIR for attribute (always name it with ! twigil).
            my $variable := $<scoped><variable_decl><variable>;
            $name := ~$variable<sigil> ~ '!' ~ ~$variable<name>;
            $class_def.push(PAST::Op.new(
                :pasttype('callmethod'),
                :name('!keyword_has'),
                PAST::Var.new(
                    :name('Perl6Object'),
                    :scope('package')
                ),
                PAST::Var.new(
                    :name('$def'),
                    :scope('lexical')
                ),
                PAST::Val.new( :value($name) )
            ));

            # If we have no twigil, make $name as an alias to $!name.
            if $variable<twigil>[0] eq '' {
                $?BLOCK.symbol(~$variable<sigil> ~ ~$variable<name>, :scope($scope));
            }

            # If we have a . twigil, we need to generate an accessor.
            elsif $variable<twigil>[0] eq '.' {
                my $accessor := PAST::Block.new(
                    PAST::Stmts.new(
                        PAST::Var.new( :name($name), :scope('attribute') )
                    ),
                    :name($variable<name>),
                    :blocktype('declaration'),
                    :pirflags(':method'),
                    :node( $/ )
                );
                $?CLASS.unshift($accessor);
            }

            # If it's a ! twigil, we're done; otherwise, error.
            elsif $variable<twigil>[0] ne '!' {
                $/.panic("invalid twigil " ~ $variable<twigil>[0] ~ " in attribute declaration");
            }

            # We don't want to generate any PAST at the point of the declaration.
            $past := PAST::Stmts.new();
        }
        else {
            $/.panic("scope declarator '" ~ $declarator ~ "' not implemented");
        }
        $?BLOCK.symbol($name, :scope($scope));
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
            :scope('keyed'),
            :node($/),
            :viviself('Undef'),
            PAST::Var.new(
                :scope('lexical'),
                :name('$/')
            ),
            PAST::Val.new(
                :value(~$<matchidx>),
                :returns('Integer')
            )
        );
    }
    elsif $key eq '$<>' {
        $past := $( $<postcircumfix> );
        $past.unshift(PAST::Var.new(
            :scope('lexical'),
            :name('$/'),
            :viviself('Undef')
        ));
    }
    else {
        # Handle naming.
        my @ident := $<name><ident>;
        my $name;
        PIR q<  $P0 = find_lex '@ident'  >;
        PIR q<  $P0 = clone $P0          >;
        PIR q<  store_lex '@ident', $P0  >;
        PIR q<  $P1 = pop $P0            >;
        PIR q<  store_lex '$name', $P1   >;

        # If it's $.x, it's a method call, not a variable.
        if $<twigil>[0] eq '.' {
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
            # Variable. Set how it vivifies.
            my $viviself := 'Undef';
            if $<sigil> eq '@' { $viviself := 'List'; }
            if $<sigil> eq '%' { $viviself := 'Hash'; }

            # ! twigil should be kept in the name.
            if $<twigil>[0] eq '!' { $name := '!' ~ ~$name; }

            # All but subs should keep their sigils.
            my $sigil := '';
            if $<sigil> ne '&' {
                $sigil := ~$<sigil>;
            }

            # If we have no twigil, but we see the name noted as an attribute in
            # an enclosing scope, add the ! twigil anyway; it's an alias.
            if $<twigil>[0] eq '' {
                our @?BLOCK;
                for @?BLOCK {
                    if defined( $_ ) {
                        my $sym_table := $_.symbol($sigil ~ $name);
                        if defined( $sym_table ) && $sym_table<scope> eq 'attribute' {
                            $name := '!' ~ $name;
                        }
                    }
                }
            }

            $past := PAST::Var.new( :name( $sigil ~ $name ),
                                    :viviself($viviself),
                                    :node($/)
                                  );
            if @ident || $<twigil>[0] eq '*' {
                $past.namespace(@ident);
                $past.scope('package');
            }
        }
    }
    make $past;
}


method circumfix($/, $key) {
    my $past;
    if $key eq '( )' {
        $past := $( $<statementlist> );
    }
    if $key eq '[ ]' {
        $past := $( $<statementlist> );
    }
    elsif $key eq '{ }' {
        $past := $( $<pblock> );
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
    make PAST::Val.new( :value( +$str ),
                        :returns('Integer'),
                        :node( $/ )
                      );
}


method dec_number($/) {
    make PAST::Val.new( :value( +$/ ), :returns('Float'), :node( $/ ) );
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
        my $return_type := 'Integer';
        if $fracpart { $return_type := 'Float'; }
        make PAST::Val.new(
            :value(
                radcalc( $radix, $intpart, $fracpart, ~$base, ~$exp )
            ),
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
        $past := PAST::Block.new( $<quote_regex>,
                                  :compiler('PGE::Perl6Regex'),
                                  :blocktype('declaration'),
                                  :node( $/ )
                                )
    }
    elsif $key eq 'quote_concat' {
        if +$<quote_concat> == 1 {
            $past := $( $<quote_concat>[0] );
        }
        else {
            $past := PAST::Op.new( :name('list'),
                                   :pasttype('call'),
                                   :node( $/ ) );
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
        $past := PAST::Op.new( $past,
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
        $past := PAST::Val.new( :value( ~$<quote_literal> ), :returns('Perl6Str'), :node($/) );
    }
    if ($key eq 'variable') {
        $past := $( $<variable> );
    }
    make $past;
}


method typename($/) {
    my $ns := $<name><ident>;
    my $shortname;
    PIR q<    $P0 = find_lex '$ns'         >;
    PIR q<    $P0 = clone $P0              >;
    PIR q<    $P1 = pop $P0                >;
    PIR q<    store_lex '$ns', $P0         >;
    PIR q<    store_lex '$shortname', $P1  >;
    make PAST::Var.new( :name($shortname),
                        :namespace($ns),
                        :scope('package'),
                        :node($/)
                      );
}


method subcall($/) {
    # Build call node.
    my $past := PAST::Op.new(
        :name( ~$<ident> ),
        :pasttype('call'),
        :node($/)
    );

    # Process arguments.
    my $args := $( $<semilist> );
    process_arguments($past, $args);

    make $past;
}


method semilist($/) {
    my $past := PAST::Op.new( :node($/) );
    if $<EXPR> {
        my $expr := $($<EXPR>[0]);
        if $expr.name() eq 'infix:,' {
            for @($expr) {
                $past.push( $_ );
            }
        }
        else {
            $past.push( $expr );
        }
    }
    make $past;
}


method listop($/, $key) {
    my $past;
    if $key eq 'arglist' {
        $past := $( $<arglist> );
    }
    if $key eq 'noarg' {
        $past := PAST::Op.new( );
    }
    $past.name( ~$<sym> );
    $past.pasttype('call');
    $past.node($/);
    make $past;
}


method arglist($/) {
    my $past := PAST::Op.new( :node($/) );
    my $expr := $($<EXPR>);
    if $expr.name() eq 'infix:,' {
        for @($expr) {
            $past.push( $_ );
        }
    }
    else {
        $past.push( $expr );
    }
    make $past;
}


method EXPR($/, $key) {
    if $key eq 'end' {
        make $($<expr>);
    }
    else {
        my $past := PAST::Op.new( :name($<type>),
                                  :pasttype($<top><pasttype>),
                                  :pirop($<top><pirop>),
                                  :lvalue($<top><lvalue>),
                                  :node($/)
                                );
        for @($/) {
            $past.push( $($_) );
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
    $past.name( ~$<ident>[0] );
    make $past;
}


method regex_declarator_rule($/) {
    my $past := $( $<quote_expression> );
    $past.name( ~$<ident>[0] );
    make $past;
}


method fatarrow($/) {
    my $key := PAST::Val.new( :value(~$<key>) );
    my $val := $( $<val> );
    my $past := PAST::Op.new(
        :node($/),
        :inline("   %0[%1] = %2\n" ~
                "   %r = %0\n"),
        :returns('Pair'),
        PAST::Op.new(
            :pasttype('callmethod'),
            :name('new'),
            PAST::Var.new(
                :name('Pair'),
                :scope('package')
            )
        ),
        $key,
        $val
    );
    make $past;
}


method colonpair($/, $key) {
    my $pair_key;
    my $pair_val;

    if $key eq 'false' {
        $pair_key := PAST::Val.new( :value(~$<ident>) );
        $pair_val := PAST::Val.new( :value(0), :returns('Integer') );
    }
    elsif $key eq 'value' {
        $pair_key := PAST::Val.new( :value(~$<ident>) );
        if $<postcircumfix> {
            # What type of postcircumfix?
            my $type := substr($<val>, 0, 1);
            if $type eq '(' {
                my $val := $( $<postcircumfix><semilist> );
                $pair_val := $val[0];
            }
            else {
                $/.panic($type ~ ' postcircumfix colonpairs not yet implemented');
            }
        }
        else {
            $pair_val := PAST::Val.new( :value(1), :returns('Integer') );
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
        :inline("   %0[%1] = %2\n" ~
                "   %r = %0\n"),
        :returns('Pair'),
        PAST::Op.new(
            :pasttype('callmethod'),
            :name('new'),
            PAST::Var.new(
                :name('Pair'),
                :scope('package')
            )
        ),
        $pair_key,
        $pair_val
    );
    make $past;
}


method whatever($/) {
    my $past := PAST::Op.new(
        :pasttype('callmethod'),
        :name('new'),
        :node($/),
        PAST::Var.new(
            :name('Whatever'),
            :scope('package'),
            :node($/)
        )
    );
    make $past;
}


# Used by all calling code to process arguments into the correct form.
sub process_arguments($call_past, $args) {
    for @($args) {
        if $_.returns() eq 'Pair' {
            $_[2].named($_[1]);
            $call_past.push($_[2]);
        } else {
            $call_past.push($_);
        }
    }
}


# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
