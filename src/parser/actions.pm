# Copyright (C) 2007-2009, The Perl Foundation.
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

# $?RAKUDO_HLL identifies the .HLL to use for compilation --
# it's ultimately set by the .RAKUDO_HLL macro in F<perl6.pir> .
our $?RAKUDO_HLL;

method TOP($/) {
    my $past := $<statement_block>.ast;
    $past.blocktype('declaration');
    $past.hll($?RAKUDO_HLL);
    declare_implicit_routine_vars($past);
    $past.lexical(0);

    #  Make sure we have the interpinfo constants.
    $past.unshift( PAST::Op.new( :inline('.include "interpinfo.pasm"') ) );
    # Set package for unit mainline
    $past.unshift(set_package_magical());

    # Create the unit's startup block, unless it's suppressed.
    our $?SUPPRESS_MAIN;
    my $main;
    our @?BLOCK;
    if $?SUPPRESS_MAIN {
        $past.push(PAST::Stmts.new());
        $main := $past;
    }
    elsif +@?BLOCK && @?BLOCK[0]<eval> == 1 {
        $past.blocktype('immediate');
        $past.lexical(1);
        @?BLOCK[0].push($past);
        $main := @?BLOCK[0];
    }
    else {
        $main := PAST::Block.new( :pirflags(':main') );
        $main.loadinit().push(
            PAST::Op.new( :inline('$P0 = compreg "perl6"',
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
    }

    $main.hll($?RAKUDO_HLL);
    my $?FILE := Q:PIR { %r = find_caller_lex '$?FILES' };
    $main.unshift(PAST::Op.new(:inline(".annotate 'file', '" ~ $?FILE ~ "'")));
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
        $past.push($<statementlist>.ast);
        $past.hll($?RAKUDO_HLL);
        make $past;
    }
}


method block($/) {
    my $past := $<statement_block>.ast;
    unless $past<pkgdecl> {
        set_block_type($past, 'Block');
    }
    make $past;
}


method statementlist($/) {
    my $past := PAST::Stmts.new( :node($/) );
    for $<statement> {
        $past.push( $_.ast );
    }
    make $past;
}


method statement($/, $key) {
    my $past;
    if $key eq 'control' {
        $past := $<statement_control>.ast;
    }
    elsif $key eq 'null' {
        $past := PAST::Stmts.new();
    }
    else {
        my $sml;
        $past := $<expr>.ast;
        if $past.isa(PAST::Block) && !$past.blocktype() {
            $past.blocktype('immediate');
        }
        if $key eq 'mod_cond' {
            my $body := $past;
            $past := $<statement_mod_cond>.ast;
            $past.push( $body );
            $past.push( PAST::Op.new( :name('list') ) );
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
            $past := $sml.ast;
            $past.push( $body );
        }
    }
    make $past;
}


method statement_control($/, $key) {
    make $/{$key}.ast;
}


method if_statement($/) {
    my $count := +$<xblock> - 1;
    my $past  := $<xblock>[$count].ast;
    declare_implicit_block_vars($past[1], 0);
    ## add any 'else' clause
    if $<pblock> {
        my $else := $<pblock>[0].ast;
        $else.blocktype('immediate');
        declare_implicit_block_vars($else, 0);
        $past.push( $else );
    }
    ## build if/then/elsif structure
    while $count != 0 {
        $count--;
        my $else := $past;
        $past := $<xblock>[$count].ast;
        declare_implicit_block_vars($past[1], 0);
        $past.push($else);
    }
    make $past;
}

method unless_statement($/) {
    my $past := $<xblock>.ast;
    $past.pasttype('unless');
    declare_implicit_block_vars($past[1], 0);
    make $past;
}

method while_statement($/) {
    my $past := $<xblock>.ast;
    $past.pasttype(~$<sym>);
    declare_implicit_block_vars($past[1], 0);
    make $past;
}

method repeat_statement($/) {
    my $cond  := $<EXPR>.ast;
    my $block := $<block>.ast;
    $block.blocktype('immediate');
    declare_implicit_block_vars($block, 0);
    # pasttype is 'repeat_while' or 'repeat_until'
    my $pasttype := 'repeat_' ~ ~$<loop>;
    make PAST::Op.new( $cond, $block, :pasttype($pasttype), :node($/) );
}

method given_statement($/) {
    my $past := $<xblock>.ast;
    $past.push( $past.shift() );              # swap <EXPR> and <pblock>
    $past[0].blocktype('declaration');
    declare_implicit_function_vars($past[0]);
    $past.pasttype('call');
    make $past;
}

method when_statement($/) {
    my $block := $<block>.ast;
    $block.blocktype('immediate');
    declare_implicit_block_vars($block, 0);

    # Push a handler onto the innermost block so that we can exit if we
    # successfully match
    when_handler_helper($block);

    # Invoke smartmatch of the expression.
    my $match_past := process_smartmatch(
        PAST::Var.new( :name('$_') ),
        $<EXPR>.ast,
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
    my $block := $<block>.ast;
    $block.blocktype('immediate');
    declare_implicit_block_vars($block, 0);

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
    my $block := $<block>.ast;
    $block.blocktype('immediate');
    declare_implicit_block_vars($block, 0);
    my $cond  := $<e2> ?? $<e2>[0].ast !! 1;
    my $loop := PAST::Op.new( $cond, $block, :pasttype('while'), :node($/) );
    if $<e3> {
        $loop.push( $<e3>[0].ast );
    }
    if $<e1> {
        $loop := PAST::Stmts.new( $<e1>[0].ast, $loop, :node($/) );
    }
    make $loop;
}

method for_statement($/) {
    my $past := $<xblock>.ast;
    $past.pasttype('for');
    $past[0] := PAST::Op.new(:name('list'), $past[0]);
    declare_implicit_function_vars($past[1]);
    make $past;
}

method pblock($/) {
    my $block := $<block>.ast;
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
    my $pblock := $<pblock>.ast;
    $pblock.blocktype('immediate');
    prevent_null_return($pblock);
    my $past := PAST::Op.new(
        $<EXPR>.ast, $pblock,
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
        my $use_call := PAST::Op.new(
            PAST::Val.new( :value($name) ),
            :name('use'),
            :pasttype('call'),
            :node( $/ )
        );

        ##  Handle tags.
        my $tags;
        if $<EXPR> {
            $tags := $<EXPR>[0].ast;
            if !($tags.isa(PAST::Op) && $tags.name() eq 'infix:,') {
                $tags := PAST::Op.new( $tags );
            }
            for @($tags) {
                if $_.returns() ne 'Pair' {
                    $/.panic("Unknown import list expression in use");
                }
            }
            $tags.name('hash');
            $tags.pasttype('call');
            $tags.named('tags');
            $use_call.push($tags);
        }

        ##  Handle versioning
        my $ver;
        if $<colonpair> {
            $ver := PAST::Op.new( :pasttype('call'), :name('hash') );
            for $<colonpair> {
                my $pair := $_.ast;
                $ver.push( $pair );
                if $pair[0].value() eq 'from' {
                    $/.add_type($name);
                }
            }
            $ver.named('ver');
            $use_call.push($ver);
        }
        @?BLOCK[0].loadinit().push($use_call);

        ##  ...and load it immediately to get its BEGIN semantics and
        ##  symbols for the current compilation.
        ##  XXX Need to handle tags here too, and creating needed lexical
        ##  slots.
        our @?NS;
        my %ver_hash;
        for @($ver) { if $_ { %ver_hash{$_[0].value()} := $_[1].value() } }
        if $tags {
            my %tag_hash;
            for @($tags) { %tag_hash{$_[0].value()} := 1 }
            use($name, :import_to(@?NS ?? @?NS[0] !! ''), :ver(%ver_hash), :tags(%tag_hash));
        } else {
            use($name, :import_to(@?NS ?? @?NS[0] !! ''), :ver(%ver_hash),);
        }
    }
    $past := PAST::Stmts.new( :node($/) );
    make $past;
}

method begin_statement($/) {
    my $past := $<block>.ast;
    $past.blocktype('declaration');
    declare_implicit_routine_vars($past);                  # FIXME
    my $sub := PAST::Compiler.compile( $past );
    $sub();
    # XXX - should emit BEGIN side-effects, and do a proper return()
    make PAST::Block.new();
}

method start_statement($/) {
    make make_start_block($<block>.ast);
}

method end_statement($/) {
    my $past := $<block>.ast;
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
    my $past := $<block>.ast;
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
    my $past := $<block>.ast;
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


method no_statement($/) {
    if ~$<module_name><name> eq 'Main' {
        our $?SUPPRESS_MAIN := 1;
    }
    make PAST::Stmts.new();
}


method statement_mod_loop($/) {
    my $expr := $<EXPR>.ast;
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
    my $expr := $<EXPR>.ast;
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
    my $past := $<statement>.ast;
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
        my $elsepir  := "    new %r, ['Failure']\n    store_lex '$!', %r";
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
    my $past :=  $<declarator> ?? $<declarator>.ast !! $<routine_def>.ast;

    if $past.isa(PAST::Block) {
        # If we have a multi declarator, must have a named routine too.
        if $sym ne "" && $past.name() eq "" {
            $/.panic("'" ~ $<sym> ~ "' can only be used on named routines");
        }

        # If we're declaring a multi or a proto, flag the sub as :multi,
        # and transform the sub's container to a Perl6MultiSub.
        if $sym eq 'multi' || $sym eq 'proto' {
            transform_to_multi($past);
            our @?BLOCK;
            my $existing := @?BLOCK[0].symbol($past.name());
            @?BLOCK[0].symbol($past.name(), :does_callable(1),
                              :is_proto($sym eq 'proto' || $existing<is_proto>),
                              :is_multi($sym eq 'multi'));
        }

        # Protos also need the proto property setting on them, plus we note
        # that we have one in scope.
        if $<sym> eq 'proto' {
            $past.loadinit().push(
                PAST::Op.new(:inline('    setprop block, "proto", %0'), 1)
            );
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
    my $values := $/{$key}.ast;

    my $name := ~$<name>[0];
    if $name {
        # It's a named enumeration. Ensure the type isn't already declared.
        if $/.type_redeclaration() {
            $/.panic("Re-declaration of type " ~ $name);
        }

        # Get all of the names of the enum values we will introduce and register
        # them as type names.
        our @?BLOCK;
        my $getvals_sub := PAST::Compiler.compile(PAST::Block.new(
            :blocktype('declaration'),
            :hll($?RAKUDO_HLL),
            PAST::Op.new(
                :pasttype('call'),
                :name('!create_anon_enum'),
                $values
            ),
        ));
        my %values := $getvals_sub();
        for %values.keys() {
            @?BLOCK[0].symbol($name ~ '::' ~ $_, :does_abstraction(1));
            @?BLOCK[0].symbol($_, :does_abstraction(1));
        }

        # Emit call to enum constructor in the block's loadinit.
        @?BLOCK[0].loadinit().push(PAST::Op.new(
            :pasttype('call'),
            :name('!create_enum'),
            $name,
            $values
        ));

        # Finally, since it's a decl, we don't have anything to emit at this
        # point; just hand back empty statements block.
        make PAST::Stmts.new();
    }
    else {
        # Emit runtime call anonymous enum constructor.
        make PAST::Op.new(
            :pasttype('call'),
            :name('!create_anon_enum'),
            $values
        );
    }
}


method routine_declarator($/, $key) {
    my $past;
    if $key eq 'sub' {
        $past := $<routine_def>.ast;
        set_block_type($past, 'Sub');
    }
    elsif $key eq 'method' {
        $past := $<method_def>.ast;
        set_block_type($past, 'Method');
        if $past.name() eq 'BUILD' {
            warn("BUILD declared as a method; you probably wanted to declare it as a submethod.");
        }
    }
    elsif $key eq 'submethod' {
        $past := $<method_def>.ast;
        set_block_type($past, 'Submethod');
    }
    $past.node($/);
    if (+@($past[1])) {
        declare_implicit_routine_vars($past);
    }
    else {
        $past[1].push( PAST::Op.new( :name('list') ) );
    }
    ##  Add a call to !SIGNATURE_BIND to fixup params and do typechecks, and
    ##  a return to make sure we type-check any implicitly return values for
    ##  routines with return type constraints.
    $past[0].push(
        PAST::Op.new( :pasttype('call'), :name('!SIGNATURE_BIND') )
    );
    add_return_type_check_if_needed($past);
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
    our $?BLOCK_OPEN;
    unless $?BLOCK_OPEN {
        $?BLOCK_OPEN := PAST::Block.new( PAST::Stmts.new(), :node($/) );
    }
    my $block := $?BLOCK_OPEN;
    $block.blocktype('declaration');
    if $<deflongname> {
        my $name := ~$<deflongname>[0];
        my $match := Perl6::Grammar::opname($name, :grammar('Perl6::Grammar') );
        if $match { $name := add_optoken($block, $match); }
        our @?BLOCK;
        my $existing := @?BLOCK[0].symbol($name);
        if $existing && !$existing<is_proto> && !$existing<is_multi> {
            warn("Redefinition of routine " ~ $name);
        }
        elsif !$existing || !$existing<is_proto> {
            @?BLOCK[0].symbol( $name, :scope('package') );
        }
        $block.name( $name );
    }
    $block.control(return_handler_past());
    block_signature($block);
    $block<default_param_type_node>.name('Any');

    if $<trait> {
        my $loadinit := $block.loadinit();
        my $blockreg := PAST::Var.new( :name('block'), :scope('register') );
        for @($<trait>) {
            my $name := $_.ast.name;
            if $name eq 'trait_mod:returns' || $name eq 'trait_mod:of' {
                $block<has_return_constraint> := 1;
            }
        }
        emit_traits($<trait>, $loadinit, $blockreg);
    }
    make $block;
}


method method_def($/) {
    my $block := $<block>.ast;
    $block.blocktype('method');

    if $<deflongname> {
        my $name := ~$<deflongname>;
        if $<meth_mod> eq '!' { $name := '!' ~ $name }
        my $match := Perl6::Grammar::opname($name, :grammar('Perl6::Grammar') );
        if $match { $name := add_optoken($block, $match); }
        $block.name( $name );
    }

    $block.control(return_handler_past());
    block_signature($block);
    $block<default_param_type_node>.name('Any');

    # Add lexical 'self' and a slot for the candidate dispatcher list.
    $block[0].unshift(
        PAST::Var.new( :name('self'), :scope('lexical'), :isdecl(1),
            :viviself( PAST::Var.new( :name('self'), :scope('register' ) ) )
        )
    );
    $block[0].unshift(PAST::Var.new( :name('__CANDIDATE_LIST__'), :scope('lexical'), :isdecl(1) ));

    # Add *%_ parameter if there's no other named slurpy or the package isn't hidden.
    my $need_slurpy_hash := 1;
    for @($block[0]) {
        if $_.isa(PAST::Var) && $_.scope() eq 'parameter' && $_.named() && $_.slurpy() {
            $need_slurpy_hash := 0;
        }
    }
    if $need_slurpy_hash && !package_has_trait('hidden') {
        $block[0].push(PAST::Var.new( :name('%_'), :scope('parameter'), :named(1), :slurpy(1) ));
        $block.loadinit().push(PAST::Op.new(
            :pasttype('callmethod'),
            :name('!add_param'),
            PAST::Var.new( :name('signature'), :scope('register') ),
            PAST::Val.new( :value('%_') ),
            PAST::Val.new( :value(1), :named('named') ),
            PAST::Val.new( :value(1), :named('slurpy') )
        ));
    }

    # Ensure there's an invocant in the signature.
    $block.loadinit().push(PAST::Op.new(
        :pasttype('callmethod'),
        :name('!add_implicit_self'),
        PAST::Var.new( :name('signature'), :scope('register') )
    ));

    # Handle traits.
    if $<trait> {
        my $loadinit := $block.loadinit();
        my $blockreg := PAST::Var.new( :name('block'), :scope('register') );
        for @($<trait>) {
            my $name := $_.ast.name;
            if $name eq 'trait_verb:returns' || $name eq 'trait_verb:of' {
                $block<has_return_constraint> := 1;
            }
        }
        emit_traits($<trait>, $loadinit, $blockreg);
    }

    # If it's a metaclass method, make it anonymous and then push a call to
    # !add_metaclass_method onto the current class definition.
    if $<meth_mod> eq '^' {
        our $?METACLASS;
        our @?BLOCK;
        $block.pirflags(~$block.pirflags() ~ ' :anon ');
        @?BLOCK[0][0].push(PAST::Op.new(
            :pasttype('call'),
            :name('!add_metaclass_method'),
            $?METACLASS,
            $block.name,
            PAST::Op.new( :inline('    .const "Sub" %r = "' ~ $block.subid ~ '"') )
        ));
    }

    make $block;
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


method trait_mod($/) {
    my $sym   := ~$<sym>;

    # Traits are mostly handled by a call to a trait_mod routine. We build
    # a call to this; the declarand will get unshifted onto it later.
    my $trait := PAST::Op.new(
        :pasttype('call'),
        :name('trait_mod:' ~ $sym)
    );

    # Now handle the bits specific to each type of trait.
    if $sym eq 'is' {
        if $<postcircumfix> {
            my $arg := $<postcircumfix>[0].ast;
            $arg.name('!capture');
            $trait.push($arg);
        }
        if $/.is_type(~$<longname>) {
            # It's a type - look it up and send it in as a positional, before
            # the parameter.
            my @name := Perl6::Compiler.parse_name(~$<longname>);
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
    }
    elsif $sym eq 'does' {
        $trait.push( $<fulltypename>.ast );
    }
    elsif $sym eq 'handles' { 
        $trait.push( $<noun>.ast );
    }
    elsif $sym eq 'will' {
        $trait.push( $<block>.ast );
        if $/.is_type(~$<identifier>) {
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
    }
    else {
        $trait.push( $<fulltypename>.ast );
    }

    make $trait;
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

        block_signature($block);

        ##  loop through parameters of signature
        my $arity := $<parameter> ?? +@($<parameter>) !! 0;
        $block.arity($arity);
        my $i                  := 0;
        my $multi_inv_suppress := 0;
        while $i < $arity {
            my $var    := $<parameter>[$i].ast;
            my $name   := $var.name();

            if $var<type_binding> {
                $sigpast.push( $var<type_binding> );
            }

            ##  add parameter to the signature object
            my $sigparam := make_sigparam( $var );

            ##  add any typechecks
            my $type := $var<type>;
            if +@($type) > 0 {
                $type.named('type');
                $sigparam.push($type);
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

            ##  add traits (we're not using this yet.)
            ##  XXX review this
            my $trait := $var<trait>;
            if $trait {
                $trait.named('trait');
                $sigparam.push($trait);
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

        ##  handle return type written with --> T
        if $<fulltypename> {
            set_return_type($block, $<fulltypename>);
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
        $past := $<fulltypename>.ast;
    }
    elsif $<EXPR> {
        $past := make_anon_subtype($<EXPR>.ast);
    }
    else {
        my $value := $<value>.ast;
        $past := PAST::Op.new(
            :name('infix:,'),
            PAST::Op.new(
                :pasttype('callmethod'),
                :name('WHAT'),
                $value
            ),
            make_anon_subtype($value)
        );
    }
    make $past;
}


method post_constraint($/) {
    my $past := make_anon_subtype($<EXPR>.ast);
    make $past;
}


method parameter($/) {
    my $sigil := $<named_param> ?? $<named_param><param_var><sigil> !! $<param_var><sigil>;
    my $quant := $<quant>;

    ##  if it was type a type capture and nothing else, need to make a PAST::Var
    my $var;
    if $<named_param> {
        $var := $<named_param>.ast;
    }
    elsif $<param_var> {
        $var := $<param_var>.ast;
    }
    else {
        unless $<type_constraint> == 1 {
            $/.panic("Invalid signature; cannot have two consecutive parameter separators.");
        }
        our @?BLOCK;
        my $name := ~$<type_constraint>[0];
        $var     := PAST::Var.new( :scope('parameter') );
        $var.name($var.unique('::TYPE_CAPTURE'));
        @?BLOCK[0].symbol( $var.name(), :scope('lexical') );
    }

    ##  handle slurpy and optional flags
    if $quant eq '*' {
        $var.slurpy( $sigil eq '@' || $sigil eq '%' );
        $var.named( $sigil eq '%' );
    }
    elsif $<named_param> {          # named
        if $quant ne '!' {          # required (optional is default)
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
        $var.viviself( $<default_value>[0]<EXPR>.ast );
    }

    ##  keep track of any type constraints
    my $typelist := PAST::Op.new( :name('all'), :pasttype('call') );
    $var<type> := $typelist;
    if $<type_constraint> {
        if $<type_constraint> != 1 {
            $/.panic("Multiple prefix constraints not yet supported");
        }
        for @($<type_constraint>) {
            my $type_past := $_.ast;
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
            $typelist.push($_.ast);
        }
    }

    # Attatch list of traits.
    $var<traitlist> := $<trait>;

    make $var;
}


method named_param($/) {
    my $var := $<param_var>.ast;
    if $<name> {
        $var.named(~$<name>);
    }
    else {
        $var.named(~$<param_var><identifier>[0]);
    }
    make $var;
}

method param_var($/) {
    my $sigil  := ~$<sigil>;
    my $twigil := ~$<twigil>[0];
    if $sigil eq '&' { $sigil := ''; }
    my $name := $sigil ~ $twigil ~ ~$<identifier>[0];
    if $twigil eq '.' {
        $name := $sigil ~ '!' ~ $<identifier>[0];
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
    my $past := $/{$key}.ast;

    if $<post> {
        for $<post> {
            my $term := $past;
            $past := $_.ast;
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
                $past<invocant_holder>.unshift(deref_invocant($term));
            }
            else {
                $past.unshift($term);
            }
        }
    }
    make $past;
}


method post($/, $key) {
    my $past := $/{$key}.ast;

    if $<postfix_prefix_meta_operator> {
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
            $/.panic("Unimplemented or invalid use of parallel dispatch");
        }
    }

    make $past;
}


method dotty($/, $key) {
    my $past;

    if $key eq '.' {
        # Just a normal method call.
        $past := $<dottyop>.ast;
    }
    elsif $key eq '!' {
        # Private method call. Need to put ! on the start of the name
        # (unless it was call to a code object, in which case we don't do
        # anything more).
        $past := $<methodop>.ast;
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
        $past := $<dottyop>.ast;
        if $/[0] eq '.?' || $/[0] eq '.+' || $/[0] eq '.*' || $/[0] eq '.^'  || $/[0] eq '.=' {
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
    # through the.dispatcher.
    if $<dottyop><methodop><variable> {
        $past.name('!dispatch_method_indirect');
        $past.pasttype('call');
    }
    $past<invocant_holder> := $past;

    make $past;
}


method dottyop($/, $key) {
    make $/{$key}.ast;
}


method methodop($/, $key) {
    my $past;

    if $key eq 'null' {
        $past := PAST::Op.new();
    }
    else {
        $past := build_call( $/{$key}.ast );
    }
    $past.pasttype('callmethod');
    $past.node($/);

    if $<name> {
        $past.name(~$<name>);
    }
    elsif $<variable> {
        $past.unshift( $<variable>.ast );
    }
    else {
        $past.name( $<quote>.ast );
    }

    make $past;
}

method postcircumfix($/, $key) {
    my $past;
    if $key eq '[ ]' {
        $past := PAST::Op.new( :name('postcircumfix:[ ]'), :node($/) );
        if $<semilist><EXPR> {
            $past.push( $<semilist>.ast );
        }
    }
    elsif $key eq '( )' {
        $past := build_call( $<semilist>.ast );
        $past.node($/);
    }
    elsif $key eq '{ }' {
        $past := build_call( $<semilist>.ast );
        $past.node($/);
        $past.name('postcircumfix:{ }');
    }
    elsif $key eq '< >' {
        $past := build_call( $<quote_expression>.ast );
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
        $past := $/{$key}.ast;
        $past<invocant_holder>.unshift(deref_invocant(PAST::Var.new(
            :name('$_'),
            :scope('lexical'),
            :viviself('Failure'),
            :node($/)
        )));
    }
    else {
        $past := $/{$key}.ast;
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
    elsif $key eq 'package_def' {
        make $<package_def>.ast;
        @?PKGDECL.shift();
    }
    elsif $key eq 'does' {
        our @?BLOCK;
        our $?METACLASS;
        my $block    := @?BLOCK[0];
        my $pkgdecl  := $block<pkgdecl>;
        unless $pkgdecl eq 'class' || $pkgdecl eq 'role' || $pkgdecl eq 'grammar' {
            $/.panic("Cannot use does package declarator outside of class, role, or grammar");
        }
        $block[0].push(PAST::Op.new(
            :name('trait_mod:does'),
            $?METACLASS,
            $<typename>.ast
        ));
        make PAST::Stmts.new()
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
        my $add := ~$<def_module_name>[0]<longname><name> eq '::' ?? '' !!
            (~$<def_module_name>[0]<longname><name> ~ ~$<def_module_name>[0]<role_params>);
        my $fqname := +@?NS ?? @?NS[0] ~ '::' ~ $add !! $add;
        @?NS.unshift($fqname);

        # Also attach traits to the node.
        our $?BLOCK_OPEN;
        $?BLOCK_OPEN<traits> := $<trait>;

        return 0;
    }
    else {
        @?NS.shift();
    }

    my $block := $/{$key}.ast;
    $block.lexical(0);
    declare_implicit_routine_vars($block);

    my $modulename;
    my $is_anon := 0;
    if $<def_module_name> && ~$<def_module_name>[0]<longname><name> ne '::' {
        $modulename :=  ~$<def_module_name>[0]<longname><name> ~ ~$<def_module_name>[0]<role_params>;
    }
    else {
        $modulename := $block.unique('!ANON');
        $is_anon := 1;
    }
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
        if !$<def_module_name> {
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

    #  Set is also flag.
    $block<isalso> := has_compiler_trait_with_val($<trait>, 'trait_mod:is', 'also');

    #  Emit traits; make sure rw and hidden, if given, were marked as handled first.
    has_compiler_trait_with_val($<trait>, 'trait_mod:is', 'rw');
    has_compiler_trait_with_val($<trait>, 'trait_mod:is', 'hidden');
    emit_traits($<trait>, $init, $?METACLASS);

    #  If it's not an "is also", have a name and aren't a role (since they can
    #  have many declarations) we need to check it's not a duplicate.
    our $?METACLASS;
    if !$block<isalso> && !$is_anon && $?PKGDECL ne 'role' {
        if $/.type_redeclaration() {
            $/.panic("Re-declaration of type " ~ ~$<def_module_name>[0]);
        }
    }

    ##  If it is an "is also", check that the type did already exist.
    if $block<isalso> && !$/.type_redeclaration() {
        $/.panic("Cannot use 'is also' on non-existent class " ~ ~$<def_module_name>[0]);
    }

    #  At the beginning, create the "class/module/grammar/role/etc"
    #  metaclass handle on which we do the other operations.
    $init.unshift(
        PAST::Op.new( :pasttype('bind'),
            PAST::Var.new(:name('metaclass'), :scope('register'), :isdecl(1) ),
            PAST::Op.new(:name('!meta_create'),
                $?PKGDECL, $modulename, ($block<isalso> ?? 1 !! 0)
            )
        )
    );

    #  ...and at the end of the block's initializer (after any other
    #  items added by the block), we finalize the composition.
    if $?PKGDECL eq 'role' {
        #  For a role, we now need to produce a copy of the role
        #  and clones of the methods (having captured the current
        #  lexical context).
        $block[0].push(
            PAST::Op.new(
                :inline('    .tailcall "!create_parametric_role"(%0)'),
                $?METACLASS
            )
        );
    }
    elsif $is_anon && ($?PKGDECL eq 'class' || $?PKGDECL eq 'grammar') {
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
    elsif !$block<isalso> {
        $block[0].push( PAST::Op.new( :name('!meta_compose'), $?METACLASS) );
    }

    make $block;
}


method scope_declarator($/) {
    our @?BLOCK;
    my $block := @?BLOCK[0];
    my $sym   := ~$<sym>;
    my $past  := $<scoped>.ast;
    my $scope := 'lexical';
    if    $sym eq 'our'      { $scope := 'package'; }
    elsif $sym eq 'has'      { $scope := 'attribute'; }
    elsif $sym eq 'state'    { $scope := 'state'; }

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
                if +@($var<type>) == 1 {
                    $type := $var<type>[0];
                }
                elsif +@($var<type>) > 1 {
                    $/.panic("Multiple prefix constraints not yet supported");
                }

                # If the var has a '.' twigil, we need to create an
                # accessor method for it in the block (class/grammar/role)
                if $var<twigil> eq '.' {
                    my $method := PAST::Block.new( :blocktype('method') );
                    if $var<sigil> eq '&' {
                        $method.name( substr($var.name(), 1) );
                    } else {
                        $method.name( substr($var.name(), 2) );
                    }
                    my $value := PAST::Var.new( :name($var.name()) );
                    my $default_readtype := package_has_trait('rw') ?? 'rw' !! 'readonly';
                    my $readtype := trait_readtype( $var<traitlist> ) || $default_readtype;
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
                        $init_value := make_attr_init_closure($init_value);
                        $init_value.named('init_value');
                        $has.push($init_value);
                    }
                    if $var<twigil> eq '.' {
                        $has.push(PAST::Val.new( :value(1), :named('accessor') ));
                    }
                    if $var<traitlist> {
                        # If we have a handles, then we pass that specially.
                        my $handles := has_compiler_trait($var<traitlist>, 'trait_mod:handles');
                        if $handles {
                            $handles[0].named('handles');
                            $has.push($handles[0]);
                        }
                        
                        # We'll make a block for calling other handles, which'll be
                        # thunked.
                        my $trait_stmts := PAST::Stmts.new();
                        emit_traits($var<traitlist>, $trait_stmts, PAST::Var.new( :name('$_'), :scope('lexical') ));
                        if +@($trait_stmts) > 0 {
                            my $trait_block := PAST::Block.new(
                                :blocktype('declaration'),
                                PAST::Var.new( :name('$_'), :scope('parameter') ),
                                $trait_stmts
                            );
                            $trait_block.named('traits');
                            $has.push($trait_block);
                        }
                    }
                    $block[0].push( $has );
                }
                else {
                    # $scope eq 'package' | 'lexical' | 'state'
                    my $viviself := PAST::Op.new( :pirop('new PsP'), $var<itype> );
                    if $init_value        { $viviself.push( $init_value ); }
                    my $init_reg_name := $viviself.unique('initvar_');
                    my $init_reg := PAST::Var.new( :name($init_reg_name), :scope('register') );
                    $var.viviself(PAST::Stmts.new(
                        PAST::Op.new(
                            :pasttype('bind'),
                            PAST::Var.new( :name($init_reg_name), :scope('register'), :isdecl(1) ),
                            $viviself
                        )
                    ));
                    emit_traits($var<traitlist>, $var.viviself(), $init_reg);
                    if $type {
                        if $var<sigil> ne '$' && $var<sigil> ne '@' && $var<sigil> ne '%' && $var<sigil> ne '' {
                            $/.panic("Cannot handle typed variables with sigil " ~ $var<sigil>);
                        }
                        $var.viviself.push(PAST::Op.new(
                            :pasttype('call'),
                            :name('trait_mod:of'),
                            $init_reg,
                            $type
                        ));
                    }
                    $var.viviself.push(PAST::Op.new( :inline('    %r = %0'), $init_reg ));
                    if $sym eq 'constant' {
                        # Do init in viviself, and then make sure we mark it readonly after
                        # that point.
                        $var := PAST::Op.new(
                            :pasttype('call'),
                            :name('infix:='),
                            $var
                        );
                        $var := PAST::Op.new( :pirop('setprop'), $var, 'readonly', 1);
                        $var<constant_value_slot> := $var[0];
                        $var<scopedecl> := 'constant';
                    }
                }
                $past[$i] := $var;
            }
            $i++;
        }
        if $scope eq 'attribute' {
            $past<scopedecl> := $scope;
            $past.pasttype('null');
        }
        elsif +@($past) == 1 { $past := $past[0]; }
        else { $past.name('infix:,'); $past.pasttype('call'); }
        if $scope eq 'state' {
            $past<scopedecl> := $scope;
            block_has_state($block);
        }
    }

    # If we have a lexical sub, need to do some work. If it's single dispatch
    # then we just need to grab and bind it to a lexical. If it's a multi, we
    # need to clone the outer multi if we didn't already and push this candidate
    # onto it. To avoid doing this clone every time we invoke the block (would
    # be costly) we use state variables to persist it.
    if $past.isa(PAST::Block) && $past.blocktype() ne 'method' {
        if $scope eq 'lexical' {
            # Block needs to become anonymous.
            my $name := $past.name();
            $past.name($past.unique('block_'));

            if $past<multi_flag> {
                my $sym_info := $block.symbol($name);
                my $result := PAST::Stmts.new(:node($/));

                if $sym_info<scope> ne 'lexical' {
                    # First multi of this name. Create state var for storing candidate
                    # list.
                    my $outer := outer_symbol($name, 1);
                    $result.push(PAST::Var.new(
                        :name($name),
                        :scope('state'),
                        :isdecl(1),
                        :viviself(PAST::Op.new(
                            :pasttype('call'),
                            :name('!clone_multi_for_lexical'),
                            $outer<scope> eq 'lexical' ??
                                PAST::Op.new( :inline("    %r = find_lex_skip_current '" ~ $name ~ "'") ) !!
                                PAST::Var.new( :name($name), :scope('package') )
                        ))
                    ));
                    block_has_state($block);
                    $block.symbol($name, :scope('lexical'), :does_callable(1), :is_multi(1));
                }
                elsif !$sym_info<is_multi> {
                    $/.panic('only sub conflicts with multi');
                }
                
                # Emit START block for adding this candidate.
                $result.push(make_start_block(PAST::Block.new(
                    PAST::Stmts.new(),
                    PAST::Op.new(
                        :pasttype('callmethod'),
                        :name('push'),
                        PAST::Var.new( :name($name), :scope('lexical') ),
                        $past
                    )
                )));
                $past := $result;
            }
            else {
                $past := PAST::Var.new(
                    :name($name),
                    :scope('lexical'),
                    :isdecl(1),
                    :viviself($past)
                );
                $block.symbol($name, :scope('lexical'), :does_callable(1));
            }
        }
        elsif $scope ne 'package' {
            $/.panic('Can not use ' ~ $scope ~ ' scope with a sub.');
        }
    }

    make $past;
}


method scoped($/) {
    my $past;
    if $<declarator> {
        $past := $<declarator>.ast;
    }
    elsif $<multi_declarator> {
        $past := $<multi_declarator>.ast;
        if $past.isa(PAST::Var) {
            my $type := $past<type>;
            for @($<fulltypename>) {
                $type.push( $_.ast );
            }
            if $past<sigil> eq '$' {
                # Scalars auto-vivify to the proto of their type.
                $past.viviself( $<fulltypename>[0].ast.clone() );
            }
        }
        elsif $past.isa(PAST::Block) && $<fulltypename> {
            set_return_type($past, $<fulltypename>);
            add_return_type_check_if_needed($past);
        }
    }
    make $past;
}


method declarator($/) {
    my $past;
    if $<variable_declarator> {
        $past := $<variable_declarator>.ast;
    }
    elsif $<constant_declarator> {
        $past := $<constant_declarator>.ast;
    }
    elsif $<signature> {
        $past := $<signature>.ast;
        our $?BLOCK_OPEN;
        $?BLOCK_OPEN := 0;
    }
    elsif $<routine_declarator> {
        $past := $<routine_declarator>.ast;
    }
    make $past;
}


method variable_declarator($/) {
    our @?BLOCK;
    my $var    := $<variable>.ast;

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
        $var<traitlist> :=  $<trait>;
    }

    make $var;
}


method constant_declarator($/) {
    our @?BLOCK;
    my $past := PAST::Var.new(
        :name(~$<identifier>),
        :scope('lexical'),
    );
    $past<itype> := container_itype('Perl6Scalar');
    $past<type>  := PAST::Op.new( :name('and'), :pasttype('call') );
    $/.add_type(~$<identifier>);
    @?BLOCK[0].symbol(~$<identifier>, :scope('lexical'));
    make $past;
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
                $?BLOCK.loadinit().push( make_sigparam( $param ) );
            }
            ## use twigil-less form afterwards
            $twigil := '';
        }

        $var := PAST::Var.new( :name($varname), :node($/) );
        $var<sigil> := ~$<sigil>;
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
                block_signature($?BLOCK);
                $?BLOCK.loadinit().push( make_sigparam( $param ) );
                $?BLOCK[0].unshift($param);
            }
        }

        # Until PCT has 'name' scope, we handle lexical/package lookup here.
        if $<sigil> eq '&' {
            my $sym := outer_symbol($varname);
            $var.scope( ($sym && $sym<scope>) || 'package');
            if $var.scope() eq 'package' {
                $var.viviself(PAST::Op.new( :pasttype('call'), :name('undef') ));
            }
        }

        # The ! twigil always implies attribute scope and needs self.
        if $twigil eq '!' {
            $var.scope('attribute');
            $var.unshift( PAST::Var.new( :name('self'), :scope('lexical') ) );
        }

    }
    elsif $key eq 'methcall' {
        my $name := ~$<longname>;
        my $sigil := ~$<sigil>;
        if $sigil eq '&' { $sigil := ''; }

        # Normally $.foo is a method call, so we return a PAST::Op node for it.
        $var := $<postcircumfix> ?? $<postcircumfix>[0].ast !! PAST::Op.new();
        $var.pasttype('callmethod');
        $var.name($name);
        $var.unshift( PAST::Var.new( :name('self'), :scope('lexical') ) );
        $var.node($/);

        # Sometimes $.foo is an attribute declaration, so we create a
        # PAST::Var node in $var<vardecl> where it can be retrieved
        # by <variable_declarator>.  (Eventually we'll be able to use
        # $*IN_DECL to decide which to return.)
        my $vardecl := PAST::Var.new( 
                           :name($sigil ~ '!' ~ $name),
                           :scope('attribute'),
                           :node($/),
                           PAST::Var.new( :name('self'), :scope('lexical') ) );
        $vardecl<sigil> := ~$<sigil>;
        $vardecl<twigil> := '.';
        $var<vardecl> := $vardecl;
    }
    elsif $key eq 'special_variable' {
        $var := $<special_variable>.ast;
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
        $var := $<postcircumfix>.ast;
        $var.unshift( PAST::Var.new( :scope('lexical'), :name('$/'),
                                     :viviself('Failure'), :node($/) )
        );
    }
    elsif $key eq 'subnoun' {
        my $varname := ~$<sublongname>;
        my $match := 
            Perl6::Grammar::opname($varname, :grammar('Perl6::Grammar'));
        if $match { $varname := ~$match<category> ~ ':' ~ ~$match[0]; }
        $var := PAST::Var.new( :name($varname), :node($/) );
        $var<sigil> := '';
        my $sym := outer_symbol($varname);
        $var.scope( ($sym && $sym<scope>) || 'package');
        if $var.scope() eq 'package' {
            $var.viviself(PAST::Op.new( :pasttype('call'), :name('undef') ));
        }
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
                     ?? $<statementlist>.ast
                     !! PAST::Op.new(:pirop('new Ps'), 'Nil');
    }
    if $key eq '[ ]' {
        $past := PAST::Op.new(:name('circumfix:[ ]'), :node($/) );
        if $<statementlist><statement> { $past.push( $<statementlist>.ast ); }
    }
    elsif $key eq '{ }' {
        # If it is completely empty or consists of a single list, the first
        # element of which is either a hash or a pair, it's a hash constructor.
        $past := $<pblock>.ast;
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
        my $call_on := $<semilist>.ast;
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
    make $/{$key}.ast;
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
    my $past := $<typename>.ast;
    if substr( ~$<typename>, 0, 2) eq '::' {
        $past.isdecl(1);
        $past.scope('lexical');
    }
    if $<postcircumfix> {
        my $call := $<postcircumfix>[0].ast;
        $call.unshift($past);
        $past := $call;
    }
    if $<fulltypename> {
        $past := PAST::Op.new(
            :pasttype('call'),
            :name('postcircumfix:[ ]'),
            $past,
            $<fulltypename>[0].ast
        );
    }
    make $past;
}


method number($/, $key) {
    make $/{$key}.ast;
}


##  for a variety of reasons, this is easier in PIR than NQP for now.
##  NQP doesn't have assign yet, and Str is lighter-weight than Str.
method integer($/) {
    my $str;
    PIR q<  $P0 = find_lex '$/'   >;
    PIR q<  $S0 = $P0             >;
    PIR q<  $P1 = new ['Str']  >;
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
    PIR q<  $P1 = new ['Str']  >;
    PIR q<  assign $P1, $S0       >;
    PIR q<  store_lex '$str', $P1 >;
    make PAST::Val.new(
        :value( +$str ),
        :returns('Num'),
        :node( $/ )
    );
}

method radint($/, $key) {
    make $/{$key}.ast;
}

method rad_number($/) {
    my $radix    := ~$<radix>;
    my $intpart  := ~$<intpart>;
    my $fracpart := ~$<fracpart>;
    my $base;
    my $exp;
    if defined( $<base>[0] ) { $base := ~$<base>[0]; }
    if defined( $<exp>[0] ) { $exp := ~$<exp>[0]; }
    if ~$<postcircumfix> {
        my $radcalc := $<postcircumfix>.ast;
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
    my $past := $<quote_expression>.ast;
    if $<x> eq 'x' {
        $past := PAST::Op.new( :name('!qx'), :pasttype('call'), $past );
    }
    make $past;
}

method quote_expression($/, $key) {
    my $past;
    if $key eq 'quote_concat' {
        if +$<quote_concat> == 1 {
            $past := $<quote_concat>[0].ast;
        }
        else {
            $past := PAST::Op.new(
                :name('list'),
                :pasttype('call'),
                :node( $/ )
            );
            for $<quote_concat> {
                $past.push( $_.ast );
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
        set_block_type($past, 'Regex');
    }
    elsif $key eq 'quote_p5regex' {
        $past := PAST::Block.new(
            $<quote_p5regex>,
            :compiler('PGE::P5Regex'),
            :blocktype('declaration'),
            :node( $/ )
        );
        set_block_type($past, 'Regex');
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
    my $past := $quote_term[0].ast;
    while ($count != $terms) {
        $past := PAST::Op.new(
            $past,
            $quote_term[$count].ast,
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
            :value( ~$<quote_literal>.ast ),
            :returns('Str'), :node($/)
        );
    }
    elsif ($key eq 'variable') {
        $past := PAST::Op.new( $<variable>.ast, :name('prefix:~'), :pasttype('call') );
    }
    elsif ($key eq 'circumfix') {
        $past := $<circumfix>.ast;
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

    if $key eq '*' {
        # Whatever.
        $past := make_whatever($/);
    }
    elsif $key eq 'noarg' {
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
        $past := $<args>.ast;
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
        $past := build_call( $<semilist>.ast );
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
            $<variable>.ast
        );
    }
    elsif $key eq 'sigil' {
        my $method := contextualizer_name($/, $<sigil>);

        $past := PAST::Op.new(
            :pasttype('callmethod'),
            :name($method),
            :node($/),
            $<arglist>.ast
        );
    }
    else { $past := $/{$key}.ast; }
    $past.node($/);
    make $past;
}


method term_START($/) {
    make make_start_block($<block>.ast);
}


method args($/, $key) {
    my $past := build_call( $key eq 'func args'
        ?? $<semilist>.ast
        !! $<arglist>.ast
    );
    make $past;
}


method semilist($/) {
    my $past := $<EXPR>
        ?? $<EXPR>[0].ast
        !! PAST::Op.new( :node($/), :name('infix:,') );
    make $past;
}


method arglist($/) {
    my $past := $<EXPR>
        ?? $<EXPR>.ast
        !! PAST::Op.new( :node($/), :name('infix:,') );
    make $past;
}


method EXPR($/, $key) {
    my $type := ~$<type>;

    if $key eq 'end' {
        make $<expr>.ast;
    }
    elsif +@($/) == 2 && $/[0].ast<scopedecl> eq 'state' && $<top><lvalue> {
        # State variables - only want to actually do an assignment if
        # there is no value.
        my $lhs := $/[0].ast;
        my $rhs := $/[1].ast;
        make PAST::Op.new(
            :pasttype('unless'),
            :node($/),
            PAST::Op.new(
                :pasttype('call'),
                :name('!state_var_inited'),
                $lhs.isa(PAST::Var) ?? $lhs.name() !! $lhs[0].name()
            ),
            PAST::Op.new(
                :pasttype('call'),
                :name('infix:='),
                :lvalue(1),
                $lhs,
                $rhs
            )
        );
    }
    elsif ~$type eq 'infix:=' {
        my $lhs := $/[0].ast;
        my $rhs := $/[1].ast;
        my $past;

        if $lhs<scopedecl> eq 'attribute' {
            # Need to transform RHS into an anonymous method.
            $rhs := make_attr_init_closure($rhs);
            $rhs.named('init_value');
            our $?METACLASS;
            $past := PAST::Op.new( :name('!meta_attribute'),
                         $?METACLASS, $lhs[0].name(), $rhs
            );
            our @?BLOCK;
            @?BLOCK[0][0].push($past);
            $past := PAST::Stmts.new();
        }
        elsif $lhs<scopedecl> eq 'constant' {
            $lhs<constant_value_slot>.push($rhs);
            $past := $lhs;
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
        my $invocant  := $/[0].ast;
        my $call      := $/[1].ast;

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
            $/[0].ast,
            :pasttype('call'),
            :name(~$type),
            :node($/)
        );
        my $rhs := $/[1].ast;
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
        my $lhs := $/[0].ast;
        my $rhs := $/[1].ast;
        make process_smartmatch($lhs, $rhs, $/[1]);
    }
    elsif ~$type eq 'prefix:|' {
        # Need to make it flatten the argument.
        my $past := $/[0].ast;
        $past.flat(1);
        if $past<sigil> eq '%' {
            $past.named(1);
        }
        make $past;
    }
    elsif ~$type eq 'infix://=' || ~$type eq 'infix:||=' || ~$type eq 'infix:&&=' {
        my $lhs := $/[0].ast;
        my $rhs := $/[1].ast;
        make PAST::Op.new(
            :pasttype('call'),
            :name('infix:='),
            $lhs,
            PAST::Op.new($lhs, $rhs, :pasttype(
                ~$type eq 'infix://=' ?? 'def_or' !!
                (~$type eq 'infix:||=' ?? 'unless' !!
                 'if'))
            )
        );
    }
    else {
        my $past := PAST::Op.new(
            :node($/),
            :name($type),
            :opattr($<top>)
        );
        if $<top><subname> { $past.name(~$<top><subname>); }
        for @($/) {
            unless +$_.from() == +$_.to() { $past.push( $_.ast ) };
        }

        make $past;
    }
}


method regex_declarator($/) {
    my $sym  := ~$<sym>;
    my $past := $<regex_def>.ast;
    if $sym eq 'token'
        { $past.compiler_args( :grammar(''), :ratchet(1) ); }
    elsif $sym eq 'rule'
        { $past.compiler_args( :grammar(''), :s(1), :ratchet(1) ); }
    else
        { $past.compiler_args( :grammar('') ); }
    make $past;
}

method regex_def($/) {
    my $past := $<regex_block>.ast;
    $past.name( ~$<deflongname>[0] );
    make $past;
}

method regex_block($/) {
    make $<quote_expression>.ast;
}


method type_declarator($/) {
    # Make sure it's not a re-declaration.
    if $/.type_redeclaration() {
        $/.panic("Re-declaration of type " ~ ~$<name>);
    }

    # We need a block containing the constraint condition if there is one; if
    # not, we just pass along the PAST for Whatever, which smart-matches anything.
    my $past := make_anon_subtype($<EXPR> ?? $<EXPR>[0].ast !! make_whatever($/));

    # Create subset type.
    my @name := Perl6::Compiler.parse_name($<name>);
    $past.blocktype('declaration');
    $past.loadinit().push(PAST::Op.new(
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
                $<fulltypename>[0].ast
                !!
                PAST::Var.new(
                    :name('Any'),
                    :scope('package')
                ),
            PAST::Var.new( :name('block'), :scope('register') )
        )
    ));

    make $past;
}


method fatarrow($/) {
    my $past := PAST::Op.new(
        :node($/),
        :pasttype('call'),
        :name('infix:=>'),
        :returns('Pair'),
        PAST::Val.new( :value(~$<key>) ),
        $<val>.ast
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
            $pair_val := $<postcircumfix>[0].ast;
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
            $pair_key := PAST::Val.new( :value( ~$<desigilname> ) );
            $pair_val := PAST::Var.new(
                :name( ~$<sigil> ~ ~$<twigil> ~ $<desigilname> )
            );
        }
        else {
            $/.panic('complex varname colonpair case not yet implemented');
        }
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
    my $past := build_call( $<capture>.ast );
    $past.name('prefix:\\');
    make $past;
}


method capture($/) {
    make $<EXPR>.ast;
}


method sigterm($/) {
    my $past := $/<signature>.ast;
    make $past;
}


# search through outer blocks for a symbol table entry
sub outer_symbol($name, $skip_first?) {
    our @?BLOCK;
    my $symbol;
    for @?BLOCK {
        if !$skip_first || !($_ =:= @?BLOCK[0]) {
            $symbol := $_.symbol($name);
            if $symbol { return $symbol; }
        }
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
            $block[0].push( PAST::Var.new( 
                :name($_), :scope('lexical'), :isdecl(1), 
                :viviself(PAST::Op.new(
                    :inline('    %r = root_new ["parrot";"Perl6Scalar"]') ) ) 
            ) );
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
            my $var := PAST::Var.new( :name($_), :scope('lexical'), 
                                      :isdecl(1), :viviself($lex) );
            if $tparam && $_ eq '$_' {
                $var.scope('parameter');
                block_signature($block);
                $block.loadinit().push( make_sigparam( $var ) );
            }
            $block[0].push( $var );
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


sub trait_readtype($traits) {
    my $readtype;
    if has_compiler_trait_with_val($traits, 'trait_mod:is', 'readonly') {
        $readtype := 'readonly';
    }
    if has_compiler_trait_with_val($traits, 'trait_mod:is', 'rw') {
        $readtype := $readtype ?? 'CONFLICT' !! 'rw';
    }
    if has_compiler_trait_with_val($traits, 'trait_mod:is', 'copy') {
        $readtype := $readtype ?? 'CONFLICT' !! 'copy';
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
                '    %r = root_new ["parrot";"ObjectRef"], %0',
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
            PAST::Stmts.new(),
            PAST::Stmts.new(
                PAST::Op.new(
                    :name('infix:~~'),
                    :pasttype('call'),
                    PAST::Var.new( :name('$_'), :scope('lexical') ),
                    $past
                )
            )
        );
        declare_implicit_function_vars($past);
    }

    $past;
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
        $block<default_param_type_node> := PAST::Var.new(
            :scope('package'), :name('Object'), :namespace(list()) );
        $block.loadinit().push(
            PAST::Op.new( :inline('    .local pmc signature',
                                  '    signature = new ["Signature"]',
                                  '    setprop block, "$!signature", signature',
                                  '    signature."!set_default_param_type"(%0)'),
                          $block<default_param_type_node>
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
        # The following is to make sure the Parrot-level sub has a backlink
        # to the Rakudo-level object, since it's all that we can find from
        # interpinfo.
        $block.loadinit().unshift(PAST::Op.new(
            :inline("    $P0 = getattribute block, ['Sub'], 'proxy'",
                    "    setprop $P0, '$!real_self', block") ) );
        $block.loadinit().unshift($set_type);
    }
}


# Makes a routine into a multi, if it isn't already one.
sub transform_to_multi($past) {
    unless $past<multi_flag> {
        my $pirflags := ~$past.pirflags();
        $past.pirflags( $pirflags ~ ' :multi()' );
        $past.loadinit().unshift(
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
        # method truth
        $rhs<invocant_holder>[0] := deref_invocant($lhs);
        if $rhs_pt<noun><dotty><dottyop><postcircumfix> {
            # array/hash slice truth
            $rhs := PAST::Op.new( :pasttype('call'), :name('all'), $rhs);
        }
        return PAST::Op.new( :pasttype('call'), :name('prefix:?'), $rhs);
    }
    else {
        return PAST::Op.new( :pasttype('call'), :name('infix:~~'), $lhs, $rhs);
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


# This makes a START block (factored out since used as a term and a statement).
sub make_start_block($past) {
    # Set up block.
    $past.blocktype('immediate');
    declare_implicit_routine_vars($past);

    # Mark block as needing to load state.
    our @?BLOCK;
    block_has_state(@?BLOCK[0]);

    # We now need to emit code to run the block only once, and store the
    # result. We'll just piggy-back off state vars.
    return PAST::Var.new(
        :scope('state'),
        :name($past.unique('start_block_')),
        :viviself($past),
        :isdecl(1)
    );
}

# This takes a block and ensures we emit code to load any associated state
# (START blocks, state variables) at block entry.
sub block_has_state($block) {
    unless $block<needs_state_loaded> {
        $block[0].push(PAST::Op.new(
            :pasttype('call'),
            :name('!state_var_init')
        ));
        $block<needs_state_loaded> := 1;
    }
}

# Manufactures PAST to handle check of return type.
sub return_handler_past() {
    PAST::Stmts.new(
        PAST::Op.new( :inline('    exception = getattribute exception, "payload"') ),
        PAST::Op.new(
            :pasttype('if'),
            PAST::Op.new(
                :pasttype('callmethod'),
                :name('ACCEPTS'),
                PAST::Op.new( :inline("    %r = interpinfo .INTERPINFO_CURRENT_SUB",
                                      "    %r = getprop '$!real_self', %r",
                                      "    %r = %r.'of'()",
                                      "    $P0 = %r") ),
                PAST::Var.new( :name('exception'), :scope('register') )
            ),
            PAST::Op.new(
                :inline('    .return (%0)'),
                PAST::Var.new( :name('exception'), :scope('register') )
            ),
            PAST::Op.new(
                :pasttype('call'),
                :name('die'),
                PAST::Op.new(
                    :pasttype('call'),
                    :name('!make_type_fail_message'),
                    'Return value',
                    PAST::Var.new( :name('exception'), :scope('register') ),
                    PAST::Var.new( :name('$P0'), :scope('register') )
                )
            )
        )
    )
}


sub make_sigparam($var) {
    my $sigparam := 
        PAST::Op.new( :pasttype('callmethod'), :name('!add_param'), 
                      PAST::Var.new( :name('signature'), :scope('register') ),
                      $var.name() 
        );
    if $var.named() ne "" {
        $sigparam.push(PAST::Val.new( :value($var.named()), :named('named') ));
    }
    if $var.viviself() {
        $sigparam.push(PAST::Val.new( :value(1), :named('optional') ));
    }
    if $var.slurpy() {
        $sigparam.push(PAST::Val.new( :value(1), :named('slurpy') ));
    }
    $sigparam;
}


sub add_optoken($block, $match) {
    my $category := ~$match<category>;
    my $name := $category ~ ':' ~ ~$match[0];
    if $category ne 'trait_mod' {
        my $equiv := 'infix:+';
        if $category eq 'prefix' { $equiv := 'prefix:+' }
        elsif $category eq 'postfix' { $equiv := 'postfix:++' }
        elsif $category eq 'circumfix' || $category eq 'postcircumfix' { $equiv := 'term:' }
        my $past := PAST::Op.new( :name('newtok'), :pasttype('callmethod'),
            PAST::Op.new( 
                :inline("    %r = get_hll_global ['Perl6';'Grammar'], '$optable'")
            ),
            $name,
            PAST::Val.new( :value($equiv), :named('equiv') ),
        );
        my $sub := PAST::Compiler.compile( 
            PAST::Block.new( $past, :hll($?RAKUDO_HLL), :blocktype('declaration') )
        );
        $sub();
        $block.loadinit().push($past);
        if $category eq 'infix' {
            # For infix operators, we generate the meta-operators too.
            $past := PAST::Op.new(
                :name('!generate_meta_ops'), :pasttype('call'),
                $name, $equiv
            );
            $sub := PAST::Compiler.compile( 
                PAST::Block.new( $past, :hll($?RAKUDO_HLL), :blocktype('declaration') )
            );
            $sub();
            $block.loadinit().push($past);
        }
    }
    $name;
}


sub make_attr_init_closure($init_value) {
    # Need to not just build the closure, but new_closure it; otherwise, we
    # run into trouble if our initialization value involves a parameter from
    # a parametric role.
    PAST::Op.new(
        :inline('%r = newclosure %0'),
        PAST::Block.new(
            :blocktype('method'),
            PAST::Stmts.new(
                PAST::Var.new( :name('$_'), :scope('parameter') ),
                PAST::Op.new( :pasttype('bind'),
                    PAST::Var.new( :name('self'), :scope('lexical'), :isdecl(1) ),
                    PAST::Var.new( :name('self'), :scope('register') )
                )
            ),
            PAST::Stmts.new( $init_value )
        )
    );
}


sub deref_invocant($inv) {
    PAST::Op.new( :inline('    %r = descalarref %0'), $inv )
}


sub set_return_type($block, $type_parse_tree) {
    if +$type_parse_tree > 1 {
        $type_parse_tree[0].panic("Multiple prefix constraints not yet supported");
    }
    if $block<has_return_constraint> {
        $type_parse_tree[0].panic("Can not apply two sets of return types to one routine");
    }
    $block.loadinit().push(PAST::Op.new(
        :pasttype('call'),
        :name('trait_mod:of'),
        PAST::Var.new( :name('block'), :scope('register') ),
        $type_parse_tree[0].ast
    ));
    $block<has_return_constraint> := 1;
}


sub add_return_type_check_if_needed($block) {
    if $block<has_return_constraint> {
        $block[1] := PAST::Op.new(
            :pasttype('call'),
            :name('return'),
            $block[1]
        );
    }
}


sub package_has_trait($name) {
    our $?BLOCK_OPEN;
    our @?BLOCK;
    my $block := $?BLOCK_OPEN || @?BLOCK[0];
    return has_compiler_trait_with_val($block<traits>, 'trait_mod:is', $name);
}


sub make_whatever($/) {
    PAST::Op.new(
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
    )
}


# This routine checks if the given list of traits contains one of the given
# name. If so, it marks it as compiler handled so no multi call will be
# emitted when we emit the traits. If there is such a trait, it returns it's
# AST.
sub has_compiler_trait($trait_list, $name) {
    if $trait_list {
        for @($trait_list) {
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
        for @($trait_list) {
            my $ast := $_.ast;
            if $ast.name eq $name && $ast<is_name> eq $value {
                $ast<trait_is_compiler_handled> := 1;
                return $ast;
            }
        }
    }
    return 0;
}


# This sub takes a list of traits, the PAST node for a declarand and a
# target PAST node. For all non-compiler-handled traits, it unshifts
# the declarand onto the call and adds it to the target node.
sub emit_traits($trait_list, $to, $declarand) {
    if $trait_list {
        for @($trait_list) {
            my $ast := $_.ast;
            unless $ast<trait_is_compiler_handled> {
                $ast.unshift($declarand);
                $to.push($ast);
            }
        }
    }
}

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
