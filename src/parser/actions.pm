# $Id$
#
# Copyright (C) 2007, The Perl Foundation.

class Perl6::Grammar::Actions ;

method TOP($/) {
    my $past := $( $<statement_block> );
    $past.blocktype('declaration');
    make $past;
}


method statement_block($/, $key) {
    our $?BLOCK;
    our @?BLOCK;
    our $?BLOCK_SIGNATURED;
    ##  when entering a block, use any $?BLOCK_SIGNATURED if it exists,
    ##  otherwise create an empty block with an empty first child to
    ##  hold any parameters we might encounter inside the block.
    if ($key eq 'open') {
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
        }
        unless $?BLOCK.symbol('$!') {
            $init.push( PAST::Var.new( :name('$!'), :isdecl(1) ) );
            $?BLOCK.symbol( '$!', :scope('lexical') ); }
    }
    if ($key eq 'close') {
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
    if ( $<else> ) {
        my $else := $( $<else>[0] );
        $else.blocktype('immediate');
        $past.push( $else );
    }
    while ($count != 0) {
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
                    PAST::Var.new( :name('$_'), :scope('parameter') )
                ),
                :blocktype('declaration'),
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
    if ($sym eq 'do') {
        # fall through, just use the statement itself
    }
    ## after the code in the try block is executed, bind $! to Undef,
    ## and set up the code to catch an exception, in case one is thrown
    elsif ($sym eq 'try') {
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
            # Check if it's a call; if so, need special handling.
            my $term := $past;
            $past := $($_);
            $past.unshift($term);
        }
    }
    make $past;
}

method compiler_directive($/) {
    make $( $<term> );
}


method postfix($/, $key) {
    make $( $/{$key} );
}

method methodop($/, $key) {
    my $past;
    if ($key eq 'null') {
        $past := PAST::Op.new();
    }
    else {
        $past := $( $/{$key} );
    }
    $past.name(~$<ident>);
    $past.pasttype('callmethod');
    $past.node($/);
    make $past;
}

method postcircumfix($/, $key) {
    my $semilist := $( $<semilist> );
    my $past;
    if ($key eq '[ ]') {
        $past := PAST::Var.new( $semilist[0],
                                :scope('keyed'),
                                :vivibase('List'),
                                :viviself('Undef'),
                                :node( $/ )
                              );
    } elsif ($key eq '( )') {
        $past := PAST::Op.new( :node($/), :pasttype('call') );
        for @($semilist) {
            $past.push( $_ );
        }
    } else {
        $past := PAST::Var.new( $semilist[0],
                                :scope('keyed'),
                                :vivibase('Hash'),
                                :viviself('Undef'),
                                :node( $/ )
                              );
    }
    make $past;
}


method noun($/, $key) {
    make $( $/{$key} );
}


method package_declarator($/, $key) {
    our $?CLASS;

    if $key eq 'open' {
        # Start of the block; if it's a class, need to make $?CLASS available
        # for storing current class definition in.
        # XXX need array to support nested classes
        my $decl_past := PAST::Stmts.new();

        # Code to create the class.
        my $pir := "    $P0 = subclass 'Perl6Object', '" ~ $<name> ~ "'\n";
        $decl_past.push(PAST::Op.new( :inline($pir) ));

        $?CLASS := $decl_past;
    }
    else {
        my $past := $( $/{$key} );
        if $<sym> eq 'class' {
            # Declare the namespace and that this is something we do
            # "on load".
            $past.namespace($<name><ident>);
            $past.blocktype('declaration');
            $past.pirflags(':init :load');

            my $pir := "    $P1 = get_hll_global ['Perl6Object'], 'make_proto'\n" ~
                       "    $P1($P0, '" ~ $<name> ~ "')\n";
            $?CLASS.push(PAST::Op.new( :inline($pir) ));

            $past.unshift( $?CLASS );
        }
        else {
            $past.namespace($<name><ident>);
            $past.blocktype('declaration');
            $past.pirflags(':init :load');
        }
        make $past;
    }
}


method scope_declarator($/) {
    my $past := $( $<variable> );
    my $name := $past.name();
    our $?BLOCK;
    unless $?BLOCK.symbol($name) {
        $past.isdecl(1);
        my $scope := 'lexical';
        if ($<declarator> eq 'our') {
            $scope := 'package';
        }
        elsif ($<declarator> eq 'has') {
            # Set that it's attribute scope.
            $scope := 'attribute';

            # The class needs to declare it.
            our $?CLASS;
            my $class_def := $?CLASS;
            my $pir := "    addattribute $P0, '" ~ $name ~ "'\n";
            $class_def.push( PAST::Op.new( :inline($pir) ) );

            # If we have a . twigil, we need to generate an accessor.
            if $<variable><twigil>[0] eq '.' {
                my $accessor := PAST::Block.new(
                    PAST::Stmts.new(
                        PAST::Var.new( :name($name), :scope('attribute') )
                    ),
                    :name($<variable><name>),
                    :blocktype('declaration'),
                    :pirflags(':method'),
                    :node( $/ )
                );
                $?CLASS.unshift($accessor);
            }
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
    else {
        my $viviself := 'Undef';
        if $<sigil> eq '@' { $viviself := 'List'; }
        if $<sigil> eq '%' { $viviself := 'Hash'; }
        my @ident := $<name><ident>;
        my $name;
        PIR q<  $P0 = find_lex '@ident'  >;
        PIR q<  $P0 = clone $P0          >;
        PIR q<  store_lex '@ident', $P0  >;
        PIR q<  $P1 = pop $P0            >;
        PIR q<  store_lex '$name', $P1   >;
        if $<sigil> ne '&' { $name := ~$<sigil> ~ ~$name; }
        $past := PAST::Var.new( :name( $name ),
                                :viviself($viviself),
                                :node($/)
                              );
        if @ident || $<twigil>[0] eq '*' {
            $past.namespace(@ident);
            $past.scope('package');
        }
    }
    make $past;
}


method circumfix($/, $key) {
    my $past;
    if ($key eq '( )') {
        $past := $( $<statementlist> );
    }
    if ($key eq '[ ]') {
        $past := $( $<statementlist> );
    }
    elsif ($key eq '{ }') {
        $past := $( $<block> );
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
    if ($key eq 'quote_regex') {
        $past := PAST::Block.new( $<quote_regex>,
                                  :compiler('PGE::Perl6Regex'),
                                  :blocktype('declaration'),
                                  :node( $/ )
                                )
    }
    elsif ($key eq 'quote_concat') {
        if ( +$<quote_concat> == 1 ) {
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
    my $past := $($<semilist>);
    $past.name( ~$<ident> );
    $past.pasttype('call');
    $past.node($/);
    make $past;
}


method semilist($/) {
    my $past := PAST::Op.new( :node($/) );
    if ($<EXPR>) {
        my $expr := $($<EXPR>[0]);
        if ($expr.name() eq 'infix:,') {
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
    if ($key eq 'arglist') {
        $past := $( $<arglist> );
    }
    if ($key eq 'noarg') {
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
    if ($expr.name() eq 'infix:,') {
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
    if ($key eq 'end') {
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


# Builds the PAST for a sub or method call, including auto-threading of
# junctions.
sub make_call_past($/, $callee_past, $args_past) {
    my $past;

    # Build non-junctional call.
    my $call := PAST::Op.new( :node($/),
                              :pasttype('call')
                            );
    if $callee_past.WHAT() eq 'Val' {
        $call.name( $callee_past.value() );
    }
    else {
        $call.push( $callee_past );
    }
    for @($args_past) {
        $call.push( $_ );
    }

    # Build short-circuiting OR to look for junctional parameters.
    my $unless_list;
    my $num_args := 0;
    for @($args_past) {
        # If it's a value, we need not check it.
        unless $_.WHAT() eq 'Val' {
            # Build "is it a junction" check code.
            my $check := PAST::Op.new( :name('infix:eq'),
                                       :pasttype('call'),
                                       :node($/)
                                     );
            my $what := PAST::Op.new( :name('WHAT'),
                                      :pasttype('callmethod'),
                                      :node($/),
                                      $_
                                    );
            $check.push( $what );
            $check.push( PAST::Val.new( :value( "Junction" ) ) );

            if $num_args == 0 {
                $unless_list := $check
            }
            else {
                # Need to upgrade to an unless statement.
                $unless_list := PAST::Op.new( :pasttype('unless'),
                                              :node($/),
                                              $check,
                                              $unless_list
                                            );
            }

            $num_args := $num_args + 1;
        }
    }

    # If we had no args we need to check, it's easy.
    if $num_args == 0 {
        $past := $call;
    }
    else {
        # Need to build if statement to do the check.
        $past := PAST::Op.new( :pasttype('if'),
                               :node( $/ ),
                               $unless_list
                             );
        my $junc_disp := PAST::Op.new( :pasttype('call'),
                                       :node( $/ ),
                                       :name( '!junction_dispatcher' )
                                     );
        $junc_disp.push( $callee_past );
        for @($args_past) {
            $junc_disp.push( $_ );
        }
        $past.push( $junc_disp );   # then - when we have junctions
        $past.push( $call );        # else - when we don't.
    }

    $past
}


# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
