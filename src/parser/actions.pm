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

method end_statement($/) {
    $/.panic( ~$<sym> ~ ' not implemented');
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
    if ($key eq 'sub') {
        my $past := $($<routine_def>);
        $past.blocktype('declaration');
        $past.node($/);
        make $past;
    }
}


method routine_def($/) {
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
        my $param_var := $($_<param_var>);
        $past.symbol($param_var.name(), :scope('lexical'));
        $params.push($param_var);
    }
    $past.arity( +$/[0] );
    our $?BLOCK_SIGNATURED := $past;
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
            $past.unshift($term);
        }
    }
    make $past;
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
    my $past := PAST::Var.new( $semilist[0],
                               :scope('keyed'),
                               :vivibase('List'),
                               :viviself('Undef'),
                               :node( $/ )
                             );
    make $past;
}


method noun($/, $key) {
    make $( $/{$key} );
}

method scope_declarator($/) {
    my $past := $( $<variable> );
    my $name := $past.name();
    our $?BLOCK;
    unless $?BLOCK.symbol($name) {
        $past.isdecl(1);
        my $scope := 'lexical';
        if ($<declarator> eq 'our') { $scope := 'package'; }
        $?BLOCK.symbol($name, :scope($scope));
    }
    make $past;
}


method variable($/, $key) {
    my $viviself := 'Undef';
    if ($<sigil> eq '@') { $viviself := 'List'; }
    make PAST::Var.new( :node($/), :name( ~$/ ), :viviself($viviself) );
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


# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
