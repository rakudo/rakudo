class Perl6::Grammar::Actions ;

method TOP($/) {
    my $past := $( $<statement_block> );
    $past.blocktype('declaration');
    make $past;
}


method statement_block($/, $key) {
    ##  FIXME: $?BLOCK, @?BLOCK
    our $?BLOCK;
    our @?BLOCK;
    if ($key eq 'open') {
       $?BLOCK := PAST::Block.new( PAST::Stmts.new(),
                                  :blocktype('immediate'),
                                  :node($/)
                                  );
       @?BLOCK.unshift($?BLOCK);
    }
    if ($key eq 'close') {
       my $past := @?BLOCK.shift();
       $?BLOCK := @?BLOCK[0];
       $past.push($($<statementlist>));
       make $past;
    }
    PIR q<  .return () >;  # FIXME:  ought to eliminate this somehow
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
    make $( $/{$key} );
}


method statement_control($/, $key) {
    make $( $/{$key} );
}


method if_statement($/) {
    my $cond := +$<EXPR> - 1;
    my $past := PAST::Op.new( $( $<EXPR>[$cond] ),
                              $( $<block>[$cond] ),
                              :pasttype('if'),
                              :node( $/ )
                            );
    if ( $<else> ) {
        $past.push( $( $<else>[0] ) );
    }
    while ($cond != 0) {
        $cond := $cond - 1;
        $past := PAST::Op.new( $( $<EXPR>[$cond] ),
                               $( $<block>[$cond] ),
                               $past,
                               :pasttype('if'),
                               :node( $/ )
                             );
    }
    make $past;
}


method unless_statement($/) {
    my $past := PAST::Op.new( $( $<EXPR> ),
                              $( $<block> ),
                              :pasttype('unless'),
                              :node( $/ )
                            );
    make $past;
}


method use_statement($/) {
    make PAST::Stmts.new( :node($/) );
}


method term($/, $key) {
    make $( $/{$key} );
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
        if (~$<declarator> eq 'our') { $scope := 'package'; }
        $?BLOCK.symbol($name, :scope($scope));
    }
    make $past;
}


method variable($/, $key) {
    make PAST::Var.new( :node($/), :name( ~$/ ), :viviself('Undef') );
}


method circumfix($/, $key) {
    my $past;
    if ($key eq '( )') {
        $past := $( $<statementlist> );
    }
    if ($key eq '{ }') {
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


method quote($/) {
    make $( $<quote_expression> );
}


method quote_expression($/) {
    my $past;
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
        $past := PAST::Val.new( :value( ~$<quote_literal> ), :node($/) );
    }
    if ($key eq 'variable') {
        $past := $( $<variable> );
    }
    make $past;
}


method listop($/) {
    my $past := $( $<arglist> );
    if (~$past.name() ne 'infix:,') {
        $past := PAST::Op.new($past);
    }
    $past.name( ~$<sym> );
    $past.pasttype('call');
    $past.node($/);
    make $past;
}


method arglist($/) {
    make $( $<EXPR> );
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



