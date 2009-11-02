class Perl6::Actions is HLL::Actions;

our @BLOCK;
our $TRUE;

INIT {
    # initialize @BLOCK
    our @BLOCK := Q:PIR { %r = new ['ResizablePMCArray'] };
    our $TRUE := PAST::Var.new( :name('true'), :scope('register') );

    # Tell PAST::Var how to encode Perl6Str and Str values
    my %valflags := 
        Q:PIR { %r = get_hll_global ['PAST';'Compiler'], '%valflags' };
    %valflags<Perl6Str> := 'e';
    %valflags<Str>      := 'e';
}

sub xblock_immediate($xblock) {
    $xblock[1] := block_immediate($xblock[1]);
    $xblock;
}

sub block_immediate($block) {
    $block.blocktype('immediate');
    $block;
}

sub sigiltype($sigil) {
    $sigil eq '%' 
    ?? 'Hash' 
    !! ($sigil eq '@' ?? 'Array' !! 'Perl6Scalar');
}

method deflongname($/) {
    if $<sym> { make ~$<identifier> ~ ':sym<' ~ ~$<sym>[0] ~ '>'; }
}

method comp_unit($/) {
    my $past := $<statementlist>.ast;
    my $BLOCK := @BLOCK.shift;
    $BLOCK.push($past);
    $BLOCK.node($/);
    our $?RAKUDO_HLL;
    $BLOCK.hll($?RAKUDO_HLL);
    make $BLOCK;
}

method statementlist($/) {
    my $past := PAST::Stmts.new( :node($/) );
    if $<statement> {
        for $<statement> { 
            my $ast := $_.ast;
            if $ast.isa(PAST::Block) && !$ast.blocktype {
                $ast := block_immediate($ast);
            }
            $past.push( $ast ); 
        }
    }
    make $past;
}

method statement($/) { 
    my $past;
    if $<EXPR> { $past := $<EXPR>.ast; }
    elsif $<statement_control> { $past := $<statement_control>.ast; }
    else { $past := 0; }
    make $past;
}

method xblock($/) {
    make PAST::Op.new( $<EXPR>.ast, $<pblock>.ast, :pasttype('if'), :node($/) );
}

method pblock($/) {
    make $<blockoid>.ast;
}

method block($/) {
    make $<blockoid>.ast;
}

method blockoid($/) {
    my $past := $<statementlist>.ast;
    my $BLOCK := @BLOCK.shift;
    $BLOCK.push($past);
    $BLOCK.node($/);
    make $BLOCK;
}

method newpad($/) {
    our @BLOCK;
    @BLOCK.unshift( PAST::Block.new( PAST::Stmts.new(
        PAST::Op.new(
            :inline("    .local pmc true\n    true = get_hll_global 'True'")
        )
    )));
}

method finishpad($/) {
    # Generate the $_, $/, and $! lexicals if they aren't already
    # declared.  For routines and methods, they're simply created as
    # undefs; for other blocks they initialize to their outer lexical.

    my $BLOCK := @BLOCK[0];
    my $outer := $*IN_DECL ne 'routine' && $*IN_DECL ne 'method';

    for <$_ $/ $!> {
        # Generate the lexical variable except if...
        #   (1) the block already has one, or
        #   (2) the variable is '$_' and $*IMPLICIT is set
        #       (this case gets handled by getsig)
        unless $BLOCK.symbol($_) || ($_ eq '$_' && $*IMPLICIT) {
            my $base := 
                $outer
                ?? PAST::Op.new( :inline("    %r = new ['Perl6Scalar'], %0"),
                       PAST::Op.new(:pirop('find_lex_skip_current Ps'), $_)
                   )
                !! PAST::Op.new( :inline("    %r = new ['Perl6Scalar']") );
            $base := PAST::Op.new( $base, 'rw', $TRUE, :pirop('setprop') );
            $BLOCK[0].push(
                PAST::Var.new( :name($_), :scope('lexical'), :isdecl(1),
                               :viviself($base), :node($/) )
            );
            $BLOCK.symbol($_, :scope('lexical') );
        }
    }
}


## Statement control

method statement_control:sym<if>($/) {
    my $count := +$<xblock> - 1;
    my $past := xblock_immediate( $<xblock>[$count].ast );
    if $<else> {
        $past.push( block_immediate( $<else>[0].ast ) );
    }
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
        $past := PAST::Op.new( $<EXPR>.ast, block_immediate( $<pblock>.ast ),
                               :pasttype($pasttype), :node($/) );
    }
    make $past;
}

method statement_control:sym<for>($/) {
    my $past := $<xblock>.ast;
    $past.pasttype('for');
    my $block := $past[1];
    $block[0].push( PAST::Var.new( :name('$_'), :scope('parameter') ) );
    $block.symbol('$_', :scope('lexical') );
    $block.arity(1);
    $block.blocktype('immediate');
    make $past;
}

method statement_control:sym<use>($/) {
    make PAST::Stmts.new( :node($/) );
}

method statement_control:sym<return>($/) {
    make PAST::Op.new( $<EXPR>.ast, :pasttype('return'), :node($/) );
}

method statement_control:sym<make>($/) {
    make PAST::Op.new(
             PAST::Var.new( :name('$/'), :scope('contextual') ),
             $<EXPR>.ast,
             :pasttype('callmethod'),
             :name('!make'),
             :node($/)
    );
}

method statement_prefix:sym<INIT>($/) {
    @BLOCK[0].loadinit.push($<blorst>.ast);
    make PAST::Stmts.new(:node($/));
}

method blorst($/) {
    make $<block>
         ?? block_immediate($<block>.ast)
         !! $<statement>.ast;
}

## Terms

method term:sym<colonpair>($/)          { make $<colonpair>.ast; }
method term:sym<variable>($/)           { make $<variable>.ast; }
method term:sym<package_declarator>($/) { make $<package_declarator>.ast; }
method term:sym<scope_declarator>($/)   { make $<scope_declarator>.ast; }
method term:sym<routine_declarator>($/) { make $<routine_declarator>.ast; }
method term:sym<multi_declarator>($/)   { make $<multi_declarator>.ast; }
method term:sym<regex_declarator>($/)   { make $<regex_declarator>.ast; }
method term:sym<statement_prefix>($/)   { make $<statement_prefix>.ast; }

method name($/) { }

method colonpair($/) {
    my $past := $<circumfix> 
                ?? $<circumfix>[0].ast 
                !! PAST::Val.new( :value( !$<not> ) );
    $past.named( ~$<identifier> );
    make $past;
}

method variable($/) {
    my $past;
    if $<postcircumfix> {
        $past := $<postcircumfix>.ast;
        $past.unshift( PAST::Var.new( :name('$/') ) );
    }
    else {
        $past := PAST::Var.new( :name(~$/) );
        if $<twigil>[0] eq '*' { 
            $past.scope('contextual'); 
            $past.viviself( PAST::Op.new( 'Contextual ' ~ ~$/ ~ ' not found', 
                                          :pirop('die') )
            );
        }
        elsif $<twigil>[0] eq '!' {
            $past.scope('attribute');
            $past.viviself( sigiltype( $<sigil> ) );
        }
    }
    make $past;
}

method package_declarator:sym<module>($/) { make $<package_def>.ast; }
method package_declarator:sym<class>($/) {
    my $past := $<package_def>.ast;
    my $classinit :=
        PAST::Op.new(
            PAST::Op.new( 
                :inline( '    %r = get_root_global ["parrot"], "P6metaclass"')
            ),
            ~$<package_def><name>,
            :name('new_class'),
            :pasttype('callmethod')
        );
    my $parent := ~$<package_def><parent>[0]
                  || ($<sym> eq 'grammar' ?? 'Regex::Cursor' !! '');
    if $parent {
        $classinit.push( PAST::Val.new( :value($parent), :named('parent') ) );
    }
    if $past<attributes> {
        $classinit.push( $past<attributes> );
    }
    @BLOCK[0].loadinit.push($classinit);
    make $past;
}

method package_def($/) {
    my $past := $<block> ?? $<block>.ast !! $<comp_unit>.ast;
    $past.namespace( Perl6::Grammar::parse_name(~$<name>) );
    $past.blocktype('immediate');
    make $past;
}

method scope_declarator:sym<my>($/)  { make $<scoped>.ast; }
method scope_declarator:sym<our>($/) { make $<scoped>.ast; }
method scope_declarator:sym<has>($/) { make $<scoped>.ast; }

method declarator($/) {
    if    $<variable_declarator> { make $<variable_declarator>.ast }
    elsif $<routine_declarator>  { make $<routine_declarator>.ast  }
    elsif $<regex_declarator>    { make $<regex_declarator>.ast    }
    else {
        $/.CURSOR.panic('Unknown declarator type');
    }
}

method multi_declarator:sym<multi>($/) { make $<declarator> ?? $<declarator>.ast !! $<routine_def>.ast }
method multi_declarator:sym<proto>($/) { make $<declarator> ?? $<declarator>.ast !! $<routine_def>.ast }
method multi_declarator:sym<only>($/)  { make $<declarator> ?? $<declarator>.ast !! $<routine_def>.ast }
method multi_declarator:sym<null>($/)  { make $<declarator>.ast }

method scoped($/) {
    make $<routine_declarator>
         ?? $<routine_declarator>.ast
         !! $<variable_declarator>.ast;
}

method variable_declarator($/) {
    my $past := $<variable>.ast;
    my $sigil := $<variable><sigil>;
    my $name := $past.name;
    my $BLOCK := @BLOCK[0];
    if $BLOCK.symbol($name) {
        $/.CURSOR.panic("Redeclaration of symbol ", $name);
    }

    # First, create a container and give it a 'rw' property
    # Create the container, give it a 'rw' property
    my $cont := PAST::Op.new( sigiltype($sigil), :pirop('new Ps') );
    my $true := PAST::Var.new( :name('true'), :scope('register') );
    my $vivipast := PAST::Op.new( $cont, 'rw', $true, :pirop('setprop'));

    # If it's an array or hash, it flattens in list context.
    if $sigil eq '@' || $sigil eq '%' {
        $vivipast := PAST::Op.new($vivipast,'flatten',$true,:pirop('setprop'));
    }

    # For 'our' variables, we first bind or lookup in the namespace
    if $*SCOPE eq 'our' {
        $vivipast := PAST::Var.new( :name($name), :scope('package'), :isdecl(1),
                                     :lvalue(1), :viviself($vivipast), :node($/) );
    }
    # Now bind a lexical in the block
    my $decl := PAST::Var.new( :name($name), :scope('lexical'), :isdecl(1),
                               :lvalue(1), :viviself($vivipast), :node($/) );
    $BLOCK.symbol($name, :scope('lexical') );
    $BLOCK[0].push($decl);
    make $past;
}

method routine_declarator:sym<sub>($/) { make $<routine_def>.ast; }
method routine_declarator:sym<method>($/) { make $<method_def>.ast; }

method routine_def($/) {
    my $past := $<blockoid>.ast;
    $past.blocktype('declaration');
    $past.control('return_pir');
    add_signature($past, $<signature> ?? $<signature>[0].ast !! Perl6::Compiler::Signature.new());
    if $<deflongname> {
        # Set name.
        my $name := '&' ~ ~$<deflongname>[0].ast;
        $past.name($name);
        $past.nsentry($*SCOPE eq 'our' ?? $name !! '');

        # Wrap it in the correct routine type.
        $past := wrap_parrot_sub($past, 'Sub');

        # Handle multi-ness, if any.
        my $symbol := @BLOCK[0].symbol($name);
        if $*MULTINESS eq 'only' {
            if $symbol {
                $/.CURSOR.panic('Can not declare only routine ' ~ $name ~
                    ' when another routine with this name was already declared');
            }
        }
        elsif $*MULTINESS || ($symbol && $symbol<multis>) {
            # If no multi declarator and no proto, error.
            if !$*MULTINESS && !$symbol<proto> {
                $/.CURSOR.panic('Can not re-declare sub ' ~ $name ~ ' without declaring it multi');
            }

            # If it's a proto, stash it away in the symbol entry.
            if $*MULTINESS eq 'proto' { @BLOCK[0].symbol($name, :proto($past)) }

            # Otherwise, create multi container if we don't have one; otherwise,
            # just push this candidate onto it.
            if $symbol<multis> {
                $symbol<multis>.push($past);
                $past := 0;
            }
            else {
                $past := PAST::Op.new(
                    :pasttype('callmethod'),
                    :name('set_candidates'),
                    PAST::Op.new( :inline('    %r = new ["Perl6MultiSub"]') ),
                    $past
                );
                @BLOCK[0].symbol($name, :multis($past))
            }
        }

        # Install in lexical scope if it's not package scoped.
        if $*SCOPE ne 'our' {
            if $past {
                @BLOCK[0][0].push(PAST::Var.new( :name($name), :isdecl(1), 
                                      :viviself($past), :scope('lexical') ) );
                @BLOCK[0].symbol($name, :scope('lexical') );
            }
            $past := PAST::Var.new( :name($name) );
        }
    }
    elsif $*MULTINESS {
        $/.CURSOR.panic('Can not put ' ~ $*MULTINESS ~ ' on anonymous routine');
    }
    make $past;
}


method method_def($/) {
    my $past := $<blockoid>.ast;
    $past.blocktype('method');
    $past.control('return_pir');
    $past[0].unshift( PAST::Op.new( :inline('    .lex "self", self') ) );
    $past.symbol('self', :scope('lexical') );
    add_signature($past, $<signature>.ast);
    if $<deflongname> {
        my $name := ~$<deflongname>[0].ast;
        $past.name($name);
    }
    elsif $*MULTINESS {
        $/.CURSOR.panic('Can not put ' ~ $*MULTINESS ~ ' on anonymous routine');
    }
    make $past;
}


method signature($/) {
    my $signature := Perl6::Compiler::Signature.new();
    for $<parameter> {
        my $param := $_.ast;
        $param.multi_invocant(1);
        $signature.add_parameter($param);
    }
    make $signature;
}

method parameter($/) { 
    my $quant := $<quant>;
    
    # Sanity checks.
    if $<default_value> { 
        if $quant eq '*' { 
            $/.CURSOR.panic("Can't put default on slurpy parameter");
        }
        if $quant eq '!' { 
            $/.CURSOR.panic("Can't put default on required parameter");
        }
    }

    # Set various flags on the parameter.
    $*PARAMETER.pos_slurpy( $quant eq '*' && $*PARAMETER.sigil eq '@' );
    $*PARAMETER.named_slurpy( $quant eq '*' && $*PARAMETER.sigil eq '%' );
    $*PARAMETER.optional( $quant eq '?' || $<default_value> || ($<named_param> && $quant ne '!') );
    if $<default_value> {
        $*PARAMETER.default( PAST::Block.new( $<default_value>[0]<EXPR>.ast ) );
    }
    make $*PARAMETER;
}

method param_var($/) {
    $*PARAMETER.var_name(~$/);
}

method named_param($/) {
    if $<name>               { $*PARAMETER.names.push(~$<name>); }
    if $<param_var><name>    { $*PARAMETER.names.push(~$<param_var><name>); }
}

method type_constraint($/) {
    if $<typename> {
        if pir::substr(~$<typename>, 0, 2) eq '::' {
            $*PARAMETER.type_captures.push(pir::substr(~$<typename>, 2));
        }
        else {
            if $*PARAMETER.nom_type {
                $/.CURSOR.panic('Parameter may only have one prefix type constraint');
            }
            $*PARAMETER.nom_type($<typename>.ast);
        }
    }
    else {
        $/.CURSOR.panic('Can not do non-typename cases of type_constraint yet');
    }
}

method post_constraint($/) {
    if $<signature> {
        if $*PARAMETER.sub_signature {
            $/.CURSOR.panic('Can not have more than one sub-signature for a parameter');
        }
        $*PARAMETER.sub_signature( $<signature>.ast );
    }
    else {
        my $past := $<EXPR>.ast;
        unless $past.isa(PAST::Block) {
            $/.CURSOR.panic('Non-block anonymous sub-types su todo');
        }
        $*PARAMETER.cons_types.push($past);
    }
}

method regex_declarator($/, $key?) {
    my @MODIFIERS := Q:PIR {
        %r = get_hll_global ['Regex';'P6Regex';'Actions'], '@MODIFIERS'
    };
    my $name := ~$<deflongname>.ast;
    my $past;
    if $key eq 'open' {
        my %h;
        if $<sym> eq 'token' { %h<r> := 1; }
        if $<sym> eq 'rule'  { %h<r> := 1;  %h<s> := 1; }
        @MODIFIERS.unshift(%h);
        Q:PIR {
            $P0 = find_lex '$name'
            set_hll_global ['Regex';'P6Regex';'Actions'], '$REGEXNAME', $P0
        };
        @BLOCK[0].symbol('$¢', :scope('lexical'));
        @BLOCK[0].symbol('$/', :scope('lexical'));
        return 0;
    }
    elsif $<proto> {
        $past :=
            PAST::Stmts.new(
                PAST::Block.new( :name($name),
                    PAST::Op.new(
                        PAST::Var.new( :name('self'), :scope('register') ),
                        $name,
                        :name('!protoregex'),
                        :pasttype('callmethod'),
                    ),
                    :blocktype('method'),
                    :lexical(0),
                    :node($/)
                ),
                PAST::Block.new( :name('!PREFIX__' ~ $name),
                    PAST::Op.new(
                        PAST::Var.new( :name('self'), :scope('register') ),
                        $name,
                        :name('!PREFIX__!protoregex'),
                        :pasttype('callmethod'),
                    ),
                    :blocktype('method'),
                    :lexical(0),
                    :node($/)
                )
            );
    }
    else {
        my $rpast := $<p6regex>.ast;
        my %capnames := Regex::P6Regex::Actions::capnames($rpast, 0);
        %capnames{''} := 0;
        $rpast := PAST::Regex.new(
                     $rpast,
                     PAST::Regex.new( :pasttype('pass') ),
                     :pasttype('concat'),
                     :capnames(%capnames)
        );
        $past := @BLOCK.shift;
        $past.blocktype('method');
        $past.name($name);
        $past.push($rpast);
        @MODIFIERS.shift;
    }
    make $past;
}


method dotty($/) {
    my $past := $<args> ?? $<args>[0].ast !! PAST::Op.new( :node($/) );
    $past.name( ~$<identifier> );
    $past.pasttype('callmethod');
    make $past;
}

## Terms

method term:sym<self>($/) {
    make PAST::Var.new( :name('self') );
}

method term:sym<identifier>($/) {
    my $past := $<args>.ast;
    $past.name('&' ~ $<identifier>);
    make $past;
}

method term:sym<name>($/) {
    my $ns := Perl6::Grammar::parse_name(~$<name>);
    $ns := Q:PIR { 
               $P0 = find_lex '$ns'
               %r = clone $P0
           };
    my $name := $ns.pop;
    my $var := 
        PAST::Var.new( :name(~$name), :namespace($ns), :scope('package') );
    my $past := $var;
    if $<args> {
        $past := $<args>[0].ast;
        if $ns { $past.unshift($var); }
        else { $past.name('&' ~ $name); }
    }
    make $past;
}

method term:sym<pir::op>($/) {
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
    my $past := PAST::Op.new( :pasttype('call'), :node($/) );
    if $<EXPR> {
        my $expr := $<EXPR>.ast;
        if $expr.name eq '&infix:<,>' && !$expr.named {
            for $expr.list { $past.push($_); }
        }
        else { $past.push($expr); }
    }
    make $past;
}


method term:sym<value>($/) { make $<value>.ast; }

method circumfix:sym<( )>($/) { make $<EXPR>.ast; }

method circumfix:sym<ang>($/) { make $<quote_EXPR>.ast; }

method circumfix:sym<{ }>($/) { make $<pblock>.ast; }

method circumfix:sym<[ ]>($/) {
    make PAST::Op.new( :name('&circumfix:<[ ]>'), $<EXPR>.ast, :node($/) );
}

method circumfix:sym<sigil>($/) {
    my $name := ~$<sigil> eq '@' ?? 'list' !!
                ~$<sigil> eq '%' ?? 'hash' !!
                                    'item';
    make PAST::Op.new( :pasttype('callmethod'), :name($name), $<semilist>.ast );
}

method semilist($/) { make $<statement>.ast }

method infixish($/) {
    if $<infix_postfix_meta_operator> {
        my $sym := ~$<infix><sym>;
        my $opsub := "&infix:<$sym=>";
        unless %*METAOPGEN{$opsub} {
            @BLOCK[0].loadinit.push(
                PAST::Op.new( :name('!gen_assign_metaop'), $sym, 
                              :pasttype('call') )
            );
            %*METAOPGEN{$opsub} := 1;
        }
        make PAST::Op.new( :name($opsub), :pasttype('call') );
    }
}

method postcircumfix:sym<[ ]>($/) {
    make PAST::Op.new( $<EXPR>.ast, :name('!postcircumfix:<[ ]>'),
                       :pasttype('call'), :node($/) );
}

method postcircumfix:sym<{ }>($/) {
    make PAST::Var.new( $<EXPR>.ast , :scope('keyed'),
                        :viviself('Undef'),
                        :vivibase('Hash') );
}

method postcircumfix:sym<ang>($/) {
    make PAST::Var.new( $<quote_EXPR>.ast, :scope('keyed'),
                        :viviself('Undef'),
                        :vivibase('Hash') );
}

method postcircumfix:sym<( )>($/) {
    make $<arglist>.ast;
}

method value($/) {
    my $past := $<quote>
                ?? $<quote>.ast
                !! PAST::Val.new( :value($<integer>.ast) );
    make $past;
}

method typename($/) {
    my @name := Perl6::Grammar::parse_name($<longname>.Str);
    my $past := PAST::Var.new(
        :name(@name.pop),
        :namespace(@name),
        :scope('package')
    );
    make $past;
}

method quote:sym<apos>($/) { make $<quote_EXPR>.ast; }
method quote:sym<dblq>($/) { make $<quote_EXPR>.ast; }
method quote:sym<qq>($/)   { make $<quote_EXPR>.ast; }
method quote:sym<q>($/)    { make $<quote_EXPR>.ast; }
method quote:sym<Q>($/)    { make $<quote_EXPR>.ast; }
method quote:sym<Q:PIR>($/) {
    make PAST::Op.new( :inline( $<quote_EXPR>.ast.value ),
                       :pasttype('inline'),
                       :node($/) );
}

method quote_escape:sym<$>($/) { make $<variable>.ast; }

# overrides versions from HLL::Actions to handle Perl6Str
# and use &infix:<,> to build the parcel
method quote_EXPR($/) {
    my $past := $<quote_delimited>.ast;
    if HLL::Grammar::quotemod_check($/, 'w') {
        if !$past.isa(PAST::Val) {
            $/.CURSOR.panic("Can't form :w list from non-constant strings (yet)");
        }
        else {
            my @words := HLL::Grammar::split_words($/, $past.value);
            if +@words > 1 {
                $past := PAST::Op.new( :name('&infix:<,>'), :node($/) );
                for @words { $past.push($_); }
            }
        }
    }
    make $past;
}
     

method quote_delimited($/) {
    my @parts;
    my $lastlit := '';
    for $<quote_atom> {
        my $ast := $_.ast;
        if !HLL::Actions::isaPAST($ast) {
            $lastlit := $lastlit ~ $ast;
        }
        elsif $ast.isa(PAST::Val) {
            $lastlit := $lastlit ~ $ast.value;
        }
        else {
            if $lastlit gt '' {
                @parts.push(
                    PAST::Val.new( :value($lastlit), :returns('Perl6Str') ) 
                ); 
            }
            @parts.push($ast);
            $lastlit := '';
        }
    }
    if $lastlit gt '' || !@parts { 
        @parts.push(
            PAST::Val.new( :value($lastlit), :returns('Perl6Str') ) 
        ); 
    }
    my $past := @parts.shift;
    while @parts {
        $past := PAST::Op.new( $past, @parts.shift, :pirop('concat') );
    }
    make $past;
}


## Operators

method nulltermish($/) {
    make $<term> ?? $<term>.ast !! 0;
}

method postfix:sym<.>($/) { make $<dotty>.ast; }


class Perl6::RegexActions is Regex::P6Regex::Actions {

    method metachar:sym<:my>($/) {
        my $past := $<statement>.ast;
        make PAST::Regex.new( $past, :pasttype('pastnode') );
    }

    method metachar:sym<{ }>($/) { make $<codeblock>.ast; }

    method assertion:sym<{ }>($/) { make $<codeblock>.ast; }

    method codeblock($/) {
        my $block := $<block>.ast;
        $block.blocktype('immediate');
        my $past := 
            PAST::Regex.new(
                PAST::Stmts.new(
                    PAST::Op.new(
                        PAST::Var.new( :name('$/') ),
                        PAST::Op.new(
                            PAST::Var.new( :name('$¢') ),
                            :name('MATCH'),
                            :pasttype('callmethod')
                        ),
                        :pasttype('bind')
                    ),
                    $block
                ),
                :pasttype('pastnode')
            );
        make $past;
    }
}

sub add_signature($block, $sig_obj) {
    # Add signature building code to the block.
    $block.loadinit.push($sig_obj.ast);
    $block.loadinit.push(PAST::Op.new( :inline('    setprop block, "$!signature", signature') ));

    # Add call to signature binder as well as lexical declarations
    # to the start of the block.
    $block[0].push(PAST::Var.new( :name('call_sig'), :scope('parameter'), :call_sig(1) ));
    my $decls := $sig_obj.get_declarations();
    for @($decls) { $block.symbol( $_.name, :scope('lexical') ) }
    $block[0].push($decls);
    $block[0].push(PAST::Op.new(
        :pirop('bind_signature vP'),
        PAST::Var.new( :name('call_sig'), :scope('lexical') )
    ));
}

sub wrap_parrot_sub($block, $type) {
    PAST::Op.new(
        :pasttype('callmethod'),
        :name('new'),
        PAST::Var.new( :name($type), :scope('package') ),
        $block
    );
}
