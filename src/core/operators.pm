our multi infix:<~~>(Mu $topic, Mu $matcher) {
    $matcher.ACCEPTS($topic)
}

our multi infix:<~~>(Mu $topic, Regex $matcher) {
    Q:PIR {
        $P0 = find_lex '$matcher'
        $P1 = find_lex '$topic'
        %r = $P0.'ACCEPTS'($P1)
        store_dynamic_lex '$/', %r
    };
}

our multi prefix:<?>(Mu $a) {
    pir::can($a, 'Bool')
    ?? $a.Bool
    !!  ( pir::istrue($a) ?? True !! False );
}

our multi prefix:<!>(Mu $a) {
    $a.Bool ?? False !! True;
}

our multi sub prefix:<->($a) {
    pir::neg__NN($a)
}

our multi sub infix:<+>($a, $b) {
    +$a + +$b;
}

our multi sub infix:<->($a, $b) {
    +$a - +$b;
}

our multi sub infix:<*>($a, $b) {
    +$a * +$b;
}

our multi sub infix:</>($a, $b) {
    +$a / +$b;
}

our multi sub infix:<%>($a, $b) {
    +$a % +$b;
}

our multi sub infix:<%%>($a, $b) {
    +$a % +$b == 0;
}

our multi sub infix:<**>($a, $b) {
    (+$a) ** +$b; # parenthesis needed because of precendence.
}

our multi sub infix:<&>(*@items) {
    Junction.new(@items, :all)
}

our multi sub infix:<|>(*@items) {
    Junction.new(@items, :any)
}

our multi sub infix:<^>(*@items) {
    Junction.new(@items, :one)
}

our multi sub infix:<+&>($a, $b) {
    pir::band__III($a, $b);
}

our multi sub infix:<+|>($a, $b) {
    pir::bor__III($a, $b);
}

our multi sub infix:<+^>($a, $b) {
    pir::bxor__III($a, $b);
}

our multi sub infix:«+<»($a, $b) {
    pir::shl__III($a, $b);
}

our multi sub infix:«+>»($a, $b) {
    pir::shr__III($a, $b);
}

our multi sub infix:<~|>($a, $b) {
    pir::bors__SSS($a, $b);
}

our multi sub infix:<~&>($a, $b) {
    pir::bands__SSS($a, $b);
}

our multi sub infix:<~^>($a, $b) {
    pir::bxors__SSS($a, $b);
}

our sub all(*@items) {
    Junction.new(@items, :all);
}

our sub any(*@items) {
    Junction.new(@items, :any);
}

our sub one(*@items) {
    Junction.new(@items, :one);
}

our sub none(*@items) {
    Junction.new(@items, :none);
}

our multi prefix:<not>(Mu $x) { !$x }

our multi prefix:<so>(Mu $x) { ?$x }

our multi prefix:sym<+^>($x) {
    pir::bnot__PP($x)
}

our sub undefine(Mu \$x) {
    my $undefined;
    $x = $undefined;
}

our multi infix:<does>(Mu \$doee, Role $r) {
    &infix:<does>($doee, $r!select)
}

our multi infix:<does>(Mu \$doee, ConcreteRole $r) {
    my $applicator = $r.^applier_for($doee);
    $applicator.apply($doee, [$r]);
    $doee
}

our multi infix:<does>(Mu \$doee, Parcel $roles) {
    my $*SCOPE = 'my';
    my $mr = RoleHOW.new();
    for @($roles) -> $r {
        $mr.^add_composable($r);
    }
    my $r = $mr.^compose();
    $doee does $r;
}

our multi infix:<does>(Mu \$doee, \$value) {
    # Need to manufacture a role here.
    my $r = RoleHOW.new();
    $r.^add_method($value.WHAT.perl, method () { $value });
    $doee does $r.^compose()
}

our multi infix:<but>(Mu \$doee, \$r) {
    $doee.clone() does $r
}

our multi infix:<before>($a, $b) {
    ($a cmp $b) == -1;
}

our multi infix:<after>($a, $b) {
    ($a cmp $b) == +1;
}

our multi infix:<?|>($a, $b) {
    ?(?$a +| ?$b)
}

our multi infix:<?&>($a, $b) {
    ?(?$a +& ?$b)
}

our multi infix:<?^>($a, $b) {
    ?(?$a +^ ?$b)
}

our multi infix:<min>(*@args) {
    @args.min;
}

our multi infix:<max>(*@args) {
    @args.max;
}

our multi infix:<minmax>(*@args) {
    @args.minmax;
}

our multi infix:«=>»($key, Mu $value) {
    Pair.new(key => $key, value => $value);
}

our multi infix:<~>($a, $b) {
    my $result = pir::new__Ps('Str');
    pir::assign__vPS($result, pir::concat__SSS(~$a, ~$b));
    $result
}

our sub circumfix:<{ }>(*@elements) {
    my %h = @elements;
    %h.item
}

our sub hash(*@list, *%hash) {
    my %h = (@list, %hash);
    %h
}

our multi infix:sym<//>(Mu $a, Mu $b) {
    $a.defined ?? $a !! $b
}

our multi infix:<==>($a, $b) {
    +$a == +$b;
}

our multi infix:<!=>(Mu $a, Mu $b) {
    $a !== $b
}

our multi infix:«<»($a, $b) {
    +$a < +$b;
}

our multi infix:«<=»($a, $b) {
    +$a <= +$b;
}

our multi infix:«>»($a, $b) {
    +$a > +$b;
}

our multi infix:«>=»($a, $b) {
    +$a >= +$b;
}

our multi infix:<eq>($a, $b) {
    pir::iseq__ISS(~$a, ~$b) ?? True !! False
}

our multi infix:<ne>(Mu $a, Mu $b) {
    $a !eq $b
}

our multi infix:<lt>($a, $b) {
    pir::islt__ISS(~$a, ~$b) ?? True !! False
}

our multi infix:<le>($a, $b) {
    pir::isle__ISS(~$a, ~$b) ?? True !! False
}

our multi infix:<gt>($a, $b) {
    pir::isgt__ISS(~$a, ~$b) ?? True !! False
}

our multi infix:<ge>($a, $b) {
    pir::isge__ISS(~$a, ~$b) ?? True !! False
}

# XXX Lazy version would be nice in the future too.
class Whatever { ... }

our multi infix:<xx>(Mu \$item, Whatever) {
    (1..*).map( { $item } )
}

our multi infix:<xx>(Mu \$item, $n) {
    (1..+$n).map( { $item } )
}


our multi prefix:<|>(@a) { @a.Capture }
our multi prefix:<|>(%h) { %h.Capture }
our multi prefix:<|>(Capture $c) { $c }
our multi prefix:<|>(Mu $anything) { Capture.new($anything) }

our multi infix:<:=>(Mu \$target, Mu \$source) {
    #Type Checking. The !'s avoid putting actual binding in a big nest.
    if !pir::isnull(pir::getprop__PsP('type', $target)) {
        if !pir::getprop__PsP('type', $target).ACCEPTS($source) {
            die("You cannot bind a variable of type {$source.WHAT} to a variable of type {$target.WHAT}.");
        }
    }

    if !pir::isnull(pir::getprop__PsP('WHENCE', pir::descalarref__PP($target)))
        { pir::getprop__PsP('WHENCE', pir::descalarref__PP($target)).() }

    #and now, for the actual process
    pir::setprop__0PsP(
        pir::copy__0PP($target, pir::new__PsP('ObjectRef', $source)),
        'rw',
        pir::getprop__PsP('rw', $source)
    );
}

our multi infix:<:=>(Signature $s, Parcel \$p) {
    $s!BIND($p.Capture());
}

our multi infix:<:=>(Signature $s, Mu \$val) {
    $s!BIND(Capture.new($val));
}

our multi infix:<::=>(Mu \$target, Mu \$source) {
    #since it's only := with setting readonly, let's avoid recoding.
    $target := $source;
    #XX pay attention to this little guy, we don't quite understand or are
    #able to implement the full details of ::=
    pir::delprop__0Ps($target, 'rw');
}

# XXX Wants to be a macro when we have them.
our sub WHAT(\$x) {
    $x.WHAT
}

our multi sub item(*@values) {
    @values.Seq
}
our multi sub item(@values) {
    @values.Seq
}
our multi sub item($item) {
    $item
}


our sub _HELPER_get-series-params (@lhs, $limit? ) {
	fail "Need something on the LHS" unless @lhs.elems;
	fail "Need more than one item on the LHS" if @lhs.elems == 1 && $limit ~~ Code;

	#TODO: Maybe should return an Enum for the info part)
	return ( 'code' , @lhs[*-1])  if @lhs[* - 1] ~~ Code ; # case: (a,b,c,{code}) ... *
	return ( 'stag' , { $_ } ) if @lhs.elems > 1 && @lhs[*-1] cmp @lhs[*-2] == 0 ;  # case: (a , a) ... *

	if  @lhs[*-1] ~~ Str ||  $limit ~~ Str {
		if ($limit.defined) {
	        if @lhs[*-1].chars == 1 && $limit.chars == 1 {
				return ( 'char-succ' , { $_.ord.succ.chr } ) if @lhs[*-1] lt  $limit;# case (... , non-number) ... limit
				return ( 'char-pred' , { $_.ord.pred.chr } ) if @lhs[*-1] gt  $limit;# case (... , non-number) ... limit
			}
			return ( 'text-succ' , { $_.succ } ) if @lhs[*-1] lt  $limit;# case (... , non-number) ... limit
			return ( 'text-pred' , { $_.pred } ) if @lhs[*-1] gt  $limit;# case (... , non-number) ... limit
		}
		return ( 'text-pred' , { $_.pred } ) if @lhs.elems > 1 && @lhs[*-2] gt  @lhs[*-1];# case (non-number , another-smaller-non-number) ... *
		return ( 'text-succ' , { $_.succ } ) ;# case (non-number , another-non-number) ... *
	}
	return ( 'pred' , { $_.pred } ) if @lhs.elems == 1 && $limit.defined && $limit before @lhs[* - 1];  # case: (a) ... b where b before a
	return ( 'succ' , { $_.succ } ) if @lhs.elems == 1 ;  # case: (a) ... *

	my $diff = @lhs[*-1] - @lhs[*-2];
	return ('arithmetic' , { $_ + $diff } ) if @lhs.elems == 2 || @lhs[*-2] - @lhs[*-3] == $diff ; #Case Arithmetic series

	if @lhs[*-2] / @lhs[*-3] == @lhs[*-1] / @lhs[*-2] { #Case geometric series
		my $factor = @lhs[*-2] / @lhs[*-3];
		if $factor ~~ ::Rat && $factor.denominator == 1 {
			$factor = $factor.Int;
		}
		if ($factor < 0) {
			return ( 'geometric-switching-sign' , { $_ * $factor } );
		} else {
			return ( 'geometric-same-sign' , { $_ * $factor } );
		}
	}
	fail "Unable to figure out pattern of series";
}

our sub _HELPER_infinite-series (@lhs, $next , $type) {
	my $arity = any( $next.signature.params>>.slurpy ) ?? Inf !! $next.count;

	my @args=@lhs;
	pop @args if $type eq 'code';;
	gather {
		take $_ for @args; #First we take the lhs
		loop {                         #Then we extrapolate using $next
			@args.shift while @args.elems > $arity ;
			my $current = $next.(|@args) // last;
			take $current ;
			@args.push($current) if $arity;
		}
    }
}

our sub _HELPER_limited-series(@lhs, $limit , :$exclude-limit) {
	my sub limit-reached($previous , $current , $limit , $get-value-to-compare) {
		return $limit($current) if ($limit ~~ Code) ; #TODO Check arity to know how many items to pass

		my $current_cmp = $get-value-to-compare($limit) cmp $get-value-to-compare($current);
		return $current_cmp == 0 unless $previous.defined;
		my $previous_cmp =  $get-value-to-compare($limit) cmp $get-value-to-compare($previous) ;

		return ($current_cmp == 0 #We reached the limit exactly
				|| $previous_cmp != $current_cmp) ; #We went past the limit
	}

	my sub is-on-the-wrong-side($type , $get-value-to-compare, $limit , @lhs ) {
		my $first = @lhs[*-3] // @lhs[0];

		if $type eq 'arithmetic' | 'geometric-switching-sign' | 'geometric-same-sign' {
			($get-value-to-compare(@lhs[*-2]) >= $get-value-to-compare(@lhs[*-1]) && $get-value-to-compare($limit) > $get-value-to-compare($first) )
			||
			($get-value-to-compare(@lhs[*-2]) <= $get-value-to-compare(@lhs[*-1]) && $get-value-to-compare($limit) < $get-value-to-compare($first) );
		}
	}


	my ($type , $next) = _HELPER_get-series-params(@lhs , $limit );
	my $get-value-to-compare = $type eq 'geometric-switching-sign' ?? { $_.abs; } !! { $_; };
	return Nil if @lhs.elems > 1 && is-on-the-wrong-side($type , $get-value-to-compare,  $limit , @lhs);

	my $series = _HELPER_infinite-series(@lhs , $next , $type);
	my $previous ;
	gather {
		while $series {
			my $val = $series.shift();
			if limit-reached($previous , $val , $limit , $get-value-to-compare) {
				take $val if $get-value-to-compare($val) cmp $get-value-to-compare($limit) == 0  && !$exclude-limit;
				last ;
			};
			take $val;
			$previous = $val;
		}
	}
}


our multi sub infix:<...>(@lhs, Whatever) {
	my ($type , $next) = _HELPER_get-series-params(@lhs);
	_HELPER_infinite-series(@lhs , $next , $type);
}
our multi sub infix:<...>(@lhs, $limit) {
	_HELPER_limited-series(@lhs, $limit )
}
our multi sub infix:<...^>(@lhs, $limit) {
	_HELPER_limited-series(@lhs, $limit , :exclude-limit)
}
our multi sub infix:<...^>($lhs , $limit) {
	$lhs.list ...^ $limit;
}
our multi sub infix:<...^>(@lhs, @rhs) {
    fail "Need something on RHS" if !@rhs;
    (@lhs ...^ @rhs.shift), @rhs
}

our multi sub infix:<...>($lhs, $rhs) {
    $lhs.list ... $rhs;
}

our multi sub infix:<...>($lhs, @rhs is copy) {
    fail "Need something on RHS" if !@rhs;
    ($lhs ... @rhs.shift), @rhs
}

our multi sub infix:<...>(@lhs, @rhs is copy) {
    fail "Need something on RHS" if !@rhs;
    (@lhs ... @rhs.shift), @rhs
}

our multi sub infix:<eqv>(Mu $a, Mu $b) {
    $a.WHAT === $b.WHAT && $a === $b;
}

our multi sub infix:<eqv>(@a, @b) {
    unless @a.WHAT === @b.WHAT && @a.elems == @b.elems {
        return Bool::False
    }
    for @a.keys -> $i {
        unless @a[$i] eqv @b[$i] {
            return Bool::False;
        }
    }
    Bool::True
}

our multi sub infix:<eqv>(Pair $a, Pair $b) {
    $a.key eqv $b.key && $a.value eqv $b.value;
}

our multi sub infix:<eqv>(Capture $a, Capture $b) {
    @($a) eqv @($b) && %($a) eqv %($b)
}

class EnumMap { ... }
our multi sub infix:<eqv>(EnumMap $a, EnumMap $b) {
    if +$a != +$b { return Bool::False }
    for $a.kv -> $k, $v {
        unless $b.exists($k) && $b{$k} eqv $v {
            return Bool::False;
        }
    }
    Bool::True;
}

our multi sub infix:<eqv>(Numeric $a, Numeric $b) {
    $a.WHAT === $b.WHAT && ($a cmp $b) == 0;
}

our multi sub infix:<Z>($lhs, $rhs) {
    my $lhs-list = flat($lhs.list);
    my $rhs-list = flat($rhs.list);
    gather while ?$lhs-list && ?$rhs-list {
        my $a = $lhs-list.shift;
        my $b = $rhs-list.shift;
        take $a;
        take $b;
    }
}

our multi sub infix:<X>($lhs, $rhs) {
    my $lhs-list = flat($lhs.list);
    my $rhs-list = flat($rhs.list);
    gather while ?$lhs-list {
        my $a = $lhs-list.shift;
        for @($rhs-list) -> $b {
            my $b-copy = $b;
            take ($a, $b-copy);
        }
    }
}

# if we want &infix:<||> accessible (for example for meta operators), we need
# to define it, because the normal || is short-circuit and special cased by
# the grammar. Same goes for 'or', '&&' and 'and'

our multi sub infix:<||>(Mu $a, Mu $b) { $a || $b }
our multi sub infix:<or>(Mu $a, Mu $b) { $a or $b }
our multi sub infix:<&&>(Mu $a, Mu $b) { $a && $b }
our multi sub infix:<and>(Mu $a, Mu $b) { $a and $b }

# Eliminate use of this one, but keep the pir around for
# the moment, as it may come in handy elsewhere.
#
# multi sub infix_prefix_meta_operator:<!>($a, $b, $c) {
#     !(pir::get_hll_global__CS($a)($b, $c));
# }

our multi sub infix:«<==»($a, $b) {
    die "Sorry, feed operators not yet implemented";
}

our multi sub infix:«==>»($a, $b) {
    die "Sorry, feed operators not yet implemented";
}

our multi sub infix:«<<==»($a, $b) {
    die "Sorry, feed operators not yet implemented";
}

our multi sub infix:«==>>»($a, $b) {
    die "Sorry, feed operators not yet implemented";
}
