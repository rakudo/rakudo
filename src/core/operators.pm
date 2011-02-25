our multi infix:<~~>(Mu $topic, Mu $matcher) {
    ? $matcher.ACCEPTS($topic)
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

our multi infix:sym<orelse>(Mu $a, Mu $b) {
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


our sub _HELPER_generate-series(@lhs, $rhs , :$exclude-limit) {
    my sub get-next-closure (@lhs, $limit? ) {
        fail "Need something on the LHS" unless @lhs.elems;
        fail "Need more items on the LHS" if @lhs[*-1] ~~ Code && @lhs[*-1] !~~ Multi && @lhs[*-1].count != Inf && @lhs.elems < @lhs[*-1].count;

        #BEWARE: Here be ugliness
        if @lhs[* - 1] ~~ Code { # case: (a,b,c,{code}) ... *
            return @lhs[*-1] if @lhs[*-1] !~~ Multi;
            return @lhs[*-1].candidates[0] if @lhs[*-1].candidates.elems == 1;
            if (@lhs[*-1].candidates>>.count).grep( * == 2) {
                return { @lhs[*-1]($^a,$^b) } ; # case: (a,b,c,&[+] ... *
            } else {
                fail "Don't know how to handle Multi on the lhs yet";
            }
        }
        return { .succ }  if @lhs.elems == 1 && $limit ~~ Code;
        return { $_ } if @lhs.elems > 1 && @lhs[*-1] cmp @lhs[*-2] == 0 ;  # case: (a , a) ... *

        if  @lhs[*-1] ~~ Str ||  $limit ~~ Str {
            if @lhs[*-1].chars == 1 && $limit.defined && $limit.chars == 1 {
                return { .ord.succ.chr } if @lhs[*-1] lt  $limit;# case (... , non-number) ... limit
                return { .ord.pred.chr } if @lhs[*-1] gt  $limit;# case (... , non-number) ... limit
            }
            return { .succ } if $limit.defined && @lhs[*-1] lt  $limit;# case (... , non-number) ... limit
            return { .pred } if $limit.defined && @lhs[*-1] gt  $limit;# case (... , non-number) ... limit
            return { .pred } if @lhs.elems > 1 && @lhs[*-2] gt  @lhs[*-1];# case (non-number , another-smaller-non-number) ... *
            return { .succ } ;# case (non-number , another-non-number) ... *
        }
        return { .pred } if @lhs.elems == 1 && $limit.defined && $limit before @lhs[* - 1];  # case: (a) ... b where b before a
        return { .succ } if @lhs.elems == 1 ;  # case: (a) ... *

        my $diff = @lhs[*-1] - @lhs[*-2];
        return { $_ + $diff } if @lhs.elems == 2 || @lhs[*-2] - @lhs[*-3] == $diff ; #Case Arithmetic series

        if @lhs[*-2] / @lhs[*-3] == @lhs[*-1] / @lhs[*-2] { #Case geometric series
            my $factor = @lhs[*-2] / @lhs[*-3];
            if $factor ~~ ::Rat && $factor.denominator == 1 {
                $factor = $factor.Int;
            }
            return { $_ * $factor } ;
        }
        fail "Unable to figure out pattern of series";
    }

    my sub infinite-series (@lhs, $limit ) {
        gather {
            my $i = 0;
            while @lhs[$i+1].defined { take @lhs[$i]; $i++; } #We blindly take all elems of the LHS except last one.
            if @lhs[$i] !~~ Code { take @lhs[$i]; $i++; }     #We take the last element only when it is not code

            my $next = get-next-closure(@lhs , $limit );
            my $arity = $next.count;
            my @args=@lhs[$i-($arity ~~ Inf ?? $i !! $arity) .. $i-1]; #We make sure there are $arity elems in args

            loop {                         #Then we extrapolate using $next and the $args
                my $current = $next.(|@args) // last;
                take $current ;
                if $arity {
                    @args.push($current) ;
                    @args.munch(1) if @args.elems > $arity
                }
            }
        }
    }

    my $limit = ($rhs ~~ Whatever ?? Any !! $rhs);
    return infinite-series(@lhs , $limit) if $rhs ~~ Whatever; #shortcut infinite series so we avoid the comparisions

    die 'Sequence limit cannot be a multi-sub or multi-method' if $limit ~~ Multi;
    my $series = infinite-series(@lhs , $limit);

    gather {
        if $limit ~~ Code && $limit.count > 1 {
            my @limit-args;
            while $series {
                @limit-args.shift if @limit-args == $limit.count;
                my $val = $series.shift;
                @limit-args.push($val);
                my $done = @limit-args >= $limit.arity && $limit(|@limit-args);
                take $val unless $done && $exclude-limit;
                last if $done;
            }
        }
        else {
            while $series {
                my $val = $series.shift();
                if $val ~~ $limit {
                    take $val unless $exclude-limit ;
                    last ;
                };
                take $val;
            }
        }
    }
}

our multi sub infix:<...>(@lhs, $limit) {
    _HELPER_generate-series(@lhs, $limit )
}
our multi sub infix:<...^>(@lhs, $limit) {
    _HELPER_generate-series(@lhs, $limit , :exclude-limit)
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
    my ($a, $b);
    gather while ?$lhs-list && ?$rhs-list {
        $a = $lhs-list.shift unless $lhs-list[0] ~~ ::Whatever;
        take -> $q is copy { $q }($a);
          # Workaround for RT #77302.
        $b = $rhs-list.shift unless $rhs-list[0] ~~ ::Whatever;
        take -> $q is copy { $q }($b);
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
# the grammar. Same goes for 'or', '&&', 'and', '^^', and 'xor'

our multi sub infix:<||>(Mu $a, Mu $b) { $a || $b }
our multi sub infix:<or>(Mu $a, Mu $b) { $a or $b }
our multi sub infix:<&&>(Mu $a, Mu $b) { $a && $b }
our multi sub infix:<and>(Mu $a, Mu $b) { $a and $b }
our multi sub infix:<^^>(Mu $a, Mu $b) { $a ^^ $b }
our multi sub infix:<xor>(Mu $a, Mu $b) { $a xor $b }

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
