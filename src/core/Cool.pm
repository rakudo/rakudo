my class IO { ... }

my class Cool {

    ## numeric methods

    method abs()  { self.Numeric.abs }
    method conj()  { self.Numeric.conj }
    method sqrt()  { self.Numeric.sqrt }
    method sign()  { self.Numeric.sign }
    method rand() { self.Num.rand }
    method sin()  { self.Numeric.sin }
    method asin() { self.Numeric.asin }
    method cos()  { self.Numeric.cos }
    method acos() { self.Numeric.acos }
    method tan()  { self.Numeric.tan }
    method atan() { self.Numeric.atan }
    method atan2($y = 1e0) { self.Numeric.atan2($y.Numeric) }
    method sec()  { self.Numeric.sec }
    method asec() { self.Numeric.asec }
    method cosec()  { self.Numeric.cosec }
    method acosec() { self.Numeric.acosec }
    method cotan()  { self.Numeric.cotan }
    method acotan() { self.Numeric.acotan }
    method sinh()  { self.Numeric.sinh }
    method asinh() { self.Numeric.asinh }
    method cosh()  { self.Numeric.cosh }
    method acosh() { self.Numeric.acosh }
    method tanh()  { self.Numeric.tanh }
    method atanh() { self.Numeric.atanh }
    method sech()  { self.Numeric.sech }
    method asech() { self.Numeric.asech }
    method cosech()  { self.Numeric.cosech }
    method acosech() { self.Numeric.acosech }
    method cotanh()  { self.Numeric.cotanh }
    method acotanh() { self.Numeric.acotanh }
    method cis()     { self.Numeric.cis }
    
    proto method log(|$) {*}
    multi method log(Cool:D: )      { self.Numeric.log          }
    multi method log(Cool:D: $base) { self.Numeric.log($base.Numeric) }

    proto method exp(|$) {*}
    multi method exp(Cool:D: )      { self.Numeric.exp          }
    multi method exp(Cool:D: $base) { self.Numeric.exp($base.Numeric) }


    method roots(Cool $n)   { self.Numeric.roots($n)    }
    method log10()          { self.Numeric.log10        }
    method unpolar($n)      { self.Numeric.unpolar($n.Numeric) }

    method round($base = 1) { self.Numeric.round($base) }
    method floor()          { self.Numeric.floor        }
    method ceiling()        { self.Numeric.ceiling      }
    method truncate()       { self.Numeric.truncate     }

    ## string methods

    method bytes() is DEPRECATED {
        nqp::p6box_i(pir::bytelength__IS(nqp::unbox_s(self.Str)));
    }

    method chars() {
        nqp::p6box_i(nqp::chars(nqp::unbox_s(self.Str)));
    }

    method fmt($format = '%s') {
        nqp::p6box_s(
            nqp::sprintf(nqp::unbox_s($format.Stringy), nqp::list(self))
        )
    }

    method substr($start as Int, $length?) { 
        self.Stringy.substr($start, $length);
    }

    method uc() {
        nqp::p6box_s(nqp::uc(nqp::unbox_s(self.Str)))
    }

    method lc() {
        nqp::p6box_s(nqp::lc(nqp::unbox_s(self.Str)))
    }

    method ucfirst() is DEPRECATED {
        my $self-str = self.Str;
        $self-str eq '' ?? '' !! $self-str.substr(0, 1).uc ~ $self-str.substr(1)
    }

    method lcfirst() is DEPRECATED {
        my $self-str = self.Str;
        $self-str eq '' ?? '' !! $self-str.substr(0, 1).lc ~ $self-str.substr(1)
    }

    method capitalize() { self.Stringy.capitalize }

    method chomp() {
        self.Str.chomp;
    }

    method chop() {
        self.Str.chop
    }

    method ord() {
        nqp::p6box_i(nqp::ord(nqp::unbox_s(self.Str)))
    }
    method chr() {
        self.Int.chr;
    }

    method flip() {
        nqp::p6box_s(pir::box__PS(nqp::unbox_s(self.Str)).reverse)
    }
    method trans(*@a) { self.Str.trans(@a) }

    proto method index(|$) {*}
    multi method index(Cool $needle, Cool $pos = 0) {
        if $needle eq '' {
            my $chars = self.chars;
            return $pos < $chars ?? $pos !! $chars;
        }
        my $result := nqp::p6box_i(nqp::index(
                nqp::unbox_s(self.Str),
                nqp::unbox_s($needle.Str),
                nqp::unbox_i($pos.Int)
        ));
        # TODO: fail() instead of returning Int
        $result < 0 ?? Int !! $result;
    }

    proto method rindex(|$) {*}
    multi method rindex(Cool $needle, Cool $pos?) {
        if $needle eq '' {
            return $pos.defined && $pos < self.chars
                    ?? $pos
                    !! self.chars;
        }
        my $result = $pos.defined
            ?? nqp::p6box_i(
                nqp::rindex(
                    nqp::unbox_s(self.Str),
                    nqp::unbox_s($needle.Str),
                    nqp::unbox_i($pos.Int)
                ))
            !! nqp::p6box_i(
                nqp::rindex(
                    nqp::unbox_s(self.Str),
                    nqp::unbox_s($needle.Str),
                ));
        fail "substring not found" if $result < 0;
        $result;
    }

    method ords(Cool:D:) { self.Str.ords }
    proto method split(|$) {*}
    multi method split(Regex $pat, $limit = $Inf, :$all) {
        self.Stringy.split($pat, $limit, :$all);
    }
    multi method split(Cool $pat, $limit = $Inf, :$all) {
        self.Stringy.split($pat.Stringy, $limit, :$all);
    }
    proto method match(|$) {*}
    multi method match(Cool:D: $target, *%adverbs) {
        self.Stringy.match($target, |%adverbs)
    }

    proto method comb(|$) {*}
    multi method comb() { self.Str.comb() }
    multi method comb(Regex $matcher, $limit = $Inf) { self.Str.comb($matcher, $limit) }

    proto method subst(|$) {*}
    multi method subst($matcher, $replacement, *%adverbs) {
        self.Stringy.subst($matcher, $replacement, |%adverbs);
    }

    method sprintf(*@args) { sprintf(self, @args) };
    method printf (*@args) {  printf(self, @args) };
    method samecase(Cool:D: Cool $pattern) { self.Stringy.samecase($pattern) }

    method IO() { IO.new(:path(self.Stringy)) }
    method trim         () { self.Stringy.trim          };
    method trim-leading () { self.Stringy.trim-leading  };
    method trim-trailing() { self.Stringy.trim-trailing };

    method eval(*%opts) {
        eval(self.Stringy, |%opts);
    }

    multi method Real() { self.Numeric.Real }
    method Int()  { self.Numeric.Int }
    method Num()  { self.Numeric.Num }
    method Rat()  { self.Numeric.Rat }
}
Metamodel::ClassHOW.exclude_parent(Cool);

sub chop(Cool $s)                  { $s.chop }
sub chomp(Cool $s)                 { $s.chomp }
sub flip(Cool $s)                  { $s.flip }
sub index(Cool $s,$needle,$pos=0)  { $s.index($needle,$pos) }
sub lc(Cool $s)                    { $s.lc }
sub ord(Cool $s)                   { $s.ord }
sub substr(Cool $s,$pos,$chars?)   { $s.substr($pos,$chars) }
sub uc(Cool $s)                    { $s.uc }

sub lcfirst(Cool $s) is DEPRECATED { $s.lcfirst }
sub ucfirst(Cool $s) is DEPRECATED { $s.ucfirst }

proto sub rindex(|$) { * };
multi sub rindex(Cool $s, Cool $needle, Cool $pos) { $s.rindex($needle, $pos) };
multi sub rindex(Cool $s, Cool $needle)            { $s.rindex($needle) };

proto sub ords(|$)            { * }
multi sub ords(Cool $s)       { ords($s.Stringy) }

proto sub comb(|$)            { * }
multi sub comb(Regex $matcher, Cool $input, $limit = *) { $input.comb($matcher, $limit) }

proto sub capitalize(|$)       { * }
multi sub capitalize(Str:D $x) {$x.capitalize }
multi sub capitalize(Cool $x)  {$x.Stringy.capitalize }

sub sprintf(Cool $format, *@args) {
    @args.gimme(*);
    nqp::p6box_s(
        nqp::sprintf(nqp::unbox_s($format.Stringy),
            nqp::clone(nqp::getattr(@args, List, '$!items'))
        )
    );
}

sub printf(Cool $format, *@args) { print sprintf $format, @args };
sub samecase(Cool $string, Cool $pattern) { $string.samecase($pattern) }
sub split($pat, Cool $target, $limit = $Inf, :$all) {
    $target.split($pat, $limit, :$all);
}

sub chars(Cool $x) { $x.Str.chars }
