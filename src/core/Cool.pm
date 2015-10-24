my role  IO         { ... }
my class IO::Path   { ... }

my class SprintfHandler {
    method mine($x) { nqp::reprname($x) eq "P6opaque"; }

    method int($x) { $x.Int }
}

my $sprintfHandlerInitialized = False;

my class Cool { # declared in BOOTSTRAP
    # class Cool is Any {

    ## numeric methods

    method abs()  { self.Numeric.abs }
    method conj()  { self.Numeric.conj }
    method sqrt()  { self.Numeric.sqrt }
    method sign()  { self.Real.sign }
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

    proto method log(|) {*}
    multi method log(Cool:D: )      { self.Numeric.log          }
    multi method log(Cool:D: $base) { self.Numeric.log($base.Numeric) }

    proto method exp(|) {*}
    multi method exp(Cool:D: )      { self.Numeric.exp          }
    multi method exp(Cool:D: $base) { self.Numeric.exp($base.Numeric) }

    proto method round(|) { * }
    multi method round()      { self.Numeric.round()      }
    multi method round($base) { self.Numeric.round($base) }

    method roots(Cool $n)   { self.Numeric.roots($n)    }
    method log10()          { self.Numeric.log10        }
    method unpolar($n)      { self.Numeric.unpolar($n.Numeric) }

    method floor()          { self.Numeric.floor        }
    method ceiling()        { self.Numeric.ceiling      }
    method truncate()       { self.Numeric.truncate     }

    ## string methods

    method chars() returns Int:D {
        self.Str.chars
    }
    method codes() {
        self.Str.codes
    }

    method fmt($format = '%s') {
        unless $sprintfHandlerInitialized {
            nqp::sprintfaddargumenthandler(SprintfHandler.new);
            $sprintfHandlerInitialized = True;
        }
        nqp::p6box_s(
            nqp::sprintf(nqp::unbox_s($format.Stringy), nqp::list(self))
        )
    }

    method substr($from, $length?)           { substr(   self,$from,$length) }
    method substr-rw(\SELF: $from, $length?) { substr-rw(SELF,$from,$length) }

    method uc() {
        self.Str.uc
    }

    method lc() {
        self.Str.lc
    }

    method tc() {
        self.Str.tc
    }

    method fc() {
        self.Str.fc
    }

    method tclc() {
        self.Str.tclc
    }

    method wordcase()   { self.Str.wordcase }

    method uniname()        { uniname(self) }
    method uninames()       { uninames(self) }
    method unival()         { unival(self) }
    method univals()        { univals(self) }
    method uniprop(|c)      { uniprop(self, |c) }
    method uniprop-int(|c)  { uniprop-int(self, |c) }
    method uniprop-bool(|c) { uniprop-bool(self, |c) }
    method uniprop-str(|c)  { uniprop-str(self, |c) }
    method unimatch(|c)     { unimatch(self, |c) }

    method chomp() {
        self.Str.chomp;
    }

    method chop(Int() $n = 1) {
        self.Str.chop($n)
    }

    method ord(--> Int) {
        self.Str.ord
    }
    method chr() {
        self.Int.chr;
    }
    method chrs(Cool:D:) {
        self>>.chr.join;
    }
    method ords(Cool:D:) { self.Str.ords }


    method flip() {
        self.Str.flip
    }
    method trans(*@a) { self.Str.trans(@a) }

    proto method starts-with(|) {*}
    multi method starts-with(Str:D: Str(Cool) $needle) {
        nqp::p6bool(
          nqp::eqat(nqp::unbox_s(self),nqp::unbox_s($needle),0)
        );
    }
    multi method starts-with(Cool:D: Str(Cool) $needle) {
        nqp::p6bool(
          nqp::eqat(nqp::unbox_s(self.Str),nqp::unbox_s($needle),0)
        );
    }

    proto method ends-with(Str(Cool) $suffix) { * }
    multi method ends-with(Str:D: Str(Cool) $suffix) {
        my str $str    = nqp::unbox_s(self);
        my str $needle = nqp::unbox_s($suffix);
        nqp::p6bool(
          nqp::eqat($str,$needle,nqp::chars($str) - nqp::chars($needle))
        );
    }
    multi method ends-with(Cool:D: Str(Cool) $suffix) {
        my str $str    = nqp::unbox_s(self.Str);
        my str $needle = nqp::unbox_s($suffix);
        nqp::p6bool(
          nqp::eqat($str,$needle,nqp::chars($str) - nqp::chars($needle))
        );
    }

    proto method substr-eq(|) {*}
    multi method substr-eq(Str:D: Str(Cool) $needle, Int(Cool) $pos) {
        $pos >= 0 && nqp::p6bool(
          nqp::eqat(nqp::unbox_s(self),nqp::unbox_s($needle),nqp::unbox_i($pos))
        );
    }
    multi method substr-eq(Cool:D: Str(Cool) $needle, Int(Cool) $pos) {
        $pos >= 0 && nqp::p6bool(nqp::eqat(
          nqp::unbox_s(self.Str),
          nqp::unbox_s($needle),
          nqp::unbox_i($pos)
        ));
    }

    proto method contains(|) {*}
    multi method contains(Cool:D: Str(Cool) $needle) {
        nqp::index(nqp::unbox_s(self.Str), nqp::unbox_s($needle)) != -1;
    }
    multi method contains(Cool:D: Str(Cool) $needle, Int(Cool) $pos) {
        my str $str = nqp::unbox_s(self.Str);
        $pos >= 0
          && $pos <= nqp::chars($str)
          && nqp::index($str, nqp::unbox_s($needle), nqp::unbox_i($pos)) != -1;
    }

    proto method indices(|) {*}
    multi method indices(Cool:D: Str(Cool) $needle, Int(Cool) $start = 0, :$overlap) {
        my int $pos  = $start;
        my str $str  = nqp::unbox_s(self.Str);
        my str $need = nqp::unbox_s($needle);
        my int $add  = $overlap ?? 1 !! nqp::chars($need) || 1;

        my $rpa := nqp::list();
        my int $i;
        loop {
            $i = nqp::index($str, $need, $pos);
            last if $i == -1;
            nqp::push($rpa,nqp::box_i($i,Int));
            $pos = $i + $add;
        }
        nqp::p6bindattrinvres(nqp::create(List), List, '$!reified', $rpa)
    }

    proto method index(|) {*}
    multi method index(Cool:D: Str(Cool) $needle) {
        my int $i = nqp::index(nqp::unbox_s(self.Str), nqp::unbox_s($needle));
        $i < 0 ?? Nil !! nqp::box_i($i,Int);
    }
    multi method index(Cool:D: Str(Cool) $needle, Int(Cool) $pos) {
        my int $i;
        try {
            $i = nqp::unbox_i($pos);
            CATCH {
                default {
                    return Nil
                }
            }
        }
        $i = nqp::index(
          nqp::unbox_s(self.Str),
          nqp::unbox_s($needle),
          $i
        );
        $i < 0 ?? Nil !! nqp::box_i($i,Int);
    }

    proto method rindex(|) {*}
    multi method rindex(Cool:D: Str(Cool) $needle) {
        my int $i = nqp::rindex(nqp::unbox_s(self.Str), nqp::unbox_s($needle));
        $i < 0 ?? Nil !! nqp::box_i($i,Int);
    }
    multi method rindex(Cool:D: Str(Cool) $needle, Int(Cool) $pos) {
        my int $i;
        try {
            $i = nqp::unbox_i($pos);
            CATCH {
                default {
                    return Nil
                }
            }
        }
        $i = nqp::rindex(
          nqp::unbox_s(self.Str),
          nqp::unbox_s($needle),
          $i
        );
        $i < 0 ?? Nil !! nqp::box_i($i,Int);
    }

    multi method split(Cool: Regex:D $pat, $limit = Inf;; :$all) {
        self.Stringy.split($pat, $limit, :$all);
    }
    multi method split(Cool: Cool:D $pat, $limit = Inf;; :$all) {
        self.Stringy.split($pat.Stringy, $limit, :$all);
    }
    proto method match(|) {*}
    multi method match(Cool:D: $target, *%adverbs) {
        self.Stringy.match($target, |%adverbs)
    }

    proto method comb(|) { * }
    multi method comb() { self.Str.comb() }
    multi method comb(Regex $matcher, $limit = Inf) { self.Str.comb($matcher, $limit) }
    multi method comb(Str $matcher, $limit = Inf) { self.Str.comb($matcher, $limit) }

    proto method lines(|) {*}
    multi method lines(Cool:D: |c) { self.Str.lines(|c) }

    proto method words(|) {*}
    multi method words(Cool:D: |c) { self.Str.words(|c) }

    proto method subst(|) {
        $/ := nqp::getlexdyn('$/');
        {*}
    }
    multi method subst($matcher, $replacement, *%adverbs) {
        $/ := nqp::getlexdyn('$/');
        self.Stringy.subst($matcher, $replacement, |%adverbs);
    }

    proto method subst-mutate(|c) {
        $/ := nqp::getlexdyn('$/');
        {*}
    }
    multi method subst-mutate($self is rw: |c) {
        $/ := nqp::getlexdyn('$/');
        my $str = Str($self);
        my $match = $str.subst-mutate(|c);
        $self = $str;
        $match;
    }

    proto method IO(|) { * }
    multi method IO(|c) { IO::Path.new(self) }

    method sprintf(*@args) { sprintf(self, @args) };
    method printf (*@args) {  printf(self, @args) };
    method samecase(Cool:D: Cool $pattern) { self.Stringy.samecase($pattern) }

    method path() { self.Stringy.IO }
    method trim         () { self.Stringy.trim          };
    method trim-leading () { self.Stringy.trim-leading  };
    method trim-trailing() { self.Stringy.trim-trailing };

    method EVAL(*%opts) {
        EVAL(self, context => CALLER::, |%opts);
    }

    multi method Real() { self.Numeric.Real }

    proto method Int(|) { * }
    multi method Int()  { self.Numeric.Int }

    proto method UInt(|) { * }
    multi method UInt()  {
        my $got := self.Int;
        fail X::OutOfRange.new(
          :what('Coercion to UInt'),
          :$got,
          :range("0..Inf")
        ) if $got < 0;
        $got;
    }

    method Num()  { self.Numeric.Num }
    method Rat()  { self.Numeric.Rat }
}
Metamodel::ClassHOW.exclude_parent(Cool);

sub chop(Cool $s, Int() $n = 1) returns Str { $s.chop($n) }
sub chomp(Cool $s) returns Str     { $s.chomp }
sub flip(Cool $s) returns Str      { $s.flip }
sub index(Cool $s,$needle,$pos=0)  { $s.index($needle,$pos) }
sub lc(Cool $s)                    { $s.lc }
sub ord(Cool $s)                   { $s.ord }
sub uc(Cool $s)                    { $s.uc }
sub tc(Cool $s)                    { $s.tc }
sub fc(Cool $s)                    { $s.fc }
sub tclc(Cool $s)                  { $s.tclc }

sub indices(Cool $s,$needle,$pos=0,:$overlap) {
    $s.indices($needle,$pos,:$overlap);
}

proto sub rindex($, $, $?) is pure { * };
multi sub rindex(Cool $s, Cool $needle, Cool $pos) { $s.rindex($needle, $pos) };
multi sub rindex(Cool $s, Cool $needle)            { $s.rindex($needle) };

proto sub ords($) is pure     { * }
multi sub ords(Cool $s)       { ords($s.Stringy) }

proto sub comb($, $, $?)            { * }
multi sub comb(Regex $matcher, Cool $input, $limit = *) { $input.comb($matcher, $limit) }
multi sub comb(Str $matcher, Cool $input, $limit = *) { $input.comb($matcher, $limit) }

proto sub wordcase($) is pure { * }
multi sub wordcase(Str:D $x) {$x.wordcase }
multi sub wordcase(Cool $x)  {$x.Str.wordcase }

sub sprintf(Cool $format, *@args) {
    unless $sprintfHandlerInitialized {
        nqp::sprintfaddargumenthandler(SprintfHandler.new);
        $sprintfHandlerInitialized = True;
    }

    @args.elems;
    nqp::p6box_s(
        nqp::sprintf(nqp::unbox_s($format.Stringy),
            nqp::clone(nqp::getattr(@args, List, '$!reified'))
        )
    );
}

sub printf(Cool $format, *@args) { print sprintf $format, @args };
sub samecase(Cool $string, Cool $pattern) { $string.samecase($pattern) }
sub split($pat, Cool $target, $limit = Inf, :$all) {
    $target.split($pat, $limit, :$all);
}

proto sub chars($) is pure {*}
multi sub chars(Cool $x)  { $x.Str.chars }
multi sub chars(Str:D $x) { nqp::p6box_i(nqp::chars($x)) }
multi sub chars(str $x) returns int { nqp::chars($x) }

# vim: ft=perl6 expandtab sw=4
