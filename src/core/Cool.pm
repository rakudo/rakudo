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
        Rakudo::Internals.initialize-sprintf-handler;
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

    method chomp(Cool:D:) { self.Str.chomp }

    proto method chop(|)                { * }
    multi method chop(Cool:D:)          { self.Str.chop }
    multi method chop(Cool:D: Int() $n) { self.Str.chop($n) }

    method ord(--> Int) {
        self.Str.ord
    }
    method chr() {
        self.Int.chr;
    }
    method chrs(Cool:D:) { chrs(self.list) }
    method ords(Cool:D:) { self.Str.ords }


    method flip() {
        self.Str.flip
    }
    method trans(|c) { self.Str.trans(|c) }

    proto method starts-with(|) {*}
    multi method starts-with(Cool:D: Str(Cool) $needle) {
        self.Str.starts-with($needle)
    }

    proto method ends-with(|) {*}
    multi method ends-with(Cool:D: Str(Cool) $suffix) {
        self.Str.ends-with($suffix)
    }

    proto method substr-eq(|) {*}
    multi method substr-eq(Cool:D: Str(Cool) $needle, Cool $pos = 0) {
        self.Str.substr-eq($needle,$pos)
    }

    proto method contains(|) {*}
    multi method contains(Cool:D: Str(Cool) $needle, Cool $pos = 0) {
        self.Str.contains($needle,$pos.Int)
    }

    proto method indices(|) {*}
    multi method indices(Cool:D: Str(Cool) $needle, :$overlap) {
        self.Str.indices($needle,:$overlap)
    }
    multi method indices(Cool:D: Str(Cool) $needle,Int(Cool) $start,:$overlap) {
        self.Str.indices($needle,$start,:$overlap)
    }

    proto method index(|) {*}
    multi method index(Cool:D: Str(Cool) $needle) {
        self.Str.index($needle)
    }
    multi method index(Cool:D: Str(Cool) $needle, Int(Cool) $pos) {
        self.Str.index($needle,$pos)
    }

    proto method rindex(|) {*}
    multi method rindex(Cool:D: Str(Cool) $needle) {
        self.Str.rindex($needle)
    }
    multi method rindex(Cool:D: Str(Cool) $needle, Int(Cool) $pos) {
        self.Str.rindex($needle,$pos)
    }

    method split(Cool: |c) {
        self.Stringy.split(|c);
    }

    proto method match(|) {
        $/ := nqp::getlexdyn('$/');
        {*}
    }
    multi method match(Cool:D: $target, *%adverbs) {
        $/ := nqp::getlexdyn('$/');
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

    proto method subst-mutate(|) {
        $/ := nqp::getlexdyn('$/');
        {*}
    }
    multi method subst-mutate(
      Cool:D $self is rw: $matcher, $replacement, *%named
    ) {
        $/ := nqp::getlexdyn('$/');
        my $str   = $self.Str;
        my $match = $str.subst-mutate($matcher,$replacement,|%named);
        $self     = $str;
        $match
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

    multi method Real() {
        nqp::if(
            nqp::istype((my $numeric := self.Numeric), Failure),
            $numeric,
            $numeric.Real
        )
    }

    proto method Int(|) { * }
    multi method Int()  {
        nqp::if(
            nqp::istype((my $numeric := self.Numeric), Failure),
            $numeric,
            $numeric.Int
        )
    }

    proto method UInt(|) { * }
    multi method UInt()  {
        my $got := self.Int;
        $got < 0
          ?? Failure.new(X::OutOfRange.new(
               :what('Coercion to UInt'),
               :$got,
               :range("0..Inf")))
          !! $got
    }

    method Num()  {
        nqp::if(
            nqp::istype((my $numeric := self.Numeric), Failure),
            $numeric,
            $numeric.Num
        )
    }

    method Rat()  {
        nqp::if(
            nqp::istype((my $numeric := self.Numeric), Failure),
            $numeric,
            $numeric.Rat
        )
    }
}
Metamodel::ClassHOW.exclude_parent(Cool);

proto sub chop(|) { * }
multi sub chop(Cool:D $s)           returns Str { $s.chop }
multi sub chop(Cool:D $s, Int() $n) returns Str { $s.chop($n) }

sub chomp(Cool:D $s) returns Str { $s.chomp }

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
multi sub comb(Int:D $matcher, Cool $input, $limit = *) { $input.comb($matcher, $limit) }

proto sub wordcase($) is pure { * }
multi sub wordcase(Str:D $x) {$x.wordcase }
multi sub wordcase(Cool $x)  {$x.Str.wordcase }

sub sprintf(Cool $format, *@args) {
    CATCH {
        when X::Cannot::Lazy {
            X::Cannot::Lazy.new(:action('(s)printf')).throw
        }
        default {
            Rakudo::Internals.HANDLE-NQP-SPRINTF-ERRORS($_).throw
        }
    }
    Rakudo::Internals.initialize-sprintf-handler;
    @args.elems;
    nqp::p6box_s(
        nqp::sprintf(nqp::unbox_s($format.Stringy),
            nqp::clone(nqp::getattr(@args, List, '$!reified'))
        )
    )
}

sub printf(Cool $format, *@args)          { print sprintf $format, @args }
sub samecase(Cool $string, Cool $pattern) { $string.samecase($pattern) }
sub split($pat, Cool $target, |c)         { $target.split($pat, |c) }

proto sub chars($) is pure {*}
multi sub chars(Cool $x)  { $x.Str.chars }
multi sub chars(Str:D $x) { nqp::p6box_i(nqp::chars($x)) }
multi sub chars(str $x) returns int { nqp::chars($x) }

# vim: ft=perl6 expandtab sw=4
