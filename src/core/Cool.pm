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

    method chars() {
        nqp::p6box_i(nqp::chars(nqp::unbox_s(self.Str)));
    }
    method codes() {
        nqp::p6box_i(nqp::chars(nqp::unbox_s(self.Str)));
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
        nqp::p6box_s(nqp::uc(nqp::unbox_s(self.Str)))
    }

    method lc() {
        nqp::p6box_s(nqp::lc(nqp::unbox_s(self.Str)))
    }

    method tc() {
        my $u := nqp::unbox_s(self.Str);
        nqp::p6box_s(nqp::uc(nqp::substr($u,0,1)) ~ nqp::substr($u,1));
    }

    method tclc() {
        nqp::p6box_s(nqp::tclc(nqp::unbox_s(self.Str)))
    }

    method wordcase()   { self.Str.wordcase }

    method chomp() {
        self.Str.chomp;
    }

    method chop() {
        self.Str.chop
    }

    method ord(--> Int) {
        my $s := self.Str;
        $s.chars
          ?? nqp::p6box_i(nqp::ord(nqp::unbox_s($s)))
          !! Int;
    }
    method chr() {
        self.Int.chr;
    }
    method chrs(Cool:D:) {
        self>>.chr.join;
    }

    method flip() {
        nqp::p6box_s(nqp::flip(nqp::unbox_s(self.Str)))
    }
    method trans(*@a) { self.Str.trans(@a) }

    proto method index(|) {*}
    multi method index(Cool $needle, Cool $pos = 0) {
        if $needle eq '' {
            my $chars = self.chars;
            return $pos < $chars ?? $pos !! $chars;
        }
        my int $result = nqp::index(
                nqp::unbox_s(self.Str),
                nqp::unbox_s($needle.Str),
                nqp::unbox_i($pos.Int)
        );
        # TODO: fail() instead of returning Int
        $result < 0 ?? Int !! nqp::p6box_i($result);
    }

    proto method rindex(|) {*}
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
    proto method split(|) {*}
    multi method split(Regex $pat, $limit = Inf, :$all) {
        self.Stringy.split($pat, $limit, :$all);
    }
    multi method split(Cool $pat, $limit = Inf, :$all) {
        self.Stringy.split($pat.Stringy, $limit, :$all);
    }
    proto method match(|) {*}
    multi method match(Cool:D: $target, *%adverbs) {
        self.Stringy.match($target, |%adverbs)
    }

    proto method comb(|) {*}
    multi method comb() { self.Str.comb() }
    multi method comb(Regex $matcher, $limit = Inf) { self.Str.comb($matcher, $limit) }

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
    method Num()  { self.Numeric.Num }
    method Rat()  { self.Numeric.Rat }
}
Metamodel::ClassHOW.exclude_parent(Cool);

sub chop(Cool $s) returns Str      { $s.chop }
sub chomp(Cool $s) returns Str     { $s.chomp }
sub flip(Cool $s) returns Str      { $s.flip }
sub index(Cool $s,$needle,$pos=0)  { $s.index($needle,$pos) }
sub lc(Cool $s)                    { $s.lc }
sub ord(Cool $s)                   { $s.ord }
sub uc(Cool $s)                    { $s.uc }
sub tc(Cool $s)                    { $s.tc }
sub tclc(Cool $s)                  { $s.tclc }

proto sub rindex($, $, $?) is pure { * };
multi sub rindex(Cool $s, Cool $needle, Cool $pos) { $s.rindex($needle, $pos) };
multi sub rindex(Cool $s, Cool $needle)            { $s.rindex($needle) };

proto sub ords($) is pure     { * }
multi sub ords(Cool $s)       { ords($s.Stringy) }

proto sub comb($, $, $?)            { * }
multi sub comb(Regex $matcher, Cool $input, $limit = *) { $input.comb($matcher, $limit) }

proto sub wordcase($) is pure { * }
multi sub wordcase(Str:D $x) {$x.wordcase }
multi sub wordcase(Cool $x)  {$x.Str.wordcase }

sub sprintf(Cool $format, *@args) {
    unless $sprintfHandlerInitialized {
        nqp::sprintfaddargumenthandler(SprintfHandler.new);
        $sprintfHandlerInitialized = True;
    }

    @args.gimme(*);
    nqp::p6box_s(
        nqp::sprintf(nqp::unbox_s($format.Stringy),
            nqp::clone(nqp::getattr(@args, List, '$!items'))
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
