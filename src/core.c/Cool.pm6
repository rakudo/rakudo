my class Cool { # declared in BOOTSTRAP
    # class Cool is Any

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
    method is-prime(--> Bool:D) { self.Real.is-prime }

    proto method log(|) {*}
    multi method log(Cool:D: )      { self.Numeric.log          }
    multi method log(Cool:D: $base) { self.Numeric.log($base.Numeric) }

    proto method exp(|) {*}
    multi method exp(Cool:D: )      { self.Numeric.exp          }
    multi method exp(Cool:D: $base) { self.Numeric.exp($base.Numeric) }

    proto method round(|) {*}
    multi method round()      { self.Numeric.round()      }
    multi method round($base) { self.Numeric.round($base) }

    method roots(Cool $n)   { self.Numeric.roots($n)    }
    method log2()           { self.Numeric.log2         }
    method log10()          { self.Numeric.log10        }
    method unpolar($n)      { self.Numeric.unpolar($n.Numeric) }

    method floor()          { self.Numeric.floor        }
    method ceiling()        { self.Numeric.ceiling      }
    method truncate()       { self.Numeric.truncate     }

    ## string methods

    proto method chars(*%) {*}
    multi method chars(Cool:D: --> Int:D) { self.Str.chars }

    proto method codes(*%) {*}
    multi method codes(Cool:D: --> Int:D) { self.Str.codes }

    proto method encode($?, *%) {*}
    multi method encode(Cool:D: |c) { self.Str.encode(|c) }

    method fmt(Str(Cool) $format = '%s') {
        Rakudo::Internals.initialize-sprintf-handler;
        nqp::p6box_s(
            nqp::sprintf(nqp::unbox_s($format), nqp::list(self))
        )
    }

    proto method wordcase(*%) {*}
    multi method wordcase(Cool:D:) { self.Str.wordcase(|%_) }

    proto method trans(|) { $/ := nqp::getlexcaller('$/'); {*} }
    multi method trans(Cool:D: |c) { self.Str.trans(|c) }

    proto method indent($, *%) {*}
    multi method indent(Cool:D: $steps) { self.Str.indent($steps) }

    proto method uc(*%) {*}
    multi method uc(Cool:D:) { self.Str.uc }

    proto method lc(*%) {*}
    multi method lc(Cool:D:) { self.Str.lc }

    proto method tc(*%) {*}
    multi method tc(Cool:D:) { self.Str.tc }

    proto method fc(*%) {*}
    multi method fc(Cool:D:) { self.Str.fc }

    proto method tclc(*%) {*}
    multi method tclc(Cool:D:) { self.Str.tclc }

    proto method flip(*%) {*}
    multi method flip(Cool:D:) { self.Str.flip }

    proto method chomp(*%) {*}
    multi method chomp(Cool:D:) { self.Str.chomp }

    proto method chop(|)                {*}
    multi method chop(Cool:D:)          { self.Str.chop }
    multi method chop(Cool:D: Int() $n) { self.Str.chop($n) }

    proto method samecase($, *%) {*}
    multi method samecase(Cool:D: Cool:D $pattern) {
        self.Str.samecase($pattern)
    }

    proto method samemark($, *%) {*}
    multi method samemark(Cool:D: Cool:D $pattern) {
        self.Str.samemark($pattern)
    }

    proto method samespace($, *%) {*}
    multi method samespace(Cool:D: Cool:D $pattern) {
        self.Str.samespace($pattern)
    }

    proto method starts-with(|) {*}
    multi method starts-with(Cool:D:
      Cool:D $needle, :i(:$ignorecase)!, :m(:$ignoremark) --> Bool:D) {
        self.Str.starts-with($needle.Str, :$ignorecase, :$ignoremark)
    }
    multi method starts-with(Cool:D:
      Cool:D $needle, :m(:$ignoremark)! --> Bool:D) {
        self.Str.starts-with($needle.Str, :$ignoremark)
    }
    multi method starts-with(Cool:D: Cool:D $needle --> Bool:D) {
        self.Str.starts-with($needle.Str)
    }

    proto method ends-with(|) {*}
    multi method ends-with(Cool:D:
      Cool:D $suffix, :i(:$ignorecase)!, :m(:$ignoremark) --> Bool:D) {
        self.Str.ends-with($suffix.Str, :$ignorecase, :$ignoremark)
    }
    multi method ends-with(Cool:D:
      Cool:D $suffix, :m(:$ignoremark)! --> Bool:D) {
        self.Str.ends-with($suffix.Str, :$ignoremark)
    }
    multi method ends-with(Cool:D: Cool:D $suffix --> Bool:D) {
        self.Str.ends-with($suffix.Str)
    }

    proto method substr(|) {*}
    multi method substr(Cool:D:)               { self.Str.substr             }
    multi method substr(Cool:D: \from)         { self.Str.substr(from)       }
    multi method substr(Cool:D: \from, \chars) { self.Str.substr(from,chars) }

    proto method substr-rw(|) {*}
    multi method substr-rw(Cool:D \SELF:) is rw {
        (SELF = self.Str).substr-rw
    }
    multi method substr-rw(Cool:D \SELF: \from) is rw {
        (SELF = self.Str).substr-rw(from)
    }
    multi method substr-rw(Cool:D \SELF: \from, \want) is rw {
        (SELF = self.Str).substr-rw(from, want)
    }

    proto method substr-eq(|) {*}
    multi method substr-eq(Cool:D:
      Cool:D $needle, :i(:$ignorecase)!, :m(:$ignoremark) --> Bool:D) {
        self.Str.starts-with($needle.Str, :$ignorecase, :$ignoremark)
    }
    multi method substr-eq(Cool:D:
      Cool:D $needle, :m(:$ignoremark) --> Bool:D) {
        self.Str.starts-with($needle.Str, :$ignoremark)
    }
    multi method substr-eq(Cool:D: Cool:D $needle --> Bool:D) {
        self.Str.starts-with($needle.Str)
    }

    multi method substr-eq(Cool:D:
      Cool:D $needle, Cool:D $pos, :i(:$ignorecase)!, :m(:$ignoremark)
    --> Bool:D) {
        self.Str.substr-eq($needle.Str, $pos.Int, :$ignorecase, :$ignoremark)
    }
    multi method substr-eq(Cool:D:
      Cool:D $needle, Cool:D $pos, :m(:$ignoremark)!  --> Bool:D) {
        self.Str.substr-eq($needle.Str, $pos.Int, :$ignoremark)
    }
    multi method substr-eq(Cool:D: Cool:D $needle, Cool:D $pos --> Bool:D) {
        self.Str.substr-eq($needle.Str, $pos.Int)
    }

    method !list-as-string($suggestion) is hidden-from-backtrace {
        warn "Calling '.{callframe(2).code.name}' on a {self.^name}, did you mean '$suggestion'?";
    }

    proto method contains(|) {*}
    multi method contains(List:D: Cool:D \needle) {  # Warn about newbie trap
        self!list-as-string('needle (elem) list');
        self.Str.contains: needle.Str, |%_
    }
    multi method contains(Cool:D:
      Cool:D $needle, :i(:$ignorecase)!, :m(:$ignoremark) --> Bool:D) {
        self.Str.contains: $needle.Str, :$ignorecase, :$ignoremark
    }
    multi method contains(Cool:D:
      Cool:D $needle, :m(:$ignoremark)! --> Bool:D) {
        self.Str.contains: $needle.Str, :$ignoremark
    }
    multi method contains(Cool:D: Cool:D $needle --> Bool:D) {
        self.Str.contains: $needle.Str
    }
    multi method contains(Cool:D: Regex:D $needle --> Bool:D) {
        self.Str.contains: $needle
    }

    multi method contains(Cool:D:
      Cool:D $needle, Cool:D $pos, :i(:$ignorecase)!, :m(:$ignoremark)
    --> Bool:D) {
        self.Str.contains($needle.Str, $pos.Int, :$ignorecase, :$ignoremark)
    }
    multi method contains(Cool:D:
      Cool:D $needle, Cool:D $pos, :m(:$ignoremark)! --> Bool:D) {
        self.Str.contains($needle.Str, $pos.Int, :$ignoremark)
    }
    multi method contains(Cool:D: Cool:D $needle, Cool:D $pos --> Bool:D) {
        self.Str.contains($needle.Str, $pos.Int)
    }
    multi method contains(Cool:D: Regex:D $needle, Cool:D $pos --> Bool:D) {
        self.Str.contains($needle, $pos)
    }

    proto method indices(|) {*}
    multi method indices(List:D: Cool:D \needle) {  # Warn about newbie trap
        self!list-as-string('.grep( ..., :k)');
        self.Str.indices(needle.Str, |%_)
    }
    multi method indices(Cool:D: Cool:D $needle,
      :i(:$ignorecase)!, :m(:$ignoremark), :$overlap) {
        self.Str.indices($needle.Str, :$ignorecase, :$ignoremark, :$overlap)
    }
    multi method indices(Cool:D: Cool:D $needle,
      :m(:$ignoremark)!, :$overlap) {
        self.Str.indices($needle.Str, :$ignoremark, :$overlap)
    }
    multi method indices(Cool:D: Cool:D $needle, :$overlap) {
        self.Str.indices($needle.Str, :$overlap)
    }

    multi method indices(Cool:D: Cool:D $needle, Cool:D $pos,
      :i(:$ignorecase), :m(:$ignoremark), :$overlap) {
        self.Str.indices($needle.Str, $pos.Int,
          :$ignorecase, :$ignoremark, :$overlap)
    }
    multi method indices(Cool:D: Cool:D $needle, Cool:D $pos,
     :m(:$ignoremark)!, :$overlap) {
        self.Str.indices($needle.Str, $pos.Int, :$ignoremark, :$overlap)
    }
    multi method indices(Cool:D: Cool:D $needle, Cool:D $pos, :$overlap) {
        self.Str.indices($needle.Str, $pos.Int, :$overlap)
    }

    proto method index(|) {*}
    multi method index(List:D: Cool:D $needle) {  # Warn about newbie trap
        self!list-as-string('.first( ..., :k)');
        self.Str.index(nqp::istype($needle,List) ?? $needle !! $needle.Str,|%_)
    }
    multi method index(Cool:D:
      Cool:D $needle, :i(:$ignorecase)!, :m(:$ignoremark) --> Int:D) {
        self.Str.index(
          nqp::istype($needle,List) ?? $needle !! $needle.Str,
          :$ignorecase,
          :$ignoremark
        )
    }
    multi method index(Cool:D:
      Cool:D $needle, :m(:$ignoremark)! --> Int:D) {
        self.Str.index(
          nqp::istype($needle,List) ?? $needle !! $needle.Str,
          :$ignoremark
        )
    }
    multi method index(Cool:D: Cool:D $needle --> Int:D) {
        self.Str.index(nqp::istype($needle,List) ?? $needle !! $needle.Str)
    }

    multi method index(Cool:D:
      Cool:D $needle, Cool:D $pos, :i(:$ignorecase)!, :m(:$ignoremark)
    --> Int:D) {
        self.Str.index: $needle.Str, $pos.Int, :$ignorecase, :$ignoremark
    }
    multi method index(Cool:D:
      Cool:D $needle, Cool:D $pos, :m(:$ignoremark)!  --> Int:D) {
        self.Str.index: $needle.Str, $pos.Int, :$ignoremark
    }
    multi method index(Cool:D: Cool:D $needle, Cool:D $pos --> Int:D) {
        self.Str.index: $needle.Str, $pos.Int
    }

    proto method rindex(|) {*}
    multi method rindex(List:D: Cool:D $needle) {  # Warn about newbie trap
        self!list-as-string('.first( ..., :k, :end)');
        self.Str.rindex(nqp::istype($needle,List) ?? $needle !! $needle.Str,|%_)
    }
    multi method rindex(Cool:D: Cool:D $needle --> Int:D) {
        self.Str.rindex: nqp::istype($needle,List) ?? $needle !! $needle.Str
    }
    multi method rindex(Cool:D: Cool:D $needle, Cool:D $pos --> Int:D) {
        self.Str.rindex:
          nqp::istype($needle,List) ?? $needle !! $needle.Str,
          $pos.Int
    }

    method split(Cool: |c) {
        self.Str.split(|c);
    }

    method match(Cool:D: |c) {
        $/ := nqp::getlexcaller('$/');
        self.Str.match(|c)
    }

    proto method comb(|) {*}
    multi method comb(Cool:D: --> Seq:D) {
        self.Str.comb
    }
    multi method comb(Cool:D: Cool:D $size, $limit = * --> Seq:D) {
        self.Str.comb($size.Int, $limit)
    }
    multi method comb(Cool:D: Int:D $size, $limit = * --> Seq:D) {
        self.Str.comb($size, $limit)
    }
    multi method comb(Cool:D: Cool:D $pat --> Seq:D) {
        self.Str.comb($pat.Str)
    }
    multi method comb(Cool:D: Str:D $pat --> Seq:D) {
        self.Str.comb($pat)
    }
    multi method comb(Cool:D: Cool:D $pat, $limit --> Seq:D) {
        self.Str.comb($pat.Str, $limit)
    }
    multi method comb(Cool:D: Str:D $pat, $limit --> Seq:D) {
        self.Str.comb($pat, $limit)
    }
    multi method comb(Cool:D: Regex:D $pattern, :$match --> Seq:D) {
        self.Str.comb($pattern, :$match)
    }
    multi method comb(Cool:D: Regex:D $pattern, $limit, :$match --> Seq:D) {
        self.Str.comb($pattern, $limit, :$match)
    }

    proto method lines(|) {*}
    multi method lines(Cool:D:)           { self.Str.lines          }
    multi method lines(Cool:D: :$count! ) { self.Str.lines(:$count) }
    multi method lines(Cool:D: $limit )   { self.Str.lines($limit)  }

    proto method words(|) {*}
    multi method words(Cool:D:)         { self.Str.words         }
    multi method words(Cool:D: $limit ) { self.Str.words($limit) }

    proto method subst(|) {
        $/ := nqp::getlexcaller('$/');
        {*}
    }
    multi method subst(Cool:D: $original, $replacement = "", *%options) {
        $/ := nqp::getlexcaller('$/');
        self.Str.subst($original, $replacement, |%options);
    }

    # `$value-to-subst-mutate` will show up in errors when called on non-rw
    # container, so use more descriptive name instead of just `$self`
    method subst-mutate(Cool:D $value-to-subst-mutate is rw: |c) {
        $/ := nqp::getlexcaller('$/');
        my $str   = $value-to-subst-mutate.Str;
        my $match := $str.subst-mutate(|c);
        $value-to-subst-mutate = $str if $match;  # only change if successful
        $match
    }

    proto method IO(|) {*}
    multi method IO(Cool:D:) { IO::Path.new(self) }
    multi method IO(Cool:U:) { IO::Path }

    method sprintf(*@args) { sprintf(self, @args) };
    method printf (*@args) {  printf(self, @args) };

    proto method trim(*%) {*}
    multi method trim(Cool:D:) { self.Str.trim }

    proto method trim-leading(*%) {*}
    multi method trim-leading(Cool:D:) { self.Str.trim-leading }

    proto method trim-trailing(*%) {*}
    multi method trim-trailing(Cool:D:) { self.Str.trim-trailing }

    method EVAL(*%opts) {
        EVAL(self, context => CALLER::, |%opts);
    }

    multi method Real() {
        nqp::istype((my $numeric := self.Numeric),Failure)
          ?? $numeric
          !! $numeric.Real
    }

    proto method Int(|) {*}
    multi method Int()  {
        nqp::istype((my $numeric := self.Numeric),Failure)
          ?? $numeric
          !! $numeric.Int
    }

    proto method UInt(|) {*}
    multi method UInt()  {
        nqp::istype((my $got := self.Int),Failure)
          ?? $got
          !! $got < 0
            ?? Failure.new(X::OutOfRange.new(
                 :what('Coercion to UInt'),
                 :$got,
                 :range<0..^Inf>
               ))
            !! $got
    }

    method Num()  {
        nqp::istype((my $numeric := self.Numeric),Failure)
          ?? $numeric
          !! $numeric.Num
    }

    method Rat()  {
        nqp::istype((my $numeric := self.Numeric),Failure)
          ?? $numeric
          !! $numeric.Rat
    }

    method FatRat()  {
        nqp::istype((my $numeric := self.Numeric),Failure)
          ?? $numeric
          !! $numeric.FatRat
    }

    method Complex()  {
        nqp::istype((my $numeric := self.Numeric),Failure)
          ?? $numeric
          !! $numeric.Complex
    }
}
Metamodel::ClassHOW.exclude_parent(Cool);

proto sub chop($, $?, *%) {*}
multi sub chop($s --> Str:D) { $s.chop }
multi sub chop($s, Int() $n --> Str:D) { $s.chop($n) }

proto sub chomp($, *%) {*}
multi sub chomp($s --> Str:D) { $s.chomp }

proto sub flip($, *%) {*}
multi sub flip($s --> Str:D) { $s.flip }

proto sub index($, $, $?, *%) {*}
multi sub index($s,
  Cool:D $needle, :i(:$ignorecase), :m(:$ignoremark) --> Int:D) {
    $s.index($needle, :$ignorecase, :$ignoremark)
}
multi sub index($s,
  Cool:D $needle, Cool:D $pos, :i(:$ignorecase), :m(:$ignoremark) --> Int:D) {
    $s.index($needle, $pos, :$ignorecase, :$ignoremark)
}

proto sub rindex($, $, $?, *%) {*}
multi sub rindex($s, Cool:D $needle --> Int:D) {
    $s.rindex($needle)
}
multi sub rindex($s, Cool:D $needle, Cool:D $pos --> Int:D) {
    $s.rindex($needle,$pos)
}

proto sub lc($, *%) {*}
multi sub lc($s) { $s.lc }

proto sub uc($, *%) {*}
multi sub uc($s) { $s.uc }

proto sub tc($, *%) {*}
multi sub tc($s) { $s.tc }

proto sub fc($, *%) {*}
multi sub fc($s) { $s.fc }

proto sub tclc($, *%) {*}
multi sub tclc($s) { $s.tclc }

proto sub indices($, $, $?, *%) {*}
multi sub indices($s,
  Cool:D $needle, :i(:$ignorecase), :m(:$ignoremark), :$overlap) {
    $s.indices($needle, :$ignorecase, :$ignoremark, :$overlap)
}
multi sub indices($s,
  Cool:D $needle, Cool:D $pos, :i(:$ignorecase), :m(:$ignoremark), :$overlap) {
    $s.indices($needle, $pos, :$ignorecase, :$ignoremark, :$overlap)
}

proto sub comb($, $, $?, *%) {*}
multi sub comb(Regex $matcher, $input, $limit = *, :$match) {
    $input.comb($matcher, $limit, :$match)
}
multi sub comb(Str $matcher, $input, $limit = *) {
    $input.comb($matcher, $limit)
}
multi sub comb(Int:D $size, $input, $limit = *) {
    $input.comb($size, $limit)
}

proto sub wordcase($, *%) is pure {*}
multi sub wordcase($x) { $x.wordcase }

proto sub sprintf($, |) {*}
multi sub sprintf(Str(Cool) $format, *@args) {
    CATCH {
        when X::Cannot::Lazy {
            X::Cannot::Lazy.new(:action('(s)printf')).throw
        }
        default {
            Rakudo::Internals.HANDLE-NQP-SPRINTF-ERRORS($_).throw
        }
    }
    Rakudo::Internals.initialize-sprintf-handler;
    nqp::p6box_s(
      nqp::sprintf(
        nqp::unbox_s($format),
        @args.elems
          ?? nqp::clone(nqp::getattr(@args,List,'$!reified'))
          !! nqp::create(IterationBuffer)
      )
    )
}

proto sub samecase($, $, *%) {*}
multi sub samecase($s, Cool:D $pattern) { $s.samecase($pattern) }

proto sub split($, $, |) {*}
multi sub split($pat, $target, |c) { c ?? $target.split($pat, |c) !! $target.split($pat) }

proto sub chars($, *%) is pure {*}
multi sub chars(Str:D $x) { nqp::p6box_i(nqp::chars($x)) } #?js: NFG
multi sub chars(str $x --> int) { nqp::chars($x) } #?js: NFG
multi sub chars($x)  { $x.chars }

# vim: expandtab shiftwidth=4
